#'ready for plotting comorbidity data
#'@import dplyr
#'@import FeatureExtraction
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohortId
#'@export
#'
getConditionCovariate <- function(connectionDetails,
                                  Resultschema,
                                  CDMschema,
                                  cohortTable,
                                  cohortId){

    condition_covariate<- FeatureExtraction::createCovariateSettings(useConditionOccurrenceLongTerm = TRUE, endDays = 30)
    
    covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                           cdmDatabaseSchema = CDMschema,
                                           cohortDatabaseSchema = Resultschema,
                                           cohortTable = cohortTable,
                                           cohortId = cohortId,
                                           rowIdField = "subject_id",
                                           covariateSettings = condition_covariate)
    covariateId <- ff::as.ram(covariateData_ff$covariateRef$covariateId)
    conceptId <- ff::as.ram(covariateData_ff$covariateRef$conceptId)
    conceptIdMapping_df <- data.frame(covariateId,conceptId)
    condition_rawdf <- ff::as.ram(covariateData_ff$covariates)
    condition_df <- condition_rawdf %>% left_join(conceptIdMapping_df, by = "covariateId")
    colnames(condition_df) <- c("subjectId","covariateId","covariateValue","conceptId")

    out <- condition_df %>%
        mutate(cohortDefinitionId = cohortId) %>%
        filter(conceptId %in% unlist(diseaseList$conceptIdSet)) %>%
        mutate(diseaseId = NA)

    for(i in 1:length(diseaseList$diseaseId)){
        if( nrow(out[out$conceptId %in% diseaseList$conceptIdSet[[i]],]) == 0 ) next
        out[out$conceptId %in% diseaseList$conceptIdSet[[i]],]$diseaseId <- diseaseList$diseaseId[[i]]
    }

    out <- out %>%
        select(cohortDefinitionId, subjectId, diseaseId, conceptId)

    return(out)
}

#'ready for calculate co-prevalence and RR
#'@import dplyr
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohort_definition_id_set
#'@export
#'
baseline_comorbidity <- function(connectionDetails,
                                 Resultschema,
                                 CDMschema,
                                 cohortTable,
                                 cohort_definition_id_set){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  comorbidData <- data.frame()
  for(i in cohort_definition_id_set){
    getbaselinecondition <- getConditionCovariate(connectionDetails = connectionDetails,
                                                  CDMschema = CDMschema,
                                                  Resultschema = Resultschema,
                                                  cohortTable = cohortTable,
                                                  cohortId = i)
    if(length(comorbidData) == 0) {
      comorbidData <- getbaselinecondition
    } else {
      comorbidData <- rbind(comorbidData,getbaselinecondition) 
    }
  }
   
  templete <- data.frame(cohortDefinitionId = rep( c(cohort_definition_id_set),each = length(diseaseList$diseaseId) ),
                         diseaseId = rep(diseaseList$diseaseId,length(cohort_definition_id_set) ) )
  
  out <- comorbidData %>%
    group_by(cohortDefinitionId, diseaseId) %>%
    summarise(Count = n_distinct(subjectId)) %>%
    right_join( templete, by = c("cohortDefinitionId","diseaseId")) %>%
    left_join( demographicData %>% group_by(cohortDefinitionId) %>% summarise(totalCount = n_distinct(personId)), by = "cohortDefinitionId" ) %>%
    mutate(notdisease = totalCount - Count) %>%
    mutate(notdisease = if_else(is.na(notdisease),totalCount,notdisease))

  if( sum(is.na(out$Count))>0 ) out[is.na(out$Count),]$Count <- 0
  out<- as.data.frame(out)

  return(out)
}

#'ready for calculate Relative Ratio and CI
#'@import dplyr
#'@import epitools
#'@param comorbManufacData               the result of comorbManufacture code (list)
#'@export
#'
calculateRR <- function(comorbManufacData){

  df <- comorbManufacData
  split_df <- split(df, df$diseaseId)
  RRresult <- lapply(split_df, FUN = function(x){
    simply <- x[,c("Count","notdisease")]
    row.names(simply)<-x$cohortDefinitionId
    RR_cal<-epitools::riskratio(as.matrix(simply))
    c(diseaseName = diseaseList$diseaseName[which(diseaseList$diseaseId == unique(x$diseaseId))],
      RR_cal$measure[2,],
      pvalue = round(RR_cal$p.value[6], 4))
  })
  out <- t(as.data.frame(RRresult))
  out <- data.frame(out)
  out[,'pvalue'] <- as.numeric(as.character(out[,'pvalue'])) 
  
  return(out)
}

#'plot risk ratio and its CIs
#'@import dplyr
#'@import ggplot2
#'@param RRResult       the result of calculateRR code
#'@export
#'
RRplot <- function(RRResult){
  RR <- RRResult %>%
    mutate(comorbId = as.numeric(gsub("X","",row.names(RRResult)) ) ) %>%
    mutate(comorbName = factor(comorbId, levels = diseaseList$diseaseId,
                               labels = diseaseList$diseaseName) ) %>%
    mutate(estimate = as.numeric(as.character(estimate)),
           lower = as.numeric(as.character(lower)),
           upper = as.numeric(as.character(upper)) ) 
  
  RRplotOut <- RR %>%
    ggplot(aes(x = comorbName, y = estimate))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = 1, size = 1)+
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
    #geom_text(aes(y = upper+0.01, label = round(estimate,3)), size = 4 )+
    theme_bw()+
    theme(legend.title = element_blank(),
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 11),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_blank())
  
  return(RRplotOut)
}

#'calculate co-prevalence rate
#'@import dplyr
#'@import reshape2
#'@param comorbManufacData               the result of comorbManufacture code
#'@export
#'

co_prevtable <- function(comorbManufacData){

    df <- comorbManufacData
    #str(ready)
    df_coprev <- as.data.frame(df) %>%
      dplyr::mutate(co_prevalence = paste0( round((Count/totalCount)*100,2),"%") ) %>%
      dplyr::mutate(result = paste0(Count,"(",co_prevalence,")")) %>%
      dplyr::mutate(diseaseName = factor(diseaseId, levels = diseaseList$diseaseId,
                                         labels = diseaseList$diseaseName) ) %>%
      dplyr::select(cohortDefinitionId, diseaseName, result)
    
    out <- dcast(df_coprev, diseaseName~cohortDefinitionId)

    return(out)
}
