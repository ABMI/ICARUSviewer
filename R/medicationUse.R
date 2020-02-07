#'Analysis of baseline medication use of cohorts
#'
#'@import dplyr
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohort_definition_id_set
#'@export
#'

getDrugCovariate <- function(connectionDetails,
                                  Resultschema,
                                  CDMschema,
                                  cohortTable,
                                  cohortId){
  
  drugCovariate<- FeatureExtraction::createCovariateSettings(useDrugExposureLongTerm = TRUE, endDays = 0)
  
  covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = CDMschema,
                                         cohortDatabaseSchema = Resultschema,
                                         cohortTable = cohortTable,
                                         cohortId = cohortId,
                                         rowIdField = "subject_id",
                                         covariateSettings = drugCovariate)
  
  covariateId <- ff::as.ram(covariateData_ff$covariateRef$covariateId)
  conceptId <- ff::as.ram(covariateData_ff$covariateRef$conceptId)
  conceptIdMapping_df <- data.frame(covariateId,conceptId)
  drug_rawdf <- ff::as.ram(covariateData_ff$covariates)
  drug_df <- drug_rawdf %>% left_join(conceptIdMapping_df, by = "covariateId")
  colnames(drug_df) <- c("subjectId","covariateId","covariateValue","conceptId")
  
  out <- drug_df %>%
    mutate(cohortDefinitionId = cohortId) %>%
    filter(conceptId %in% unlist(drugList$conceptIdSet)) %>%
    mutate(drugId = NA)
  for(i in 1:length(drugList$drugId)){
    if( nrow(out[out$conceptId %in% drugList$conceptIdSet[[i]],]) == 0 ) next
    out[out$conceptId %in% drugList$conceptIdSet[[i]],]$drugId <- drugList$drugId[[i]]
  }
  out <- out %>% select (cohortDefinitionId, subjectId, drugId, conceptId)
  
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
baseline_drug <- function(connectionDetails,
                                 Resultschema,
                                 CDMschema,
                                 cohortTable,
                                 cohort_definition_id_set){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  drugData <- data.frame()
  for(i in cohort_definition_id_set){
    getBaselineDrug <- getDrugCovariate(connectionDetails = connectionDetails,
                                                  CDMschema = CDMschema,
                                                  Resultschema = Resultschema,
                                                  cohortTable = cohortTable,
                                                  cohortId = i)
    if(length(drugData) == 0) {
      drugData <- getBaselineDrug
    } else {
      drugData <- rbind(drugData,getBaselineDrug) 
    }
  }
  
  templete <- data.frame(cohortDefinitionId = rep(c(cohort_definition_id_set), each = length(drugList$drugId) ),
                         drugId = rep(drugList$drugId,length(cohort_definition_id_set)))
  
  out <- drugData %>%
    group_by(cohortDefinitionId, drugId) %>%
    summarise(Count = n_distinct(subjectId)) %>%
    right_join(templete, by = c("cohortDefinitionId","drugId")) %>%
    left_join(demographicData %>% group_by(cohortDefinitionId) %>%
                summarise(totalCount = n_distinct(personId)), by = "cohortDefinitionId" ) %>%
    mutate(noDrug = totalCount - Count) %>%
    mutate(noDrug = if_else(is.na(noDrug),totalCount,noDrug))
  
  if(sum(is.na(out$Count))>0 ) out[is.na(out$Count),]$Count <- 0
  out<- as.data.frame(out)
  
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

drugTable <- function(drugManufacData){
  
  df <- drugManufacData
  #str(ready)
  df_drug <- as.data.frame(df) %>%
    dplyr::mutate(percent = paste0( round((Count/totalCount)*100,2),"%") ) %>%
    dplyr::mutate(result = paste0(Count,"(",percent,")")) %>%
    dplyr::mutate(drugName = factor(drugId, levels = drugList$drugId,
                                       labels = drugList$drugName) ) %>%
    dplyr::select(cohortDefinitionId, drugName, result)
  
  out <- dcast(df_drug, drugName~cohortDefinitionId)
  
  return(out)
}
