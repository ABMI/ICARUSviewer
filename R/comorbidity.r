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

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")

    condition_covariate<- FeatureExtraction::createCovariateSettings(useConditionOccurrenceLongTerm = TRUE)
    covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                           cohortDatabaseSchema = resultsDatabaseSchema,
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
#'@param cohortId_1
#'@param cohortId_2
#'@export
#'
baseline_comorbidity <- function(connectionDetails,
                                 Resultschema,
                                 CDMschema,
                                 cohortTable,
                                 cohortId_1,
                                 cohortId_2){

    baselineComorb_cohort_1 <- getConditionCovariate(connectionDetails = connectionDetails,
                                                     Resultschema = Resultschema,
                                                     CDMschema = CDMschema,
                                                     cohortTable = cohortTable,
                                                     cohortId = cohortId_1)

    baselineComorb_cohort_2 <- getConditionCovariate(connectionDetails = connectionDetails,
                                                     Resultschema = Resultschema,
                                                     CDMschema = CDMschema,
                                                     cohortTable = cohortTable,
                                                     cohortId = cohortId_2)

    baselineComorbidity <- rbind(baselineComorb_cohort_1,baselineComorb_cohort_2)

    templete <- data.frame(cohortDefinitionId = rep( c(cohortId_1,cohortId_2),each = length(diseaseList$diseaseId) ),
                           diseaseId = rep(diseaseList$diseaseId,2) )

    out <- baselineComorbidity %>%
        group_by(cohortDefinitionId, diseaseId) %>%
        summarise(Count = n()) %>%
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
    if ( min(df$cohortDefinitionId) == 2){

        split <- split(df, df$diseaseId)
        RRresult <- lapply(split, FUN = function(x){
            simply <- x[,c("Count","notdisease")]
            simply <- simply[c(2,1),]
            row.names(simply)<-x$cohortDefinitionId[c(2,1)]
            RR_cal<-epitools::riskratio(as.matrix(simply))

            c(diseaseName = diseaseList$diseaseName[which(diseaseList$diseaseId == unique(x$diseaseId))],
              RR_cal$measure[2,],
              pvalue = RR_cal$p.value[6])
        })

    } else {

        split <- split(df, df$diseaseId)
        RRresult <- lapply(split, FUN = function(x){
            simply <- x[,c("Count","notdisease")]
            row.names(simply)<-x$cohortDefinitionId
            RR_cal<-epitools::riskratio(as.matrix(simply))
            c(diseaseName = diseaseList$diseaseName[which(diseaseList$diseaseId == unique(x$diseaseId))],
              RR_cal$measure[2,],
              pvalue = RR_cal$p.value[6])
        })
    }

    output <- t(as.data.frame(RRresult))
    output <- data.frame(output)
    return(output)
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
                                   labels = diseaseList$diseaseName) )

    RRplotOut <- RR %>%
        ggplot(aes(x = comorbName, y = estimate))+
        geom_point()+
        geom_line()+
        geom_hline(yintercept = 1, size = 1)+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
        geom_text(aes(y = upper+0.01, label = round(estimate,3)), size = 4 )+
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
        mutate(co_prevalence = paste0( round((Count/totalCount)*100,2),"%") ) %>%
        mutate(result = paste0(Count,"(",co_prevalence,")")) %>%
        mutate(diseaseName = factor(diseaseId, levels = diseaseList$diseaseId,
                                    labels = diseaseList$diseaseName) ) %>%
        select(cohortDefinitionId, diseaseName, result)

    out <- dcast(df_coprev, diseaseName~cohortDefinitionId)

    return(out)
}
