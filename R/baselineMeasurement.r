#'get measurement baseline measurement data
#'@import dplyr
#'@import FeatureExtraction
#'@param connectionDetails
#'@param CDMschema
#'@param Resultschema
#'@param cohortTable
#'@param cohortId
#'@export
getBaselineCovariate <- function(connectionDetails,
                                 CDMschema,
                                 Resultschema,
                                 cohortTable,
                                 cohortId){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")

    baseline_covariate<-FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                   useDemographicsAge = TRUE,
                                                                   useMeasurementValueAnyTimePrior = TRUE,
                                                                   longTermStartDays = -365)
    baselineData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          cohortDatabaseSchema = resultsDatabaseSchema,
                                          cohortTable = cohortTable,
                                          cohortId = cohortId,
                                          rowIdField = "subject_id",
                                          covariateSettings = baseline_covariate)
    covariateId <- ff::as.ram(baselineData_ff$covariateRef$covariateId)
    conceptId <- ff::as.ram(baselineData_ff$covariateRef$conceptId)
    conceptIdMapping_df <- data.frame(covariateId,conceptId)

    baselineData_rawdf <- ff::as.ram(baselineData_ff$covariates)
    baselineData <- baselineData_rawdf %>% left_join(conceptIdMapping_df, by = "covariateId")
    colnames(baselineData) <- c("subjectId","covariateId","covariateValue","conceptId")

    baselineData <- baselineData %>% mutate(cohortDefinitionId = cohortId)

    return(baselineData)
}

#'comparing measurement value between two cohorts
#'@import dplyr
#'@param  connectionDetails
#'@param  Resultschema
#'@param  CDMschema
#'@param  cohortTable
#'@param  cohortId_1
#'@param  cohortId_2
#'@param  measurementConceptIdSet
#'@export
baselineMeasure_compare <- function(connectionDetails,
                                    Resultschema,
                                    CDMschema,
                                    cohortTable,
                                    cohortId_1,
                                    cohortId_2,
                                    measurementConceptIdSet = c(2,3,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469,
                                                                3011708,3006504,3005322,3005600,3017501,3026514,3005791,3021940,
                                                                3011505,3013115,3018010,3022096) ){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")

    cohort_1_baseline <- getBaselineCovariate(connectionDetails = connectionDetails,
                                              CDMschema = CDMDatabaseSchema,
                                              Resultschema = resultDatabaseSchema,
                                              cohortTable = cohortTable,
                                              cohortId = cohortId_1)
    cohort_2_baseline <- getBaselineCovariate(connectionDetails = connectionDetails,
                                              CDMschema = CDMDatabaseSchema,
                                              Resultschema = resultDatabaseSchema,
                                              cohortTable = cohortTable,
                                              cohortId = cohortId_2)

    baselineMeasure_cohort_1_out <- cohort_1_baseline %>%
        filter(conceptId %in% measurementConceptIdSet )
    baselineMeasure_cohort_2_out <- cohort_2_baseline %>%
        filter(conceptId %in% measurementConceptIdSet )

    baseline_measure <- rbind(baselineMeasure_cohort_1_out,baselineMeasure_cohort_2_out) %>% select(-covariateId)
    baseline_measure_list <- split(baseline_measure,baseline_measure$conceptId)

    out_summary_list <- lapply(baseline_measure_list,FUN = baselineMeasure_summary)
    out_pvalue_list <- lapply(baseline_measure_list,FUN = baselineMeasure_pvalue)

    summary_basemeasure <- data.frame(t(as.data.frame(out_list)))
    names(summary_basemeasure) <- sort(unique(baseline_measure$cohortDefinitionId))
    summary_basemeasure <- cbind(measurementConceptId = sort(unique(baseline_measure$conceptId)),
                                 summary_basemeasure)
    pvalue_basemeasure <- data.frame(measurementConceptId = sort(unique(baseline_measure$conceptId)),
                                     pvalue = unlist(out_pvalue_list) )
    result <- left_join(summary_basemeasure,pvalue_basemeasure,by = "measurementConceptId")

    return(result)
}

#'function for calculate mean or median of measurement values
#'@param x
#'@export
baselineMeasure_summary <- function(x){
    value <- x$covariateValue
    shapiro <- shapiro.test(value)
    if(shapiro$p.value >= 0.05){
        mean_x          <- tapply(x$covariateValue,x$cohortDefinitionId,FUN = function(x) round(mean(x, na.rm = T),2) )
        sd_x            <- tapply(x$covariateValue,x$cohortDefinitionId,FUN = function(x) round(sd(x, na.rm = T),2) )
        count_x         <- tapply(x$subjectId,x$cohortDefinitionId,FUN = function(x) length(unique(x)) )
        result          <- paste0(mean_x,"+/-",sd_x,"(",count_x,")")
    } else {
        median_x        <- tapply(x$covariateValue,x$cohortDefinitionId,FUN = function(x) median(x, na.rm = T) )
        inquantile_x    <- tapply(x$covariateValue,x$cohortDefinitionId,FUN = function(x) paste0(quantile(x, na.rm = T)[2],",",quantile(x, na.rm = T)[4] ) )
        count_x         <- tapply(x$subjectId,x$cohortDefinitionId,FUN = function(x) length(unique(x)) )
        result          <- paste0(median_x,"(",inquantile_x,")","(",count_x,")")
    }
    return(result)
}

#'function for calculate p-value of measurement values
#'@param x
#'@export
baselineMeasure_pvalue <- function(x){
    value <- x$covariateValue
    shapiro <- shapiro.test(value)
    if(shapiro$p.value >= 0.05){
        ttest_pvalue    <- t.test(data = x, covariateValue~cohortDefinitionId)
        p_value         <- round(ttest_pvalue$p.value,4)
        out             <- paste0(p_value,"(t test)")
    } else {
        wilcox_pvalue   <- wilcox.test(data = x, covariateValue~cohortDefinitionId)
        p_value         <- round(wilcox_pvalue$p.value,4)
        out             <- paste0(p_value,"(Wilcoxon rank sum test)")
    }
    return(out)
}
