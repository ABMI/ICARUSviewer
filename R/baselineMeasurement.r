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

    baseline_covariate<-FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                   useDemographicsAge = TRUE,
                                                                   useMeasurementValueAnyTimePrior = TRUE,
                                                                   longTermStartDays = -365)
    baselineData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = CDMschema,
                                          cohortDatabaseSchema = Resultschema,
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
#'@param  cohort_definition_id_set
#'@param  measurementConceptIdSet
#'@export
baselineMeasure_compare <- function(connectionDetails,
                                    Resultschema,
                                    CDMschema,
                                    cohortTable,
                                    cohort_definition_id_set,
                                    measurementConceptIdSet = c(2,3,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469,
                                                                3011708,3006504,3005322,3005600,3017501,3026514,3005791,3021940,
                                                                3011505,3013115,3018010,3022096) ){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  baselineMeasureData <- data.frame()
  for(i in cohort_definition_id_set){
    getbaselineMeasure <- getBaselineCovariate(connectionDetails = connectionDetails,
                                               CDMschema = CDMschema,
                                               Resultschema = Resultschema,
                                               cohortTable = cohortTable,
                                               cohortId = i)
    if(length(baselineMeasureData) == 0) baselineMeasureData <- getbaselineMeasure
    else baselineMeasureData <- rbind(baselineMeasureData,getbaselineMeasure)
  }
  
  baselineMeasureSubData <- baselineMeasureData %>% dplyr::filter(conceptId %in% measurementConceptIdSet )
  baselineMeasure <- baselineMeasureSubData %>% dplyr::select(-covariateId)
  baseline_measure_list <-split(baselineMeasure,baselineMeasure$conceptId)
  
  out_summary_list <- lapply(baseline_measure_list,FUN = baselineMeasure_summary)
  out_pvalue_list <- lapply(baseline_measure_list,FUN = baselineMeasure_pvalue)
  
  temp = data.frame()
  for(i in 1:length(out_summary_list)){
    if(i == 1) out_summary_df <- dplyr::bind_rows(temp,out_summary_list[[i]])
    else out_summary_df <- dplyr::bind_rows(out_summary_df,out_summary_list[[i]])
  }
  summary_basemeasure <- cbind(measurementConceptId = names(out_summary_list),
                               out_summary_df) 
  pvalue_basemeasure <- data.frame(measurementConceptId = names(out_summary_list),
                                   pvalue = unlist(out_pvalue_list) )
  result <- dplyr::left_join(summary_basemeasure,pvalue_basemeasure,by = "measurementConceptId")
  result <- result %>%
    rename(measurementName = measurementConceptId) %>%
    mutate(measurementName = factor(measurementName, levels = measurementId$maesurementConceptId,
                                    labels = measurementId$measureName) )
  return(result)
}

#'function for calculate mean or median of measurement values
#'@param x
#'@export
baselineMeasure_summary <- function(x){
  result_summary <- tapply(x$covariateValue,x$cohortDefinitionId,FUN = function(y) {
    mean_x = round(mean(y, na.rm = TRUE),2)
    sd_x = round(sd(y, na.rm = TRUE),2)
    count_x = length(unique(y))
    result = paste0(mean_x,"+/-",sd_x,"(",count_x,")")
  } )
  # names(result_summary) <- sort(cohort_definition_id_set)
  return(result_summary)
}

#'function for calculate p-value of measurement values
#'@param measuredData
#'@export
baselineMeasure_pvalue <- function(measuredData){
  cohortIdLength = length(unique(measuredData$cohortDefinitionId))
  if( cohortIdLength == 1 ){
    
    pvalue <- NA
    
  } else if( cohortIdLength == 2 ){
    
    pvalue <- pvalueCalBetweenTwo(y = measuredData$covariateValue, index = measuredData$cohortDefinitionId, cohortIdLength = cohortIdLength)
    
  } else if( cohortIdLength > 2 ){
    
    pvalue <- pvalueCalAmongMoreThanTwo(y = measuredData$covariateValue, index = measuredData$cohortDefinitionId, cohortIdLength = cohortIdLength)
    
  }
  return(pvalue)
}

