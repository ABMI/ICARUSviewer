#'extract longitudinal analysis using FeatureExtraction
#'@import   FeatureExtraction
#'@import   dplyr
#'@param    connectionDetails
#'@param    cdmDatabaseSchema
#'@param    resultsDatabaseSchema
#'@param    cohortTable
#'@param    cohortId
#'@param    timeUnit
#'@param    measurementConceptId
#'@export
#'
getLongitudinal <- function(connectionDetails,
                            cdmDatabaseSchema,
                            resultsDatabaseSchema,
                            cohortTable,
                            cohortId,
                            timeUnit = 'year',
                            measurementConceptId){

    temporalSettings <- createTemporalCovariateSettings(useMeasurementValue = TRUE,
                                                        temporalStartDays = 1:7300,
                                                        temporalEndDays = 1:7300+1,
                                                        includedCovariateConceptIds = c(measurementConceptId))

    covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                           cohortDatabaseSchema = resultsDatabaseSchema,
                                           cohortTable = cohortTable,
                                           cohortId = cohortId,
                                           rowIdField = "subject_id",
                                           covariateSettings = temporalSettings)

    covariateData_rawdf <- ff::as.ram(covariateData_ff$covariates)

    colnames(covariateData_rawdf) <- c("subjectId","covariateId","covariateValue","time")

    covariateData_df <- covariateData_rawdf %>%
        mutate(covariateId = floor(covariateId/1000000) )

    longitudinalData <- covariateData_df %>% filter(covariateId == measurementConceptId )

    if(timeUnit == 'year'){
        longitudinalData <- longitudinalData %>%
            mutate(time = time/365.25)
    }

    longitudinalData <- longitudinalData %>%
        mutate(cohortId == cohortId)

    return(longitudinalData)
}
