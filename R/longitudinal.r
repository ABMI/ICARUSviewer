#'extract longitudinal analysis using FeatureExtraction
#'@import   FeatureExtraction
#'@import   dplyr
#'@param    connectionDetails
#'@param    cdmDatabaseSchema
#'@param    resultsDatabaseSchema
#'@param    cohortTable
#'@param    cohortId
#'@export
#'
getAllLongitudinal <- function(connectionDetails,
                               cdmDatabaseSchema,
                               resultsDatabaseSchema,
                               cohortTable,
                               cohortId){

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

    allLongitudinalData <- covariateData_rawdf %>%
        mutate(covariateId = floor(covariateId/1000000) ) %>%
        mutate(cohortId = cohortId)

    return(allLongitudinalData)
}

#'subsetting features that have specific measurement_concept_id
#'@import   dplyr
#'@param    allLongitudinalData
#'@param    measurementConceptId
#'@param    timeUnit
#'@export
#'
getLongitudinal <- function(allLongitudinalData,
                            measurementConceptId,
                            timeUnit = 'year'){

        longitudinalData <- allLongitudinalData %>% filter(covariateId == measurementConceptId )

        if(timeUnit == 'year'){
            longitudinalData <- longitudinalData %>%
                mutate(time = time/365.25)
        }

        return(longitudinalData)
}
