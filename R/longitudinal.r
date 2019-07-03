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
                               CDMschema,
                               Resultschema,
                               cohortTable,
                               cohortId){

    cdmDatabaseSchema <- paste0(CDMschema,".dbo")
    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    connectionDetails <-connectionDetails
    connection <- DatabaseConnector::connect(connectionDetails)

    temporalSettings <- createTemporalCovariateSettings(useMeasurementValue = TRUE,
                                                        temporalStartDays = 0:7300,
                                                        temporalEndDays = 0:7300+1)

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
#'@param    all_longitudinal_data
#'@param    measurement_concept_id
#'@param    time_unit
#'@export
#'
getLongitudinal <- function(all_longitudinal_data,
                            measurement_concept_id,
                            time_unit = 'year'){

    longitudinalData <- all_longitudinal_data %>% filter(covariateId == measurement_concept_id )

    if(time_unit == 'year'){
        longitudinalData <- longitudinalData %>%
            mutate(time = time/365.25)
    }

    return(longitudinalData)
}

#'use linear mixed model regression, find regression line and its CI 95%
#'@import lme4
#'@import lmerTest
#'@param  longitudinalData result of PFTmanufacture function
#'@export
#'
lmePft <- function(longitudinalData){

    split_list <- split(longitudinalData, longitudinalData$cohortId)
    f <- as.formula( covariateValue ~ time + (time||subjectId) )

    df_1 <- split_list[[1]]
    lmm_randomSIind_1 <- lme4::lmer(formula = f, data = df_1, REML = T)
    #nlme::lme(fixed = covariateValue ~ 1 + time, random = ~time|subjectId, data = df_1)
    fixef_value_1 <- t(lme4::fixef(lmm_randomSIind_1))
    # effect_value_1 <- effects::effect("time", lmm_randomSIind_1)
    # effect_value_df_1 <- as.data.frame(effect_value_1)
    cohortId_1 <- unique(df_1$cohortId)

    df_2 <- split_list[[2]]
    lmm_randomSIind_2 <-lme4::lmer(formula = f, data = df_2, REML = T)
    #nlme::lme(fixed = covariateValue ~ 1 + time, random = ~time|subjectId, data = df_2)
    fixef_value_2 <- t(lme4::fixef(lmm_randomSIind_2))
    # effect_value_2 <- effects::effect("time", lmm_randomSIind_2)
    # effect_value_df_2 <- as.data.frame(effect_value_2)
    cohortId_2 <- unique(df_2$cohortId)

    out <- list( cohortId_1 = list(cohortId_1,
                                   fixef_value_1
                                   #,
                                   #effect_value_df_1
                                   ),
                 cohortId_2 = list(cohortId_2,
                                   fixef_value_2
                                   #,
                                   #effect_value_df_2
                                   ) )

    return(out)
}

#'plot pft trajectory line and its CI
#'@import ggplot2
#'@import dplyr
#'@param longitudinalData
#'@param lmePftData
#'@param pftIndividual                  logical (TRUE/FALSE)
#'@param ConfidencialIntervalVisualize  logical (TRUE/FALSE)
#'@export
#'

plotpftLmm <- function(longitudinalData,
                       lmePftData,
                       pftIndividual = TRUE,
                       ConfidencialIntervalVisualize = TRUE){

    plotpft <- ggplot(data = longitudinalData)
    longitudinalData <- longitudinalData %>% mutate(cohortId = as.factor(cohortId))

    split_list <- split(longitudinalData, longitudinalData$cohortId)

    colourList <- c("red","blue")

    if(pftIndividual){
        i <- 1
        while(1){
            df <- split_list[[i]]
            plotpft <- plotpft + geom_line(data = df, aes(x = time, y = covariateValue, group = subjectId, colour = as.factor(cohortId) ), size = 0.4, alpha = 0.5)
            i <- i + 1
            if(i > length( unique(longitudinalData$cohortId) ) ) break
        }
    }

    intercept  <- function(x) lmePftData[[x]][[2]][1]
    slope      <- function(x) lmePftData[[x]][[2]][2]

    for (i in 1:length(unique(longitudinalData$cohortId))){
        plotpft <- plotpft + geom_abline(intercept = intercept(i), slope = slope(i), colour = colourList[i], size = 1 )
    }

    if(ConfidencialIntervalVisualize){
        for (i in 1:length(unique(longitudinalData$cohortId))){
            plotpft <- plotpft +
                geom_line(data = lmePftData[[i]][[3]], aes(x = time, y = lower), linetype = "longdash", size = 0.5, colour = colourList[i]) +
                geom_line(data = lmePftData[[i]][[3]], aes(x = time, y = upper), linetype = "longdash", size = 0.5, colour = colourList[i])
        }
    }

    plotpft <- plotpft +
        coord_cartesian(xlim = c(0,15))

    return(plotpft)
}

