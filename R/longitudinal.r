#'extract longitudinal analysis using FeatureExtraction
#'@import   FeatureExtraction
#'@import   dplyr
#'@param    connectionDetails
#'@param    CDMschema
#'@param    Resultschema
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
                                                        temporalEndDays = 0:7300)

    covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                           cohortDatabaseSchema = resultDatabaseSchema,
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
#'@import Matrix
#'@param  longitudinalData result of PFTmanufacture function
#'@export
#'
lme_logitudinal <- function(longitudinalData){
  
  f <- as.formula( covariateValue ~ time + (time||subjectId) )
  newdata_lmm <- data.frame(x=seq(0,15,length.out=100))
  mm <- model.matrix(~x,newdata_lmm)
  lmm_randomSIind_1 <- lme4::lmer(formula = f, data = longitudinalData, REML = T)
  predict_lmm <- mm%*%fixef(lmm_randomSIind_1) 
  pvar1 <- Matrix::diag(mm %*% Matrix::tcrossprod(vcov(lmm_randomSIind_1),mm))
  out <- data.frame(time = newdata_lmm$x,
                    predict = predict_lmm,
                    upper = predict_lmm+1.96*sqrt(pvar1),
                    lower = predict_lmm-1.96*sqrt(pvar1) )
  return(out)
}

#'aggregate total code for longitudinal analysis
#'@param cohortId_1
#'@param cohortId_2 
#'@param allLongitudinal_cohort_1
#'@param allLongitudinal_cohort_2
#'@param measurement_concept_id
#'@export
longitudinal <- function(cohortId_1,
                         cohortId_2,
                         allLongitudinal_cohort_1,
                         allLongitudinal_cohort_2,
                         measurement_concept_id){
  
  get_mea_cohort_1 <- getLongitudinal(all_longitudinal_data = allLongitudinal_cohort_1,
                                      measurement_concept_id = measurement_concept_id,
                                      time_unit = 'year')
  
  get_mea_cohort_2 <- getLongitudinal(all_longitudinal_data = allLongitudinal_cohort_2,
                                      measurement_concept_id = measurement_concept_id,
                                      time_unit = 'year')
  
  lme_result_1 <- lme_logitudinal(longitudinalData = get_mea_cohort_1)
  lme_result_2 <- lme_logitudinal(longitudinalData = get_mea_cohort_2)
  
  result <- list(cohort_1_result = list(cohortId_1,get_mea_cohort_1,lme_result_1),
                 cohort_2_result = list(cohortId_2,get_mea_cohort_2,lme_result_2))
  
  return(result)
}

#'plot trajectory line and its CI
#'@import ggplot2
#'@import dplyr
#'@param longitudinal_result  result of longitudinal code
#'@param pftIndividual                  logical (TRUE/FALSE)
#'@export
#'

plotLmm <- function(longitudinal_result,
                    pftIndividual = TRUE){
  
  longitudinal_all <- rbind(longitudinal_result[[1]][[2]],
                            longitudinal_result[[2]][[2]])
  plotlongitudinal <- ggplot(data = longitudinal_all)
  split_list <- split(longitudinal_all, longitudinal_all$cohortId)
  colourList <- c("red","blue")
  
  if(pftIndividual){
    i <- 1
    while(1){
      df <- split_list[[i]]
      plotlongitudinal <- plotlongitudinal + geom_line(data = df, aes(x = time, y = covariateValue, group = subjectId, colour = as.factor(cohortId) ), size = 0.4, alpha = 0.5)
      i <- i + 1
      if(i > length( unique(longitudinal_all$cohortId) ) ) break
    }
  }
  
  for (i in 1:length(unique(longitudinal_all$cohortId))){
    plotlongitudinal <- plotlongitudinal + geom_line(data = longitudinal_result[[i]][[3]],aes(x = time, y = predict),colour = colourList[i], size = 1)+
      geom_ribbon(data = longitudinal_result[[i]][[3]], aes(ymin = lower, ymax = upper, x = time), fill = colourList[i], alpha = 0.25 )
  }
  plotlongitudinal <- plotlongitudinal + theme_bw() + xlab("time (years)") + 
    theme(legend.title = element_blank(), axis.title.x = element_text(size=13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 13),axis.text.y = element_text(size = 13)) 
  return(plotlongitudinal)
}

