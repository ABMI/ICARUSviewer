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
  
    connectionDetails <-connectionDetails
    connection <- DatabaseConnector::connect(connectionDetails)

    temporalSettings <- createTemporalCovariateSettings(useMeasurementValue = TRUE,
                                                        temporalStartDays = 0:7300,
                                                        temporalEndDays = 0:7300)

    covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                           cdmDatabaseSchema = CDMschema,
                                           cohortDatabaseSchema = Resultschema,
                                           cohortTable = cohortTable,
                                           cohortId = cohortId,
                                           rowIdField = "subject_id",
                                           covariateSettings = temporalSettings)

    covariateData_rawdf <- ff::as.ram(covariateData_ff$covariates)

    colnames(covariateData_rawdf) <- c("subjectId","covariateId","covariateValue","time")

    allLongitudinalData <- covariateData_rawdf %>%
      dplyr::mutate(covariateId = floor(covariateId/1000000) ) %>%
      dplyr::mutate(cohortId = cohortId)

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

    longitudinalData <- all_longitudinal_data %>% dplyr::filter(covariateId == measurement_concept_id )

    if(time_unit == 'year'){
        longitudinalData <- longitudinalData %>% dplyr::mutate(time = time/365.25)
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
  lmm_randomSIind_1 <- lme4::lmer(formula = f, data = longitudinalData, REML = TRUE)
  predict_lmm <- mm%*%lme4::fixef(lmm_randomSIind_1) 
  pvar1 <- Matrix::diag(mm %*% Matrix::tcrossprod(vcov(lmm_randomSIind_1),mm))
  result_fixef <- lme4::fixef(lmm_randomSIind_1) 
  result_ci <- lme4::confint.merMod(lmm_randomSIind_1)
  result_slope <- paste0(round(result_fixef[2],3), "(", paste0(round(result_ci['time',],3), collapse = ","), ")" )
  
  result_df <- data.frame(time = newdata_lmm$x,
                          predict = predict_lmm,
                          upper = predict_lmm+1.96*sqrt(pvar1),
                          lower = predict_lmm-1.96*sqrt(pvar1) )
  out <- list(result_slope,result_df)
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
  
  result <- list(cohort_1_result = list(cohortId_1,get_mea_cohort_1,lme_result_1[[2]],lme_result_1[[1]]),
                 cohort_2_result = list(cohortId_2,get_mea_cohort_2,lme_result_2[[2]],lme_result_2[[1]]))
  
  return(result)
}

#'plot trajectory line and its CI
#'@import ggplot2
#'@import dplyr
#'@param longitudinal_result  result of longitudinal code
#'@param pftIndividual        logical (TRUE/FALSE)
#'@export
#'

plotLmm <- function(longitudinal_result,
                    pftIndividual = TRUE){
  longitudinal_all <- NA
  for(i in 1 : length(longitudinal_result)) {
    if(is.na(longitudinal_all)){
      longitudinal_all <- longitudinal_result[[i]]$subLongitudinalData
    } else {
      longitudinal_all <- rbind(longitudinal_all,longitudinal_result[[i]]$subLongitudinalData)
    }
  }
  orderCohortId <- sapply(longitudinal_result, FUN = function(x){ as.character(x$cohortId) })
  orderCohortId <- as.numeric(orderCohortId)
  
  longitudinal_all$cohortId <- factor(longitudinal_all$cohortId, 
                                      levels = orderCohortId)
  
  plotlongitudinal <- NA
  plotlongitudinal <- ggplot(data = longitudinal_all)
  
  Individual_colourList <- c("#FF3333","#3366FF","#339933", "#FF9933","#660099","#99FFFF")
  Predict_colourList <- c("red","blue","#66FF66", "orange","#9900cc","#0099cc")
  
  if(pftIndividual){
    plotlongitudinal <- plotlongitudinal + geom_line( aes(x = time, y = covariateValue, group = subjectId, colour = cohortId ), size = 0.4, alpha = 0.5) + scale_colour_manual(values = Individual_colourList)
  }
  
  for (i in 1:length(unique(longitudinal_all$cohortId))){
    plotlongitudinal <- plotlongitudinal + geom_line(data = longitudinal_result[[i]]$lmeResult1,aes(x = time, y = predict),colour = Predict_colourList[i], size = 0.6)+
      geom_ribbon(data = longitudinal_result[[i]]$lmeResult1, aes(ymin = lower, ymax = upper, x = time), fill = Predict_colourList[i], alpha = 0.15 )
  }
  plotlongitudinal <- plotlongitudinal + theme_bw() + xlab("time (years)") + 
    theme(legend.title = element_blank(), axis.title.x = element_text(size=13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 13),axis.text.y = element_text(size = 13)) 

  return(plotlongitudinal)
}

#'table for representative estimated profile trajectory and its CI
#'@import dplyr
#'@param longitudinal_result  result of longitudinal code
#'@export
#'

tableLmm <- function(longitudinal_result){
  
  list_stuck <- list()
  for(i in 1:length(longitudinal_result)){
    predict_sub <- longitudinal_result[[i]]$lmeResult1 %>%
      filter(time %in% c(0,5,10,15) ) %>%
      mutate(summaryPredict = paste0(round(predict,3), "(" , round(lower,3) , "," , round(upper,3) , ")" ) ) 
    cohortId        <- longitudinal_result[[i]]$cohortId
    estimated_value <- predict_sub$summaryPredict
    slope           <- longitudinal_result[[i]]$lmeResult2
    count           <- length(unique(longitudinal_result[[i]]$subLongitudinalData$subjectId))
    result          <- c(cohortId, estimated_value, slope, count)
    names(result)   <- c("cohortId","time = 0","time = 5","time = 10","time = 15","slope","person count")
    list_stuck[[i]] <- result
  }
  
  # out_dataframe <- as.data.frame(rbind(list_stuck[[1]],list_stuck[[2]]))
  out_dataframe <- NA
  for(i in 1:length(longitudinal_result)){
    if(is.na(out_dataframe)) {
      out_dataframe <- list_stuck[[i]] 
    } else {
      out_dataframe <- rbind(out_dataframe,list_stuck[[i]])
    }
  }
  return(out_dataframe)
}
