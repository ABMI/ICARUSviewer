#'create Plp data and population data (ready for run Plp)
#'@import PatientLevelPrediction
#'@param  connectionDetails
#'@param  Resultschema
#'@param  CDMschema
#'@param  cohortTable                             table name which contains asthma_cohort (default = 'asthma_cohort')
#'@param  targetCohortConceptId                   target cohort Id (default = 1)
#'@param  outcomeCohortConceptId                  outcome cohort Id
#'@param  covariateSetting
#'@param  washoutPeriod                           0
#'@param  removeSubjectsWithPriorOutcome          TRUE
#'@param  riskWindowStart
#'@param  riskWindowEnd
#'@param  minTimeAtRisk
#'@export

getPlpDataList <- function(connectionDetails,
                           Resultschema,
                           CDMschema,
                           cohortTable = 'asthma_cohort',
                           targetCohortConceptId,
                           outcomeCohortConceptId,
                           covariateSetting,
                           washoutPeriod = 0,
                           removeSubjectsWithPriorOutcome = TRUE,
                           riskWindowStart,
                           riskWindowEnd,
                           minTimeAtRisk){
  
  Sys.setlocale(category="LC_CTYPE", locale="C")
  
  plpOut<- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = CDMschema,
                                              cohortDatabaseSchema = Resultschema,
                                              cohortTable = cohortTable,
                                              cohortId = targetCohortConceptId,
                                              covariateSettings = covariateSetting,
                                              outcomeDatabaseSchema = Resultschema,
                                              outcomeTable = cohortTable,
                                              outcomeIds = outcomeCohortConceptId)
  
  populationOut<- PatientLevelPrediction::createStudyPopulation(plpData = plpOut,
                                                                outcomeId = outcomeCohortConceptId,
                                                                binary = TRUE,
                                                                firstExposureOnly = FALSE,
                                                                washoutPeriod = washoutPeriod,
                                                                removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                riskWindowStart = riskWindowStart,
                                                                riskWindowEnd = riskWindowEnd,
                                                                minTimeAtRisk = minTimeAtRisk,
                                                                addExposureDaysToEnd = FALSE,
                                                                addExposureDaysToStart = FALSE)
  
  result<-list(plpOut = plpOut,
               populationOut = populationOut)
  
  return(result)
}

#'Run Plp
#'@import  PatientLevelPrediction
#'@param   getplpOut              get from getPlpDataList code
#'@param   learningModel          input$ModelSelect
#'@param   splitSeed              seed setting
#'@param   outputFolder
#'@export
RunPlp <- function(getplpOut,
                   learningModel,
                   splitSeed = NULL,
                   outputFolder = outputFolder){

    # Sys.setlocale(category="LC_CTYPE", locale="C")

    MLresult<-PatientLevelPrediction::runPlp(population = getplpOut[[2]],
                                             plpData = getplpOut[[1]],
                                             modelSettings = learningModel,
                                             testSplit = "person",
                                             testFraction = 0.25,
                                             nfold = 4,
                                             saveDirectory = outputFolder,
                                             savePlpData = F,
                                             savePlpResult = F,
                                             savePlpPlots = F,
                                             saveEvaluation = F,
                                             splitSeed = splitSeed)
    #?PatientLevelPrediction::runPlp
    return(MLresult)
}

#'ranking top n predictive variables and draw plot
#'@import  dplyr
#'@import  ggplot2
#'@param   machineLearningData get from RunPlp code
#'@param   rankCount           how many variables do you want to see?
#'@export

plotPredictiveVariables <- function(machineLearningData,
                                    rankCount = 40){

    MLresult <- machineLearningData

    Covariates<- MLresult$covariateSummary %>%
      dplyr::filter( covariateValue != 0.0000000000) %>%
      dplyr::select( analysisId, conceptId, covariateName, covariateValue, CovariateCount) %>%
      dplyr::arrange( desc(abs(covariateValue)))%>%
      head( rankCount )
    
    plot <- ggplot(data = Covariates,
                   aes(x = reorder(as.factor(conceptId), abs(covariateValue)), y = covariateValue) )+
      geom_bar(stat = "identity", width = .5)+
      coord_flip()+
      xlab("conceptId")+
      ylab("covariate value")+
      theme_bw()+
      theme(legend.title = element_blank(),
            strip.text = element_text(size = 15),
            #legend.position = "none",
            legend.text = element_text(size = 11),
            axis.title.x = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    return(plot)
}

#'ranking top n predictive variables and conceptName
#'@import  dplyr
#'@param   machineLearningData get from RunPlp code
#'@param   rankCount           how many variables do you want to see?
#'@param
#'@export

tablePredictiveVariables <- function(machineLearningData,
                                     rankCount = 20){
  MLresult <- machineLearningData
  
  Covariates<- MLresult$covariateSummary %>%
    dplyr::filter( covariateValue != 0.0000000000) %>%
    dplyr::select( analysisId, conceptId, covariateName, covariateValue, CovariateCount) %>%
    dplyr::arrange( desc(abs(covariateValue)))%>%
    head( rankCount )
  
  table <- Covariates %>%
    dplyr::select( conceptId,covariateName,covariateValue,CovariateCount) %>%
    dplyr::mutate( notNullProportion = ( CovariateCount/sum(demographicData$cohortDefinitionId==1)*100 ) )
  
  colnames(table)[5] <- 'non-missing percent (%)'
  
  return(table)
}

#'get AUROC curve From PatientLevelPrediction
#'@import PatientLevelPrediction
#'@import ggplot2
#'@param machineLearningData
#'@export

AUROCcurve <- function(machineLearningData){
  MLresult <- machineLearningData
  
  evaluationStat <- MLresult$performanceEvaluation$evaluationStatistics
  
  evaluationStat_df<-as.data.frame(evaluationStat)
  
  AUC <- evaluationStat_df[which(evaluationStat_df$Eval == 'test' & evaluationStat_df$Metric == c('AUC.auc','AUC.auc_lb95ci','AUC.auc_ub95ci')),]$Value
  
  AUC_round<-round(as.numeric(as.character(AUC)), 3)
  
  AUROC <- plotSparseRoc(MLresult$performanceEvaluation) +
    ggplot2::annotate("text", label = paste0("AUC = ",AUC_round[1],"(",AUC_round[2],"-",AUC_round[3],")"), x = 0.75, y = 0.15, size = 6)
  
  return(AUROC)
}

