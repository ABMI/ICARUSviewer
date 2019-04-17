#'create Plp data and population data (ready for run Plp)
#'
#'@import PatientLevelPrediction
#'@import FeatureExtraction
#'@param  connectionDetails
#'@param  connection
#'@param  Resultschema
#'@param  CDMschema
#'@param  cohortTable                             table name which contains asthma_cohort (default = 'asthma_cohort')
#'@param  targetId                                target cohort Id (default = 1)
#'@param  outcomeCohortConceptId                  outcome cohort Id (2 = NSA, 3 = SA, 4 = AERD, 5 = ATA)
#'@param  covariateSetting
#'@param  washoutPeriod                           0
#'@param  removeSubjectsWithPriorOutcome          TRUE
#'@param  riskWindowStart                         1
#'@param  riskWindowEnd                           365*15
#'@export

getPlpData <- function(connectionDetails,
                       connection,
                       Resultschema,
                       CDMschema,
                       cohortTable = 'asthma_cohort',
                       targetId = 1,
                       outcomeCohortConceptId,
                       covariateSetting,
                       washoutPeriod = 0,
                       removeSubjectsWithPriorOutcome = TRUE,
                       riskWindowStart = 0,
                       riskWindowEnd = 365*15){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    CDMDatabaseSchema <- paste0(CDMschema,".dbo")

    Sys.setlocale(category="LC_CTYPE", locale="C")

    plpOut<- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                cdmDatabaseSchema = CDMDatabaseSchema,
                                                cohortDatabaseSchema = resultDatabaseSchema,
                                                cohortTable = cohortTable,
                                                cohortId = targetId,
                                                covariateSettings = covariateSetting,
                                                outcomeDatabaseSchema = resultDatabaseSchema,
                                                outcomeTable = cohortTable,
                                                outcomeIds = outcomeCohortConceptId)

    populationOut<- PatientLevelPrediction::createStudyPopulation(plpData = plpOut,
                                                                  outcomeId = outcomeCohortConceptId,
                                                                  binary = TRUE,
                                                                  firstExposureOnly = TRUE,
                                                                  washoutPeriod = washoutPeriod,
                                                                  removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                  riskWindowStart = riskWindowStart,
                                                                  riskWindowEnd = riskWindowEnd,
                                                                  addExposureDaysToEnd = FALSE,
                                                                  addExposureDaysToStart = FALSE)

    result<-list(plpOut = plpOut,
                 populationOut = populationOut)

    return(result)
}

#'Run Plp
#'@import  PatientLevelPrediction
#'@param   getplpOut              get from getPlpData code
#'@param   learningModel          machin learning model to develop prediction model (lassologistic, gradientboosting, randomforest)
#'@param
#'@export
RunPlp <- function(getplpOut,
                   learningModel){

    Sys.setlocale(category="LC_CTYPE", locale="C")

    if(learningModel == "lassologistic"){
        Model <- PatientLevelPrediction::setLassoLogisticRegression()
    } else if(learningModel == "gradientboosting"){
        Model <- PatientLevelPrediction::setGradientBoostingMachine(maxDepth = c(4,6),
                                                                    ntrees = c(1,10,100),
                                                                    minRows = c(2,10,20))
    } else if(learningModel == "randomforest"){
        Model <- PatientLevelPrediction::setRandomForest()
    }

    MLresult<-PatientLevelPrediction::runPlp(population = getplpOut[[2]],
                                             plpData = getplpOut[[1]],
                                             modelSettings = Model,
                                             testSplit = "person",
                                             testFraction = 0.25,
                                             nfold = 3,
                                             saveDirectory = outputFolder,
                                             savePlpData = F,
                                             savePlpResult = F,
                                             savePlpPlots = F,
                                             saveEvaluation = F)
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
                                    rankCount = 20){

    MLresult <- machineLearningData
colnames(MLresult$covariateSummary)
    Covariates<- MLresult$covariateSummary %>%
        filter( covariateValue != 0.0000000000) %>%
        select( analysisId, conceptId, covariateName, covariateValue, CovariateCount) %>%
        arrange( desc(abs(covariateValue)))%>%
        head( rankCount )

    plot <- ggplot(data = Covariates,
                   aes(x = reorder(as.factor(conceptId), abs(covariateValue)), y = covariateValue) )+
        geom_bar(stat = "identity", width = .5)+
        coord_flip()+
        xlab("conceptId")+
        ylab("variable value ( if negative = more non-outcome or if positive = more outcome )")+
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
        filter( covariateValue != 0.0000000000) %>%
        select( analysisId, conceptId, covariateName, covariateValue, CovariateCount) %>%
        arrange( desc(abs(covariateValue)))%>%
        head( rankCount )

    table <- Covariates %>%
        select( conceptId,covariateName,covariateValue,CovariateCount) %>%
        mutate( notNullProportion = ( CovariateCount/sum(demographicData$cohortDefinitionId==1)*100 ) )

    colnames(table)[5] <- 'not Null percent (%)'

    return(table)
}

# plp<-getPlpData(connectionDetails, connection, Resultschema = 'ICARUS', CDMschema = 'ICARUS', outcomeCohortConceptId = 4,
#                 covariateSetting = covariateSetting)
#
# plp$populationOut
#
# run<-RunPlp(getplpOut = plp,
#                    learningModel = 'lassologistic')
# plot<-plotPredictiveVariables(machineLearningData = run,
#                                           rankCount = 20)
