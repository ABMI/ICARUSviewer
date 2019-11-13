#'switch input from label to cohortDefinitionId
#'@param input
#'@export
#'
switchcohort <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = {c(2,3)},
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = {c(4,5)},
           "Exacerbation vs Non-exacerbation" = {c(300,301)},
           "AERD subtype compare" = {c(51,52,53,54)} )
}

#'switch input from label to cohortDefinitionId for plp
#'@param input
#'@export
#'
switchselect_plp <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = 3,
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = 4
    )
}

#'switch input from label to machine learning model
#'@param input
#'@export
#'
switchselect_model <- function(input){
    switch(input,
           "Lasso Logistic" = PatientLevelPrediction::setLassoLogisticRegression(),
           "Gradient Boosting" = PatientLevelPrediction::setGradientBoostingMachine(maxDepth = c(4,6),
                                                                                    ntrees = c(1,10,100),
                                                                                    minRows = c(2,10,20)),
           "randomforest" = PatientLevelPrediction::setRandomForest()
    )
}

#'switch input from label to cohortDefinitionId
#'@param input
#'@export
#'
switchselect_pft <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = {c(2,3)},
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = {c(4,5)},
           "Exacerbation vs Non-exacerbation" = {c(300,301)},
           "Biomarker" = {selectInput(inputId = "selectbiomarker", label = "select biomarker",
                                      choices = c('EDN','periostin','Eotaxin1','Eotaxin2','SP_D','DPP10','MBL','TGFb1','chitinase','TIMP1','OPN','IL8','MPO'),
                                      multiple = FALSE)}
    )
}

#'remove setting
#'@export
#'
removeTempAndOutput <- function(){
  # if( length(list.files("./inst/Output"))!=0 ){ unlink(file.path("./inst/Output",list.files("./inst/Output"))) }
  if( length(list.files(file.path(.libPaths()[1],"ICARUSviewer/Temp")))!=0 ){ file.remove(file.path(.libPaths()[1],"ICARUSviewer/Temp",list.files(file.path(.libPaths()[1],"ICARUSviewer/Temp")))) }
}