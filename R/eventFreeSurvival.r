#'get measurement baseline measurement data
#'@import dplyr
#'@import survival
#'@import ggplot2
#'@import ggfortify
#'@param cohort_definition_id_set
#'@param cohortId_event
#'@param targetSurvivalEndDate
#'@export
eventFreeSurvival <- function(cohort_definition_id_set,
                              cohortId_event,
                              targetSurvivalEndDate){
  sub_totalCohort <- totalCohort %>% dplyr::filter( cohortDefinitionId %in% cohort_definition_id_set )
  event_cohort    <- totalCohort %>% dplyr::filter( cohortDefinitionId == cohortId_event )
  # unique(totalCohort$cohortDefinitionId)
  eventSubject <- event_cohort %>% dplyr::select(subjectId, cohortStartDate)
  colnames(eventSubject)[2] <- "eventStartDate"
  
  templet <- sub_totalCohort %>% dplyr::select(cohortDefinitionId,subjectId,cohortStartDate,cohortEndDate)
  
  eventReady <- left_join(sub_totalCohort,eventSubject, by = c("subjectId") ) %>%
    filter(cohortStartDate < eventStartDate) 
  eventInc <- left_join(templet,eventReady,by = c("cohortDefinitionId","subjectId","cohortStartDate","cohortEndDate") ) %>%
    dplyr::mutate(eventDuration = as.numeric(difftime(eventStartDate,cohortStartDate,units = "days")),
                  observDuration = as.numeric(difftime(cohortEndDate,cohortStartDate,units = "days")) ) %>%
    dplyr::mutate(observDuration = if_else(observDuration>targetSurvivalEndDate,targetSurvivalEndDate,observDuration) )  %>%
    dplyr::group_by(cohortDefinitionId, subjectId, cohortStartDate, observDuration) %>%
    dplyr::summarise(survivalTime = min(eventDuration) ) %>%
    dplyr::mutate(outcome = if_else(!is.na(survivalTime) & survivalTime <= observDuration, 1, 0) ) %>%
    dplyr::mutate(survivalTime = if_else(!is.na(survivalTime),survivalTime,observDuration) ) %>%
    dplyr::mutate(survivalTime = if_else(survivalTime > observDuration, targetSurvivalEndDate, survivalTime) )
  
  survfit <- survival::survfit( survival::Surv(survivalTime, outcome)~cohortDefinitionId, data = eventInc )
  
  ##result
  pvalue  <- survival::survdiff( survival::Surv(survivalTime, outcome)~cohortDefinitionId, data = eventInc )
  Predict_colourList <- c("red","blue","#66FF66", "orange","#9900cc","#0099cc")
  eventFreeSurvivalPlot<- autoplot(survfit) + 
    scale_color_manual(values = Predict_colourList,aesthetics = "colour") +
    ylab("survival probability") + 
    xlab("time (years)") + 
    # annotate("text", label = "p-value < 0.001", x = 100, y = 0.85, size = 5) +
    theme_bw()+
    theme(legend.position = "none", 
          #c(0.9,0.85),
          legend.background = element_rect(colour = "black", size = 0.3),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15))
  
  resultList <- list(pvalue = pvalue,
                     eventFreeSurvivalPlot = eventFreeSurvivalPlot)
  
}
