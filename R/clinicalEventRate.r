#'clinical Event data were extracted
#'@import   SqlRender
#'@import   dplyr
#'@param    cohort_definition_id_set
#'@param    cohortId_event
#'@export
call_event <- function(cohort_definition_id_set,
                       cohortId_event){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  
  eventcohort<-totalCohort %>% filter(cohortDefinitionId == cohortId_event)
  cohortData<-totalCohort %>% filter(cohortDefinitionId %in% cohort_definition_id_set) 
  out <- list(eventcohort = eventcohort,
              cohortData = cohortData)
  
  return(out)
}

#'combine and calculate incidence rate of clinical event
#'@import   dplyr
#'@param    callEvent_result
#'@export
event_incidence <- function(callEvent_result){
  
  event_data = callEvent_result[[1]]
  cohort_data = callEvent_result[[2]]
  
  eventSubject <- event_data %>% select(subjectId, cohortStartDate)
  colnames(eventSubject)[2] <- "eventStartDate"
  eventInc <- inner_join(eventSubject,cohort_data,by = "subjectId")
  eventAfterIndex <- eventInc %>% filter(cohortStartDate < eventStartDate)
  eventAfterIndex <- eventAfterIndex %>%
    mutate(eventDuration = as.numeric(difftime(eventStartDate,cohortStartDate,units = "days")),
           observDuration = as.numeric(difftime(cohortEndDate,cohortStartDate,units = "days")) ) %>%
    mutate(eventDurationYear = floor(eventDuration/365.25),
           observDurationYear = floor(observDuration/365.25) ) %>%
    mutate(observDurationYear = if_else(observDurationYear>20,20,observDurationYear) )
  
  totalCohortObservIndex <- cohort_data %>%     
    mutate(observDuration = as.numeric(difftime(cohortEndDate,cohortStartDate,units = "days")) ) %>%
    mutate(observDurationYear = floor(observDuration/365.25) ) %>%
    mutate(observDurationYear = if_else(observDurationYear>20,20,observDurationYear) )
  
  eventResult <- data.frame()
  for(j in unique(eventAfterIndex$cohortDefinitionId) ){
    
    sub <- subset(eventAfterIndex,cohortDefinitionId == j)
    totalSub <- subset(totalCohortObservIndex,cohortDefinitionId == j)
    
    for(i in 0:max(sub$observDurationYear)){
      cohortDefinitionId <- j
      year <- i
      eventCount <- sum(sub$eventDurationYear == i)
      totalCount <- length(unique(totalSub[totalSub$observDurationYear >= i,]$subjectId) )
      
      df <- data.frame(cohortDefinitionId,year,eventCount,totalCount)
      eventResult <- rbind(eventResult,df)
    }
  }
  
  eventResult <- eventResult %>%
    mutate(incidenceRate = eventCount/totalCount) %>%
    mutate(se = sqrt(eventCount)/totalCount ) %>%
    mutate(upper = incidenceRate + se*1.96,
           lower = incidenceRate - se*1.96)
  
  return(eventResult)
}

#'plot for clinical event rate
#'@import ggplot2
#'@param  event_result
#'@export
plot_event_rate <- function(event_result){
  Predict_colourList <- c("red","blue","#66FF66", "orange","#9900cc","#0099cc")
  eventplot <- ggplot(data = event_result) +
    geom_errorbar(aes(x = as.factor(year),ymin = lower,ymax = upper,group = cohortDefinitionId, colour = as.factor(cohortDefinitionId))) +
    geom_point(aes(x = as.factor(year),y = incidenceRate, group = cohortDefinitionId, colour = as.factor(cohortDefinitionId)), size = 1.5) +
    geom_line(aes(x = as.factor(year),y = incidenceRate, group = cohortDefinitionId, colour = as.factor(cohortDefinitionId)), size = 1) +
    xlab("time (years)") +
    ylab("Mean clinical event count per 1 year")+
    coord_cartesian(xlim = c(0,16))+
    theme_bw()+
    theme(legend.position = c(0.9,0.85),
          legend.background = element_rect(colour = "black", size = 0.3),
          axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15)) +
    scale_colour_manual(values = Predict_colourList) 
    # scale_color_discrete(name = "Cohort Id")
  
  return(eventplot)
}
