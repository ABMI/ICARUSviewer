#'clinical Event data were extracted
#'@import   SqlRender
#'@import   dplyr
#'@param    connectionDetails
#'@param    cdmDatabaseSchema
#'@param    resultsDatabaseSchema
#'@param    cohortTable
#'@param    eventId
#'@export
call_event <- function(connectionDetails,
                       resultsDatabaseSchema,
                       cohortTable,
                       eventId){

    resultDatabaseSchema <- paste0(Resultschema,".dbo")
    connectionDetails <-connectionDetails
    connection <- DatabaseConnector::connect(connectionDetails)

    Sys.setlocale(category="LC_CTYPE", locale="C")

    sql <- 'select * FROM @resultDatabaseSchema.@cohortTable WHERE cohort_definition_id = @eventId'
    sql <- SqlRender::renderSql(sql,
                                resultDatabaseSchema = resultDatabaseSchema,
                                cohortTable = cohortTable,
                                eventId = eventId)$sql
    eventData<-DatabaseConnector::querySql(connection, sql)
    colnames(eventData)<-SqlRender::snakeCaseToCamelCase(colnames(eventData))
}

#'combine and calculate incidence rate of clinical event
#'@import   dplyr
#'@param    eventData
#'@export
eventIncidence <- function(eventData){

    eventSubject <- eventData %>% select(subjectId, cohortStartDate)
    colnames(eventSubject)[2] <- "eventStartDate"
    eventInc <- inner_join(eventSubject,cohortData,by = "subjectId")
    eventAfterIndex <- eventInc %>% filter(cohortStartDate < eventStartDate)
    eventAfterIndex <- eventAfterIndex %>%
        mutate(eventDuration = as.numeric(difftime(eventStartDate,cohortStartDate,units = "days")),
               observDuration = as.numeric(difftime(cohortEndDate,cohortStartDate,units = "days")) ) %>%
        mutate(eventDurationYear = floor(eventDuration/365.25),
               observDurationYear = floor(observDuration/365.25) ) %>%
        mutate(observDurationYear = if_else(observDurationYear>20,20,observDurationYear) )

    sub <- subset(eventAfterIndex,cohortDefinitionId == 2)
    eventResult <- data.frame()
    for(j in unique(eventAfterIndex$cohortDefinitionId) ){

        sub <- subset(eventAfterIndex,cohortDefinitionId == j)

        for(i in 0:max(sub$observDurationYear)){
            cohortDefinitionId <- j
            year <- i
            eventCount <- sum(sub$eventDurationYear == i)
            totalCount <- length(unique(sub[sub$observDurationYear >= i,]$subjectId) )

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
#'@param  eventResult
#'@export
plotEventRate <- function(eventResult){
    eventplot <- ggplot(data = eventResult) +
        geom_errorbar(aes(x = as.factor(year),ymin = lower,ymax = upper,group = cohortDefinitionId, colour = as.factor(cohortDefinitionId))) +
        geom_point(aes(x = as.factor(year),y = incidenceRate, group = cohortDefinitionId, colour = as.factor(cohortDefinitionId)), size = 1.5) +
        geom_line(aes(x = as.factor(year),y = incidenceRate, group = cohortDefinitionId, colour = as.factor(cohortDefinitionId)), size = 1) +
        xlab("time (years)") +
        ylab("Clinical event rate per 1 year")+
        coord_cartesian(xlim = c(0,16))+
        theme_bw()+
        theme(legend.position = c(0.9,0.85),
              legend.background = element_rect(colour = "black", size = 0.3),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 13),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 13),
              strip.text.x = element_text(size = 15)) +
        scale_color_discrete(name = "Cohort Id")
}
