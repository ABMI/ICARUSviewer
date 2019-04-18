#'exacerbation count according to asthma cohort
#'@import dplyr
#'@export
#'
#'
exacerbaManufacture<-function(){
    df <- exacerbation %>%
        rename(maxExacerbationCount = cohortDefinitionId) %>%
        left_join(demographicData[,c('cohortDefinitionId','personId')], by = c("subjectId"= "personId"))
    #exacerbation %>% group_by(cohortDefinitionId) %>% summarise(sum = n())
    return(df)
}

#'sum the exacerbation count according to asthma cohort
#'@import dplyr
#'@param exacerbationCount         the result of exacerbaManufacture function
#'@param cohortDefinitionIdSet
#'@export
#'
#'
sumCountExacerbation<-function(exacerbationCount,
                               cohortDefinitionIdSet){

    df <- exacerbationCount  %>%
        filter(maxExacerbationCount !=0) %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
        group_by(cohortDefinitionId, maxExacerbationCount) %>%
        summarise(count = n()) %>%
        left_join(demographicData %>% group_by(cohortDefinitionId) %>% summarise(totalCount = n_distinct(personId)), by = "cohortDefinitionId")


    df<-as.data.frame(df)

    return(df)

}

#'plot exacerbation Count
#'@import dplyr
#'@import ggplot2
#'@param sumCountExacerbation         the result of sumCountExacerbation function
#'@export
#'
#'
plotExacerbationCount<-function(sumCountExacerbation){

    exacerbationPlot <- sumCountExacerbation %>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD",
                                                       "ATA")) ) %>%
        ggplot(aes(x = as.factor(maxExacerbationCount), y = count, group = cohortDefinitionId, colour = cohortDefinitionId)) +
        geom_point(size = 2.5)+
        geom_line(size = 2) +
        labs(x = "Maximum Exacerbation Count per 2 years", y = "person Count")+
        theme_bw()+
        scale_color_brewer(palette = "Dark2")+
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank())

    return(exacerbationPlot)
}

#'table of exacerbation Count
#'@import dplyr
#'@import reshape2
#'@param sumCountExacerbation         the result of sumCountExacerbation function
#'@export
#'
#'
tableExacerbationCoun <- function(sumCountExacerbation){
    df <- sumCountExacerbation %>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD",
                                                       "ATA")) ) %>%
        mutate(percent = paste0(round((count/totalCount)*100,2), "%")) %>%
        mutate(result = paste0(count, "(",percent,")")) %>%
        select(cohortDefinitionId, maxExacerbationCount, result)

    out <- dcast(df,maxExacerbationCount~cohortDefinitionId)
    colnames(out)[1] <-"Exacerbation Count per 2 year"

    return(out)
}

#'t-test p-value calculate about exacerbation count
#'@import dplyr
#'@import reshape2
#'@param exacerbationCount         the result of exacerbaManufacture function
#'@param cohortDefinitionIdSet
#'@export
#'
#'
p_value_ExacerbationCouny <- function(exacerbationCount,
                                     cohortDefinitionIdSet){

    df <- exacerbationCount  %>%
        filter(maxExacerbationCount !=0) %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD",
                                                       "ATA")) )

    t.test <- t.test(data = df, maxExacerbationCount~cohortDefinitionId)
    average <- t.test$estimate
    p_value <- t.test$p.value

    out <- paste0('Mean of ', unique(df$cohortDefinitionId)[1], 'Exacerbation Count (per 2 years) : ', average[1],"\n",
                  'Mean of ', unique(df$cohortDefinitionId)[2], 'Exacerbation Count (per 2 years) : ', average[2],"\n",
                  't-test p-value : ',p_value)

    return(out)
}

