#'ready for plotting comorbidity data
#'@import dplyr
#'@param comorbidityData
#'@param cohortDefinitionIdSet
#'@export
#'

comorbManufacture <- function(comorbidityData,
                              cohortDefinitionIdSet){

    out <- comorbidityData %>%
        mutate(gapBetween = difftime(conditionStartDate,cohortStartDate, units = "day") ) %>%
        group_by(cohortDefinitionId, subjectId, cohortStartDate, conditionConceptId) %>%
        summarise(gapBetween = min(abs(gapBetween))) %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
        mutate(diseaseId = NA)

    for(i in 1:length(diseaseList$diseaseId)){
        out[out$conditionConceptId %in% diseaseList$conceptIdSet[[i]],]$diseaseId <- diseaseList$diseaseId[[i]]
    }

    out_joindemographic <- left_join(out, demographicData, by = c("cohortDefinitionId"="cohortDefinitionId","subjectId" = "personId"))

    metabolic <- out_joindemographic %>%
        filter(age>=50) %>%
        left_join( demographicData %>% filter(age>=50) %>% group_by(cohortDefinitionId) %>% summarise(total = n_distinct(personId)), by = "cohortDefinitionId" ) %>%
        filter(diseaseId %in% c(11,12,13,14,15,16,19)) %>%
        group_by(cohortDefinitionId, diseaseId) %>%
        summarise(Count = n_distinct(subjectId),
                  totalCount = unique(total)) %>%
        mutate(notdisease = totalCount - Count)

    immune <- out_joindemographic %>%
        filter(age>=12)  %>%
        left_join( demographicData %>% filter(age>=12) %>% group_by(cohortDefinitionId) %>% summarise(total = n_distinct(personId)), by = "cohortDefinitionId" ) %>%
        filter(diseaseId %in% c(1,3,5,6,7,8,9,10)) %>%
        group_by(cohortDefinitionId, diseaseId) %>%
        summarise(Count = n_distinct(subjectId),
                  totalCount = unique(total)) %>%
        mutate(notdisease = totalCount - Count)

    ready <- list(metabolic = metabolic,
                  immune = immune)

    return(ready)

}

#'ready for calculate Relative Ratio and CI
#'@import dplyr
#'@import epitools
#'@param comorbManufacData               the result of comorbManufacture code (list)
#'@param whichDisease                    metabolic or immune disease
#'@export
#'
calculateRR <- function(comorbManufacData,
                        whichDisease){

    df <- comorbManufacData[[whichDisease]]
    #str(ready)
    if ( min(df$cohortDefinitionId) == 2){

        split <- split(df, df$diseaseId)
        RRresult <- lapply(split, FUN = function(x){
            simply <- x[,c("Count","notdisease")]
            simply <- simply[c(2,1),]
            row.names(simply)<-x$cohortDefinitionId[c(2,1)]
            RR_cal<-epitools::riskratio(as.matrix(simply))
            RR_cal$measure[2,]
        })

    } else {

        split <- split(df, df$diseaseId)
        RRresult <- lapply(split, FUN = function(x){
            simply <- x[,c("Count","notdisease")]
            row.names(simply)<-x$cohortDefinitionId
            RR_cal<-epitools::riskratio(as.matrix(simply))
            RR_cal$measure[2,]
        })
    }

    output <- t(as.data.frame(RRresult))
    output <- data.frame(output)
    return(output)
}

#'plot risk ratio and its CIs
#'@import dplyr
#'@import ggplot2
#'@param RRResult       the result of calculateRR code
#'@export
#'

RRplot <- function(RRResult){
    RR <- RRResult %>%
        mutate(comorbId = as.numeric(gsub("X","",row.names(RRResult)) ) ) %>%
        mutate(comorbName = factor(comorbId, levels = diseaseList$diseaseId,
                                   labels = diseaseList$diseaseName) )

    RRplotOut <- RR %>%
        ggplot(aes(x = comorbName, y = estimate))+
        geom_point()+
        geom_line()+
        geom_hline(yintercept = 1, size = 1)+
        geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
        geom_text(aes(y = upper+0.01, label = round(estimate,3)), size = 4 )+
        theme_bw()+
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 11),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank())

    return(RRplotOut)
}

#'calculate co-prevalence rate
#'@import dplyr
#'@import reshape2
#'@param comorbManufacData               the result of comorbManufacture code (list)
#'@param whichDisease                    metabolic or immune disease
#'@export
#'

co_prevtable <- function(comorbManufacData,
                         whichDisease){

    df <- comorbManufacData[[whichDisease]]
    #str(ready)
    df_coprev <- as.data.frame(df) %>%
        mutate(co_prevalence = paste0( round((Count/totalCount)*100,2),"%") ) %>%
        mutate(result = paste0(Count,"(",co_prevalence,")")) %>%
        mutate(diseaseName = factor(diseaseId, levels = diseaseList$diseaseId,
                                    labels = diseaseList$diseaseName) ) %>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD",
                                                       "ATA"))) %>%
        select(cohortDefinitionId, diseaseName, result)

    out <- dcast(df_coprev, diseaseName~cohortDefinitionId)

    return(out)
}
