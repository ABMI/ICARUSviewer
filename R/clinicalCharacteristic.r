#'analysis of demographic characteristics according to disease cohort
#'@import dplyr
#'@param cohortDefinitionIdSet
#'@export

clinicalCharManufacture<-function(cohortDefinitionIdSet){
    demog <- demographicData %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
        #filter(age >=12) %>%
        mutate( followUpDuration = round(followUpDuration/365.25, 2) ) %>%
        select( cohortDefinitionId,personId, genderConceptId, age, followUpDuration )

    measure <- measureData %>%
        filter(measurementConceptId == 3011708,
               time <= 0) %>%
        group_by(cohortDefinitionId,subjectId) %>%
        filter(time == max(time)) %>%
        select( cohortDefinitionId,subjectId,measurementConceptId,time,valueAsNumber)

    out <- left_join(demog,measure,by = c("personId" = "subjectId", "cohortDefinitionId" = "cohortDefinitionId"))

    out <- out %>%
        mutate(cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,
                                                                          51,52,53,54),
                                           labels = c("Asthma", "Non-Severe Asthma",
                                                      "Severe Asthma", "AERD","ATA",
                                                      "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4") ) )
    return(out)
}

# demographicData%>%filter(cohortDefinitionId %in% c(51,52,53,54))%>% group_by(cohortDefinitionId)%>% summarise(count = n())
# totalCohort %>%filter(cohortDefinitionId %in% c(51,52,53,54))%>% group_by(cohortDefinitionId)%>% summarise(count = n())
# out%>% group_by(cohortDefinitionId)%>% summarise(count = n())

#'analysis of demographic characteristics : mean, SD
#'@param clinicalCharData  result of clinicalCharManufacture
#'@export

characterAnalysis <- function(clinicalCharData){

    clinicalCharData$cohortDefinitionId <- as.character(clinicalCharData$cohortDefinitionId)

    split_df <- split(clinicalCharData,clinicalCharData$cohortDefinitionId)

    cal <- sapply(split_df, FUN = function(x){

        totalPopulation <- nrow(x)
        age_meanSd <- paste( round(mean(x$age, na.rm = T),2), "+/-", round(sd(x$age, na.rm = T),2) )
        follow_up_meanSd <- paste( round(mean(x$followUpDuration, na.rm = T),2), "+/-", round(sd(x$followUpDuration, na.rm = T),2) )
        femalePropor <- paste( round((sum(x$genderConceptId == 8532, na.rm = T)/nrow(x))*100,2),"%" )
        FEV1base_meanSd <- paste( round(mean(x$valueAsNumber, na.rm = T),2),"+/-",round(sd(x$valueAsNumber, na.rm = T),2),
                                  "( N =", sum(!is.na(x$valueAsNumber)), ")" )

        df <- data.frame(totalPopulation = totalPopulation,
                         age = age_meanSd,
                         followUpDuration = follow_up_meanSd,
                         femaleProportion = femalePropor,
                         baseFEV1 = FEV1base_meanSd,
                         stringsAsFactors = FALSE)
        return(df)
    })

    value <- c("totalPopulation (persons)","age (years)","followUp Duration (years)","Female Proportion (%)","baseFEV1 (%)")
    cal <- cbind(value,cal)

    return(cal)
}

#'analysis of demographic characteristics : p-value using ANOVA with Tukey
#'@param clinicalCharData  result of clinicalCharManufacture
#'@export

characPvalue <- function(clinicalCharData){

    clinicalCharData$cohortDefinitionId <- as.character(clinicalCharData$cohortDefinitionId)

    ANOVA <- function(x){
        f <- as.formula(paste(x,"~cohortDefinitionId"))
        anova <- aov(f, data = clinicalCharData)
        a<-summary(anova)
        anova_p_value <- round(unlist(a)[9],3)
        tukey <- TukeyHSD(anova)
        tukey_p_value<- as.data.frame( t( round( tukey$cohortDefinitionId[,4],3 ) ) )
        p_value <- data.frame(anova_p_value,tukey_p_value)
        return(p_value)
    }
    age_anova <- ANOVA("age")
    followUpDuration_anova <- ANOVA("followUpDuration")
    gender_anova <- ANOVA("genderConceptId")
    FEV1_anova <- ANOVA("valueAsNumber")

    value <- c("age_ANOVA","followUpDuration_ANOVA","gender_ANOVA","FEV1_ANOVA")
    outcome <- rbind(age_anova, followUpDuration_anova, gender_anova, FEV1_anova)
    outcome <- cbind(value,outcome)

    return(outcome)
}

