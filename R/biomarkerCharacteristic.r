#'analysis of demographic characteristics according to disease cohort
#'@import dplyr
#'@param cohortDefinitionIdSet
#'@export

biomarkerManufac<-function(cohortDefinitionIdSet){
    biomarker <- measureData %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
        filter(measurementConceptId %in% measurementId$maesurementConceptId)

    out <- biomarker %>%
        mutate(cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,
                                                                          51,52,53,54),
                                           labels = c("Asthma", "Non-Severe Asthma",
                                                      "Severe Asthma", "AERD","ATA",
                                                      "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4") ),
               biomarker = factor(measurementConceptId, levels = measurementId$maesurementConceptId,
                                  labels = measurementId$measureName) )
    return(out)
}

#'analysis of biomarker characteristics : mean, SD
#'@param biomarkerData  result of biomarkerManufac
#'@export

BiomarkerAnalysis <- function(biomarkerData){

    biomarkerData$cohortDefinitionId <- as.character(biomarkerData$cohortDefinitionId)

    split_df <- split(biomarkerData,biomarkerData$cohortDefinitionId)

    cal <- sapply(split_df, FUN = function(x){

        out<-data.frame()
        for(i in as.character(unique(x$biomarker)) ){
            sub <- subset(x,x$biomarker==i)
            meanSd <- paste( round(mean(sub$valueAsNumber, na.rm = T),2), "+/-", round(sd(sub$valueAsNumber, na.rm = T),2) ,
                             "( N =",length(unique(sub$subjectId)), ")")
            meanSd_df <- data.frame(meanSd, stringsAsFactors = FALSE)
            #colnames(meanSd_df) <- c(cohortDefinitionId,meanSd)

            out<-rbind(out,meanSd_df)
        }

        return(out)
    })

    outcome <- as.data.frame(cal)
    biomarker <- as.character(unique(biomarkerData$biomarker) )

    outcome <- cbind(biomarker,outcome)

    totalPopulation <- data.frame( matrix(c("total Population counts",sapply(split_df,FUN = function(x){length(unique(x$subjectId))} ) ),nrow = 1 ) )
    colnames(totalPopulation) <- colnames(outcome)

    outcome <- rbind(totalPopulation,outcome)

    return(outcome)
}

#'analysis of biomarker characteristics : p-value using ANOVA with Tukey
#'@param biomarkerData  result of biomarkerManufac
#'@export

biomarkerPvalue <- function(biomarkerData){

    biomarkerData$cohortDefinitionId <- as.character(biomarkerData$cohortDefinitionId)

    ANOVA <- function(x){
        anova <- aov(valueAsNumber~cohortDefinitionId, data = x)
        a<-summary(anova)
        anova_p_value <- round(unlist(a)[9],3)
        tukey <- TukeyHSD(anova)
        tukey_p_value<- as.data.frame( t( round( tukey$cohortDefinitionId[,4],3 ) ) )
        p_value <- data.frame(anova_p_value,tukey_p_value)
        return(p_value)
    }

    out <- data.frame()
    for(i in as.character(unique(biomarkerData$biomarker)) ){
        sub <- subset(biomarkerData,biomarkerData$biomarker==i)
        pvalue <- ANOVA(sub)

        out <- rbind(out,pvalue)
    }

    biomarker <- as.character(unique(biomarkerData$biomarker))
    outcome <- cbind(biomarker,out)

    return(outcome)
}

#'analysis of biomarker characteristics : p-value using Kruskal Wallis test
#'@param biomarkerData  result of biomarkerManufac
#'@export

biomarkerPvalue_KW <- function(biomarkerData){

    biomarkerData$cohortDefinitionId <- as.character(biomarkerData$cohortDefinitionId)

    KWtest <- function(x){
        KW <- kruskal.test(valueAsNumber~as.factor(cohortDefinitionId), data = x)
        k <- KW$p.value
        tukey <- TukeyHSD(anova)
        tukey_p_value<- as.data.frame( t( round( tukey$cohortDefinitionId[,4],3 ) ) )
        p_value <- data.frame(anova_p_value,tukey_p_value)
        return(p_value)
    }

    out <- data.frame()
    for(i in as.character(unique(biomarkerData$biomarker)) ){
        sub <- subset(biomarkerData,biomarkerData$biomarker==i)
        pvalue <- ANOVA(sub)

        out <- rbind(out,pvalue)
    }

    biomarker <- as.character(unique(biomarkerData$biomarker))
    outcome <- cbind(biomarker,out)

    return(outcome)
}


