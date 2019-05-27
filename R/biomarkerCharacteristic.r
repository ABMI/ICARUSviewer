#'analysis of demographic characteristics according to disease cohort
#'@import dplyr
#'@param cohortDefinitionIdSet
#'@param logTransformBiomarkerSet
#'@export

biomarkerManufac<-function(cohortDefinitionIdSet,
                           logTransformBiomarkerSet){
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
                                  labels = measurementId$measureName) ) %>%
        mutate(biomarker = as.character(biomarker))

    output <- out %>%
        filter(valueAsNumber > 0) %>%
        mutate(valueAsNumber = if_else(biomarker %in% logTransformBiomarkerSet, log(valueAsNumber), valueAsNumber) )

    return(output)
}

#'analysis of biomarker characteristics : mean, SD
#'@param biomarkerData  result of biomarkerManufac
#'@export

BiomarkerAnalysis <- function(biomarkerData){

    biomarkerData$cohortDefinitionId <- as.character(biomarkerData$cohortDefinitionId)

    split_df <- split(biomarkerData,biomarkerData$cohortDefinitionId)

    cal <- sapply(split_df, FUN = function(x){
        measurementId$maesurementConceptId

        out<-data.frame()
        for(i in measurementId$maesurementConceptId ){
            sub <- subset(x,x$measurementConceptId==i)
            meanSd <- paste( round(mean(sub$valueAsNumber, na.rm = T),2), "+/-", round(sd(sub$valueAsNumber, na.rm = T),2) ,
                             "( N =",length(unique(sub$subjectId)), ")")
            meanSd_df <- data.frame(meanSd, stringsAsFactors = FALSE)

            out<-rbind(out,meanSd_df)
        }

        return(out)
    })

    outcome <- as.data.frame(cal)
    biomarker <- measurementId$measureName

    outcome <- cbind(biomarker,outcome)
    totalPopulation <- demographicData %>%
        mutate(cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,
                                                                          51,52,53,54),
                                           labels = c("Asthma", "Non-Severe Asthma",
                                                      "Severe Asthma", "AERD","ATA",
                                                      "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4") ) )%>%
        filter(cohortDefinitionId %in% unique(biomarkerData$cohortDefinitionId) ) %>%
        group_by(cohortDefinitionId) %>%
        summarise(totalPopulation = n_distinct(personId))

    totalPopulation <- data.frame( matrix(as.matrix(totalPopulation)[,2],nrow = 1) )
    totalPopulation <- cbind('totalPopulation',totalPopulation)
    colnames(totalPopulation) <- colnames(outcome)

    outcome <- rbind(totalPopulation,outcome)

    return(outcome)
}

#'analysis of biomarker characteristics : p-value if length of cohortDefinitionIdSet is bigger than 2, use ANOVA with Tukey else t.test
#'@param biomarkerData  result of biomarkerManufac
#'@export

biomarkerPvalue <- function(biomarkerData){

    biomarkerData$cohortDefinitionId <- as.character(biomarkerData$cohortDefinitionId)

    if(length(unique(biomarkerData$cohortDefinitionId))>2){
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
        for(i in measurementId$maesurementConceptId ){
            sub <- subset(biomarkerData,biomarkerData$measurementConceptId==i)
            pvalue <- ANOVA(sub)

            out <- rbind(out,pvalue)
        }
    } else if(length(unique(biomarkerData$cohortDefinitionId)) == 2){
        Ttest <- function(x){
            ttest <- t.test(valueAsNumber~cohortDefinitionId, data = x)
            ttest_p_value <- round(ttest$p.value,3)
            p_value <- data.frame(ttest_p_value)
            return(p_value)
        }

        out <- data.frame()
        for(i in measurementId$maesurementConceptId  ){
            sub <- subset(biomarkerData,biomarkerData$measurementConceptId == i)
            pvalue <- Ttest(sub)
            colnames(pvalue) <- "t.test_pvalue"

            out <- rbind(out,pvalue)
        }
    }
    biomarker <- measurementId$measureName
    out <- cbind(biomarker,out)

    return(out)
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


