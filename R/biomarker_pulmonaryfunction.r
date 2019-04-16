biomarkerList <- list ( cohortId = seq(11,11+13-1,1),
                        measurementName = c('EDN','periostin','Eotaxin1','Eotaxin2','SP_D','DPP10','MBL','TGFb1','chitinase','TIMP1','OPN','IL8','MPO' ),
                        measurementConceptId = c(2,3,4,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469))

measureId<-2
measureData %>%
    filter(cohortDefinitionId == 11)

PFTmanufacture (measurementData = measureData,
                measurementType = 3011708,
                biomarkerType = 2,
                biomarkerValue = 10,
                cohortDefinitionIdSet = 11)

    out <- measurementData %>%
        filter( cohortDefinitionId %in% cohortDefinitionIdSet)

    if(!is.null(biomarkerType)){

        biomarker <- out %>%
            filter(measurementConceptId == biomarkerType)

        subset(biomarker, biomarker$valueAsNumber >=median(biomarker$valueAsNumber))

        out <- out %>%
            mutate(cohortDefinitionId = if_else())
        filter(measurementConceptId == measurementType) %>%
            filter(time >= 0) %>%
            filter(valueAsNumber < 200) %>%
            filter(cohortDefinitionId %in% cohortDefinitionIdSet)%>%
            mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                                labels = c("Asthma", "Non-Severe Asthma",
                                                           "Severe Asthma", "AERD",
                                                           "ATA")))
    } else {
        out <- out %>%
            filter(measurementConceptId == measurementType) %>%
            filter(time >= 0) %>%
            filter(valueAsNumber < 200) %>%
            filter(cohortDefinitionId %in% cohortDefinitionIdSet)%>%
            mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
                                                labels = c("Asthma", "Non-Severe Asthma",
                                                           "Severe Asthma", "AERD",
                                                           "ATA")))

    }
    out <- out %>%
        filter()

