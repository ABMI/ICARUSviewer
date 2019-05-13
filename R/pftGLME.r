# check.packages("lme4")
# check.packages("lmerTest")
# check.packages("effects")
#
# PFTmanufacture <- function(measurementData,
#                            measurementType,
#                            cohortDefinitionIdSet){
#
#     out <- measurementData %>%
#         filter( measurementConceptId == measurementType ) %>%
#         filter( time >= 0 ) %>%
#         filter( valueAsNumber < 200 ) %>%
#         filter( cohortDefinitionId %in% cohortDefinitionIdSet )%>%
#         mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5),
#                                             labels = c("Asthma", "Non-Severe Asthma",
#                                                        "Severe Asthma", "AERD",
#                                                        "ATA"))) %>%
#         mutate( cohortDefinitionId = as.character(cohortDefinitionId) )
#
#     return(out)
# }

#'use linear mixed model regression, find regression line and its CI 95%
#'@import lme4
#'@import lmerTest
#'@import effects
#'@param pftmanufacData result of PFTmanufacture function
#'@export
#'
lmePft <- function(pftmanufacData){

    split_list <- split(pftmanufacData, pftmanufacData$cohortDefinitionId)

    calLmm <- lapply(split_list, FUN = function(x){

        df <- as.data.frame(x)
        lmm_randomSIind  <- lmer(formula = log(valueAsNumber) ~ time + (1|subjectId) + (0 + time|subjectId), data = df, REML = F )
        fixef_value <- t(lme4::fixef(lmm_randomSIind))

        effect_value <- effects::effect("time", lmm_randomSIind)

        effect_value$lower <- exp(effect_value$lower)
        effect_value$upper <- exp(effect_value$upper)
        effect_value$fit   <- exp(effect_value$fit)

        effect_value_df <- as.data.frame(effect_value)

        out <- list(fixef_value,
                    effect_value_df)

        return(out)
        })

    return(calLmm)
}

#'plot pft trajectory line and its CI
#'@import ggplot
#'@import dplyr
#'@param PFTmanufacData
#'@param lmePftData
#'@param cohortDefinitionIdSet
#'@param pftIndividual       logical (TRUE/FALSE)
#'@export
#'

plotpftLmm <- function(PFTmanufacData,
                       lmePftData,
                       cohortDefinitionIdSet,
                       pftIndividual = TRUE){

    plotpft <- ggplot(data = PFTmanufacData)
    split_list <- split(PFTmanufacData, PFTmanufacData$cohortDefinitionId)

    if(pftIndividual){
        i <- 1
        while(1){
            df <- split_list[[i]]
            plotpft <- plotpft + geom_line(data = df, aes(x = time, y = valueAsNumber, group = subjectId, colour = cohortDefinitionId) )
            i <- i + 1
            if(i > length(cohortDefinitionIdSet) ) break
        }
    }
    plotpft <- plotpft +
        stat_function(fun = function(x)exp(lmePftData[[1]][[1]][1] + lmePftData[[1]][[1]][2]*x), geom = "line", size = 1, colour = 'red') +
        stat_function(fun = function(x)exp(lmePftData[[2]][[1]][1] + lmePftData[[2]][[1]][2]*x), geom = "line", size = 1, colour = 'blue')

    plotpft <- plotpft +
        geom_line(data = lmePftData[[1]][[2]], aes(x = time, y = lower), linetype = "longdash", size = .5, colour = 'red') +
        geom_line(data = lmePftData[[1]][[2]], aes(x = time, y = upper), linetype = "longdash", size = .5, colour = 'red') +
        geom_line(data = lmePftData[[2]][[2]], aes(x = time, y = lower), linetype = "longdash", size = .5, colour = 'blue') +
        geom_line(data = lmePftData[[2]][[2]], aes(x = time, y = upper), linetype = "longdash", size = .5, colour = 'blue')

    return(plotpft)
}


# #FEV1/FVC(%) = 3011505, FEV1(%) = 3011708
# pftmanfac_data<- PFTmanufacture(measurementData,
#                                 measurementType = 3011708,
#                                 cohortDefinitionIdSet = c(4,5))
# lmePft_data <- lmePft(pftmanufacData = pftmanfac_data)
# plotpftLmm(PFTmanufacData = pftmanfac_data,
#            lmePftData = lmePft_data,
#            cohortDefinitionIdSet = c(4,5),
#            pftIndividual = TRUE)
# ggplot2::ggsave(file.path(outputFolder, "AERDATA_FEV1.png"))
