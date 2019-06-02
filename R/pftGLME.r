# check.packages("lme4")
# check.packages("lmerTest")
# check.packages("effects")
# #
# PFTmanufacture <- function(measurementData,
#                            measurementType,
#                            cohortDefinitionIdSet){
#
#     out <- measurementData %>%
#         filter(measurementConceptId == measurementType) %>%
#         filter(time >= 0) %>%
#         filter(valueAsNumber < 200) %>%
#         filter(cohortDefinitionId %in% cohortDefinitionIdSet)%>%
#         mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,51,52,53,54,100,101,300,301),
#                                             labels = c("Asthma", "Non-Severe Asthma",
#                                                        "Severe Asthma", "AERD","ATA",
#                                                        "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4",
#                                                        "exacerbation","GINA_STEP_4/5",
#                                                        "exacerbation","non-exacerbation"))) %>%
#         mutate( cohortDefinitionId = as.character(cohortDefinitionId) )
#
#     return(out)
# }
# fev1 <- PFTmanufacture(measureData, measurementType = 3011708, cohortDefinitionIdSet = c(300,301))

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
        lmm_randomSIind  <- lmer(formula = valueAsNumber ~ time + (1|subjectId) + (0 + time|subjectId), data = df, REML = F )
        fixef_value <- t(lme4::fixef(lmm_randomSIind))

        effect_value <- effects::effect("time", lmm_randomSIind)

        effect_value_df <- as.data.frame(effect_value)

        cohortDefinition <- unique(df$cohortDefinitionId)

        out <- list(cohortDefinition,
                    fixef_value,
                    effect_value_df)

        return(out)
        })

    return(calLmm)
}

#'plot pft trajectory line and its CI
#'@import ggplot
#'@import dplyr
#'@param pftmanufacData
#'@param lmePftData
#'@param pftIndividual                  logical (TRUE/FALSE)
#'@param ConfidencialIntervalVisualize  logical (TRUE/FALSE)
#'@export
#'

plotpftLmm <- function(pftmanufacData,
                       lmePftData,
                       pftIndividual = TRUE,
                       ConfidencialIntervalVisualize = TRUE){

    plotpft <- ggplot(data = pftmanufacData)
    split_list <- split(pftmanufacData, pftmanufacData$cohortDefinitionId)

    # #colour list
    # lmePftData[[1]][[1]] = "red"
    # lmePftData[[2]][[1]] = "orange"
    # lmePftData[[3]][[1]] = "green"
    # lmePftData[[4]][[1]] = "blue"
    # lmePftData[[5]][[1]] = "purple"
    colourList <- c("red","blue","green","orange","purple")

    if(pftIndividual){
        i <- 1
        while(1){
            df <- split_list[[i]]
            plotpft <- plotpft + geom_line(data = df, aes(x = time, y = valueAsNumber, group = subjectId, colour = cohortDefinitionId ), size = 0.4, alpha = 0.5)
            i <- i + 1
            if(i > length( unique(pftmanufacData$cohortDefinitionId) ) ) break
        }
    }

    intercept  <- function(x) lmePftData[[x]][[2]][1]
    slope      <- function(x) lmePftData[[x]][[2]][2]

    for (i in 1:length(unique(pftmanufacData$cohortDefinitionId))){
        plotpft <- plotpft + geom_abline(intercept = intercept(i), slope = slope(i), colour = colourList[i], size = 1 )
    }

    if(ConfidencialIntervalVisualize){
        for (i in 1:length(unique(pftmanufacData$cohortDefinitionId))){
            plotpft <- plotpft +
                geom_line(data = lmePftData[[i]][[3]], aes(x = time, y = lower), linetype = "longdash", size = 0.5, colour = colourList[i]) +
                geom_line(data = lmePftData[[i]][[3]], aes(x = time, y = upper), linetype = "longdash", size = 0.5, colour = colourList[i])
        }
    }

    plotpft <- plotpft +
        coord_cartesian(xlim = c(0,15))

    return(plotpft)
}
##################################################################################################


# plotpftLmm <- function(pftmanufacData,
#                        lmePftData,
#                        cohortDefinitionIdSet,
#                        pftIndividual = TRUE,
#                        alpha = 0.002,
#                        CIlineSize = 0){
#
#     plotpft <- ggplot(data = pftmanufacData)
#     split_list <- split(pftmanufacData, pftmanufacData$cohortDefinitionId)
#
#     if(pftIndividual){
#         i <- 1
#         while(1){
#             df <- split_list[[i]]
#             plotpft <- plotpft + geom_line(data = df, aes(x = time, y = valueAsNumber, group = subjectId), size = 0.01, alpha = alpha, colour = "orange")
#             i <- i + 1
#             if(i > length(cohortDefinitionIdSet) ) break
#         }
#     }
#
#     plotpft <- plotpft +
#         stat_function(fun = function(x)exp(lmePftData[[1]][[2]][1] + lmePftData[[1]][[2]][2]*x), geom = "line", size = 1, colour = 'brown') +
#         geom_line(data = lmePftData[[1]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'brown')+
#         geom_line(data = lmePftData[[1]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'brown')
#
#     if(length(cohortDefinitionIdSet) == 2){
#         plotpft <- plotpft  +
#             stat_function(fun = function(x)exp(lmePftData[[2]][[2]][1] + lmePftData[[2]][[2]][2]*x), geom = "line", size = 1, colour = 'blue') +
#             geom_line(data = lmePftData[[2]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'blue') +
#             geom_line(data = lmePftData[[2]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'blue')
#     }
#
#     else if(length(cohortDefinitionIdSet) == 5){
#         plotpft <- plotpft  +
#             stat_function(fun = function(x)exp(lmePftData[[2]][[2]][1] + lmePftData[[2]][[2]][2]*x), geom = "line", size = 1, colour = 'orange') +
#             geom_line(data = lmePftData[[2]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'orange') +
#             geom_line(data = lmePftData[[2]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'orange')+
#             stat_function(fun = function(x)exp(lmePftData[[3]][[2]][1] + lmePftData[[3]][[2]][2]*x), geom = "line", size = 1, colour = 'green') +
#             geom_line(data = lmePftData[[3]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'green') +
#             geom_line(data = lmePftData[[3]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'green')+
#             stat_function(fun = function(x)exp(lmePftData[[4]][[2]][1] + lmePftData[[4]][[2]][2]*x), geom = "line", size = 1, colour = 'blue') +
#             geom_line(data = lmePftData[[4]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'blue') +
#             geom_line(data = lmePftData[[4]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'blue')+
#             stat_function(fun = function(x)exp(lmePftData[[5]][[2]][1] + lmePftData[[5]][[2]][2]*x), geom = "line", size = 1, colour = 'purple') +
#             geom_line(data = lmePftData[[5]][[3]], aes(x = time, y = lower), linetype = "longdash", size = CIlineSize, colour = 'purple') +
#             geom_line(data = lmePftData[[5]][[3]], aes(x = time, y = upper), linetype = "longdash", size = CIlineSize, colour = 'purple')
#     }
#
#     return(plotpft)
# }

#
# # #FEV1/FVC(%) = 3011505, FEV1(%) = 3011708
# pftmanfac_data<- PFTmanufacture(measurementData,
#                                 measurementType = 3011708,
#                                 cohortDefinitionIdSet = c(51))
# lmePft_data <- lmePft(pftmanufacData = pftmanfac_data)
# plotpftLmm(pftmanufacData = pftmanfac_data,
#            lmePftData = lmePft_data,
#            cohortDefinitionIdSet = c(51),
#            pftIndividual = TRUE,
#            alpha = 0.5,
#            CIlineSize = 0)
# # plot_aerd +
# #     coord_cartesian(xlim = c(0,15),ylim = c(50,100)) +
# #     theme_bw()
# ggplot2::ggsave(file.path(outputFolder, "sub1_fev1_1.png"))
#
# pftmanfac_data<- PFTmanufacture(measurementData,
#                                 measurementType = 3011505,
#                                 cohortDefinitionIdSet = c(5,51,52,53,54))
# pftmanfac_data %>% group_by(cohortDefinitionId) %>% summarise(count = n_distinct(subjectId))
