#'ready for plotting PFT data
#'@import dplyr
#'@param measurementData
#'@param measurementConceptId         FEV1/FVC(%) = 3011505, FEV1(%) = 3011708
#'@param cohortDefinitionIdSet
#'@export
#'
PFTmanufacture <- function(measurementData,
                           measurementType,
                           cohortDefinitionIdSet){

    out <- measurementData %>%
        filter(measurementConceptId == measurementType) %>%
        filter(time >= 0) %>%
        filter(valueAsNumber < 200) %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet)%>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,51,52,53,54,100,101,300,301),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD","ATA",
                                                       "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4",
                                                       "exacerbation","GINA_STEP_4/5",
                                                       "exacerbation","not_exacerbation"))) %>%
        mutate( cohortDefinitionId = as.character(cohortDefinitionId) )

    return(out)
}

#'plot PFT according to time flow
#'@import ggplot2
#'@param PFTmanufactured        result of PFTmanufacture code
#'@export

plotPFT<-function(PFTmanufactured){

    plot<-ggplot(data = PFTmanufactured,
                 aes(x = time, y = valueAsNumber, group = cohortDefinitionId, colour = as.factor(cohortDefinitionId)))+
        geom_smooth(method = 'gam',formula = y~s(x,bs="cs"))+
        coord_cartesian(xlim = c(0,15),ylim = c(50,100))+
        xlab("years")+
        scale_color_brewer(palette = "Set1")+
        #facet_wrap(~cohortDefinitionId)+
        theme_bw()+
        theme(legend.title = element_blank(),
              #legend.position = "none",
              legend.text = element_text(size = 15),
              plot.title = element_text(size = 17),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 15),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 15),
              strip.text.x = element_text(size = 15))

    return(plot)
}

#'calculate count of cohort population
#'@import dplyr
#'@param PFTmanufactured              result of PFTmanufacture code
#'@export
#'
pft_count_table<-function(PFTmanufactured){

    seq_fun <- function(i){
        paste(i,"-",i+5)
    }

    level <- seq(0,25,5)

    df_total<-PFTmanufactured %>%
        group_by(cohortDefinitionId) %>%
        summarise(total_count = n_distinct(subjectId))

    df_count<-PFTmanufactured %>%
        mutate(timeSection = (time%/%5)*5 ) %>%
        group_by(cohortDefinitionId,timeSection) %>%
        summarise(count = n_distinct(subjectId)) %>%
        mutate(timeSection = factor(timeSection, levels = level,
                                    labels = seq_fun(level)))

    df_dcast <- reshape2::dcast(df_count, cohortDefinitionId~timeSection)
    df_dcast <- df_dcast %>%
        merge(df_total, by = "cohortDefinitionId")
    colnames(df_dcast)[1] <- "duration (years)"

    return(df_dcast)

}

#'calculate prediction values of PFT that 5, 10, 15 years after treatment
#'@import dplyr
#'@import mgcv
#'@param PFTmanufactured              result of PFTmanufacture code
#'@export
#'
pft_predict_table<-function(PFTmanufactured){

    pft_split <- split(PFTmanufactured, PFTmanufactured$cohortDefinitionId)
    pft_split <- pft_split[as.character(unique(PFTmanufactured$cohortDefinitionId))]

    time <- c(0,5,10,15)

    pft_predict <- lapply(pft_split, FUN = function(x){

        gam_out <- gam(valueAsNumber~s(time, bs = "cs"), data = x)

        need.data<-data.frame(time)
        gam_pred <- as.data.frame(predict(gam_out, newdata = need.data))
        percent.decline <- ((gam_pred-gam_pred[1,])/gam_pred[1,])*100

        out <- data.frame(gam_pred,percent.decline)

        names(out) <- c('predict', 'percent.decline')

        return(out)
    })

    df <- data.frame(pft_predict)
    outcome <- as.data.frame(t(df))
    outcome <- cbind(c(paste(unique(PFTmanufactured$cohortDefinitionId)[1], "predict PFT value"),
                       paste(unique(PFTmanufactured$cohortDefinitionId)[1], "percent decline (%)"),
                       paste(unique(PFTmanufactured$cohortDefinitionId)[2], "predict PFT value"),
                       paste(unique(PFTmanufactured$cohortDefinitionId)[2], "percent decline (%)") ),
                     outcome)

    colnames(outcome) <-c("time (year)",time)

    return(outcome)

}

