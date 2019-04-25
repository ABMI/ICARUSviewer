#'ready for PFT data getting clear in detail
#'@import dplyr
#'@param measurementConceptId         FEV1/FVC(%) = 3011505, FEV1(%) = 3011708
#'@param cohortDefinitionIdSet
#'@param ageSection                   character Input separated by '/'
#'@export
#'
PFTmanufacture_detail <- function(measurementType,
                                  cohortDefinitionIdSet,
                                  ageSection = NULL){

    out <- measureData %>%
        filter(measurementConceptId == measurementType) %>%
        filter(time >= 0) %>%
        filter(valueAsNumber < 200) %>%
        filter(cohortDefinitionId %in% cohortDefinitionIdSet)%>%
        mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,
                                                                           51,52,53,54),
                                            labels = c("Asthma", "Non-Severe Asthma",
                                                       "Severe Asthma", "AERD","ATA",
                                                       "AERDsubtype1","AERDsubtype2","AERDsubtype3","AERDsubtype4") ))
    merge <- left_join(out, demographicData %>% select(personId,age,genderConceptId), by = c("subjectId" = "personId") )

    if(!is.null(ageSection)){
        #ageSection<-"12/40/60/100"
        ageSectionDivided <- as.numeric( unlist( strsplit(ageSection, "/") ) )
        merge$ageCut <- cut(merge$age, ageSectionDivided)
    }

    output <- merge[complete.cases(merge$ageCut),]

    return(output)
}


#'plot PFT according to time flow in detail (add more covariate)
#'@import ggplot2
#'@param PFTmanufactured        result of PFTmanufacture code
#'@param genderDivided          logical (TRUE/FALSE)
#'@export

plotPFT_detail<-function(PFTmanufactured,
                         genderDivided){

    plot<-ggplot(data = PFTmanufactured,
                 aes(x = time, y = valueAsNumber, group = cohortDefinitionId, colour = as.factor( cohortDefinitionId ) ) )+
        geom_smooth(method = 'gam',formula = y~s(x,bs="cs"))+
        coord_cartesian(xlim = c(0,15),ylim = c(50,100))+
        xlab("years")+
        scale_color_brewer(palette = "Set1")+
        #facet_grid(ageCut~genderConceptId)+
        #facet_grid(~)+
        theme_bw()

    if ( "ageCut" %in% colnames(PFTmanufactured) & genderDivided ){
        plot <- plot + facet_grid( ageCut ~ genderConceptId )
    } else if ( "ageCut" %in% colnames(PFTmanufactured) & !genderDivided ){
        plot <- plot + facet_wrap( ~ageCut )
    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & genderDivided ){
        plot <- plot + facet_wrap( ~genderConceptId )
    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & !genderDivided ){
        plot <- plot
    }

    plot <- plot +
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

#'calculate count of cohort population in detail
#'@import dplyr
#'@import reshape2
#'@param PFTmanufactured              result of PFTmanufacture code
#'@param genderDivided                logical (TRUE/FALSE)
#'@export
#'
pftCountTable_indetail<-function(PFTmanufactured,
                                 genderDivided){

    seq_fun <- function(i){
        paste(i,"-",i+5)
    }

    level <- seq(0,25,5)

    if ( "ageCut" %in% colnames(PFTmanufactured) & genderDivided ){
        div <- c("cohortDefinitionId","ageCut","genderConceptId")
    } else if ( "ageCut" %in% colnames(PFTmanufactured) & !genderDivided ){
        div <- c("cohortDefinitionId","ageCut")
    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & genderDivided ){
        div <- c("cohortDefinitionId","genderConceptId")
    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & !genderDivided ){
        div <- c("cohortDefinitionId")
    }

    df_total<-PFTmanufactured %>%
        group_by_at(div) %>%
        summarise(total_count = n_distinct(subjectId))

    df_count<-PFTmanufactured %>%
        mutate(timeSection = (time%/%5)*5 ) %>%
        group_by_at(c(div,"timeSection")) %>%
        summarise(count = n_distinct(subjectId)) %>%
        mutate(timeSection = factor(timeSection, levels = level,
                                    labels = seq_fun(level)))


    formula<-as.formula( paste0( paste(div,collapse = "+") ,"~timeSection" ) )

    df_dcast <- reshape2::dcast(df_count, formula)
    df_dcast <- df_dcast %>%
        merge(df_total, by = div )
    #colnames(df_dcast)[1] <- "duration (years)"

    return(df_dcast)

}

#'calculate prediction values of PFT that 5, 10, 15 years after treatment in detail
#'@import dplyr
#'@import reshape2
#'@import mgcv
#'@param PFTmanufactured              result of PFTmanufacture code
#'@param genderDivided                logical (TRUE/FALSE)
#'@export
#'
pftPredictTable_indetail<-function(PFTmanufactured,
                                   genderDivided){

    PFTmanufactured$cohortDefinitionId<-as.character(PFTmanufactured$cohortDefinitionId)

    if ( "ageCut" %in% colnames(PFTmanufactured) & genderDivided ){

        pft_split <- split(PFTmanufactured,
                           list(PFTmanufactured$cohortDefinitionId,PFTmanufactured$ageCut,PFTmanufactured$genderConceptId) )

    } else if ( "ageCut" %in% colnames(PFTmanufactured) & !genderDivided ){

        pft_split <- split(PFTmanufactured,
                           list(PFTmanufactured$cohortDefinitionId,PFTmanufactured$ageCut) )

    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & genderDivided ){

        pft_split <- split(PFTmanufactured,
                           list(PFTmanufactured$cohortDefinitionId,PFTmanufactured$genderConceptId) )

    } else if ( !("ageCut" %in% colnames(PFTmanufactured)) & !genderDivided ){

        pft_split <- split(PFTmanufactured,
                           list(PFTmanufactured$cohortDefinitionId) )

    }
    time <- c(0,5,10,15)

    pft_predict <- lapply(pft_split, FUN = function(x){

        gam_out <- gam(valueAsNumber~s(time, bs = "cs"), data = x)

        need.data<-data.frame(time)
        cohortDefinitionId <- rep( unique(x$cohortDefinitionId), length(need.data$time) )

        ageCut <- rep( unique(x$ageCut), length(need.data$time) )

        gam_pred <- as.data.frame(predict(gam_out, newdata = need.data) )
        colnames(gam_pred) <- "predict"
        percent.decline <- ((gam_pred-gam_pred[1,])/gam_pred[1,])*100
        colnames(percent.decline) <- "percentDecline"

        out <- data.frame(cohortDefinitionId,ageCut)

        if(genderDivided){
            genderConceptId <- rep( unique(x$genderConceptId),length(need.data$time) )
            out <- cbind(out,genderConceptId)
        }

        out <- cbind(out,gam_pred,percent.decline)
        out$time <- time

        return(out)
    })

    df<-data.frame()
    for( i in 1:length(pft_predict) ){
        df <- rbind(df,pft_predict[[i]])
    }

    outcome <- df

    return(outcome)
}

