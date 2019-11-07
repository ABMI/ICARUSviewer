#'analysis of demographic characteristics according to disease cohort
#'@import dplyr
#'@param characteristicData
#'@param cohortDefinitionIdSet
#'@export
#'
charactManufacture<-function(characteristicData,
                             cohortDefinitionIdSet){
    out <- characteristicData %>%
      dplyr::filter(cohortDefinitionId %in% cohortDefinitionIdSet) %>%
      #filter(age >=12) %>%
      dplyr::mutate( cohortDefinitionId = factor(cohortDefinitionId, levels = c(1,2,3,4,5,300,301),
                                                 labels = c("Asthma", "Non-Severe Asthma","Severe Asthma", "AERD","ATA",
                                                            "Exacerbation","Non-exacerbation"))) %>%
      dplyr::mutate( bmi = round(bmi, 2)) %>%
      dplyr::mutate( ageSection = cut(age, breaks = c(11,19,49,100),
                                      labels = c("Adolescents (12-19)","Younger (19-49)","Older (50-100)")),
                     bmiSection = cut(bmi, breaks = c(0,18.499,22.999,24.999,29.999,34.999,1000),
                                      labels = c("Under","Normal","Pre-obesity","1stage","2stage","3stage")))
    return(out)
}

#'code for plotting gender Pie plot
#'@import ggplot2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@export
#'

genderpieplot <- function(charactManufac){
    df <- charactManufac %>%
      dplyr::group_by( cohortDefinitionId, genderConceptId ) %>%
      dplyr::summarise(count = n()) %>%
      dplyr::mutate(perc = (count/sum(count))*100 ) %>%
      dplyr::mutate(genderConceptId = factor(genderConceptId, levels = c(8507,8532),
                                             labels = c("men","women")))
    df<-as.data.frame(df)

    plot <- ggplot(data = df, aes(x = "", y = count, fill = genderConceptId))+
        geom_col(position = 'fill',color = 'black', width = 1) +
        facet_grid(.~cohortDefinitionId)+
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))+
        scale_fill_brewer(palette = "Pastel2")+
        coord_polar(theta = "y")+
        theme_bw()+
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 11),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank())

    return(plot)
}

#'code for plotting age pie plot
#'@import ggplot2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@export
#'

agepieplot <- function(charactManufac){
    df <- charactManufac %>%
        group_by( cohortDefinitionId, ageSection ) %>%
        summarise(count = n()) %>%
        mutate(perc = (count/sum(count))*100 )

    plot <- ggplot(data = df, aes(x = "", y = count, fill = ageSection))+
        geom_col(position = 'fill',color = 'black', width = 1) +
        facet_grid(.~cohortDefinitionId)+
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))+
        scale_fill_brewer(palette = "Set3")+
        coord_polar(theta = "y")+
        theme_bw() +
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 11),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank())

    return(plot)
}

#'code for plotting age-gender pie plot
#'@import ggplot2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@export
#'
####################
agegenderpieplot <- function(charactManufac){
    df <- charactManufac %>%
        group_by( cohortDefinitionId, ageSection , genderConceptId) %>%
        summarise(count = n()) %>%
        left_join(demographicData %>% group_by(cohortDefinitionId) %>% summarise(total = n()), by = c("cohortDefinitionId") ) %>%
        mutate(perc = (count/total)*100 )

    plot <- ggplot(data = df, aes(x = "", y = count, fill = ageSection))+
        geom_col(position = 'fill',color = 'black', width = 1) +
        facet_grid(.~cohortDefinitionId)+
        geom_text(aes(label = count), position = position_fill(vjust = 0.5))+
        scale_fill_brewer(palette = "Pastel2")+
        coord_polar(theta = "y")+
        theme_bw() +
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 11),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_blank())

    return(plot)
}


#'code for plotting bmi tree plot
#'@import ggplot2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@export
#'

bmitreeplot <- function(charactManufac){
    df <- charactManufac %>%
        filter(!is.na(bmiSection)) %>%
        mutate(genderConceptId = factor(genderConceptId, levels = c(8507,8532),
                                        labels = c("men","women"))) %>%
        group_by(genderConceptId, cohortDefinitionId, bmiSection ) %>%
        summarise(count = n()) %>%
        mutate(perc = (count/sum(count))*100 ) %>%
        mutate(perc = if_else(cohortDefinitionId == unique(charactManufac$cohortDefinitionId)[1],
                              perc, perc*(-1)))

    plot <- ggplot(data = df, aes(x = as.factor(bmiSection), y = perc, fill = as.factor(cohortDefinitionId)))+
        geom_bar(stat = "identity",color = 'black', width = .5) +
        # scale_y_continuous(breaks =  seq(-1, 1, 0.25),
        #                    labels = paste0(as.character(c(seq(100, 0, -25), seq(25, 100, 25))), "%"))+
        facet_grid(.~genderConceptId) +
        coord_flip()+
        scale_fill_brewer(palette = "Accent")+
        ylab("percent(%) = (count/cohort total population)*100 ")+
        theme_bw()+
        theme(legend.title = element_blank(),
              strip.text = element_text(size = 15),
              #legend.position = "none",
              legend.text = element_text(size = 11),
              axis.title.x = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.title.y = element_blank())

    return(plot)
}

#'table output of bmi and gender proportion
#'@import reshape2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@param dividedVariable          age, bmi (gender was default)
#'@export
#'
demographic_Table <- function(charactManufac,
                              dividedVariable){

    div <- dividedVariable
    divSection <- paste0(dividedVariable,"Section")

    by <- c("cohortDefinitionId", "genderConceptId", divSection, div)
    # by <- factor(by, exclude = "",
    #              levels = c("cohortDefinitionId", "genderConceptId", divSection, div))

    df <- charactManufac %>%
        #select(by) %>%
        #filter_at(!is.na(bmiSection))
        filter_at(vars(by[3]), any_vars(!is.na(.)) ) %>%
        group_by_at(by[1:3]) %>%
        summarise(count = n()) %>%
        mutate(perc = round((count/sum(count))*100, 1) ) %>%
        mutate(result = paste0(count," (",perc,"%)"))

    df_select <- as.data.frame(df) %>%
        select(c(by[1:3], result)) %>%
        mutate(genderConceptId = factor(genderConceptId, levels = c(8507,8532),
                                        labels = c("men","women")))

    f <- as.formula(paste(paste(by[2:3], collapse = "+"), "~", by[1]))
    table<-reshape2::dcast(data = df_select, f)
    colnames(table[,c(1,2)])<-c("Gender", div)

    return(table)
}

#'calculate p-value of gender(chi-square test) and and age mean(t-test)
#'@import reshape2
#'@import dplyr
#'@param charactManufac           result of charactManufacture code
#'@param dividedVariable          age, bmi (gender was default)
#'@export
#'
demographic_cal <- function(charactManufac,
                            dividedVariable){

    div <- dividedVariable
    divSection <- paste0(dividedVariable,"Section")

    by <- c("cohortDefinitionId", "genderConceptId", divSection, div)

    df <- charactManufac %>%
        select(by) %>%
        filter_at(vars(by[3]), any_vars(!is.na(.)) )

    #gender chi-square test
    gender_select <- df %>%
        select(cohortDefinitionId, genderConceptId)
    chi_square_cal <- chisq.test(gender_select$cohortDefinitionId, gender_select$genderConceptId)
    #chi_square_result <- chi_square_cal$p.value

    #age t-test
    age_select <- df %>%
        mutate(cohortDefinitionId = cohortDefinitionId) %>%
        select(c(by[1],by[4]))
    f<-as.formula(paste(by[4],"~",by[1]))
    t.test_cal <- t.test(data = age_select, f)
    #age wilcoxon test
    wx.test_cal <- wilcox.test(data = age_select, f,
                               alternative = c("two.sided"))

    result <- list(chi_square_p.value = chi_square_cal$p.value,
                   t.test_mean = t.test_cal$estimate,
                   t.test_p.value = t.test_cal$p.value,
                   wx.test_p.value = wx.test_cal$p.value)

    out<-paste("gender chi-square test : p.value = ", result$chi_square_p.value, "\n",
               by[4], " t-test : p.value = ", result$t.test_p.value,"\n",
               by[4], " wilcoxon test : p.value = ", result$wx.test_p.value, "\n")

    return(out)
}

