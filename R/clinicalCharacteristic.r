#'collect and mutate demographic characteristics
#'@import dplyr
#'@param cohort_definition_id_set
#'@export
charterstic_manufacture<-function(cohort_definition_id_set){
    out <- demographicData %>%
        filter(cohortDefinitionId %in% cohort_definition_id_set) %>%
        #filter(age >=12) %>%
        mutate( followUpDuration = round(followUpDuration/365.25, 2) ) %>%
        mutate( bmi = round(bmi,2)) %>%
        select( cohortDefinitionId,personId, genderConceptId, age, followUpDuration, bmi )
    return(out)
}

#'analysis of demographic characteristics; if the value has normality, calculate mean +/- sd, if not median (25%, 75% quantile) in continuous value
#'@import dplyr
#'@param characteristic_manufac  result of charterstic_manufacture
#'@export

characteristic_summary <- function(characteristic_manufac){
    normality <- function(x){
        shapiro <- shapiro.test(x)
        if(shapiro$p.value >= 0.05){
            mean_x <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) round(mean(x, na.rm = T),2) )
            sd_x   <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) round(sd(x, na.rm = T),2) )
            out <- paste0(mean_x,"+/-",sd_x)
        } else {
            median_x <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) median(x, na.rm = T) )
            inquantile_x <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) paste0(quantile(x, na.rm = T)[2],",",quantile(x, na.rm = T)[4] ) )
            out <- paste0(median_x,"(",inquantile_x,")")
        }
        return(out)
    }

    total_count <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(total_count = n_distinct(personId) )
    bmi_count <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(bmi_count = sum(!is.na(bmi)) )
    female_count <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(female_count = sum(genderConceptId == 8532) )

    age_result <- normality(characteristic_manufac$age)
    followUpDuration_result <- normality(characteristic_manufac$followUpDuration)
    bmi_result <- normality(characteristic_manufac$bmi)
    female_result <- female_count %>%
        left_join(total_count, by = "cohortDefinitionId") %>%
        mutate(female_prop = round((female_count/total_count)*100,2) ) %>%
        mutate(female_prop_result = paste0(female_count,"(",female_prop,"%)") )

    df <- data.frame(cohort_definition_id = unique(characteristic_manufac$cohortDefinitionId),
                     age = age_result,
                     follow_up_duration = followUpDuration_result,
                     bmi = bmi_result,
                     female_proportion = female_result %>% select(female_prop_result) )
    return(df)
}

#'analysis of demographic characteristics : if the value has normality, p value is calculated by t-test, if not by wilcoxon in continuous value
#'@param characteristic_manufac  result of clinicalCharManufacture
#'@export

characteristic_pvalue <- function(characteristic_manufac){
    normality_p <- function(x){
        shapiro <- shapiro.test(x)
        if(shapiro$p.value >= 0.05){
            p_value_ttest <- t.test(x~characteristic_manufac$cohortDefinitionId)
            p_value   <- round(p_value_ttest$p.value,4)
            out <- paste0(p_value,"(t test)")
        } else {
            p_value_wilcox <- wilcox.test(x~characteristic_manufac$cohortDefinitionId)
            p_value   <- round(p_value_wilcox$p.value,4)
            out <- paste0(p_value,"(Wilcoxon rank sum test)")
        }
        return(out)
    }

    age_pvalue <- normality_p(characteristic_manufac$age)
    followUpDuration_pvalue <- normality_p(characteristic_manufac$followUpDuration)
    bmi_pvalue <- normality_p(characteristic_manufac$bmi)
    female_pvalue_summary <- chisq.test(table(characteristic_manufac$genderConceptId,characteristic_manufac$cohortDefinitionId))
    female_pvalue <- paste0(round(female_pvalue_summary$p.value,5),"(chi-square test)")

    df <- data.frame(age = age_pvalue,
                     follow_up_duration = followUpDuration_pvalue,
                     bmi = bmi_pvalue,
                     female_proportion = female_pvalue)
    return(df)
}

