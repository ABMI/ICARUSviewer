#'collect and mutate demographic characteristics
#'@import dplyr
#'@param cohort_definition_id_set
#'@export
charterstic_manufacture<-function(cohort_definition_id_set){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  out <- demographicData %>%
    dplyr::filter( cohortDefinitionId %in% cohort_definition_id_set) %>%
    #filter(age >=12) %>%
    dplyr::mutate( followUpDuration = round(followUpDuration/365.25, 2) ) %>%
    dplyr::mutate( bmi = round(bmi,2)) %>%
    dplyr::select( cohortDefinitionId,personId, genderConceptId, age, followUpDuration, bmi )
  if( length(which(out$bmi > 100)) != 0 ) out[which(out$bmi > 100),]$bmi <- NA
  if( length(which(out$followUpDuration <= 0)) != 0 ) out[which(out$followUpDuration <= 0),]$followUpDuration <- NA
  
  return(out)
}

#'analysis of demographic characteristics; if the value has normality, calculate mean +/- sd, if not median (25%, 75% quantile) in continuous value
#'@import dplyr
#'@param characteristic_manufac  result of charterstic_manufacture
#'@export

characteristic_summary <- function(characteristic_manufac){
  medianSD <- function(x){
    mean_x <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) round(mean(x, na.rm = TRUE),2) )
    sd_x   <- tapply(x,characteristic_manufac$cohortDefinitionId,FUN = function(x) round(sd(x, na.rm = TRUE),2) )
    out <- paste0(mean_x,"+/-",sd_x)
  }
  total_count  <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(total_count = n_distinct(personId) )
  bmi_count    <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(bmi_count = sum( !is.na(bmi)) )
  female_count <- characteristic_manufac %>% group_by(cohortDefinitionId) %>% summarise(female_count = sum(genderConceptId == 8532) )
  
  age_result <- medianSD(characteristic_manufac$age)
  followUpDuration_result <- medianSD(characteristic_manufac$followUpDuration)
  bmi_result <- medianSD(characteristic_manufac$bmi)
  female_result <- female_count %>%
    dplyr::left_join(total_count, by = "cohortDefinitionId") %>%
    dplyr::mutate(female_prop = round((female_count/total_count)*100,2) ) %>%
    dplyr::mutate(female_prop_result = paste0(female_count,"(",female_prop,"%)") )
  
  df <- data.frame(age = age_result,
                   follow_up_duration = followUpDuration_result,
                   bmi = bmi_result,
                   female_proportion = female_result %>% select(female_prop_result) )
  demographicName <- colnames(df)
  out <- t(df)
  out <- cbind(demographicName,out)
  out <- data.frame(out)
  colnames(out) <- c('demographicName',sort(unique(characteristic_manufac$cohortDefinitionId))) 
  return(out)
}

#'calculate pvalue between two cohorts : if the value has normality, p value is calculated by t-test, if not by wilcoxon in continuous value
#'@param y must be vector of numeric data
#'@param index
#'@param cohortIdLength
#'@export
pvalueCalBetweenTwo <- function(y, index, cohortIdLength){
  normality <- function(x){ 
    shapiro <- shapiro.test(x)
    normalityPass <- shapiro$p.value >= 0.05 
  }
  normality_result <- tapply(y,INDEX = index,FUN = normality)
  person_eachCount <- tapply(y,INDEX = index,FUN = function(x){length(x)>30} )
  if( sum(normality_result) < cohortIdLength | sum(person_eachCount) < cohortIdLength ){
    p_value_wilcox <- wilcox.test(y~index)
    p_value   <- round(p_value_wilcox$p.value,4)
  } else if( sum(normality_result) >= cohortIdLength & sum(person_eachCount) >= cohortIdLength ){
    p_value_ttest <- t.test(y~index)
    p_value   <- round(p_value_ttest$p.value,4) 
  }  
}

#'calculate pvalue among more than two cohorts 
#'@param y must be vector of numeric data
#'@param index
#'@param cohortIdLength
#'@export
pvalueCalAmongMoreThanTwo <- function(y, index, cohortIdLength){
  normality <- function(x){ 
    shapiro <- shapiro.test(x)
    normalityPass <- shapiro$p.value >= 0.05 
  }
  normality_result <- tapply(y,INDEX = index,FUN = normality)
  person_eachCount <- tapply(y,INDEX = index,FUN = function(x){length(x)>30} )
  if( sum(normality_result) < cohortIdLength | sum(person_eachCount) < cohortIdLength ){
    p_value_anova <- oneway.test(y~index)
    p_value   <- round(p_value_anova$p.value,4)
  } else if( sum(normality_result) >= cohortIdLength & sum(person_eachCount) >= cohortIdLength ){
    p_value_kruskal <- kruskal.test(y~index)
    p_value   <- round(p_value_kruskal$p.value,4) 
  }  
}

#'analysis of demographic characteristics : if the value has normality, p value is calculated by t-test, if not by wilcoxon in continuous value
#'@param characteristic_manufac  result of clinicalCharManufacture
#'@export
characteristic_pvalue <- function(characteristic_manufac){
  cohortIdLength = length(unique(characteristic_manufac$cohortDefinitionId))
  if( cohortIdLength == 1 ){
    
    df <- data.frame(age = NA,
                     follow_up_duration = NA,
                     bmi = NA,
                     female_proportion = NA)
    
  } else if( cohortIdLength == 2 ){
    age_pvalue <- pvalueCalBetweenTwo(y = characteristic_manufac$age, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    followUpDuration_pvalue <- pvalueCalBetweenTwo(y = characteristic_manufac$followUpDuration, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    bmi_pvalue <- pvalueCalBetweenTwo(y = characteristic_manufac$bmi, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    female_pvalue_summary <- chisq.test(table(characteristic_manufac$genderConceptId,characteristic_manufac$cohortDefinitionId))
    female_pvalue <- round(female_pvalue_summary$p.value,5) 
    
    df <- data.frame(age = age_pvalue,
                     follow_up_duration = followUpDuration_pvalue,
                     bmi = bmi_pvalue,
                     female_proportion = female_pvalue)
    
  } else if( cohortIdLength > 2 ){
    age_pvalue <- pvalueCalAmongMoreThanTwo(y = characteristic_manufac$age, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    followUpDuration_pvalue <- pvalueCalAmongMoreThanTwo(y = characteristic_manufac$followUpDuration, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    bmi_pvalue <- pvalueCalAmongMoreThanTwo(y = characteristic_manufac$bmi, index = characteristic_manufac$cohortDefinitionId, cohortIdLength = cohortIdLength)
    female_pvalue_summary <- chisq.test(table(characteristic_manufac$genderConceptId,characteristic_manufac$cohortDefinitionId))
    female_pvalue <- round(female_pvalue_summary$p.value,5) 
    
    df <- data.frame(age = age_pvalue,
                     follow_up_duration = followUpDuration_pvalue,
                     bmi = bmi_pvalue,
                     female_proportion = female_pvalue)
    
  }
  
  demographicName <- colnames(df)
  out <- t(df)
  out <- cbind(demographicName,out)
  out <- data.frame(out)
  colnames(out) <- c('demographicName','pvalue')
  out[,'pvalue'] <- as.numeric(as.character(out[,'pvalue'])) 
  return(out)
}
