#'Analysis of baseline medication use of cohorts
#'
#'@import dplyr
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohort_definition_id_set
#'@export
#'

getDrugCovariate <- function(connectionDetails,
                                  Resultschema,
                                  CDMschema,
                                  cohortTable,
                                  cohortId){
  
  drugCovariate<- FeatureExtraction::createCovariateSettings(useDrugExposureLongTerm = TRUE, endDays = 0)
  
  covariateData_ff <- getDbCovariateData(connectionDetails = connectionDetails,
                                         cdmDatabaseSchema = CDMschema,
                                         cohortDatabaseSchema = Resultschema,
                                         cohortTable = cohortTable,
                                         cohortId = cohortId,
                                         rowIdField = "subject_id",
                                         covariateSettings = drugCovariate)
  
  covariateId <- ff::as.ram(covariateData_ff$covariateRef$covariateId)
  conceptId <- ff::as.ram(covariateData_ff$covariateRef$conceptId)
  conceptIdMapping_df <- data.frame(covariateId,conceptId)
  drug_rawdf <- ff::as.ram(covariateData_ff$covariates)
  drug_df <- drug_rawdf %>% left_join(conceptIdMapping_df, by = "covariateId")
  colnames(drug_df) <- c("subjectId","covariateId","covariateValue","conceptId")
  
  out <- drug_df %>%
    mutate(cohortDefinitionId = cohortId) %>%
    filter(conceptId %in% unlist(drugList$conceptIdSet)) %>%
    mutate(drugId = NA)
  for(i in 1:length(drugList$drugId)){
    if( nrow(out[out$conceptId %in% drugList$conceptIdSet[[i]],]) == 0 ) next
    out[out$conceptId %in% drugList$conceptIdSet[[i]],]$drugId <- drugList$drugId[[i]]
  }
  out <- out %>% select (cohortDefinitionId, subjectId, drugId, conceptId)
  
  return(out)
}


#'ready for calculate co-prevalence and RR
#'@import dplyr
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohort_definition_id_set
#'@export
#'
baseline_drug <- function(connectionDetails,
                                 Resultschema,
                                 CDMschema,
                                 cohortTable,
                                 cohort_definition_id_set){
  
  cohort_definition_id_set <- cohort_definition_id_set[!is.na(cohort_definition_id_set)]
  drugData <- data.frame()
  for(i in cohort_definition_id_set){
    getBaselineDrug <- getDrugCovariate(connectionDetails = connectionDetails,
                                                  CDMschema = CDMschema,
                                                  Resultschema = Resultschema,
                                                  cohortTable = cohortTable,
                                                  cohortId = i)
    if(length(drugData) == 0) {
      drugData <- getBaselineDrug
    } else {
      drugData <- rbind(drugData,getBaselineDrug) 
    }
  }
  
  templete <- data.frame(cohortDefinitionId = rep(c(cohort_definition_id_set), each = length(drugList$drugId) ),
                         drugId = rep(drugList$drugId,length(cohort_definition_id_set)))
  
  out <- drugData %>%
    group_by(cohortDefinitionId, drugId) %>%
    summarise(Count = n_distinct(subjectId)) %>%
    right_join(templete, by = c("cohortDefinitionId","drugId")) %>%
    left_join(demographicData %>% group_by(cohortDefinitionId) %>%
                summarise(totalCount = n_distinct(personId)), by = "cohortDefinitionId" ) %>%
    mutate(noDrug = totalCount - Count) %>%
    mutate(noDrug = if_else(is.na(noDrug),totalCount,noDrug))
  
  if(sum(is.na(out$Count))>0 ) out[is.na(out$Count),]$Count <- 0
  out<- as.data.frame(out)
  
  return(out)
}


#'ready for calculate co-prevalence and RR
#'@import dplyr
#'@param connectionDetails
#'@param Resultschema
#'@param CDMschema
#'@param cohortTable
#'@param cohort_definition_id_set
#'@export
#'

drugTable <- function(drugManufacData){
  
  df <- drugManufacData
  #str(ready)
  df_drug <- as.data.frame(df) %>%
    dplyr::mutate(percent = paste0( round((Count/totalCount)*100,2),"%") ) %>%
    dplyr::mutate(result = paste0(Count,"(",percent,")")) %>%
    dplyr::mutate(drugName = factor(drugId, levels = drugList$drugId,
                                       labels = drugList$drugName) ) %>%
    dplyr::select(cohortDefinitionId, drugName, result)
  
  out <- dcast(df_drug, drugName~cohortDefinitionId)
  
  return(out)
}


# 
# 
# extractDrug <- function(COHORT, prepost='preindex', Year = 2){
#   
#   ### 1. data extraction
#   #Announcement
#   ParallelLogger::logInfo ("Drug_exposure data extracting")
#   
#   # Extracting cohort
#   sql <- SqlRender::readSql(paste0(.libPaths()[1],"/ICARUSviewer","/SQL/loadDrugExposure.sql") )
#   sql <- SqlRender::renderSql(sql, cohort_definition_id = COHORT1)$sql
#   Cohort1 <- DatabaseConnector::querySql(conn, sql)
#   
#   # import drug list
#   manualList <- read_excel(paste0(.libPaths()[1],"/ICARUSviewer","/SQL/loadDrugExposure.sql")"~/ted9219/git/ABMI/ICARUS_Med/inst/CSV/manualList.xlsx")
#   
#   #Announcement
#   ParallelLogger::logInfo ("rbind 2 cohorts and merge drug list ")
#   
#   # rbind two cohorts & merge asthma drug list
#   cohort1_2<- rbind(Cohort1, Cohort2)
#   
#   cohort1_2_med <- merge(x = cohort1_2, y = manualList, by.x = "DRUG_SOURCE_VALUE", 
#                          by.y = "ORDCODE", all.x = T)
#   
#   cohort1_2_med <- cohort1_2_med %>% select(COHORT_DEFINITION_ID,
#                                             SUBJECT_ID,
#                                             COHORT_START_DATE,
#                                             DRUG_CONCEPT_ID,
#                                             DRUG_EXPOSURE_START_DATE,
#                                             DRUG_EXPOSURE_END_DATE, CLASSNUM,
#                                             QUANTITY, PRDDOSE, EQIDOSE, CALDOSE)
#   
#   cohort1_2_med <- cohort1_2_med %>% arrange(CLASSNUM)
#   
#   totalPatient<- data.frame(cohort1_2_med %>%
#                               group_by(COHORT_DEFINITION_ID) %>%
#                               summarize(patients = n_distinct(SUBJECT_ID)))
#   
#   subjectList <- cohort1_2 %>%
#     select(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
#     distinct(COHORT_DEFINITION_ID, SUBJECT_ID)
#   
#   
#   # pre-index data
# if (prepost == "preindex"){
#     
#     cohort1_2_med <- cohort1_2_med %>%
#       filter((DRUG_EXPOSURE_START_DATE + 365*Year >= COHORT_START_DATE &
#                 DRUG_EXPOSURE_START_DATE <= COHORT_START_DATE ) |
#                (DRUG_EXPOSURE_END_DATE <= COHORT_START_DATE &
#                   DRUG_EXPOSURE_END_DATE + 365*Year >= COHORT_START_DATE))
#     
#     cohort1_2_med_free <- cohort1_2_med
#     
#     cohort1_2_med <- cohort1_2_med %>% select(COHORT_DEFINITION_ID, SUBJECT_ID, CLASSNUM)
#     
#   }else if (prepost == "postindex"){
#     
#     cohort1_2_med <- cohort1_2_med %>%
#       filter((DRUG_EXPOSURE_START_DATE > COHORT_START_DATE &
#                 DRUG_EXPOSURE_START_DATE <= COHORT_START_DATE + 365*Year )|
#                (DRUG_EXPOSURE_END_DATE <= COHORT_START_DATE + 365*Year &
#                   DRUG_EXPOSURE_END_DATE > COHORT_START_DATE))
#     
#     cohort1_2_med_free <- cohort1_2_med
#     
#     cohort1_2_med <- cohort1_2_med %>% select(COHORT_DEFINITION_ID, SUBJECT_ID, CLASSNUM)
#     
#   }
#   else{
#     
#     print("You must select preindex or postindex")
#     
#   }
#   
#   # Divide to each cohort, class numbering
#   
#   ParallelLogger::logInfo ("make binary variables of cohort 1")
#   
#   # cohort 1
#   cohort1_med <- cohort1_2_med %>% filter(COHORT_DEFINITION_ID == COHORT1)
#   
#   CLASS1 <- cohort1_med %>% filter(CLASSNUM == 1) %>% distinct(SUBJECT_ID) %>% mutate(ICS = 1)
#   CLASS2 <- cohort1_med %>% filter(CLASSNUM == 2) %>% distinct(SUBJECT_ID) %>% mutate(LABA = 1)
#   CLASS3 <- cohort1_med %>% filter(CLASSNUM == 3) %>% distinct(SUBJECT_ID) %>% mutate(SABA = 1)
#   CLASS4 <- cohort1_med %>% filter(CLASSNUM == 4) %>% distinct(SUBJECT_ID) %>% mutate(Chromones = 1)
#   CLASS5 <- cohort1_med %>% filter(CLASSNUM == 5) %>% distinct(SUBJECT_ID) %>% mutate(LAMA = 1)
#   CLASS6 <- cohort1_med %>% filter(CLASSNUM == 6) %>% distinct(SUBJECT_ID) %>% mutate(SAMA = 1)
#   CLASS7 <- cohort1_med %>% filter(CLASSNUM == 7) %>% distinct(SUBJECT_ID) %>% mutate(LTRA = 1)
#   CLASS8 <- cohort1_med %>% filter(CLASSNUM == 8) %>% distinct(SUBJECT_ID) %>% mutate(Biologics = 1)
#   CLASS9 <- cohort1_med %>% filter(CLASSNUM == 9) %>% distinct(SUBJECT_ID) %>% mutate(SysSteroid = 1)
#   CLASS10 <- cohort1_med %>% filter(CLASSNUM == 10) %>% distinct(SUBJECT_ID) %>% mutate(Xanthines = 1)
#   CLASS11 <- cohort1_med %>% filter(CLASSNUM == 11) %>% distinct(SUBJECT_ID) %>% mutate(ICS_LABA = 1)
#   CLASS12 <- cohort1_med %>% filter(CLASSNUM == 12) %>% distinct(SUBJECT_ID) %>% mutate(LABA_LAMA= 1)
#   CLASS13 <- cohort1_med %>% filter(CLASSNUM == 13) %>% distinct(SUBJECT_ID) %>% mutate(SABA_SAMA = 1)
#   CLASS14 <- cohort1_med %>% filter(CLASSNUM == 14) %>% distinct(SUBJECT_ID) %>% mutate(Nasal_steroid = 1)
#   
#   # merge binary drug variables to cohort data
#   cohort1_drug <- cohort1_med %>% select(COHORT_DEFINITION_ID, SUBJECT_ID)
#   cohort1_drug <- cohort1_drug %>% distinct(COHORT_DEFINITION_ID, SUBJECT_ID)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS1, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS2, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS3, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS4, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS5, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS6, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS7, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS8, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS9, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS10, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS11, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS12, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS13, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS14, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   
#   # extracting free combination of ICS and LABA
#   freeCombi <- cohort1_drug %>% filter(ICS == 1 & LABA == 1) %>% select (SUBJECT_ID)
#   freeCombi <- cohort1_2_med_free %>% filter (SUBJECT_ID %in% freeCombi$SUBJECT_ID, CLASSNUM %in% c(1,2)) %>% arrange(SUBJECT_ID)
#   free1 <- freeCombi %>% filter(CLASSNUM == 1)
#   free2 <- freeCombi %>% filter(CLASSNUM == 2)
#   free <- merge(x = free1 , y = free2, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   
#   CLASS99 <- free %>% 
#     filter((DRUG_EXPOSURE_START_DATE.x >= DRUG_EXPOSURE_START_DATE.y & DRUG_EXPOSURE_START_DATE.x <= DRUG_EXPOSURE_END_DATE.y)|(DRUG_EXPOSURE_START_DATE.y >= DRUG_EXPOSURE_START_DATE.x & DRUG_EXPOSURE_START_DATE.y <= DRUG_EXPOSURE_END_DATE.x)) %>%
#     distinct (SUBJECT_ID) %>% mutate (freeCombi = 1)
#   
#   cohort1_drug <- merge(x = cohort1_drug, y = CLASS99, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   
#   # NA -> 0
#   cohort1_drug[is.na(cohort1_drug)] <- 0
#   cohort1_drug$ICS <- ifelse(cohort1_drug$freeCombi == 1,0, cohort1_drug$ICS) 
#   
#   cohort1_drug <- cohort1_drug %>% mutate(freeFixCombi = ICS_LABA + freeCombi)
#   cohort1_drug$freeFixCombi <- ifelse(cohort1_drug$freeFixCombi ==2, 1, cohort1_drug$freeFixCombi)
#   cohort1_drug <- cohort1_drug %>% mutate(anyICS = ICS + freeCombi + ICS_LABA)
#   cohort1_drug$anyICS <- ifelse(cohort1_drug$anyICS == 2, 1, cohort1_drug$anyICS)
#   cohort1_drug$anyICS <- ifelse(cohort1_drug$anyICS == 3, 1, cohort1_drug$anyICS)
#   
#   ParallelLogger::logInfo ("DONE")
#   
#   ParallelLogger::logInfo ("make binary variables of cohort 2")
#   
#   # cohort 2
#   cohort2_med <- cohort1_2_med %>% filter(COHORT_DEFINITION_ID == COHORT2)
#   
#   CLASS1 <- cohort2_med %>% filter(CLASSNUM == 1) %>% distinct(SUBJECT_ID) %>% mutate(ICS = 1)
#   CLASS2 <- cohort2_med %>% filter(CLASSNUM == 2) %>% distinct(SUBJECT_ID) %>% mutate(LABA = 1)
#   CLASS3 <- cohort2_med %>% filter(CLASSNUM == 3) %>% distinct(SUBJECT_ID) %>% mutate(SABA = 1)
#   CLASS4 <- cohort2_med %>% filter(CLASSNUM == 4) %>% distinct(SUBJECT_ID) %>% mutate(Chromones = 1)
#   CLASS5 <- cohort2_med %>% filter(CLASSNUM == 5) %>% distinct(SUBJECT_ID) %>% mutate(LAMA = 1)
#   CLASS6 <- cohort2_med %>% filter(CLASSNUM == 6) %>% distinct(SUBJECT_ID) %>% mutate(SAMA = 1)
#   CLASS7 <- cohort2_med %>% filter(CLASSNUM == 7) %>% distinct(SUBJECT_ID) %>% mutate(LTRA = 1)
#   CLASS8 <- cohort2_med %>% filter(CLASSNUM == 8) %>% distinct(SUBJECT_ID) %>% mutate(Biologics = 1)
#   CLASS9 <- cohort2_med %>% filter(CLASSNUM == 9) %>% distinct(SUBJECT_ID) %>% mutate(SysSteroid = 1)
#   CLASS10 <- cohort2_med %>% filter(CLASSNUM == 10) %>% distinct(SUBJECT_ID) %>% mutate(Xanthines = 1)
#   CLASS11 <- cohort2_med %>% filter(CLASSNUM == 11) %>% distinct(SUBJECT_ID) %>% mutate(ICS_LABA = 1)
#   CLASS12 <- cohort2_med %>% filter(CLASSNUM == 12) %>% distinct(SUBJECT_ID) %>% mutate(LABA_LAMA= 1)
#   CLASS13 <- cohort2_med %>% filter(CLASSNUM == 13) %>% distinct(SUBJECT_ID) %>% mutate(SABA_SAMA = 1)
#   CLASS14 <- cohort2_med %>% filter(CLASSNUM == 14) %>% distinct(SUBJECT_ID) %>% mutate(Nasal_steroid = 1)
#   
#   # merge binary drug variables to cohort data
#   cohort2_drug <- cohort2_med %>% select(COHORT_DEFINITION_ID, SUBJECT_ID)
#   cohort2_drug <- cohort2_drug %>% distinct(COHORT_DEFINITION_ID, SUBJECT_ID)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS1, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS2, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS3, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS4, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS5, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS6, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS7, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS8, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS9, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS10, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS11, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS12, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS13, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS14, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   
#   # extracting free combination of ICS and LABA
#   freeCombi <- cohort2_drug %>% filter(ICS == 1 & LABA == 1) %>% select (SUBJECT_ID)
#   freeCombi <- cohort1_2_med_free %>% filter (SUBJECT_ID %in% freeCombi$SUBJECT_ID, CLASSNUM %in% c(1,2)) %>% arrange(SUBJECT_ID)
#   free1 <- freeCombi %>% filter(CLASSNUM == 1)
#   free2 <- freeCombi %>% filter(CLASSNUM == 2)
#   free <- merge(x = free1 , y = free2, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   CLASS99 <- free %>%
#     filter((DRUG_EXPOSURE_START_DATE.x >= DRUG_EXPOSURE_START_DATE.y & DRUG_EXPOSURE_START_DATE.x <= DRUG_EXPOSURE_END_DATE.y)|(DRUG_EXPOSURE_START_DATE.y >= DRUG_EXPOSURE_START_DATE.x & DRUG_EXPOSURE_START_DATE.y <= DRUG_EXPOSURE_END_DATE.x)) %>%
#     distinct (SUBJECT_ID) %>%
#     mutate (freeCombi = 1)
#   
#   cohort2_drug <- merge(x = cohort2_drug, y = CLASS99, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID", all.x = T)
#   # NA -> 0
#   cohort2_drug[is.na(cohort2_drug)] <- 0
#   cohort2_drug$ICS <- ifelse(cohort2_drug$freeCombi == 1,0, cohort2_drug$ICS)
#   
#   cohort2_drug <- cohort2_drug %>% mutate(freeFixCombi = ICS_LABA + freeCombi)
#   cohort2_drug$freeFixCombi <- ifelse(cohort2_drug$freeFixCombi ==2, 1, cohort2_drug$freeFixCombi)
#   cohort2_drug <- cohort2_drug %>% mutate(anyICS = ICS + freeCombi + ICS_LABA)
#   
#   cohort2_drug$anyICS <- ifelse(cohort2_drug$anyICS == 2, 1, cohort2_drug$anyICS)
#   cohort2_drug$anyICS <- ifelse(cohort2_drug$anyICS == 3, 1, cohort2_drug$anyICS)
#   
#   ParallelLogger::logInfo ("DONE")
#   
#   
#   # rbind cohort1 and cohort2
#   cohort_total <- rbind(cohort1_drug, cohort2_drug)
#   
#   ParallelLogger::logInfo ("creating baseline table")
#   # baseline table
#   table1 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(ICS = sum(ICS)) 
#   table2 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(LABA = sum(LABA)) 
#   table3 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(SABA = sum(SABA)) 
#   table4 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(Chromones= sum(Chromones)) 
#   table5 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(LAMA = sum(LAMA)) 
#   table6 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(SAMA = sum(SAMA)) 
#   table7 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(LTRA = sum(LTRA)) 
#   table8 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(Biologics = sum(Biologics)) 
#   table9 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(SysSteroid = sum(SysSteroid)) 
#   table10 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(Xanthines = sum(Xanthines)) 
#   table11 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(ICS_LABA = sum(ICS_LABA)) 
#   table12 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(LABA_LAMA = sum(LABA_LAMA)) 
#   table13 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(SABA_SAMA = sum(SABA_SAMA))
#   table14 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(Nasal_steroid = sum(Nasal_steroid))
#   table99 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(freeCombi = sum(freeCombi))
#   table98 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(freeFixCombi = sum(freeFixCombi))
#   table97 <- cohort_total %>% group_by(COHORT_DEFINITION_ID) %>% summarize(anyICS = sum(anyICS))
#   
#   table1_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table1_pct$ICS <- round(table1$ICS/totalPatient$patients*100,2) 
#   table2_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table2_pct$LABA <- round(table2$LABA/totalPatient$patients*100,2) 
#   table3_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table3_pct$SABA <- round(table3$SABA/totalPatient$patients*100,2) 
#   table4_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table4_pct$Chromones <- round(table4$Chromones/totalPatient$patients*100,2) 
#   table5_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table5_pct$LAMA <- round(table5$LAMA/totalPatient$patients*100,2) 
#   table6_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table6_pct$SAMA <- round(table6$SAMA/totalPatient$patients*100,2) 
#   table7_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table7_pct$LTRA <- round(table7$LTRA/totalPatient$patients*100,2) 
#   table8_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table8_pct$Biologics <- round(table8$Biologics/totalPatient$patients*100,2) 
#   table9_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table9_pct$SysSteroid <- round(table9$SysSteroid/totalPatient$patients*100,2) 
#   table10_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table10_pct$Xanthines <- round(table10$Xanthines/totalPatient$patients*100,2) 
#   table11_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table11_pct$ICS_LABA <- round(table11$ICS_LABA/totalPatient$patients*100,2) 
#   table12_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table12_pct$LABA_LAMA <- round(table12$LABA_LAMA/totalPatient$patients*100,2) 
#   table13_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table13_pct$SABA_SAMA <- round(table13$SABA_SAMA/totalPatient$patients*100,2) 
#   table14_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table14_pct$Nasal_steroid <- round(table14$Nasal_steroid/totalPatient$patients*100,2) 
#   table99_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table99_pct$freeCombi <- round(table99$freeCombi/totalPatient$patients*100,2) 
#   table98_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table98_pct$freeFixCombi <- round(table98$freeFixCombi/totalPatient$patients*100,2)
#   table97_pct <- data.frame(COHORT_DEFINITION_ID = c(COHORT1, COHORT2))
#   table97_pct$anyICS <- round(table97$anyICS/totalPatient$patients*100,2) 
#   
#   table <- merge(x = table1, y = table2, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table3, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table4, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table5, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table6, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table7, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table8, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table9, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table10, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table11, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table12, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table13, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table14, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   
#   table <- merge(x = table, y = table99, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table98, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table <- merge(x = table, y = table97, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_t <- table
#   table <- t(table)
#   totalN <- t(totalPatient) 
#   table <- rbind(totalN, table)
#   table <- table[-3,]
#   
#   
#   table_pct <- merge(x = table1_pct, y = table2_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table3_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table4_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table5_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table6_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table7_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table8_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table9_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table10_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table11_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table12_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table13_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table14_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   
#   table_pct <- merge(x = table_pct, y = table99_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table98_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- merge(x = table_pct, y = table97_pct, by.x = "COHORT_DEFINITION_ID", by.y = "COHORT_DEFINITION_ID", all = T)
#   table_pct <- t(table_pct)
#   totalN <- t(totalPatient) 
#   table_pct <- rbind(totalN, table_pct)
#   table_pct <- table_pct[-3,]
#   
#   table_final <- cbind(table[,1], table_pct[,1], table[,2], table_pct[,2])
#   
#   ParallelLogger::logInfo ("doing chi-square test")
#   
#   # chi-square test in each drug
#   p <- data.frame(p_value = rep(0, times = 17))
#   for (i in 1:17){
#     name <-colnames(cohort_total[i+2])
#     #print(name)
#     a <-table(cohort_total$COHORT_DEFINITION_ID, cohort_total[,name])
#     #print(a)
#     try( 
#       {b <-chisq.test(cohort_total$COHORT_DEFINITION_ID, cohort_total[,name])
#       #print(b)
#       p[i,1] <-b$p.value
#       }
#     )
#   }
#   p_1 <- data.frame(p_value = rep(0, time = 2))
#   p <- rbind(p_1,p)
#   
#   table_final <- cbind(table_final, p)
#   
#   #AMR
#   xlabs <-c("COHORT_DEFINITION_ID", "SUBJECT_ID", "1","2","3","4","5","6","7","8","9","10","11","12","13")
#   xlabs_2 <- rep(0, times = 15)
#   x <- data.frame(xlabs)
#   x <- cbind(x, xlabs_2)
#   can <- reshape2::dcast(x, xlabs_2~xlabs)
#   can <- can %>% select(-"xlabs_2")
#   
#   AMR <- cohort1_2_med %>% arrange(COHORT_DEFINITION_ID, SUBJECT_ID)
#   summary <- AMR %>% group_by(COHORT_DEFINITION_ID, SUBJECT_ID, CLASSNUM) %>% summarise(n = n())
#   summary <- as.data.frame(summary)
#   summary <- dcast(data = summary, COHORT_DEFINITION_ID + SUBJECT_ID ~ CLASSNUM, value.var = "n")
#   summary <- summary %>% select(-"NA") 
#   summary <- bind_rows(can, summary)
#   summary <- summary %>%
#     rename(ICS = "1", LABA = "2", SABA = "3", Chromones = "4", LAMA = "5", SAMA = "6", LTRA = "7",
#            Biologics = "8", SysSteroid = "9", Xanthines = "10", ICS_LABA = "11", LABA_LAMA = "12",
#            SABA_SAMA = "13")
#   summary[is.na(summary)] <- 0
#   
#   AMR <- summary %>% select(-"14") %>% filter(COHORT_DEFINITION_ID != 0)
#   AMR <- AMR %>% mutate(Controller = ICS + ICS_LABA + LABA + Chromones + LTRA + Xanthines)
#   AMR <- AMR %>% mutate(Total = ICS + ICS_LABA + LABA + Chromones + LTRA + Xanthines + SABA)
#   AMR <- AMR %>% mutate(AMR = Controller/Total) 
#   AMR[is.na(AMR)] <- 0
#   AMR_result <- AMR %>% group_by (COHORT_DEFINITION_ID) %>%
#     summarize(Mean = mean(AMR), SD =sd(AMR), Median = median(AMR),
#               "25%" = quantile(AMR, probs = 0.25), "75%" = quantile(AMR, probs = 0.75) )
#   
#   ParallelLogger::logInfo ("DONE")
#   
#   print(table_final)
#   print(AMR_result)
#   AMR$COHORT_DEFINITION_ID <- as.factor(AMR$COHORT_DEFINITION_ID)
#   normality <- shapiro.test(AMR$AMR)
#   ParallelLogger::logInfo ("p-value is")
#   if (normality$p.value>0.05) {
#     print("t test result is")
#     test <- t.test(AMR ~ COHORT_DEFINITION_ID, data = AMR)
#     print(test$p.value)
#   } else {
#     print("wilcoxon test result is")
#     test <- wilcox.test(AMR ~ COHORT_DEFINITION_ID, data = AMR)
#     print(test$p.value)
#   } 
#   savefile <- paste0(saveFolder,"/table", COHORT1, "_", COHORT2, "_", prepost, ".xlsx")
#   write.xlsx(table_final, savefile)
#   
#   # sub- analysis of each medications
#   
#   # Oral corticosteroid
#   print("**Oral Corticosteroid analysis**")
#   ocs <- cohort1_2_med_free %>% filter(CLASSNUM == 9)
#   
#   # by OCS prescription count
#   
#   byCount <- data.frame(ocs %>% 
#                           group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
#                           summarize(records = n())) %>%
#     mutate(record_class = ifelse(records >=3, 3, ifelse(records == 2,2,1)))
#   
#   byCount <- merge(subjectList, byCount, by=c("COHORT_DEFINITION_ID", "SUBJECT_ID"), all.x = T)
#   byCount$records <- ifelse(is.na(byCount$records), 0, byCount$records)
#   byCount$record_class <- ifelse(is.na(byCount$record_class), 0, byCount$record_class)
#   
#   try(p<- chisq.test(byCount$COHORT_DEFINITION_ID, byCount$record_class))
#   
#   byCount <- data.frame(byCount %>%
#                           group_by(record_class, COHORT_DEFINITION_ID) %>%
#                           summarize(n = n()))
#   byCount <- reshape2::dcast(byCount, record_class ~ COHORT_DEFINITION_ID, value.var = "n")
#   byCount$pct1 <- byCount[,2]/totalPatient$patients[1]*100
#   byCount$pct2 <- byCount[,3]/totalPatient$patients[2]*100
#   OCSCount <- byCount %>% select(record_class, paste(COHORT1), pct1, paste(COHORT2), pct2) %>% arrange(-record_class)
#   print("OCS frequency by count is")
#   print(OCSCount)
#   print("chi-square test p-value is")
#   try(print(p$p.value))
#   
#   # by OCS prescription dose class (High/moderate to low/none )
#   byDose <- ocs
#   byDose <- byDose %>% group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
#     arrange(COHORT_DEFINITION_ID, SUBJECT_ID, desc(CALDOSE)) %>%
#     distinct(COHORT_DEFINITION_ID, SUBJECT_ID)
#   
#   dup <- which(duplicated(ocs[,c("COHORT_DEFINITION_ID", "SUBJECT_ID",
#                                  "COHORT_START_DATE", "DRUG_EXPOSURE_START_DATE")])|
#                  duplicated(ocs[,c("COHORT_DEFINITION_ID", "SUBJECT_ID",
#                                    "COHORT_START_DATE", "DRUG_EXPOSURE_START_DATE")], fromLast = T))
#   ocs_dup <- ocs[dup,]
#   ocs_dup <- data.frame(ocs_dup %>% group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>% summarize(CALDOSE = sum(CALDOSE)))
#   ocs_not_dup <- ocs[-dup,]
#   ocs_not_dup <- ocs_not_dup %>% select(COHORT_DEFINITION_ID, SUBJECT_ID, CALDOSE)
#   
#   ocs_final <- rbind(ocs_dup, ocs_not_dup)
#   ocs_final <- ocs_final %>% select(COHORT_DEFINITION_ID, SUBJECT_ID, CALDOSE) %>% 
#     group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
#     arrange(COHORT_DEFINITION_ID, SUBJECT_ID, desc(CALDOSE)) %>% group_by()
#   ocs_final <- ocs_final[!duplicated(ocs_final[, c("COHORT_DEFINITION_ID", "SUBJECT_ID")]),]
#   ocs_final[is.na(ocs_final)] <- 0
#   
#   # ocs_final %>% group_by(COHORT_DEFINITION_ID) %>%
#   #   summarise(mean = mean(CALDOSE), SD = sd(CALDOSE), median = median(CALDOSE))
#   
#   # ocs_final_1 <- ocs_final %>% filter(COHORT_DEFINITION_ID == COHORT1)
#   # ocs_final_2 <- ocs_final %>% filter(COHORT_DEFINITION_ID == COHORT2)
#   # quantile(ocs_final_1$CALDOSE)
#   # quantile(ocs_final_2$CALDOSE)
#   # shapiro.test(ocs_final_1$CALDOSE)
#   # shapiro.test(ocs_final_2$CALDOSE)
#   # 
#   # t.test(ocs_final$CALDOSE ~ ocs_final$COHORT_DEFINITION_ID, data = ocs_final)
#   # wilcox.test(ocs_final$COHORT_DEFINITION_ID, ocs_final$CALDOSE)
#   
#   ocs_final <- merge(subjectList, ocs_final, by = c("COHORT_DEFINITION_ID", "SUBJECT_ID"), all.x = T)
#   ocs_final$CALDOSE <- ifelse(is.na(ocs_final$CALDOSE), 0, ocs_final$CALDOSE)
#   
#   ocs_final <- ocs_final %>% mutate(group = ifelse(CALDOSE >= 40, 2, ifelse(CALDOSE == 0, 0, 1)))
#   try(p <- chisq.test(ocs_final$COHORT_DEFINITION_ID, ocs_final$group))
#   
#   ocs_final <- ocs_final %>% group_by(group, COHORT_DEFINITION_ID) %>% summarize(count = n())
#   byDose <- dcast(ocs_final, group ~ COHORT_DEFINITION_ID, value.var = "count")
#   byDose$pct1 <- byDose[,2]/totalPatient$patients[1]*100
#   byDose$pct2 <- byDose[,3]/totalPatient$patients[2]*100
#   OCSdoseCount <- byDose %>% select(group, paste(COHORT1), pct1, paste(COHORT2), pct2) %>% arrange(-group)
#   print("OCS frequency by dose group is")
#   print(OCSdoseCount)
#   print("chi-square test's p-value is ")
#   try(print(p$p.value))
#   
#   # Mean value of accumulative dose 
#   # 0- NO USER, 1 - MODERATE AND LOW DOSE, 2- HIGH DOSE(>=40MG)
#   
#   byYear <- ocs
#   byYear <- byYear %>% group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>% summarize(sum = sum(CALDOSE))
#   
#   byYear_1 <- byYear %>% filter(COHORT_DEFINITION_ID == COHORT1)
#   byYear_2 <- byYear %>% filter(COHORT_DEFINITION_ID == COHORT2)
#   shapiro.test(byYear$sum)
#   shapiro.test(byYear_1$sum)
#   shapiro.test(byYear_2$sum)
#   t.test(byYear$sum ~ byYear$COHORT_DEFINITION_ID, data = byYear)
#   wilcox.test(byYear$COHORT_DEFINITION_ID, byYear$sum)
#   
#   byYear <- t(byYear %>% group_by(COHORT_DEFINITION_ID) %>%
#                 summarise(mean = mean(sum), SD = sd(sum), median = median(sum)))
#   
#   quantile <- cbind(t(t(quantile(byYear_1$sum))), t(t(quantile(byYear_2$sum))))
#   
#   byYear<- rbind(byYear, quantile)
#   print("Accumulative dose of oral corticosteroid is (0- NO USER, 1 - MODERATE AND LOW DOSE, 2- HIGH DOSE(>=40MG))")
#   print(byYear)
#   
#   #ICS/LABA classification by device dose
#   print("**ICS/LABA analysis**")
#   
#   ## Medium-High dose ICS/LABA concept_id 
#   ## 43146496(Budesonide 0.32 MG / formoterol 0.009 MG Dry Powder Inhaler) 
#   ## 42902748(Budesonide 0.16 MG / formoterol fumarate 0.00045 MG Metered dose Inhaler)
#   ## 42902745(Fluticasone propionate 0.5 MG/ACTUAT / salmeterol 0.05 MG/ACTUAT Dry Powder Inhaler)
#   ## 42902746(Fluticasone propionate 0.25 MG/ACTUAT / salmeterol 0.05 MG/ACTUAT Dry Powder Inhaler)
#   ## others :  low dose
#   
#   ## Formoterol vs non-Formoterol ICS/LABA
#   ## Formoterol : 43190570, 42902748, 43146496, 21148486, 21148485, 43534839
#   ## non-Formoterol : 42902666, 42902746, 42902745
#   
#   ics_laba <- cohort1_2_med_free %>% filter(CLASSNUM == 11)
#   
#   ics_laba <- ics_laba %>%
#     mutate(High = ifelse(DRUG_CONCEPT_ID %in% c(43146496, 42902745, 42902746, 42902748), 2, 1))
#   
#   ics_laba <- ics_laba %>%
#     mutate(formoterol = ifelse(DRUG_CONCEPT_ID %in% c(43190570,42902748,43146496,21148486,21148485,43534839),2,1))
#   
#   byCount <- data.frame(ics_laba %>% 
#                           group_by(COHORT_DEFINITION_ID, SUBJECT_ID) %>%
#                           summarize(records = n())) %>% 
#     mutate(record_class = ifelse(records >=3, 3, ifelse(records == 2,2,1)))
#   
#   byCount <- merge(subjectList, byCount, by=c("COHORT_DEFINITION_ID", "SUBJECT_ID"), all.x = T)
#   byCount$records <- ifelse(is.na(byCount$records), 0, byCount$records)
#   byCount$record_class <- ifelse(is.na(byCount$record_class), 0, byCount$record_class)
#   
#   try(p <- chisq.test(byCount$COHORT_DEFINITION_ID, byCount$record_class))
#   
#   byCount <- data.frame(byCount %>%
#                           group_by(record_class, COHORT_DEFINITION_ID) %>%
#                           summarize(n = n()))
#   byCount <- reshape2::dcast(byCount, record_class ~ COHORT_DEFINITION_ID, value.var = "n")
#   byCount$pct1 <- round(byCount[,2]/totalPatient$patients[1]*100,2)
#   byCount$pct2 <- round(byCount[,3]/totalPatient$patients[2]*100,2)
#   ICSLABACount <- byCount %>% select(record_class, paste(COHORT1), pct1, paste(COHORT2), pct2) %>% arrange(-record_class)
#   
#   print("ICSLABACount frequency by count is (3 - >=3 , 2- 2, 1- 1, 0 - none)")
#   print(ICSLABACount)
#   print("chi- square test's p-value is")
#   try(print(p$p.value))
#   
#   # high dose ICS/LABA frequency
#   ics_laba_high <- ics_laba %>% arrange(COHORT_DEFINITION_ID, SUBJECT_ID, desc(High))
#   ics_laba_high <- ics_laba_high[!duplicated(ics_laba_high[, c("COHORT_DEFINITION_ID", "SUBJECT_ID")]),]
#   ics_laba_high <- merge(subjectList, ics_laba_high, by = c("COHORT_DEFINITION_ID", "SUBJECT_ID"), all.x = T)
#   ics_laba_high$High <- ifelse(is.na(ics_laba_high$High), 0, ics_laba_high$High)
#   try(p <- chisq.test(ics_laba_high$COHORT_DEFINITION_ID, ics_laba_high$High))
#   
#   ics_laba_high <- data.frame(ics_laba_high %>% group_by(High, COHORT_DEFINITION_ID) %>% summarize(count= n()))
#   highCount <- reshape2::dcast(ics_laba_high, High ~ COHORT_DEFINITION_ID, value.var = "count")
#   highCount$pct1 <- round(highCount[,2]/totalPatient$patients[1]*100,2)
#   highCount$pct2 <- round(highCount[,3]/totalPatient$patients[2]*100,2)
#   highCount <- highCount %>% select(High, paste(COHORT1), pct1, paste(COHORT2), pct2) %>% arrange(-High)
#   
#   print("ICSLABACount frequency by count is (2-High to moderate, 1- low, 0 - none)")
#   print(highCount)
#   print("chi- square test's p-value is")
#   try(print(p$p.value))
#   
#   
#   # formoterol ICS/LABA frequency
#   ics_laba_formoterol <- ics_laba %>% arrange(COHORT_DEFINITION_ID, SUBJECT_ID, desc(formoterol))
#   ics_laba_formoterol <- ics_laba_formoterol[!duplicated(ics_laba_formoterol[, c("COHORT_DEFINITION_ID", "SUBJECT_ID")]),]
#   ics_laba_formoterol <- merge(subjectList, ics_laba_formoterol, by = c("COHORT_DEFINITION_ID", "SUBJECT_ID"), all.x = T)
#   ics_laba_formoterol$formoterol <- ifelse(is.na(ics_laba_formoterol$formoterol), 0, ics_laba_formoterol$formoterol)
#   try(p <- chisq.test(ics_laba_formoterol$COHORT_DEFINITION_ID, ics_laba_formoterol$formoterol))
#   
#   ics_laba_formoterol <- data.frame(ics_laba_formoterol %>% group_by(formoterol, COHORT_DEFINITION_ID) %>% summarize(count= n()))
#   fomoterolCount <- reshape2::dcast(ics_laba_formoterol, formoterol ~ COHORT_DEFINITION_ID, value.var = "count")
#   fomoterolCount$pct1 <- round(fomoterolCount[,2]/totalPatient$patients[1]*100,2)
#   fomoterolCount$pct2 <- round(fomoterolCount[,3]/totalPatient$patients[2]*100,2)
#   fomoterolCount <- fomoterolCount %>% select(formoterol, paste(COHORT1), pct1, paste(COHORT2), pct2) %>% arrange(-formoterol)
#   print("formoterol ICS/LABA frequency by count is (2-Formoterol ICS/LABA, 1- non-formoterol ICS/LABA, 0 - none)")
#   print(fomoterolCount)
#   print("chi- square test's p-value is")
#   try(print(p$p.value))
#   
# }
