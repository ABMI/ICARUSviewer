#'code for insert cohort at result table 
#'@import dplyr
#'@import FeatureExtraction
#'@param newCohortIdSet           
#'@param target_cluster_cohort 
#'@param resultOflcmm              results of latent_class_classification code
#'@param connection
#'@param Resultschema
#'@param cohortTable
#'@export
#'
insertCohort <- function(newCohortIdSet,
                         target_cluster_cohort,
                         resultOflcmm,
                         connection,
                         Resultschema,
                         cohortTable){

  newCohortIdSetByVector <- as.numeric(unlist(strsplit(newCohortIdSet,split = '/')))
  clusterResult_lcmm <- resultOflcmm$estimate_cluster_result %>% select(subjectId, class)
  classCohortIdSet <- list(classNumber = sort(unique(clusterResult_lcmm$class)),
                           newCohortIdSetByVector = newCohortIdSetByVector)
  targetCohortNewId <- left_join(clusterResult_lcmm, 
                                 totalCohort%>%filter(cohortDefinitionId == target_cluster_cohort)%>%select(subjectId,cohortStartDate,cohortEndDate), 
                                 by = "subjectId") %>%
    mutate(cohortDefinitionId = factor(class, levels = classCohortIdSet$classNumber,
                                       labels = classCohortIdSet$newCohortIdSetByVector) ) %>%
    mutate(cohortDefinitionId = as.numeric(as.character(cohortDefinitionId)) ) %>%
    select(cohortDefinitionId,subjectId,cohortStartDate,cohortEndDate)
  
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(Resultschema,cohortTable,sep = "."),
                                 data = targetCohortNewId,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 camelCaseToSnakeCase = TRUE)
}
