#'using latent class linear mixed model (LCMM), create groups according to longitudinally measured data trajectories
#'@import lcmm
#'@import dplyr
#'@param  longitudinal_data result of getAllLongitudinal, getLongitudinal function
#'@param  cluster_number
#'@export
#'
latent_class_classification <- function(CDMschema,
                                        Resultschema,
                                        cohortTable,
                                        cohortId_trajectory,
                                        measurementConceptId_Trajectory,
                                        clusterNumber){
  
  newdata <- data.frame(time = seq(0,15,length.out = 100) )
  
  all_measure <- getAllLongitudinal(connectionDetails = connectionDetails,
                                    CDMschema = CDMschema,
                                    Resultschema = Resultschema,
                                    cohortTable = 'asthma_cohort',
                                    cohortId = cohortId_trajectory)
  
  sub_measure <- getLongitudinal(all_longitudinal_data = all_measure,
                                 measurement_concept_id = measurementConceptId_Trajectory,
                                 time_unit = 'year')
  
  fit_list <- lcmm::hlme(covariateValue ~ time,
                         mixture = ~time,
                         random = ~time,
                         subject = "subjectId",
                         ng = clusterNumber,
                         nwg = TRUE,
                         idiag = FALSE,
                         data = sub_measure)

  predict_lcmm_fit                  <- lcmm::predictY(fit_list, newdata, draws = TRUE)
  predict_dataframe                 <- cbind(predict_lcmm_fit$times, predict_lcmm_fit$pred)
  clustering_class_with_estimated   <- fit_list$pprob %>% select(subjectId,class) %>% left_join(fit_list$predRE, by = 'subjectId')
  clustering_class_with_allMeasure  <- sub_measure%>% left_join(clustering_class_with_estimated, by = 'subjectId')
  
  lcmm_result <- list(fit_result = fit_list,
                      predict_result = predict_dataframe,
                      raw_data = sub_measure, 
                      measure_cluster_result = clustering_class_with_allMeasure,
                      estimate_cluster_result = clustering_class_with_estimated)
  # saveRDS(lcmm_cluster3_result, file = file.path("/home/dbwls5223/output","result_190604.rds"))
  ###########################
    class_classification <- fit_list$pprob %>%
        select(subjectId, class) %>%
        left_join(fit_list$predRE, by = "subjectId")
    bic <- fit_list$BIC
    fixed_fit <- summary(fit_list)

    out<-list(class_classification = class_classification,
              BIC = bic,
              fixed_effect = fixed_fit)

    return(out)
}
#'trajectory plot clustered by lcmm
#'@import ggplot2
#'@import dplyr
#'@param  lcmm_out result of PFTmanufacture function
#'@param  cluster_number
#'@export
#'
latent_longitudinal_plot <- function(lcmm_out,
                                     cluster_number){

}
