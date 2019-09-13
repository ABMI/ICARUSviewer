#'using latent class linear mixed model (LCMM), create groups according to longitudinally measured data trajectories
#'@import lcmm
#'@import dplyr
#'@param  all_longitudinal_data_for_cluster    result of getAllLongitudinal code
#'@param  measurementConceptId_Trajectory      which measurement_concept_id do you want to use to cluster cohort  
#'@param  degreeOfPolynomial
#'@param  cluster_number                       the number of clusters 
#'@export
#'
latent_class_classification <- function(all_longitudinal_data_for_cluster,
                                        measurementConceptId_Trajectory,
                                        degreeOfPolynomial,
                                        cluster_number
                                        # ,save_lcmm_result = FALSE
                                        ){
  degree_of_polynomial <- degreeOfPolynomial
  degreeToFormula <- switch(degree_of_polynomial,
                            "linear"    = {"covariateValue ~ time"},
                            "quadratic" = {"covariateValue ~ time + I(time^2)"},
                            "cubic"     = {"covariateValue ~ time + I(time^2) + I(time^3)"},
                            "quartic"   = {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4)"},
                            "quintic"   = {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5)"} )
  
  newdata <- data.frame(time = seq(0,15,length.out = 100) )
  
  sub_measure <<- getLongitudinal(all_longitudinal_data = all_longitudinal_data_for_cluster,
                                 measurement_concept_id = measurementConceptId_Trajectory,
                                 time_unit = 'year')
  #sub_measure %>% filter(subjectId == 2094755) %>% arrange(time)
  fit_list <- lcmm::hlme(as.formula(degreeToFormula),
    # covariateValue ~ time,
                         mixture = ~time,
                         random = ~time,
                         subject = "subjectId",
                         ng = cluster_number,
                         nwg = TRUE,
                         idiag = FALSE,
                         data = sub_measure)

  predict_lcmm_fit                  <- lcmm::predictY(fit_list, newdata, draws = TRUE)
  predict_dataframe                 <- cbind(predict_lcmm_fit$times, predict_lcmm_fit$pred)
  clustering_class_with_estimated   <- fit_list$pprob %>% select(subjectId,class) %>% left_join(fit_list$predRE, by = 'subjectId')
  clustering_class_with_allMeasure  <- sub_measure %>% left_join(fit_list$pprob %>% select(subjectId,class), by = 'subjectId')
  summary_fit_result                <- summary(fit_list)
  fit_BIC                           <- fit_list$BIC
  fit_AIC                           <- fit_list$AIC
  
  lcmm_result <- list(fit_result = fit_list,
                      predict_result = predict_dataframe,
                      raw_data = sub_measure, 
                      measure_cluster_result = clustering_class_with_allMeasure,
                      estimate_cluster_result = clustering_class_with_estimated,
                      summary_fit_result = summary_fit_result,
                      fit_BIC = fit_BIC,
                      fit_AIC = fit_AIC)
  
  # if(save_lcmm_result){
  #   saveRDS(lcmm_result, file = file.path(outputFolder,"result_190604.rds"))
  # }
  return(lcmm_result)
}

#'trajectory plot clustered by lcmm
#'@import ggplot2
#'@param  lcmm_classification_result_list result of latent_class_classification function
#'@param  individual_trajectories         logical value (TRUE/FALSE) ; being individual trajectories?
#'@param  cluster_number                  the number of clusters 
#'@export
#'
latent_longitudinal_plot <- function(lcmm_classification_result_list,
                                     individual_trajectories = TRUE,
                                     cluster_number){

  Individual_colourList <- c("#FF3333","#3366FF","#339933", "#FF9933","#660099","#99FFFF")
  Predict_colourList <- c("red","blue","#66FF66", "orange","#9900cc","#0099cc")
  
  lcmm_result <- lcmm_classification_result_list
  
  lcmm_predict_result <- list()
  for(i in 1:cluster_number){
    lcmm_predict_result[i] <- list(i = data.frame(time    = lcmm_result$predict_result[,1],
                                                  predict = lcmm_result$predict_result[,i+1],
                                                  lower   = lcmm_result$predict_result[,i+cluster_number+1],
                                                  upper   = lcmm_result$predict_result[,i+cluster_number+cluster_number+1]))
  }
  
  plot_lcmm_result <- ggplot2::ggplot(data = lcmm_result$measure_cluster_result)
  if(individual_trajectories){
    plot_lcmm_result <- plot_lcmm_result + geom_line(aes(x = time, y = covariateValue, group = subjectId, colour = as.factor(class)), size = 0.5, alpha = 0.3) + scale_colour_manual(values = Individual_colourList) 
  }
  
  for(i in 1:cluster_number){
    plot_lcmm_result <- plot_lcmm_result + 
      geom_line(data = lcmm_predict_result[[i]], aes(x = time, y = predict), size = 1, colour = Predict_colourList[i]) +
      geom_ribbon(data = lcmm_predict_result[[i]], aes(x = time, ymin = lower, ymax = upper), fill = Predict_colourList[i], alpha = 0.3)
  }
  
  plot_lcmm_result <- plot_lcmm_result + theme_bw() + xlab("time (years)") + 
    theme(legend.title = element_blank(), axis.title.x = element_text(size=13), axis.title.y = element_text(size = 13), axis.text.x = element_text(size = 13),axis.text.y = element_text(size = 13))
  
  return(plot_lcmm_result)
}

#'trajectory table clustered by lcmm
#'@import dplyr
#'@param  lcmm_classification_result_list result of latent_class_classification function
#'@param  cluster_number                  the number of clusters 
#'@export
#'
latent_longitudinal_table <- function(lcmm_classification_result_list,
                                      cluster_number){
  lcmm_result <- lcmm_classification_result_list
  
  lcmm_predict_result <- list()
  lcmm_representative <- list()
  for(i in 1:cluster_number){
    lcmm_predict_result[i] <- list(i = data.frame(time    = lcmm_result$predict_result[,1],
                                                  predict = lcmm_result$predict_result[,i+1],
                                                  lower   = lcmm_result$predict_result[,i+cluster_number+1],
                                                  upper   = lcmm_result$predict_result[,i+cluster_number+cluster_number+1]))
    #i = 1
    lcmm_representative[i] <- list(i = data.frame(intercept_predict = lcmm_result$summary_fit_result[i,1],
                                                  intercept_upperCI = lcmm_result$summary_fit_result[i,1]+lcmm_result$summary_fit_result[i,2],
                                                  intercept_lowerCI = lcmm_result$summary_fit_result[i,1]-lcmm_result$summary_fit_result[i,2],
                                                  intercept_p_value = lcmm_result$summary_fit_result[i,4],
                                                  slope_predict = lcmm_result$summary_fit_result[i+cluster_number,1],
                                                  slope_upperCI = lcmm_result$summary_fit_result[i+cluster_number,1]+lcmm_result$summary_fit_result[i+cluster_number,2],
                                                  slope_lowerCI = lcmm_result$summary_fit_result[i+cluster_number,1]-lcmm_result$summary_fit_result[i+cluster_number,2],
                                                  slope_p_value = lcmm_result$summary_fit_result[i+cluster_number,4]))
  }
  
  predict_and_ci <- function(x){ paste0(round(lcmm_predict[lcmm_predict$time == x,][,"predict"],3), "(",
                                        round(lcmm_predict[lcmm_predict$time == x,][,"lower"],3), ",",
                                        round(lcmm_predict[lcmm_predict$time == x,][,"upper"],3),")" ) }
  
  lcmm_out_table <- data.frame()
  for(i in 1:cluster_number){
    lcmm_predict <- lcmm_predict_result[[i]]
    lcmm_repre <- lcmm_representative[[i]]
    result<- data.frame(class = i, 
                        person_count = sum(lcmm_result$estimate_cluster_result$class == i),
                        proportion = round(sum(lcmm_result$estimate_cluster_result$class == i)/nrow(lcmm_result$estimate_cluster_result),3),
                        time_0 = predict_and_ci(0),
                        time_5 = predict_and_ci(5),
                        time_10 = predict_and_ci(10),
                        time_15 = predict_and_ci(15),
                        slope = paste0(round(lcmm_repre$slope_predict,3),"(", round(lcmm_repre$slope_lowerCI,3),",",round(lcmm_repre$slope_upperCI,3),")")
    )
    if(nrow(lcmm_out_table)==0){ lcmm_out_table <- result } else { lcmm_out_table <- rbind(lcmm_out_table,result) }
  }
  return(lcmm_out_table)
}
