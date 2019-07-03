#'using latent class linear mixed model (LCMM), create groups according to longitudinally measured data trajectories
#'@import lcmm
#'@import dplyr
#'@param  longitudinal_data result of PFTmanufacture function
#'@param  cluster_number
#'@export
#'
latent_class_classification <- function(longitudinal_data,
                                        cluster_number = 3){
    fit_list <- lcmm::hlme(covariateValue ~ time,
                           mixture = ~time,
                           random = ~time,
                           subject = "subjectId",
                           ng = cluster_number,
                           data = longitudinal_data)

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
