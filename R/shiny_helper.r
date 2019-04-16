#'switch input from label to cohortDefinitionId
#'@param input
#'@export
#'
switchcohort <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = {c(2,3)},
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = {c(4,5)})
}

#'switch input from label to cohortDefinitionId
#'@param input
#'@export
#'
switchselect_pft <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = {c(2,3)},
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = {c(4,5)},
           "Biomarker" = {selectInput(inputId = "selectbiomarker", label = "select biomarker",
                                      choices = c('EDN','periostin','Eotaxin1','Eotaxin2','SP_D','DPP10','MBL','TGFb1','chitinase','TIMP1','OPN','IL8','MPO'),
                                      multiple = FALSE)}
           )
}
