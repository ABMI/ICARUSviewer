#'switch input from label to cohortDefinitionId
#'@param input
#'@export
#'
switchcohort <- function(input){
    switch(input,
           "Severe Asthma vs Non-severe Asthma" = {c(2,3)},
           "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" = {c(4,5)})
}
