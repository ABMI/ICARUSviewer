# #check package ready
source(file.path(.libPaths()[1],"ICARUSviewer/global.R"))
check.packages("Rcpp")
check.packages("dplyr")
check.packages("reshape2")
check.packages("ggplot2")
check.packages("plotly")
check.packages("shiny")
check.packages("SqlRender")
check.packages("DatabaseConnector")
check.packages("FeatureExtraction")
check.packages("PatientLevelPrediction")
check.packages("shinydashboard")
check.packages("shinyWidgets")
check.packages("shinydashboardPlus")
check.packages("tidyverse")
check.packages("epitools")
check.packages("mgcv")
check.packages("lme4")
check.packages("lmerTest")
check.packages("lcmm")
check.packages("ggfortify")
check.packages("survival")
check.packages("ICARUSviewer")

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "ICARUS WINGS", fixed = TRUE, enable_rightsidebar = TRUE, rightSidebarIcon = "edit"),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      ######database Connection UI#####
      id = "rightsider1", title = "Database Connection", icon = "user-check", active = TRUE,
      # input information need to connect database
      textInput("ip","IP",""),
      uiOutput("sqltype"),
      textInput("CDMschema","CDM Database Schema",""),
      textInput("Resultschema","CDM Results schema",""),
      textInput("cohortTable","cohort table",""),
      textInput("user","USER",""),
      passwordInput("pw","PASSWORD",""),
      actionButton("dbConnect","Connect to database"),
      textOutput("dbconnectDone"), br(), br(),
      actionButton("eraseData","Log Out"),
      textOutput("eraseDone")
    ),
    #####call cohort Table UI#####
    rightSidebarTabContent(
      id = "rightsider2", title = "Call Data", icon = "file-alt", 
      actionButton("loadData", "Load cohorts"),
      textOutput("loadDataDone")
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("ICARUS WINGS", tabName = "WINGS", icon = icon("feather-alt")),
      menuItem("Trajectory cluster", tabName = "Trajectory", icon = icon("chart-line") ),
      menuItem("Compare cohorts", tabName = "compare", icon = icon("balance-scale") ),
      menuItem("Simple Prediction", tabName = "prediction", icon = icon("arrow-alt-circle-right") )
    )
  ),
  #####analysis#####
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "WINGS", 
              titlePanel(br(strong("ICARUS WINGS"))),
              mainPanel(
                fluidRow(boxPlus(width = 12, 
                                 "Immune/Inflammatory DIsease Common Data Model Augmentation for Research Union System (ICARUS) Web-based INFOGraphics Service (WINGS)")
                         
                ),
                tags$script(
                  '
       setTimeout(function(){
         var temp = window.location.hash;
         //스크립트 변수를 R변수로 change
         Shiny.onInputChange("myInput",temp);
       }, 1000);
       $(document).bind("keydown",function(e){
           if ( e.keyCode == 123 ) {
               e.preventDefault();
               alert("Developter Tools are not available.");
               e.returnValue = false;
           }
       });
       document.onmousedown=disableclick;
       function disableclick(event){
           if (event.button==2) {
               alert("For security reasons, you cannot use the right the RMB(right mouse button).");
               return false;
           }
       }
       '
                ),
                textOutput("test")
              )
      ),
      #####trajectory clustering UI#####
      tabItem(tabName = "Trajectory",
              titlePanel( br(strong("Trajectory clustering using Latent Class Mixed Model (LCMM)")) ),
              fluidRow( h4(strong("   You can classify subjects using long-term followed measurement values")),
                        sidebarPanel(uiOutput("cohortId_trajectory"),
                                     numericInput("measurementConceptId_Trajectory","Measurement Concept ID",""),
                                     selectInput("degreeOfPolynomial_select","Degree of polynomial",choices = c("linear","quadratic","cubic","quartic","quintic") ),
                                     numericInput("clusterNumber","Cluster Count","3"),
                                     actionButton("do_cluster","Do clustering"),
                                     # progressBar(id = "clustering", value = 0, total = 100, title = "", display_pct = TRUE ),
                                     textOutput("show_results"),
                                     h4("if clustering finished, then show results"),br(),
                                     actionButton("show_cluster","Show results!"),
                                     width = 2),
                        mainPanel(fluidRow(gradientBox(plotOutput("TrajectoryClustering_withIndividual"), icon = icon("chart-line"), gradientColor = "light-blue", 
                                                       footer = "Observed individual trajectories and estimated representative trajectories"),
                                           gradientBox(plotOutput("TrajectoryClustering_onlyCI"), icon = icon("chart-line"), gradientColor = 'light-blue', 
                                                       footer = "Estimated representative trajectories. The shaded areas indicate 95% CI"),
                                           dataTableOutput("TrajectoryClusteringTable" ),
                                           verbatimTextOutput("BICandAIC"),
                                           uiOutput("trajectoryCohortIdSet"),
                                           br(), h4(strong("If clustering was done and this result was interesting, insert this result into your cohor table!")),
                                           h4(strong("Before insert, please check whether this cohort ID was used :)")),
                                           textInput("trajectoryCohortIdSet", "please write down new cohort Id set", "ex) 1/2/3 : if number of clusters is 3"),
                                           actionButton('insert_trajectory_cluster',"insert results at cohort table"),
                                           br(),textOutput("insertDone") )
                        )
              )
      ),
      #####compare tab#####
      tabItem(tabName = "compare",
              titlePanel( br(strong("Compare cohorts"))),
              fluidRow( h4(strong("You can compare two different cohorts you have!")),
                        sidebarPanel(uiOutput("cohort1"),
                                     uiOutput("cohort2"),
                                     uiOutput("cohort3"),
                                     uiOutput("cohort4"),
                                     uiOutput("cohort5"),
                                     "You can choose at most 5 different cohorts",
                                     width = 2),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      #####baseline characteristics#####
                                      tabPanel("Baseline characteristics",
                                               fluidRow( h3(strong("Compare Charateristics between two cohorts")) ),
                                               fluidRow( column(1,actionButton("characteristic_analyze","Analyze") ),
                                                         # progressBar(id = "baseline", value = 0, total = 100, title = "", display_pct = TRUE ),
                                                         br(),plotOutput("RRplot") ),
                                               fluidRow(br(),dataTableOutput("totalBaselineCharacteristicsTable") ) 
                                      ),
                                      #####longitudinal analysis#####
                                      tabPanel("Longitudinal",
                                               fluidRow( h3(strong("Longitudinal analysis of long-term measured values")) ),
                                               fluidRow( column(1,actionButton("load_all_measurement","load All") ),
                                                         br(),textOutput("load"),br(),
                                                         # progressBar(id = "callmeasurement", value = 0, total = 100, title = "", display_pct = TRUE),
                                                         br(), h4(strong("If call measurement data finished, then do analyze!")),
                                                         h4(strong("wirte down the Measurement Concept Id and click the 'Analyze' button")),br(),
                                                         column(3,numericInput("measurementConceptId","Measurement Concept ID you want to watch","") ),
                                                         column(1,actionButton("do_longitudinalAnalysis","Analyze") ) ),
                                               fluidRow( gradientBox(plotOutput("longitudinalAnalysis"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Longitudinal analysis"),
                                                         gradientBox(plotOutput("longutidinalAnalysis_only"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Only estimated profiles" ),
                                                         br(),
                                                         strong("The long-term change of measurement data over 15 years"),
                                                         dataTableOutput("longitudinalTable") 
                                               )
                                               
                                      ),
                                      #####clinical event#####
                                      tabPanel("clinical event",
                                               fluidRow( h3(strong("Compare clinical event between cohorts")) ),
                                               fluidRow( h4(strong("Put in event cohort Id")), 
                                                         uiOutput("ClinicalEventCohortId"),
                                                         column(1,actionButton("analyzeEvent","Analyze") ) ),
                                               fluidRow( br(),
                                                         gradientBox(plotOutput("ClinicalEventPlot"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Yearly count of clinical event between two cohorts"),
                                                         dataTableOutput("ClinicalEventTable")),
                                               fluidRow( br(),
                                                         h4(strong("The results of clinical event's anual count looks reasonable, the you can do survival analysis !")),
                                                         numericInput("survivalEndDate", 'if you want one year survival, write down "365" ',0),
                                                         column(1,actionButton("analyzeSurvival","Analyze") ) ),
                                               fluidRow( br(),
                                                         gradientBox(plotOutput("EventSurvivalPlot"), icon = icon("balance-scale"), gradientColor = "light-blue", 
                                                                     footer = "Kaplan-Meier survival curve for the time to clinical event"),
                                                         verbatimTextOutput("survivalAnalysisResults"))
                                      )
                          )
                          
                        ) 
              )
      ),
      #####prediction model#####
      tabItem(tabName = "prediction",
              titlePanel( br(strong("Simple prediction model")) ),
              fluidRow( h4(strong("Simple prediction model was developed using PatientLevelPrediction package")), 
                        sidebarPanel(uiOutput("Target_cohort"),
                                     uiOutput("Outcome_cohort"),
                                     numericInput("Risk_window_start","Risk window start",""),
                                     numericInput("Risk_window_end","Risk window end",""),
                                     numericInput("Minimum_TAR","Minimum time at risk",""),
                                     uiOutput("modelSelect"),
                                     actionButton("DoPredict","Predict"),
                                     br(),textOutput("predictDone"),
                                     br(),actionButton("ShowPredict","Show Result"),
                                     width = 2),
                        mainPanel(fluidRow( gradientBox(plotOutput("AUROCcurve"), icon = icon("arrow-alt-circle-right"), gradientColor = "light-blue",
                                                        footer = "AUC curve and p-value of prediction model"),
                                            gradientBox(plotOutput("contributedCovariates"), icon = icon("arrow-alt-circle-right"), gradientColor = "light-blue",
                                                        footer = "Top 20 most contrubuted covariates. If values were negative, covariates were more related to not-showing outcome and if positive, more related to showing outcome )") ),
                                  fluidRow( dataTableOutput("covariateTable") )
                        )
              )
      )
    )
  )
)

#####server code#####  
server <- function(input, output, session) {
  #####1.right side menu : DB connection#####
  output$sqltype<-renderUI({
    selectInput("sqltype", "Select DBMS",
                choices = c("sql server" = "sql server",
                            "PostgreSQL" = "postresql",
                            "Amazon Redshift" = "redshift",
                            "Microsoft Parallel Data Warehouse" = "pdw",
                            "IBM Netezza" = "netezza",
                            "Google BigQuery" = "bigquery") )
  })
  DBconnect <- eventReactive(input$dbConnect, {
    connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms = input$sqltype,
                                                                     server = input$ip,
                                                                     user = input$user,
                                                                     password = input$pw)
    CDMschema <<- input$CDMschema
    CohortSchema <<- input$Resultschema
    cohortTable <<- input$cohortTable
    connection <<-DatabaseConnector::connect(connectionDetails)
    
    "Database connection is done!"
  })
  output$dbconnectDone <- renderText({ DBconnect() })
  #####2. right side tab menu : call cohort table#####
  callCohortTable <- eventReactive(input$loadData,{
    calledData <- call_dataList(connectionDetails = connectionDetails,
                                connection = connection,
                                Resultschema = CohortSchema,
                                CDMschema = CDMschema,
                                cohortTable = cohortTable)
    totalCohort <<- calledData[[2]]
    demographicData <<- calledData[[1]]
    setting()
    "Call Cohort Table finished!"
  })
  output$loadDataDone <- renderText({ callCohortTable() })
  #####Menu Item 1 : Trajectory clustering#####
  output$cohortId_trajectory<-renderUI({
    selectInput("target_cluster_cohort","Target cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) )
  })
  
  #####Java script#####
  output$test <- renderPrint( input$myInput )
  
  trajectoryClustering <- eventReactive(input$do_cluster,{
    #load temporal measurement data
    allclustering_target <- getAllLongitudinal(connectionDetails = connectionDetails,
                                               CDMschema = CDMschema,
                                               Resultschema = CohortSchema,
                                               cohortTable = cohortTable,
                                               cohortId = as.numeric(input$target_cluster_cohort) )
    #do LCMM
    lcmm_cluster_result_list <<- latent_class_classification(all_longitudinal_data_for_cluster = allclustering_target,
                                                             measurementConceptId_Trajectory = input$measurementConceptId_Trajectory,
                                                             degreeOfPolynomial = input$degreeOfPolynomial_select,
                                                             cluster_number = input$clusterNumber)
    "Trajectory clustering is done!"
  })
  output$show_results <- renderText({ withProgress(message = "trajectory clustering...",value = 1, {trajectoryClustering()} ) })
  # # progress bar for clustering
  # observeEvent(input$do_cluster,{
  #   for(i in 1:100){
  #     updateProgressBar( session = session, id = "clustering", value = i, total = 100, title = paste("clustering", trunc(i/10)) )
  #     Sys.sleep(0.1)
  #   }
  # })
  ##plot lcmm results
  plot_lcmm_cluster_withIndividual <- eventReactive(input$show_cluster,{
    plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                             individual_trajectories = TRUE,
                                             cluster_number = input$clusterNumber)
    return(plot_cluster)
  })
  output$TrajectoryClustering_withIndividual<- renderPlot({ plot_lcmm_cluster_withIndividual() })
  
  plot_lcmm_cluster_onlyCI <- eventReactive(input$show_cluster,{
    plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                             individual_trajectories = FALSE,
                                             cluster_number = input$clusterNumber)
    return(plot_cluster)
  })
  output$TrajectoryClustering_onlyCI<- renderPlot({ plot_lcmm_cluster_onlyCI() })
  
  table_lcmm_cluster <- eventReactive(input$show_cluster,{
    table_cluster <- latent_longitudinal_table(lcmm_classification_result_list = lcmm_cluster_result_list,
                                               cluster_number = input$clusterNumber)
    return(table_cluster)
  })
  output$TrajectoryClusteringTable <- renderDataTable({ table_lcmm_cluster() })
  #BIC and AIC score
  BICAIC <- eventReactive(input$show_cluster,{
    paste("BIC score is",lcmm_cluster_result_list$fit_BIC, "and", "AIC score is",lcmm_cluster_result_list$fit_AIC,sep = " ")
  })
  output$BICandAIC <- renderText({ BICAIC() }) 
  #insert at cohort table
  insertCohortNew <- eventReactive(input$insert_trajectory_cluster,{
    insertCohort(newCohortIdSet = as.character(input$trajectoryCohortIdSet),
                 target_cluster_cohort = as.numeric(input$target_cluster_cohort),
                 resultOflcmm = lcmm_cluster_result_list,
                 connection = connection,
                 Resultschema = CohortSchema,
                 cohortTable = cohortTable)
    "Insert done!"
  })
  output$insertDone <- renderText({ withProgress(message = "Insert into CohortTable...",value = 1, { insertCohortNew() }) })
  #####Menu Item 2 : compare cohorts #####
  output$cohort1 <- renderUI({
    selectInput("cohort1","cohort1",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort2 <- renderUI({
    selectInput("cohort2","cohort2",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort3 <- renderUI({
    selectInput("cohort3","cohort3",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort4 <- renderUI({
    selectInput("cohort4","cohort4",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort5 <- renderUI({
    selectInput("cohort5","cohort5",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$ClinicalEventCohortId <- renderUI({
    selectInput("ClinicalEventCohortId","Clinical Event Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)), selected = 0)
  })
  #####Tab set : baseline characteristics#####
  demographic_result <- eventReactive(input$characteristic_analyze,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    # demographics 
    demographicRaw <- charterstic_manufacture(cohort_definition_id_set = cohortDefinitionIdSet )
    demographicSummary <- characteristic_summary(characteristic_manufac = demographicRaw)
    demographicpvalue <- characteristic_pvalue(characteristic_manufac = demographicRaw)
    demographicSum <- merge(demographicSummary,demographicpvalue,by = "demographicName")
    colnames(demographicSum)[1] <- "baseline_Measure"
    # comorbidities
    comorbidityRaw <- baseline_comorbidity(connectionDetails = connectionDetails,
                                           Resultschema = CohortSchema,
                                           CDMschema = CDMschema,
                                           cohortTable = cohortTable,
                                           cohort_definition_id_set = cohortDefinitionIdSet )
    comorb_prev <- co_prevtable(comorbManufacData = comorbidityRaw)
    RR_pvalue  <- calculateRR(comorbManufacData = comorbidityRaw)
    comorbSum <-  merge(comorb_prev,RR_pvalue%>%select(diseaseName,pvalue),by = "diseaseName")
    colnames(comorbSum)[1] <- "baseline_Measure"
    # baseline measured values
    baselinemeasurement <<- baselineMeasure_compare(connectionDetails = connectionDetails,
                                                    Resultschema = CohortSchema,
                                                    CDMschema = CDMschema,
                                                    cohortTable = cohortTable,
                                                    cohort_definition_id_set = cohortDefinitionIdSet,
                                                    measurementConceptIdSet = c(2,3,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469,
                                                                                3011708,3006504,3005322,3005600,3017501,3026514,3005791,3021940,
                                                                                3011505,3013115,3018010,3022096) )
    measureSum <- baselinemeasurement
    colnames(measureSum)[1] <- "baseline_Measure"
    characteristicsAllSum <- rbind(demographicSum,comorbSum,measureSum)
    RRplot     <- RRplot(RRResult = RR_pvalue)
    
    resultList <- list(allCharacteristicsTable = characteristicsAllSum,
                       plotForComorbidity = RRplot)
  })
  output$RRplot <- renderPlot({  demographic_result()[[2]] })
  output$totalBaselineCharacteristicsTable <- renderDataTable({ withProgress(message = "Baseline characteristics were analyzed...",value = 1, { demographic_result()[[1]] }) })
  
  #####longitudinal Analysis#####
  load_longitudinal <- eventReactive(input$load_all_measurement,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    # load all long-term measured data
    allLongitudinalList <- list()
    for(i in 1:length(cohortDefinitionIdSet)){
      allLongitudinal <- getAllLongitudinal(connectionDetails = connectionDetails,
                                            CDMschema = CDMschema,
                                            Resultschema = CohortSchema,
                                            cohortTable = cohortTable,
                                            cohortId = cohortDefinitionIdSet[i] )
      allLongitudinalList[[ i ]] <- list(cohortId = cohortDefinitionIdSet[i],
                                         allLongitudinal = allLongitudinal )
    }
    allLongitudinalList <<- allLongitudinalList
    "Load all longitudinal data is done!"
  })
  output$load <- renderText({ withProgress(message = "Measurement data were loaded...",value = 1, { load_longitudinal() }) })
  # do lme (lonaigitudinal analysis)
  longitudinalLME <- eventReactive(input$do_longitudinalAnalysis,{
    lmeResultList <- lapply(allLongitudinalList, FUN = function(x){
      subLongitudinalData <- getLongitudinal(all_longitudinal_data = x$allLongitudinal,
                                             measurement_concept_id = input$measurementConceptId,
                                             time_unit = 'year')
      lmeDone <- lme_logitudinal(longitudinalData = subLongitudinalData)
      longitudinalResult <- list(cohortId = x[[1]],
                                 subLongitudinalData = subLongitudinalData,
                                 lmeResult1 = lmeDone[[2]],
                                 lmeResult2 = lmeDone[[1]])
      return(longitudinalResult)
    })
    plot_lme_allMeasurement <- plotLmm(longitudinal_result = lmeResultList,
                                       pftIndividual = TRUE)
    plot_lme_onlyEstimated <- plotLmm(longitudinal_result = lmeResultList,
                                      pftIndividual = FALSE)
    table_lme_Estimated <- tableLmm(longitudinal_result = lmeResultList) 
    
    lme_result <- list(plot_lme_allMeasurement,plot_lme_onlyEstimated,table_lme_Estimated)
  })
  
  output$longitudinalAnalysis <- renderPlot({ withProgress(message = "Longitudinal Analysis...",value = 1, { longitudinalLME()[[1]] }) })
  output$longutidinalAnalysis_only <- renderPlot({ longitudinalLME()[[2]] })
  output$longitudinalTable <- renderDataTable({ longitudinalLME()[[3]] })
  
  #####Tab set : clinical event#####
  clinicalEvent_frequency <- eventReactive(input$analyzeEvent,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    
    # filter cohort I need
    call_event_data <- call_event(cohort_definition_id_set = cohortDefinitionIdSet,
                                  cohortId_event = as.numeric(input$ClinicalEventCohortId))
    
    # calculate yearly count and its figure
    event_table <- event_incidence(callEvent_result = call_event_data)
    event_plot  <- plot_event_rate(event_result = event_table)
    
    event_out <- list(eventTable = event_table,
                      eventPlot = event_plot)
    return(event_out)
  })
  
  output$ClinicalEventPlot <- renderPlot({ clinicalEvent_frequency()[[2]] })
  output$ClinicalEventTable <- renderDataTable({ clinicalEvent_frequency()[[1]] })
  #survival
  eventFreesurvival <- eventReactive(input$analyzeSurvival,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    
    eventFreeSurvivalResults <- eventFreeSurvival(cohort_definition_id_set = cohortDefinitionIdSet,
                                                  cohortId_event = as.numeric(input$ClinicalEventCohortId),
                                                  targetSurvivalEndDate = as.numeric(input$survivalEndDate))
  })
  output$EventSurvivalPlot <- renderPlot({ eventFreesurvival()[[2]] })
  output$survivalAnalysisResults <- renderPrint({ eventFreesurvival()[[1]] })
  
  #####Menu Item 3 : prediction#####
  output$Target_cohort  <- renderUI({ selectInput("Target_cohort","Target Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) )   })
  output$Outcome_cohort <- renderUI({ selectInput("Outcome_cohort","Outcome Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) ) })
  output$modelSelect    <- renderUI({ selectInput("modelSelect","Machine Learning Model Select",choices = c("Lasso Logistic","Gradient Boosting")) })
  switchModelSelect     <- reactive({ switchselect_model(input$modelSelect) })
  #predict
  prediction <- eventReactive(input$DoPredict,{
    plpdata <- getPlpDataList(connectionDetails = connectionDetails,
                              CDMschema = CDMschema,
                              Resultschema = CohortSchema,
                              cohortTable = cohortTable,
                              targetCohortConceptId = as.numeric(input$Target_cohort),
                              outcomeCohortConceptId = as.numeric(input$Outcome_cohort),
                              covariateSetting = covariateSetting,
                              washoutPeriod = 0,
                              removeSubjectsWithPriorOutcome = FALSE,
                              riskWindowStart = as.numeric(input$Risk_window_start),
                              riskWindowEnd = as.numeric(input$Risk_window_end),
                              minTimeAtRisk = as.numeric(input$Minimum_TAR) )
    plp_result <<- RunPlp(getplpOut = plpdata,
                          learningModel = switchModelSelect(),
                          splitSeed = NULL,
                          outputFolder = outputFolder)
    "Prediction is done!"
  })
  output$predictDone <- renderText({ withProgress(message = "Prediction is running...",value = 1, { prediction() }) })
  
  # show prediction results
  AUROCplot <- eventReactive(input$ShowPredict,{
    auroc <- AUROCcurve(machineLearningData = plp_result)
    return(auroc)
  })
  output$AUROCcurve <- renderPlot({ AUROCplot() })
  
  contributedCovariatePlot <- eventReactive(input$ShowPredict,{
    predictiveVariable <- plotPredictiveVariables(machineLearningData = plp_result,
                                                  rankCount = 20)
    return(predictiveVariable)
  })
  output$contributedCovariates <- renderPlot({ contributedCovariatePlot() })
  
  contributedCovariateTable <- eventReactive(input$ShowPredict,{
    covariateTable <- tablePredictiveVariables(machineLearningData = plp_result,
                                               rankCount = 40)
    return(covariateTable)
  })
  output$covariateTable <- renderDataTable({ contributedCovariateTable() })
  
  eraseAllData <- eventReactive(input$eraseData,{
    removeTempAndOutput()
    "your traces were erased!"
  })
  output$eraseDone <- renderText({ eraseAllData() })
}

# Run the application
shinyApp(ui = ui, server = server)