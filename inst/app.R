# #check package ready
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
check.packages("shiny")
check.packages("tidyverse")
check.packages("epitools")
check.packages("mgcv")
check.packages("lme4")
check.packages("lmerTest")
check.packages("lcmm")

# UI

ui <- dashboardPage(

    #header
    dashboardHeader(title = "ICARUS WINGS"),

    #side bar menu
    dashboardSidebar(

        sidebarMenu(
            menuItem("DB connection", tabName = "db"),
            menuItem("Comparing between two cohorts", tabName = "compare"),
            menuItem("Trajectory clustering", tabName = "Trajectory"),
            menuItem("Prediction model", tabName = "prediction")
        )
    ),

    #body
    dashboardBody(

        tags$head(tags$style(HTML('.main-header .logo {
                                  font-family: "Georgia", Times, "Times New Roman", serif;
                                  font-weight: bold;
                                  font-size: 24px;
                                  }'))),
        tabItems(
            #########tab menu = DB connection##############
            tabItem(tabName = "db",
                    fluidRow(
                        titlePanel("Database Connection"),
                        sidebarPanel(
                            textInput("ip","IP",""),
                            uiOutput("sqltype"),
                            textInput("CDMschema","CDM Database Schema","ICARUS"),
                            textInput("Resultschema","CDM Results schema","ICARUS"),
                            textInput("cohortTable","cohort table","asthma_cohort"),
                            textInput("user","USER",""),
                            passwordInput("pw","PASSWORD",""),
                            actionButton("db_load","LOAD DB"),
                            width = 3
                        ),
                        mainPanel(
                            textOutput(outputId = 'DB_Connect')
                        )
                    )
            ),
            #########tab menu = Comparing between two cohorts#########################
            tabItem(tabName = "compare",
                    titlePanel("Comparison"),
                    sidebarPanel(
                        numericInput("target_cohort","Target cohort ID",""),
                        numericInput("comparator_cohort","Comparator cohort ID",""),
                        width = 2 ),
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    ############Demographics##########################################
                                    tabPanel("Demographics",
                                             fluidRow(titlePanel("Compare Demographic Charateristics") ),
                                             fluidRow(column(1,actionButton("do_demographic_analyze","analyze") ) ),
                                             fluidRow(box(title = "Comparison of clinical characteristics between two cohorts and P value", width = 12,
                                                          dataTableOutput("clinicalCharacteristicTable"),tableOutput("clinicalCharacteristicPvalue"),dataTableOutput("baselineMeasurementTable")) ),
                                             fluidRow(box(title = "Comparison of baseline Comorbidity between two cohorts ( co-prevalence (%) )", width = 12,
                                                          dataTableOutput("prevalenceTable")) ),
                                             fluidRow(box(title = "comorbidity relative ratio of target cohort", width = 12,
                                                          plotOutput("RRplot"),dataTableOutput("RRTable")) )
                                    ),
                                    ############Longitudinal############################################
                                    tabPanel("Measurements",
                                             fluidRow(titlePanel("Longitudinal analysis of long-term measured values") ),
                                             fluidRow(column(1,actionButton("do_load_all_measurement","load All") ),
                                                      column(3,numericInput("measurementConceptId","Measurement Concept ID","") ),
                                                      column(1,actionButton("do_search_measure","Search") ) ),
                                             fluidRow(
                                                 box(title = "Longitudinal analysis", width = 6,
                                                     textOutput("load"),
                                                     plotOutput("longitudinalAnalysis")),
                                                 box(title = "Only estimated profiles", width = 6,
                                                     plotOutput("longutidinalAnalysis_only"))
                                             ),
                                             fluidRow(
                                               box(title = "estimted profile and 95% CI", width = 12,
                                                   dataTableOutput("longitudinalTable"))
                                             )
                                    ),
                                    ############Clinical Events#############################################
                                    tabPanel("ClinicalEvents",
                                             fluidRow(titlePanel("Clinical event rate per year") ),
                                             fluidRow(column(3,numericInput("ClinicalEventCohortId","Clinical Event Cohort ID","") ),
                                                      column(1,actionButton("do_search_event","Search") ) ),
                                             fluidRow(box(title = "Clinical event rate per year in two cohort groups", width = 12,
                                                          plotOutput("ClinicalEventPlot")) ),
                                             fluidRow(box(width = 12,
                                                          dataTableOutput("ClinicalEventTable")))
                                    )
                        )
                    )
            ),
            #########tab menu = Trajectory Clustering#########################
            tabItem(tabName = "Trajectory",
                    titlePanel("Trajectory Clustering"),
                    sidebarPanel(
                        numericInput("cohortId_trajectory","Cohort ID",""),
                        numericInput("measurementConceptId_Trajectory","Measurement Concept ID",""),
                        numericInput("clusterNumber","Cluster Count","3"),
                        actionButton("do_load_all_measurement_for_lcmm","Load Data"),
                        actionButton("do_cluster","Analysis"),
                        width = 2),
                    mainPanel(
                        fluidRow( titlePanel("Long-term measured value trajectory clustering"), textOutput("load_for_cluster") ),
                        fluidRow( box( plotOutput("TrajectoryClustering_withIndividual"), width = 6 ),
                                  box( plotOutput("TrajectoryClustering_onlyCI")        , width = 6 ) ),
                        fluidRow( box( dataTableOutput("TrajectoryClusteringTable")     , width = 12 ) )
                    )
            ),
            #########tab menu = Prediction Model#########################
            tabItem(tabName = "prediction",
                    titlePanel("Development of Prediction Model"),
                    sidebarPanel(
                        numericInput("Target_cohort","Target Cohort ID",""),
                        numericInput("Outcome_cohort","Outcome Cohort ID",""),
                        numericInput("Risk_window_start","Risk window start",""),
                        numericInput("Risk_window_end","Risk window end",""),
                        numericInput("Minimum_TAR","Minimum time at risk",""),
                        uiOutput("modelSelect"),
                        actionButton("do_predict","Predict"),
                        actionButton("do_predictionOutput","Show Result"),
                        width = 2),
                    mainPanel(
                        fluidRow(titlePanel("Prediction Model Develop"), textOutput("prediction_done") ),
                        fluidRow(
                            box(plotOutput("contributedCovariates"), width = 6),
                            box(plotOutput("AUROCcurve")           , width = 6) ),
                        fluidRow(
                            box(dataTableOutput("covariateTable")  , width = 12) )
                    )
            )
        )
    ),
    skin = "black"
)




####server##########################
server <- function(input, output, session) {
    ######################1. tab menu result : DB connection####
    output$sqltype<-renderUI({
        selectInput("sqltype", "Select DBMS",
                    choices = c("sql server" = "sql server",
                                "PostgreSQL" = "postresql",
                                "Amazon Redshift" = "redshift",
                                "Microsoft Parallel Data Warehouse" = "pdw",
                                "IBM Netezza" = "netezza",
                                "Google BigQuery" = "bigquery")
        )
    })

    ##############DB connection and load data###############
    DBconnect <- eventReactive(input$db_load, {
        connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms = input$sqltype,
                                                                         server = input$ip,
                                                                         schema = input$Resultschema,
                                                                         user = input$user,
                                                                         password = input$pw)
        connection <<-DatabaseConnector::connect(connectionDetails)

        dataList<<-call_dataList(connectionDetails = connectionDetails,
                                 connection = connection,
                                 Resultschema = input$Resultschema,
                                 CDMschema = input$CDMschema)

        demographicData<<-dataList[[1]]
        totalCohort <<- dataList[[2]]

        setting()

        removeModal()
        showModal(modalDialog(title = "Loading complete", "connecting success!", footer = modalButton("OK")))

    })

    output$DB_Connect <- renderText({
        DBconnect()
    })

    ######################2. tab menu result : Comparing between two cohorts###########################
    #############1-1. demographic######################
    demographic_result <- eventReactive(input$do_demographic_analyze,{
        demographicRaw <- charterstic_manufacture(cohort_definition_id_set = c(input$target_cohort,input$comparator_cohort) )
        demographicSummary <- characteristic_summary(characteristic_manufac = demographicRaw)
        demographicpvalue <- characteristic_pvalue(characteristic_manufac = demographicRaw)

        result_out <- list(demographic_table = demographicSummary,
                           demographic_pvelue = demographicpvalue)

        return(result_out)
    })
    output$clinicalCharacteristicTable <- renderDataTable({ demographic_result()[[1]] })

    output$clinicalCharacteristicPvalue <- renderTable({ as.data.frame(demographic_result()[[2]]) })

    prevalenceTable <- eventReactive(input$do_demographic_analyze,{
        comorbidityRaw <- baseline_comorbidity(connectionDetails = connectionDetails,
                                               Resultschema = input$Resultschema,
                                               CDMschema = input$CDMschema,
                                               cohortTable = input$cohortTable,
                                               cohortId_1 = input$target_cohort,
                                               cohortId_2 = input$comparator_cohort)
        prevalence <- co_prevtable(comorbManufacData = comorbidityRaw)
        return(prevalence)
    })
    output$prevalenceTable <- renderDataTable({ prevalenceTable() })

    RRresultList <- eventReactive(input$do_demographic_analyze,{
        comorbidityRaw <- baseline_comorbidity(connectionDetails = connectionDetails,
                                               Resultschema = input$Resultschema,
                                               CDMschema = input$CDMschema,
                                               cohortTable = input$cohortTable,
                                               cohortId_1 = input$target_cohort,
                                               cohortId_2 = input$comparator_cohort)
        RR_pvalue  <- calculateRR(comorbManufacData = comorbidityRaw)
        RRplot     <- RRplot(RRResult = RR_pvalue)
        
        RRresult <- list(RR_pvalue, RRplot)
        return(RRresult)
    })
    output$RRTable <- renderDataTable({ RRresultList()[[1]] })
    output$RRplot  <- renderPlot({ RRresultList()[[2]] })

    baselineMeasurementTable <- eventReactive(input$do_demographic_analyze,{
        baselinemeasurement <- baselineMeasure_compare(connectionDetails = connectionDetails,
                                                       Resultschema = input$Resultschema,
                                                       CDMschema = input$CDMschema,
                                                       cohortTable = input$cohortTable,
                                                       cohortId_1 = input$target_cohort,
                                                       cohortId_2 = input$comparator_cohort,
                                                       measurementConceptIdSet = c(2,3,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469,
                                                                                   3011708,3006504,3005322,3005600,3017501,3026514,3005791,3021940,
                                                                                   3011505,3013115,3018010,3022096) )
        return(baselinemeasurement)
    })
    output$baselineMeasurementTable <- renderDataTable({ baselineMeasurementTable() })
    #############1-2. longitudinal###############
    load_longitudinal <- eventReactive(input$do_load_all_measurement,{
        allLongitudinal_target<<- getAllLongitudinal(connectionDetails = connectionDetails,
                                                    CDMschema = input$CDMschema,
                                                    Resultschema = input$Resultschema,
                                                    cohortTable = input$cohortTable,
                                                    cohortId = input$target_cohort)
        allLongitudinal_compare<<- getAllLongitudinal(connectionDetails = connectionDetails,
                                                     CDMschema = input$CDMschema,
                                                     Resultschema = input$Resultschema,
                                                     cohortTable = input$cohortTable,
                                                     cohortId = input$comparator_cohort)
        
        removeModal()
        showModal(modalDialog(title = "Loading complete", "load measurement data success!", footer = modalButton("OK")))
    })

    output$load <- renderText({ load_longitudinal() })

    longitudinalLME <- eventReactive(input$do_search_measure,{
      longitudinal_data <- longitudinal(cohortId_1 = input$target_cohort,
                                        cohortId_2 = input$comparator_cohort,
                                        allLongitudinal_cohort_1 = allLongitudinal_target,
                                        allLongitudinal_cohort_2 = allLongitudinal_compare,
                                        measurement_concept_id = input$measurementConceptId )
      
      plot_lme_allMeasurement<- plotLmm(longitudinal_result = longitudinal_data,
                                        pftIndividual = TRUE)
      plot_lme_onlyEstimated<- plotLmm(longitudinal_result = longitudinal_data,
                                       pftIndividual = FALSE)
      table_lme_Estimated <- tableLmm(longitudinal_result = longitudinal_data) 
      
      lme_result <- list(plot_lme_allMeasurement,plot_lme_onlyEstimated,table_lme_Estimated)
      
      return(lme_result)
    })
    output$longitudinalAnalysis <- renderPlot({ longitudinalLME()[[1]] })

    output$longutidinalAnalysis_only <- renderPlot({ longitudinalLME()[[2]] })
    
    output$longitudinalTable <- renderDataTable({ longitudinalLME()[[3]] })
    #############1-3. Clinical Events###############
    clinicalEvent_frequency <- eventReactive(input$do_search_event,{
        call_event_data <- call_event(connectionDetails = connectionDetails,
                                      Resultschema = input$Resultschema,
                                      cohortTable = input$cohortTable,
                                      cohortId_1 = input$target_cohort,
                                      cohortId_2 =  input$comparator_cohort,
                                      cohortId_event = input$ClinicalEventCohortId)
        event_table <- event_incidence(callEvent_result = call_event_data)
        event_plot  <- plot_event_rate(event_result = event_table)

        event_out <- list(eventTable = event_table,
                          eventPlot = event_plot)
        return(event_out)
    })
    output$ClinicalEventPlot <- renderPlot({ clinicalEvent_frequency()[[2]] })
    output$ClinicalEventTable <- renderDataTable({ clinicalEvent_frequency()[[1]] })
    ######################3. tab menu result : Trajectory Clustering###################################
    #############1-1. trajectory#############
    load_for_clustering <- eventReactive(input$do_load_all_measurement_for_lcmm,{
      allclustering_target<- getAllLongitudinal(connectionDetails = connectionDetails,
                                                CDMschema = input$CDMschema,
                                                Resultschema = input$Resultschema,
                                                cohortTable = input$cohortTable,
                                                cohortId = input$cohortId_trajectory)
      lcmm_cluster_result_list <<- latent_class_classification(all_longitudinal_data_for_cluster = allclustering_target,
                                                               measurementConceptId_Trajectory = input$measurementConceptId_Trajectory,
                                                               cluster_number = input$clusterNumber)
      removeModal()
      showModal(modalDialog(title = "Loading complete", "load measurement data success!", footer = modalButton("OK")))
    })
    output$load_for_cluster <- renderText({ load_for_clustering() })
    
    plot_lcmm_cluster_withIndividual <- eventReactive(input$do_cluster,{
      plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                               individual_trajectories = TRUE,
                                               cluster_number = input$clusterNumber)
      return(plot_cluster)
    })
    output$TrajectoryClustering_withIndividual<- renderPlot({ plot_lcmm_cluster_withIndividual() })
    
    plot_lcmm_cluster_onlyCI <- eventReactive(input$do_cluster,{
      plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                               individual_trajectories = FALSE,
                                               cluster_number = input$clusterNumber)
      return(plot_cluster)
    })
    output$TrajectoryClustering_onlyCI<- renderPlot({ plot_lcmm_cluster_onlyCI() })
    
    table_lcmm_cluster <- eventReactive(input$do_cluster,{
      table_cluster <- latent_longitudinal_table(lcmm_classification_result_list = lcmm_cluster_result_list,
                                                 cluster_number = input$clusterNumber)
      return(table_cluster)
    })
    output$TrajectoryClusteringTable <- renderDataTable({ table_lcmm_cluster() })
    
    ######################4. tab menu result : Prediction model###################################
    #############1-1. prediction model###########
    output$modelSelect <- renderUI({
      selectInput("modelSelect","Machine Learning Model Select",choices = c("Lasso Logistic","Gradient Boosting"))
    })
    switchModelSelect <- reactive({ switchselect_model(input$modelSelect) })
    
    prediction <- eventReactive(input$do_predict,{
      plpdata <- getPlpDataList(connectionDetails = connectionDetails,
                                CDMschema = input$CDMschema,
                                Resultschema = input$Resultschema,
                                cohortTable = input$cohortTable,
                                targetCohortConceptId = input$Target_cohort,
                                outcomeCohortConceptId = input$Outcome_cohort,
                                covariateSetting = covariateSetting,
                                washoutPeriod = 0,
                                removeSubjectsWithPriorOutcome = FALSE,
                                riskWindowStart = input$Risk_window_start,
                                riskWindowEnd = input$Risk_window_end,
                                minTimeAtRisk = input$Minimum_TAR)
      plp_result <<- RunPlp(getplpOut = plpdata,
                            learningModel = switchModelSelect(),
                            splitSeed = NULL,
                            outputFolder = outputFolder)
      removeModal()
      showModal(modalDialog(title = "Prediction complete", "Prediction was completed, show the results!", footer = modalButton("OK")))
      
    })
    output$prediction_done <- renderText({ prediction() })
    
    contributedCovariatePlot <- eventReactive(input$do_predictionOutput,{
      predictiveVariable <- plotPredictiveVariables(machineLearningData = plp_result,
                                                    rankCount = 40)
      return(predictiveVariable)
    })
    output$contributedCovariates <- renderPlot({ contributedCovariatePlot() })
    
    AUROCplot <- eventReactive(input$do_predictionOutput,{
      auroc <- AUROCcurve(machineLearningData = plp_result)
      return(auroc)
    })
    output$AUROCcurve <- renderPlot({ AUROCplot() })
    
    contributedCovariateTable <- eventReactive(input$do_predictionOutput,{
      covariateTable <- tablePredictiveVariables(machineLearningData = plp_result,
                                                 rankCount = 40)
      return(covariateTable)
    })
    output$covariateTable <- renderDataTable({ contributedCovariateTable() })
}

# Run the application
shinyApp(ui = ui, server = server)

