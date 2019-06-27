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
check.packages("ICARUSviewer")
check.packages("lme4")
check.packages("lmerTest")
check.packages("effects")

outputFolder <<- Sys.getenv("outputFolder")

Sys.setlocale(category = "LC_ALL", locale = "us")

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
            # ,
            # menuItem("Comorbidity", tabName = "comorbidity")
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
                            textInput("usr","USER",""),
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
                        actionButton("do_analyze","Analysis"),
                        width = 2 ),
                    mainPanel(
                        tabsetPanel(type = "tabs",
                                    ############Demographics##########################################
                                    tabPanel("Demographics",
                                             fluidRow(titlePanel("Compare Demographic Charateristics") ),
                                             fluidRow(
                                                 box(title = "Comparison of clinical characteristics between two cohorts",
                                                     tableOutput("clinicalCharacteristicTable")),
                                                 box(title = "Comparison of Age and BMI",
                                                     plotOutput("AgePlot"),plotOutput("BmiPlot")),
                                                 box(title = "Comparison of Comorbidity Co-prevalence between two cohorts",
                                                     tableOutput("comorbidityTable"),plotOutput("comorbidityPlot"))
                                             )
                                    ),
                                    ############Longitudinal############################################
                                    tabPanel("Measurements",
                                             fluidRow(titlePanel("Longitudinal analysis of long-term measured values") ),
                                             fluidRow(column(3,numericInput("measurementConceptId","Measurement Concept ID","") ),
                                                      column(1,actionButton("do_search_measure","Search") ) ),
                                             fluidRow(
                                                 box(title = "Longitudinal analysis",
                                                     plotlyOutput("longitudinalAnalysis"))
                                             )
                                    ),
                                    ############Clinical Events#############################################
                                    tabPanel("ClinicalEvents",
                                             fluidRow(titlePanel("Clinical event rate per year") ),
                                             fluidRow(column(3,numericInput("ClinicalEventCohortId","Clinical Event Cohort ID","") ),
                                                      column(1,actionButton("do_search_event","Search") ) ),
                                             fluidRow(
                                                 box(title = "Clinical event rate per year in two cohort groups",
                                                     plotOutput("ClinicalEvent"))
                                             )
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
                        actionButton("do_cluster","Analysis"),
                        width = 2),
                    mainPanel(
                        fluidRow(titlePanel("Long-term measured value trajectory clustering") ),
                        fluidRow(
                            box(plotlyOutput("TrajectoryClustering") )
                        )
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
                        actionButton("do_predict","Analysis"),
                        width = 2),
                    mainPanel(
                        fluidRow(titlePanel("Prediction Model Develop") ),
                        fluidRow(
                            box(plotOutput("contributedCovariates"), width = 8),
                            box(plotOutput("AUROCcurve"),width = 4) ),
                        fluidRow(
                            box(tableOutput("covariateTable"), width = 12)
                        )
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
                                                                         user = input$usr,
                                                                         password = input$pw)
        connection <<-DatabaseConnector::connect(connectionDetails)

        dataList<<-call_dataList(connectionDetails = connectionDetails,
                                 connection = connection,
                                 Resultschema = input$Resultschema,
                                 CDMschema = input$CDMschema)

        demographicData<<-dataList[[1]]
        measureData <<- dataList[[2]]
        comorbidity <<- dataList[[3]]
        exacerbation <<- dataList[[4]]
        exacerbation_new <<- dataList[[5]]
        totalCohort <<- dataList[[6]]

        setting()

        removeModal()
        showModal(modalDialog(title = "Loading data complete", "Loading data were succeed!", footer = modalButton("OK")))

    })

    output$DB_Connect <- renderText({
        DBconnect()
    })

}

# Run the application
shinyApp(ui = ui, server = server)

