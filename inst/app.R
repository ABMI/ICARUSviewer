# #check package ready
check.packages("dplyr")
check.packages("reshape2")
check.packages("ggplot2")
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

outputFolder <<- Sys.getenv("outputFolder")

Sys.setlocale(category = "LC_ALL", locale = "us")

# UI

ui <- dashboardPage(

    #header
    dashboardHeader(title = "ICARUSviewer"),

    #side bar menu
    dashboardSidebar(

        sidebarMenu(
            menuItem("DB connection", tabName = "db"),
            menuItem("Asthma Phenotype", tabName = "AsthmaPhenotype"),
            menuItem("Asthma Biomarker", tabName = "Biomarker"),
            menuItem("PFT In Detail", tabName = "PFTdetail"),
            menuItem("Clinical Characteristic", tabName = "Characteristic"),
            menuItem("Biomarker Characteristic", tabName = "biomarker")
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


            #########tab menu = asthma phenotype compare##########
            tabItem(tabName = "AsthmaPhenotype",
                    fluidRow(

                        titlePanel("Compare Asthma Phenotype Cohort"),
                        sidebarPanel(
                            actionButton(inputId = "show_result_phe", label = "SHOW"),
                            uiOutput("selectcohort_phe"), width = 3 )
                    )
                    ,
                    tabsetPanel(type = "tabs",
                                ############Demographic anlaysis tab#####################
                                tabPanel("Demographic",
                                         fluidRow(
                                             titlePanel("Compare gender, age and BMI")
                                         ),
                                         fluidRow(
                                             box(title = "Gender compare", width = 4, background = "blue", solidHeader = TRUE,
                                                 plotOutput("genderPiePlot")),
                                             box(title = "Age tree compare", width = 4, background = "blue", solidHeader = TRUE,
                                                 plotOutput("agePiePlot")),
                                             box(title = "Gender-Age characteristic table", width = 4, status = "primary", solidHeader = TRUE,
                                                 tableOutput("genderageTable"),
                                                 verbatimTextOutput("genderage_pvalue"))
                                         ),
                                         #bmi compare
                                         fluidRow(
                                             box(title = "Gender-Age compare", width = 4, background = "blue", solidHeader = TRUE,
                                                 plotOutput("genderAgePiePlot")),
                                             box(title = "BMI compare", width = 4, background = "yellow", solidHeader = TRUE,
                                                 plotOutput("BMITreePlot")),
                                             box(title = "BMI-Age characteristic table", width = 4, status = "warning", solidHeader = TRUE,
                                                 tableOutput("genderbmiTable"),
                                                 verbatimTextOutput("genderbmip_value"))
                                         )
                                ),
                                ############PFT changing analysis tab#####################
                                tabPanel("PulmonaryFunctionTest",
                                         fluidRow(
                                             titlePanel("Pulmonary Function Test changing comparison")
                                         ),
                                         fluidRow(
                                             box(title = "FEV1(%) change according to time flow",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 plotOutput("FEV1plot")),
                                             box(title = "FEV1/FVC(%) change according to time flow",
                                                 width = 6,status = "warning",solidHeader = TRUE,
                                                 plotOutput("FEV1FVCplot")
                                             )
                                         ),
                                         fluidRow(
                                             box(title = "FEV1(%) change according to time flow",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 tableOutput("FEV1counttable"),
                                                 tableOutput("FEV1predicttable")
                                             ),
                                             box(title = "FEV1/FVC(%) change according to time flow",
                                                 width = 6,status = "warning",solidHeader = TRUE,
                                                 tableOutput("FEV1FVCcounttable"),
                                                 tableOutput("FEV1FVCpredicttable")
                                             )
                                         )
                                ),
                                ############comorbidity analysis tab#####################
                                tabPanel("comorbidity",
                                         fluidRow(
                                             titlePanel("Comorbidity Co-prevalence comparison")
                                         ),
                                         fluidRow(
                                             box(title = "Metabolic disease Relative Ratio among patient >= 50’s",
                                                 width = 6,background = 'blue',solidHeader = TRUE,
                                                 plotOutput("metabolicCoprev")
                                             ),
                                             box(title = "Immune relative disease Relative Ratio among patient >= 12 years old",
                                                 width = 6,background = 'blue',solidHeader = TRUE,
                                                 plotOutput("immuneCoprev")
                                             )

                                         ),
                                         fluidRow(
                                             box(title = "Metabolic disease Co-prevalence(%) among patient >= 50’s",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 tableOutput("metabolicCoprevTable")
                                             ),
                                             box(title = "Immune relative disease Co-prevalence(%) among patient >= 12 years old",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 tableOutput("immuneCoprevTable")
                                             )

                                         )
                                ),
                                ############exacerbation count analysis tab#####################
                                tabPanel("exacerbationCount",
                                         fluidRow(
                                             titlePanel("Compare Exacerbation Count Between Two Cohort")
                                         ),
                                         fluidRow(
                                             box(title = "Exacerbation Count Compare",
                                                 width = 6,background = 'blue', solidHeader = TRUE,
                                                 plotOutput("exacerbationPlot")
                                             ),
                                             box(title = "statistical analysis of Differences of exacerbation Count",
                                                 width = 6,status = "info",solidHeader = TRUE,
                                                 tableOutput("exacerbationTable"),
                                                 verbatimTextOutput("exacerbationPvalue")
                                             )

                                         )
                                ),
                                ############PLP analysis tab#####################
                                tabPanel("PredictiveVariable",
                                         fluidRow(
                                             titlePanel("Top 40 variables which could be predictive variables"),
                                             uiOutput(outputId = "ModelSelect"),
                                             actionButton(inputId = "RunPredictionModel", label = "Run Prediction Model"),
                                             textOutput("readyPLP")
                                         ),
                                         fluidRow(
                                             box(title = "Using Machine Learning Model, get predictive variables",
                                                 width = 8, background = "yellow",solidHeader = TRUE,
                                                 plotOutput("PredictiveVariablePlot")
                                             ),
                                             box(title = "Prediction Model AUC plot",
                                                 width = 4, background = "yellow",solidHeader = TRUE,
                                                 plotOutput("PredictiveAUC")
                                             )
                                         ),
                                         fluidRow(
                                             box(title = "Using Machine Learning Model",
                                                 width = 12,status = "warning",solidHeader = TRUE,
                                                 tableOutput("PredictiveVariableTable")
                                             )
                                         )
                                )
                    )

            ),
            # biomarker compare#################
            tabItem(tabName = "Biomarker",
                    fluidRow(
                        titlePanel("Compare Asthma Cohort according to biomarker"),
                        sidebarPanel(
                            actionButton(inputId = "show_result_biomarker", label = "SHOW"),
                            uiOutput("selectbiomarker_char"), width = 3 )
                    )
                    ,
                    tabsetPanel(type = "tabs",

                                tabPanel("PulmonaryFunctionTest",
                                         fluidRow(
                                             titlePanel("Pulmonary Function Test changing comparison")
                                         ),
                                         fluidRow(
                                             box(title = "FEV1(%) change according to time flow",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 plotOutput("FEV1plot_bio")),
                                             box(title = "FEV1/FVC(%) change according to time flow",
                                                 width = 6,status = "warning",solidHeader = TRUE,
                                                 plotOutput("FEV1FVCplot_bio")
                                             )
                                         ),
                                         fluidRow(
                                             box(title = "FEV1(%) change according to time flow",
                                                 width = 6,status = "primary",solidHeader = TRUE,
                                                 tableOutput("FEV1counttable_bio"),
                                                 tableOutput("FEV1predicttable_bio")
                                             ),
                                             box(title = "FEV1/FVC(%) change according to time flow",
                                                 width = 6,status = "warning",solidHeader = TRUE,
                                                 tableOutput("FEV1FVCcounttable_bio"),
                                                 tableOutput("FEV1FVCpredicttable_bio")
                                             )
                                         )
                                ),
                                tabPanel("exacerbationCount",
                                         fluidRow(
                                             titlePanel("Compare Exacerbation Count Between Two Cohort")
                                         ),
                                         fluidRow(
                                             box(title = "metabolic disease co-prevalence between patients over 50's",
                                                 width = 6,status = "info",solidHeader = TRUE,
                                                 plotOutput("metabolicCoprev_bio")
                                             ),
                                             box(title = "immune-related disease co-prevalence between patients",
                                                 width = 6,status = "info",solidHeader = TRUE,
                                                 plotOutput("immuneCoprev_bio")
                                             )

                                         )
                                )
                    )
            ),

            #########tab menu = PFT compare more detail########
            tabItem(tabName = "PFTdetail",
                    titlePanel("Pulmonary Function Test Compare in more detail"),
                    sidebarPanel( uiOutput("PFTselect") ,
                                  uiOutput("cohortSelect"),
                                  checkboxGroupInput("genderDivided", "Gender Devided?", choices = list("yes" = TRUE,
                                                                                                        "no" = FALSE) ),
                                  textInput("ageSection", label = "write down age section",value = "ex)12/40/60/100"),
                                  actionButton(inputId = "show_pft_in_detail", label = "SHOW") ),
                    mainPanel(
                        box(title = "Pulmonary Function Test Changing according to Time Flow In Detail",width = 12,
                            plotOutput("PFTchanging_indetail")
                        ),
                        box(title = "Pulmonary Function Test Count", width = 12,
                            tableOutput("PFTcountTable_indetail")
                        ),
                        box(title = "Pulmonary Function Test Predict", width = 12,
                            tableOutput("PFTpredictTable_indetail")
                        )
                    )

            ),
            #########tab menu = Clinical Characteristic################
            tabItem(tabName = "Characteristic",
                    titlePanel("Comparison of clinical characteristics among asthma cohorts"),
                    fluidRow(box(uiOutput("cohortchaSelect"), width = 6),
                             actionButton(inputId = "show_clincical_charac", label = "SHOW") ),
                    fluidRow(box(tableOutput("meanSdTable"), width = 10) ),
                    fluidRow(box(tableOutput("anovaPvalue"), width = 10) )
            ),
            #########tab menu = Biomarker Characteristic###############
            tabItem(tabName = "biomarker",
                    titlePanel("Comparison of Biomarker profiles among asthma cohorts"),
                    fluidRow(box(uiOutput("cohortbioSelect"), width = 6),
                             actionButton(inputId = "show_biomarker_charac", label = "SHOW") ),
                    fluidRow(box(tableOutput("meanSdTable_bio"), width = 10) ),
                    fluidRow(box(tableOutput("anovaPvalue_bio"), width = 10) )
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
        totalCohort <<- dataList[[5]]

        setting()

        removeModal()
        showModal(modalDialog(title = "Loading data complete", "Loading data were succeed!", footer = modalButton("OK")))

    })

    output$DB_Connect <- renderText({
        DBconnect()
    })

    ######################2. tab menu result : Asthma Phenotype##########
    #UI create
    output$selectcohort_phe <- renderUI({
        selectInput(inputId = "selectcohort_phe", label = "Select Cohort Set",
                    choices = c("Severe Asthma vs Non-severe Asthma" ,
                                "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma" ),
                    multiple = FALSE)
    })

    #switch choice to cohort Id set
    switchcohortPhe <- reactive({
        switchcohort(input$selectcohort_phe)
    })



    ##############demographics analysis result###################
    # 1. gender pie graph
    genderpie <- eventReactive(input$show_result_phe,{

        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        genderpieplot(charactManufac = demographic_ready)
    })

    output$genderPiePlot <- renderPlot({
        genderpie()
    })

    # 2. age tree graph
    agepie <- eventReactive(input$show_result_phe,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        agepieplot(charactManufac = demographic_ready)
    })

    output$agePiePlot <- renderPlot({
        agepie()
    })

    # 3. gender-age table
    genderagetable <- eventReactive(input$show_result_phe, {
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        demographic_Table(charactManufac = demographic_ready,
                          dividedVariable = "age")
    })
    output$genderageTable <- renderTable({
        genderagetable()
    })


    # 4. p-value calculate
    agegender_pvalue <- eventReactive(input$show_result_phe,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        demographic_cal(charactManufac = demographic_ready,
                        dividedVariable = "age")

    })
    output$genderage_pvalue <- renderText({
        agegender_pvalue()
    })

    # 5. BMI tree plot
    bmitree <- eventReactive(input$show_result_phe,{
        demographic_ready <- charactManufacture(characteristicData = demographicData,
                                                cohortDefinitionIdSet = switchcohortPhe())
        bmitreeplot(charactManufac = demographic_ready)
    })

    output$BMITreePlot <- renderPlot({
        bmitree()
    })

    # 6. gender - BMI table
    genderbmitable <- eventReactive(input$show_result_phe, {
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        demographic_Table(charactManufac = demographic_ready,
                          dividedVariable = "bmi")
    })
    output$genderbmiTable <- renderTable({
        genderbmitable()
    })

    # 7. p-value calculate
    bmigender_pvalue <- eventReactive(input$show_result_phe,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohortPhe())
        demographic_cal(charactManufac = demographic_ready,
                        dividedVariable = "bmi")

    })
    output$genderbmip_value <- renderText({
        bmigender_pvalue()
    })


    ##############PFT changing analysis result##################

    #FEV1plot
    plotFEV1 <- eventReactive(input$show_result_phe,{

        FEV1data <- PFTmanufacture(measurementData = measureData,
                                   measurementType = 3011708,
                                   cohortDefinitionIdSet = switchcohortPhe())
        FEV1plot <- plotPFT(PFTmanufactured = FEV1data)
        FEV1plot
    })
    output$FEV1plot <- renderPlot({
        plotFEV1()
    })
    #FEV1FVCplot
    plotFEV1FVC <- eventReactive(input$show_result_phe,{

        FEV1FVCdata <- PFTmanufacture(measurementData = measureData,
                                      measurementType = 3011505,
                                      cohortDefinitionIdSet = switchcohortPhe())
        FEV1FVCplot <- plotPFT(PFTmanufactured = FEV1FVCdata)
        FEV1FVCplot
    })
    output$FEV1FVCplot <- renderPlot({
        plotFEV1FVC()
    })

    #FEV1 table
    tablecountFEV1 <- eventReactive(input$show_result_phe,{
        FEV1data <- PFTmanufacture(measurementData = measureData,
                                   measurementType = 3011708,
                                   cohortDefinitionIdSet = switchcohortPhe())
        FEV1countTable <- pft_count_table(PFTmanufactured = FEV1data)
        FEV1countTable
    })
    tablepredictFEV1 <- eventReactive(input$show_result_phe,{
        FEV1data <- PFTmanufacture(measurementData = measureData,
                                   measurementType = 3011708,
                                   cohortDefinitionIdSet = switchcohortPhe())
        FEV1predictTable <- pft_predict_table(PFTmanufactured = FEV1data)
        FEV1predictTable
    })

    output$FEV1counttable <- renderTable({
        tablecountFEV1()
    })

    output$FEV1predicttable <- renderTable({
        tablepredictFEV1()
    })

    #FEV1FVC table
    tablecountFEV1FVC <- eventReactive(input$show_result_phe,{
        FEV1FVCdata <- PFTmanufacture(measurementData = measureData,
                                      measurementType = 3011505,
                                      cohortDefinitionIdSet = switchcohortPhe())
        FEV1FVCcountTable <- pft_count_table(PFTmanufactured = FEV1FVCdata)
        FEV1FVCcountTable
    })

    tablepredictFEV1FVC <- eventReactive(input$show_result_phe,{
        FEV1FVCdata <- PFTmanufacture(measurementData = measureData,
                                      measurementType = 3011505,
                                      cohortDefinitionIdSet = switchcohortPhe())
        FEV1FVCpredictTable <- pft_predict_table(PFTmanufactured = FEV1FVCdata)
        FEV1FVCpredictTable
    })

    output$FEV1FVCcounttable <- renderTable({
        tablecountFEV1FVC()
    })

    output$FEV1FVCpredicttable <- renderTable({
        tablepredictFEV1FVC()
    })

    ##############comorbidity analysis result####################

    #metabolic disease relative ratio
    comorbidity_metabolic <- eventReactive(input$show_result_phe,{

        comorbManufac<- comorbManufacture(comorbidityData = comorbidity,
                                          cohortDefinitionIdSet = switchcohortPhe())

        RRcaclulate<-calculateRR(comorbManufacData = comorbManufac,
                                 whichDisease = 'metabolic')

        metabolic_RR <- RRplot(RRResult = RRcaclulate)

        return(metabolic_RR)
    })

    output$metabolicCoprev <-renderPlot({
        comorbidity_metabolic()
    })

    #immune disease relative ratio
    comorbidity_immune <- eventReactive(input$show_result_phe,{

        comorbManufac<- comorbManufacture(comorbidityData = comorbidity,
                                          cohortDefinitionIdSet = switchcohortPhe())

        RRcaclulate<-calculateRR(comorbManufacData = comorbManufac,
                                 whichDisease = 'immune')

        immune_RR <- RRplot(RRResult = RRcaclulate)

        return(immune_RR)
    })

    output$immuneCoprev  <- renderPlot({
        comorbidity_immune()
    })


    coPrevTable_metabolic <- eventReactive(input$show_result_phe ,{

        comorbManufac<- comorbManufacture(comorbidityData = comorbidity,
                                          cohortDefinitionIdSet = switchcohortPhe())

        metabolicTable <- co_prevtable(comorbManufacData = comorbManufac,
                                       whichDisease = 'metabolic')
        immuneTable <- co_prevtable(comorbManufacData = comorbManufac,
                                    whichDisease = 'immune')

        co_prevTable <- list(metabolicTable = metabolicTable,
                       immuneTable = immuneTable)

        return(co_prevTable)
    })

    output$metabolicCoprevTable <- renderTable({
        out <- coPrevTable_metabolic()
        out[[1]]
    })
    output$immuneCoprevTable <- renderTable({
        out <- coPrevTable_metabolic()
        out[[2]]
    })

    ##############exacerbation Count analysis result###############

    #exacerbation Count Plot
    exacerbationCountPlot <- eventReactive(input$show_result_phe,{
        exacerbationManufac <- exacerbaManufacture()
        sumCounExacerbation <- sumCountExacerbation(exacerbationCount = exacerbationManufac,
                                                    cohortDefinitionIdSet = switchcohortPhe())
        plotExacerbation <- plotExacerbationCount(sumCountExacerbation = sumCounExacerbation)

        return(plotExacerbation)
    })
    output$exacerbationPlot <- renderPlot({
        exacerbationCountPlot()
    })

    #exacerbation Count Table
    exacerbationTable <-eventReactive(input$show_result_phe,{
        exacerbationManufac <- exacerbaManufacture()
        sumCounExacerbation <- sumCountExacerbation(exacerbationCount = exacerbationManufac,
                                                    cohortDefinitionIdSet = switchcohortPhe())
        tableExacerbation <- tableExacerbationCoun(sumCountExacerbation = sumCounExacerbation)

        return(tableExacerbation)
    })
    output$exacerbationTable <- renderTable({
        exacerbationTable()
    })

    #p_value exacerbation Count
    exacerbationPvalue <-eventReactive(input$show_result_phe,{
        exacerbationManufac <- exacerbaManufacture()
        textExacerbation <- p_value_ExacerbationCouny(exacerbationCount = exacerbationManufac,
                                                      cohortDefinitionIdSet = switchcohortPhe())

        return(textExacerbation)
    })
    output$exacerbationPvalue <- renderText({
        exacerbationPvalue()
    })
    ##############predictive Variable analysis resylt#################
    #createUI for prediction model
    output$ModelSelect <- renderUI({
        selectInput("ModelSelect", "Choose Prediction Model",
                    choices = c("Lasso Logistic Regression" = "lassologistic",
                                "Gradient Boosting Machine" = "gradientboosting",
                                "Random Forest Machine" = "randomforest")
        )
    })

    #switch asthma cohort to conceptId
    switchcohortPlp<- reactive({
        switchselect_plp(input$selectcohort_phe)
    })

    #switch machine model select to plp::machinelearning
    switchModel <- reactive({
        switchselect_model(input$ModelSelect)
    })

    readyPrediction <- eventReactive(input$show_result_phe,{

        readyPlpData <- getPlpData (connectionDetails = connectionDetails,
                                    connection = connection,
                                    Resultschema = input$Resultschema,
                                    CDMschema = input$CDMschema,
                                    cohortTable = 'asthma_cohort',
                                    targetId = 1,
                                    outcomeCohortConceptId = switchcohortPlp(),
                                    covariateSetting = covariateSetting,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = TRUE,
                                    riskWindowStart = 0,
                                    riskWindowEnd = 365*15)
        readyPlp <<- readyPlpData

        removeModal()
        showModal(modalDialog(title = "ready to start prediction", "go prediction! Press the 'Run Prediction Model' Button ", footer = modalButton("OK")))

    })

    output$readyPLP <- renderText({
        readyPrediction()
    })

    runPredictionModel <- eventReactive(input$RunPredictionModel,{

        readyPlpData<-readyPlp

        machineLearningResult <- RunPlp(getplpOut = readyPlpData,
                                        learningModel = switchModel())

        plot <- plotPredictiveVariables(machineLearningData = machineLearningResult,
                                        rankCount = 40)

        table <- tablePredictiveVariables(machineLearningData = machineLearningResult,
                                          rankCount = 40)

        AUROC <- AUROCcurve(machineLearningData = machineLearningResult)

        outputList <- list(plot = plot,
                           table = table,
                           AUROC = AUROC)
        return(outputList)
    })

    output$PredictiveVariablePlot <- renderPlot({
        out <- runPredictionModel()
        out[[1]]
    })

    output$PredictiveVariableTable <- renderTable({
        out <- runPredictionModel()
        out[[2]]
    })

    output$PredictiveAUC <- renderPlot({
        out <- runPredictionModel()
        out[[3]]
    })

    ######################3. tab menu result : PFT In Detail################
    ##############ui update###############
    output$PFTselect <- renderUI({
        selectInput("PFTselect","which PFT",
                    choices = c("FEV1(%)" = 3011708,
                                "FEV1/FVC(%)" = 3011505))
    })
    output$cohortSelect <- renderUI({
        checkboxGroupInput("cohortSelect", "which asthma phenotype do you want to see?",
                           choices = list("All Asthma Patient" = 1,
                                          "Non-severe Asthma" = 2,
                                          "Severe Asthma" = 3,
                                          "Aspirin Exacerbated Respiratory Disease" = 4,
                                          "AERD subtype 1" = 51,
                                          "AERD subtype 2" = 52,
                                          "AERD subtype 3" = 53,
                                          "AERD subtype 4" = 54,
                                          "Aspirin Tolerant Asthma" = 5)
                           )
    })

    ##############PFT in detail analysis result###################
    #pft plot
    PFT_indetail_plot <- eventReactive(input$show_pft_in_detail ,{

        pftDetail <- PFTmanufacture_detail(measurementType = input$PFTselect,
                                           cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)),
                                           ageSection = as.character(input$ageSection))

        pftDetail_plot <- plotPFT_detail(PFTmanufactured = pftDetail,
                                         genderDivided = as.logical(input$genderDivided))

        return(pftDetail_plot)
    })

    output$PFTchanging_indetail <- renderPlot({
        PFT_indetail_plot()
    })

    #pft count table
    PFT_indetail_table <- eventReactive(input$show_pft_in_detail ,{

        pftDetail <- PFTmanufacture_detail(measurementType = input$PFTselect,
                                           cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)),
                                           ageSection = as.character(input$ageSection))

        pftDetail_table <- pftCountTable_indetail(PFTmanufactured = pftDetail,
                                                  genderDivided = as.logical(input$genderDivided))

        return(pftDetail_table)
    })

    output$PFTcountTable_indetail <- renderTable({
        PFT_indetail_table()
    })

    #pft predict table
    PFTpredict_indetail_table <- eventReactive(input$show_pft_in_detail ,{

        pftDetail <- PFTmanufacture_detail(measurementType = input$PFTselect,
                                           cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)),
                                           ageSection = as.character(input$ageSection))

        pftpredict_table <- pftPredictTable_indetail(PFTmanufactured = pftDetail,
                                                     genderDivided = as.logical(input$genderDivided))

        return(pftpredict_table)
    })

    output$PFTpredictTable_indetail <- renderTable({
        PFTpredict_indetail_table()
    })

    ######################4. tab menu result : Clinical Characteristic##################
    ##############ui update##############
    output$cohortchaSelect <- renderUI({
        checkboxGroupInput("cohortchaSelect", "which asthma phenotype do you want to see?",
                           choices = list("All Asthma Patient" = 1,
                                          "Non-severe Asthma" = 2,
                                          "Severe Asthma" = 3,
                                          "Aspirin Exacerbated Respiratory Disease" = 4,
                                          "AERD subtype 1" = 51,
                                          "AERD subtype 2" = 52,
                                          "AERD subtype 3" = 53,
                                          "AERD subtype 4" = 54,
                                          "Aspirin Tolerant Asthma" = 5)
        )
    })
    ##############clinical characteristic###############
    #mean and sd
    meanSd <- eventReactive(input$show_clincical_charac,{
        clinicalCharac <- clinicalCharManufacture(cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)))
        meanSdTable <- characterAnalysis(clinicalCharData)
        return(meanSdTable)
    })
    output$meanSdTable <- renderTable({
        meanSd()
    })
    #ANOVA p-value
    anovatable <- eventReactive(input$show_clincical_charac,{
        clinicalCharac <- clinicalCharManufacture(cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)))
        anovaTable <- characPvalue(clinicalCharData)
        return(anovaTable)
    })
    output$anovaPvalue <- renderTable({
        anovatable()
    })
    ######################5. tab menu result : Biomarker Characteristic#################
    ##############ui update#############
    output$cohortbioSelect <- renderUI({
        checkboxGroupInput("cohortbioSelect", "which asthma phenotype do you want to see?",
                           choices = list("All Asthma Patient" = 1,
                                          "Non-severe Asthma" = 2,
                                          "Severe Asthma" = 3,
                                          "Aspirin Exacerbated Respiratory Disease" = 4,
                                          "AERD subtype 1" = 51,
                                          "AERD subtype 2" = 52,
                                          "AERD subtype 3" = 53,
                                          "AERD subtype 4" = 54,
                                          "Aspirin Tolerant Asthma" = 5)
        )
    })

    ##############biomarker characteristic###################
    biomarker_meanSd <- eventReactive(input$show_biomarker_charac,{
        biomarkerMan <- biomarkerManufac(cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)) )
        biomarkerMeanSd <- BiomarkerAnalysis(biomarkerData)
        return(biomarkerMeanSd)
    })
    output$meanSdTable_bio <- renderTable({
        biomarker_meanSd()
    })

    biomarker_pvalue <- eventReactive(input$show_biomarker_charac,{
        biomarkerMan <- biomarkerManufac(cohortDefinitionIdSet = as.numeric(as.vector(input$cohortSelect)) )
        biomarkerPvalue <- biomarkerPvalue(biomarkerData)
        return(biomarkerPvalue)
    })
    output$anovaPvalue_bio <- renderTable({
        biomarker_pvalue()
    })
}

# Run the application
shinyApp(ui = ui, server = server)

