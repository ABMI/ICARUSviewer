# #check package ready
check.packages("dplyr")
check.packages("reshape2")
check.packages("ggplot2")
check.packages("shiny")
check.packages("SqlRender")
check.packages("DatabaseConnector")
check.packages("shinydashboard")
check.packages("shiny")
check.packages("tidyverse")
check.packages("mgcv")

Sys.setlocale(category = "LC_ALL", locale = "us")

# UI

ui <- dashboardPage(

    #header
    dashboardHeader(title = "ICARUSviewer"),

    #side bar menu
    dashboardSidebar(

        sidebarMenu(
            menuItem("DB connection", tabName = "db"),
            menuItem("Demographics", tabName = "demographics"),
            menuItem("PFT changing", tabName = "PFT"),
            menuItem("Comorbidity", tabName = "comorbidity")
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

            #tab menu = DB connection
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

            #tab menu = demographics
            tabItem(tabName = "demographics",
                    fluidRow(
                        titlePanel("Characteristics demographic comparison"),

                        #choose disease you want to compare
                        sidebarPanel(
                            actionButton(inputId = "show_demographic", label = "SHOW"),
                            uiOutput("selectcohort_char"), width = 3
                        )
                    ),
                    #age and gender compare
                    fluidRow(
                        box(title = "Gender compare", width = 4, status = "primary", solidHeader = TRUE,
                            plotOutput("genderPiePlot")),
                        box(title = "Age tree compare", width = 4, status = "primary", solidHeader = TRUE,
                            plotOutput("agePiePlot")),
                        box(title = "Gender-Age characteristic table", width = 4, status = "primary", solidHeader = TRUE,
                            tableOutput("genderageTable"),
                            verbatimTextOutput("genderage_pvalue"))
                    ),
                    #bmi compare
                    fluidRow(
                        box(title = "BMI compare", width = 8, status = "warning", solidHeader = TRUE,
                            plotOutput("BMITreePlot")),
                        box(title = "BMI-Age characteristic table", width = 4, status = "warning", solidHeader = TRUE,
                            tableOutput("genderbmiTable"),
                            verbatimTextOutput("genderbmip_value"))
                    )

            ),

            #tab menu = PFT changing
            tabItem(tabName = "PFT",
                    fluidRow(
                        titlePanel("Pulmonary Function Test changing comparison"),
                        sidebarPanel(
                            actionButton(inputId = "show_PFT", label = "SHOW"),
                            uiOutput("selectcohort_pft"),width = 3
                        )
                    ),
                    fluidRow(
                        box(title = "FEV1(%) change according to time flow",
                            width = 6,status = "primary",solidHeader = TRUE,
                            plotOutput("FEV1plot")),
                        box(title = "FEV1/FVC(%) change according to time flow",
                            width = 6,status = "warning",solidHeader = TRUE,
                            plotOutput("FEV1FVCplot"))
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

            #tab menu = comorbidity
            tabItem(tabName = "comorbidity",
                    fluidRow(
                        titlePanel("Comorbidity Co-prevalence comparison"),
                        sidebarPanel(
                            actionButton(inputId = "show_comorb", label = "SHOW"),
                            uiOutput("selectcohort_comor"), width = 3
                        )
                    ),
                    fluidRow(
                        box(title = "metabolic disease co-prevalence between patients over 50's",
                            width = 6,status = "info",solidHeader = TRUE,
                            plotOutput("metabolicCoprev")
                        ),
                        box(title = "immune-related disease co-prevalence between patients",
                            width = 6,status = "info",solidHeader = TRUE,
                            plotOutput("immuneCoprev"))

                    )
            )
        )
    ),
    skin = "black"
)
######UI end########################


####server
server <- function(input, output, session) {

    ###tab menu result : DB connection####
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

    ####DB connection and load data
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
        PFTdata <<- dataList[[2]]

        removeModal()
        showModal(modalDialog(title = "Loading data complete", "Loading data were succeed!", footer = modalButton("OK")))

    })

    output$DB_Connect <- renderText({
        DBconnect()
    })


    ##############demographics analysis result###################
    #UI create
    output$selectcohort_char <- renderUI({
        selectInput(inputId = "selectcohort_char", label = "Select Cohort Set",
                    choices = c("Severe Asthma vs Non-severe Asthma",
                                "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma"),
                    multiple = FALSE)
    })

    #switch choice to cohort Id set
    switchcohort_char <- reactive({
        switchcohort(input$selectcohort_char)
    })

    # 1. gender pie graph
    genderpie <- eventReactive(input$show_demographic,{

        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        genderpieplot(charactManufac = demographic_ready)
    })

    output$genderPiePlot <- renderPlot({
        genderpie()
    })

    # 2. age tree graph
    agepie <- eventReactive(input$show_demographic,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        agepieplot(charactManufac = demographic_ready)
    })

    output$agePiePlot <- renderPlot({
        agepie()
    })

    # 3. gender-age table
    genderagetable <- eventReactive(input$show_demographic, {
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        demographic_Table(charactManufac = demographic_ready,
                          dividedVariable = "age")
    })
    output$genderageTable <- renderTable({
        genderagetable()
    })


    # 4. p-value calculate
    agegender_pvalue <- eventReactive(input$show_demographic,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        demographic_cal(charactManufac = demographic_ready,
                        dividedVariable = "age")

    })
    output$genderage_pvalue <- renderText({
        agegender_pvalue()
    })

    # 5. BMI tree plot
    bmitree <- eventReactive(input$show_demographic,{
        demographic_ready <- charactManufacture(characteristicData = demographicData,
                                                cohortDefinitionIdSet = switchcohort_char())
        bmitreeplot(charactManufac = demographic_ready)
    })

    output$BMITreePlot <- renderPlot({
        bmitree()
    })

    # 6. gender - BMI table
    genderbmitable <- eventReactive(input$show_demographic, {
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        demographic_Table(charactManufac = demographic_ready,
                          dividedVariable = "bmi")
    })
    output$genderbmiTable <- renderTable({
        genderbmitable()
    })

    # 7. p-value calculate
    bmigender_pvalue <- eventReactive(input$show_demographic,{
        demographic_ready<-charactManufacture(characteristicData = demographicData,
                                              cohortDefinitionIdSet = switchcohort_char())
        demographic_cal(charactManufac = demographic_ready,
                        dividedVariable = "bmi")

    })
    output$genderbmip_value <- renderText({
        bmigender_pvalue()
    })


    ####PFT changing analysis result
    #UI create
    output$selectcohort_pft <- renderUI({
        selectInput(inputId = "selectcohort_pft", label = "Select Cohort Set",
                    choices = c("Severe Asthma vs Non-severe Asthma",
                                "Aspirin Exacerbated Repiratory Disease vs Aspirin Tolerant Asthma"),
                    multiple = FALSE)

    })
    #switch choice to cohort Id set
    switchcohort_pft <- reactive({
        switchcohort(input$selectcohort_pft)
    })
    #FEV1plot
    plotFEV1 <- eventReactive(input$show_PFT,{

        FEV1data <- PFTmanufacture(PFTdata = PFTdata,
                                   measurementType = 3011708,
                                   cohortDefinitionIdSet = switchcohort_pft())
        FEV1plot <- plotPFT(PFTmanufactured = FEV1data)
        FEV1plot
    })
    output$FEV1plot <- renderPlot({
        plotFEV1()
    })
    #FEV1FVCplot
    plotFEV1FVC <- eventReactive(input$show_PFT,{

        FEV1FVCdata <- PFTmanufacture(PFTdata = PFTdata,
                                   measurementType = 3011505,
                                   cohortDefinitionIdSet = switchcohort_pft())
        FEV1FVCplot <- plotPFT(PFTmanufactured = FEV1FVCdata)
        FEV1FVCplot
    })
    output$FEV1FVCplot <- renderPlot({
        plotFEV1FVC()
    })

    #FEV1 table
    tablecountFEV1 <- eventReactive(input$show_PFT,{
        FEV1data <- PFTmanufacture(PFTdata = PFTdata,
                                      measurementType = 3011708,
                                      cohortDefinitionIdSet = switchcohort_pft())
        FEV1countTable <- pft_count_table(PFTmanufactured = FEV1data)
        FEV1countTable
    })
    tablepredictFEV1 <- eventReactive(input$show_PFT,{
        FEV1data <- PFTmanufacture(PFTdata = PFTdata,
                                      measurementType = 3011708,
                                      cohortDefinitionIdSet = switchcohort_pft())
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
    tablecountFEV1FVC <- eventReactive(input$show_PFT,{
        FEV1FVCdata <- PFTmanufacture(PFTdata = PFTdata,
                                      measurementType = 3011505,
                                      cohortDefinitionIdSet = switchcohort_pft())
        FEV1FVCcountTable <- pft_count_table(PFTmanufactured = FEV1FVCdata)
        FEV1FVCcountTable
    })

    tablepredictFEV1FVC <- eventReactive(input$show_PFT,{
        FEV1FVCdata <- PFTmanufacture(PFTdata = PFTdata,
                                      measurementType = 3011505,
                                      cohortDefinitionIdSet = switchcohort_pft())
        FEV1FVCpredictTable <- pft_predict_table(PFTmanufactured = FEV1FVCdata)
        FEV1FVCpredictTable
    })

    output$FEV1FVCcounttable <- renderTable({
        tablecountFEV1FVC()
    })

    output$FEV1FVCpredicttable <- renderTable({
        tablepredictFEV1FVC()
    })
}

# Run the application
shinyApp(ui = ui, server = server)

