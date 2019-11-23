rm(list=ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)

# reference: https://www.r-bloggers.com/curly-curly-the-successor-of-bang-bang/
filter_data <- function(dataset, column_name, filter_var){
  dataset %>%
    filter({{column_name}} == {{filter_var}}) -> dataset
  return(dataset)
}

ui <- dashboardPage(
  dashboardHeader(title = "Movie Rating"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Upload Data", tabName="data",icon=icon("upload")),
      menuItem("Table", tabName = "table", icon = icon("th")),
      menuItem("Plots", tabName = "plots", icon = icon("bar-chart-o")),
      menuItem("Model", tabName = "model", icon = icon("file-alt")),
      menuItem("Summary", tabName = "summary", icon = icon("list-alt")),
      menuItem("Download Predictions",tabName="download",icon=icon("download"))
    )
  ),
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            "Dashboard Under Construction"),
    
    tabItem(tabName="data", 
            fileInput('file1', em('Upload test data in csv format'),
                      accept=c('.csv'))),
    
    tabItem(tabName = "table",
            dataTableOutput("table1")),
    
    tabItem(tabName = "summary",
            verbatimTextOutput("summary_lm")),
    
    tabItem(tabName = "plots",
            sidebarPanel(
              uiOutput("plotInputX"),
              uiOutput("plotInputY"),
              uiOutput("plotChoice"),
              actionButton("plot_btn","Plot", width = "100%"),
              hr(),
              uiOutput("plotChoiceParam"),
              actionButton("plot_btn_param","Plot for Selected Parameter", width = "100%")
            ),
            mainPanel(
            plotlyOutput("plotTest"),
            br(),
            plotlyOutput("plotParam")
            )),
    
    tabItem(tabName = "model",
            sidebarPanel(
              uiOutput("targetOutput"),
              uiOutput("variableOutput"),
              actionButton("pred_resultbtn","Predict Movie Rating", width = "100%")
            ),
            mainPanel(
              dataTableOutput("pred_table"))),
    
    tabItem(tabName = "download",
            downloadButton("pred_download", "Download Predictions"))
  )
 )
)

server <- function(input, output, session) {
  
  ################## Upload Data ######################
  # Read Data file
  rawdataInput <- reactive({
    rawdatainFile <- input$file1
    
    if(is.null(rawdatainFile))
      return(NULL)
    read.csv(rawdatainFile$datapath)
  })
  
  ################## Table ######################
  #Display movie Data
  output$table1 <- renderDataTable({
    if(is.null(rawdataInput())) 
      {return ()}
    rawdataInput()
  })
  
  ################## Plots ######################
  #  UI for input X
  output$plotInputX <- renderUI({
    selectInput("plotInputVarX", "Input X", varNames())
  })
  
  # Input X
  pltInputvarX <- reactive({
    input$plotInputVarX
  })
  
  # variables name exclude INPUT X
  variableInputVarY <- reactive({
    varNames()[varNames()!= pltInputvarX()]
  })
  
  # UI for input Y
  output$plotInputY <- renderUI({
    selectInput("plotInputVarY", "Input Y", variableInputVarY())
  })
  
  # Input Y
  pltInputvarY <- reactive({
    input$plotInputVarY
  })
  
  # variables name exclude INPUT X & Y
  variableInputChoice <- reactive({
    variableInputVarY()[variableInputVarY()!= pltInputvarY()]
  })
  
  # UI for input Choice
  output$plotChoice <- renderUI({
    if(is.null(variableInputChoice())) return(NULL)
    selectInput("plotChoiceVar", "Parameter Control", variableInputChoice())
  })
  
  # Input Choice
  pltInputChoice <- reactive({
    if(is.null(variableInputChoice())) return(NULL)
    input$plotChoiceVar
  })
  
  # Input Choice Parameters
  inputChoiceParamData <- reactive({
    if(is.null(pltInputChoice())) return(NULL)
    rawdataInput()[, pltInputChoice()]
  })

  # UI for input choose a parameter
  output$plotChoiceParam <- renderUI({
    if(is.null(pltInputvarX()) | 
       is.null(pltInputvarY()) | is.null(pltInputChoice())) 
      return(NULL)
    selectInput("plotChoiceVarParam", "Choose a Parameter",
                sort(unique(inputChoiceParamData())))
  })

  # Input Choice
  pltInputChoiceParam <- reactive({
    input$plotChoiceVarParam
  })
  
  
  plotResult <- eventReactive(input$plot_btn, {
    if(is.null(rawdataInput())|is.null(pltInputvarX()))
      {return (NULL)}
    
    p <- ggplot(data = rawdataInput(),
                aes_string(
                  x = pltInputvarX(),
                  y = pltInputvarY(), 
                  color = pltInputChoice(),
                  fill = pltInputChoice()
                )) + geom_point()
  })
  
  output$plotTest <- renderPlotly({
    ggplotly(plotResult())
  })
  
  # filtered data
  filtered <- reactive({
    if(is.null(pltInputChoiceParam())){
      return(NULL)
    }
    
    # how to remove quotes for pltInputChoice() ???
    # 
     # rawdataInput() %>% filter(genre == pltInputChoiceParam()) # work, 
     # pic_p <- pltInputChoiceParam()
     # pic_c <- pltInputChoice()
     # print(paste0("PIC_P: ", pic_p))
     # print(paste0("PIC_C: ", pic_c))
     
      # use the filter_data function defined in the beginning          
     filter_data(rawdataInput(), pltInputChoice(), pltInputChoiceParam())
     
     # if we choose "genre" for Parameter Control in the selectInput, pltInputChoice() is 'genre', how to remove he quotes?
     
    # rawdataInput() %>% filter(as.name(pltInputChoice()) == pltInputChoiceParam()) # not work
    # rawdataInput() %>% filter(as.symbol(pltInputChoice()) == pltInputChoiceParam()) # not work
    # rawdataInput() %>% filter(stringr::str_detect(pltInputChoice(), as.character(pltInputChoiceParam())))# not work
    
  })
  
  
  plotResultParam <- eventReactive(input$plot_btn_param, {
    if(is.null(filtered())|is.null(pltInputChoiceParam()))
      {return (NULL)}
    
    p <- ggplot(data = filtered(),
                aes_string(
                  x = pltInputvarX(),
                  y = pltInputvarY()
                )) + geom_point()
  })
  
  output$plotParam <- renderPlotly({
    if(is.null(plotResultParam()))
      {return (NULL)}
    ggplotly(plotResultParam())
  })
  
  
  #plot
  # output$plotParam <- renderPlotly({
  #   if(is.null(rawdataInput())) 
  #     {return ()}
  #   
  #   p <- ggplot(data = rawdataInput(), 
  #               aes(x = imdb_rating, y = critics_score)) +
  #     geom_point(aes(x =imdb_rating, y = audience_score)) +
  #     labs(x = "Critic Score",
  #          y = "Audience Score",
  #          title = "Movies rating based on critic and audience scores",
  #          subtitle = "The divides may exist between audience and critic scores exists on movies",
  #          caption = 'Movies.csv')
  #   
  #   ggplotly(p)
  # })


  ################## Model ######################
  # all variables name of the input file
  varNames <- reactive({
    names(rawdataInput())
  })
  
  # target variable
  output$targetOutput <- renderUI({
    selectInput("targetInput", "Target Input", varNames())
  })
  
  # target variable ID
  targetinputvar <- reactive({
    input$targetInput
  })
  
  # variables name exclude target variable name
  variableInputVar <- reactive({
    varNames()[varNames()!= targetinputvar()]
  })
  
  # multiple select input Variables
  output$variableOutput <- renderUI({
    selectInput("variableInput", "Variable Input",
                variableInputVar(), multiple = TRUE)
  })

  # select Variables for linear regression model
  selected_all <- reactive({
    input$variableInput
  })
  
  # target input data
  targetInputData <- reactive({
    rawdataInput()[,input$targetInput]
  })
  
  # selected variables data
  variableNewData <- reactive({
    rawdataInput()[,selected_all()]
  })
  
  #combine target input data and selected variables data
  selectedData <- reactive({
    cbind(targetInputData(),variableNewData(),deparse.level = 2)
  })

  # output$selectedInputVariables <- renderDataTable({
  #   if(length(input$variableInput) == 0) return()
  #   head(selectedData())
  # })
  
  # select variables plus expression
  linearVariables <- reactive({
    paste(input$variableInput, collapse = " + ")
  })
  
  # output$selectedVariables <- renderText({
  #   linearVariables()
  # })
  
  # building the linear regression model
  newLinearMod <- reactive({
    if(is.null(rawdataInput()) | length(input$variableInput) == 0)
    {return ()}
    lm(formula = as.formula(paste(input$targetInput, "~", linearVariables())),
       data = na.omit(rawdataInput()))
  })
  
  # predict final result
  resultPred <- reactive({
    round(predict(newLinearMod()),2)
  })
  
  # predicted final result table
  dataResult <- eventReactive(input$pred_resultbtn, {
    if(is.null(rawdataInput()))
    {
      showModal(modalDialog(
        title = "Warning message",
        "You haven't upload data, please upload your data in the tab of Upload Data!",
        easyClose = TRUE,
        footer = NULL
      ))
      return ()
    }
    else
    {
      dataTable <- cbind(Target_Predict = resultPred(), 
                         Target_Input = targetInputData(), 
                         variableNewData())
      return(dataTable)
    }
  })
  
  output$pred_table <- renderDataTable(dataResult())
  
  ################## Summary ######################
  # Summary of the linear regression model
  output$summary_lm <- renderPrint({
    if(is.null(rawdataInput()) | length(input$variableInput) == 0) 
    {return ("You haven't built Linear Regression Model!")}
    summary(newLinearMod())
  })
  
  ################## Download Predictions ######################
  # Download predicted data
  output$pred_download <- downloadHandler(
    
    filename = function() {
      paste("predict_movie_rating", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(dataResult(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
