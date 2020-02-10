rm(list=ls(all.names = TRUE)) # clear all objects includes hidden objects.

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(DT)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Linear Regression"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName="data",icon=icon("upload")),
      menuItem("Table", tabName = "table", icon = icon("th")),
      menuItem("Scatter Plots", tabName = "plots", icon = icon("bar-chart-o")),
      menuItem("Linear Regression Model", tabName = "model", icon = icon("file-alt")),
      menuItem("Predictions",tabName="download",icon=icon("download")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    
    tabItems(
    
    tabItem(tabName="data", 
            fileInput('file1', em("Upload data with at least two columns' numeric class in CSV format."),
                      accept=c('.csv'))),
    
    tabItem(tabName = "table",
            dataTableOutput("table1")),

    tabItem(tabName = "plots",
            sidebarPanel(
              uiOutput("plotInputX"),
              uiOutput("plotInputY"),
              br(),
              uiOutput("helpinfo"),
              uiOutput("plotChoice"),
              br(),
              actionButton("plot_btn","Plot", width = "100%",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
              verticalLayout(
                plotlyOutput("plotTest"),
                br(),
                plotlyOutput("plotParam")
              )
            )),
    
    tabItem(tabName = "model",
            sidebarPanel(
              helpText(em("Choose a response variable"), color = "purple"),
              uiOutput("targetOutput"),
              br(),
              helpText(em("Choose one or more explanatory variables")),
              uiOutput("variableOutput"),
              br(),
              actionButton("linear_model_btn","Liner Regression Model Summary", width = "100%",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              br(),
              br(),
              br(),
              actionButton("linear_model_diagnostic_btn","Model Diagnostic Plots", width = "100%",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            mainPanel(
              verticalLayout(
                box(
                  width = "100%", status = "info", solidHeader = TRUE,
                  title = "Summary of Linear Regression Model",
                  verbatimTextOutput("summary_lm")
                    ),
                box(
                  width = "100%", status = "primary", solidHeader = TRUE,
                  title = "Diagnostic Plots of Linear Regression Model",
                  plotOutput("diagnostic_plots")
                )
              )
              )),
    
    tabItem(tabName = "download",
            sidebarPanel(
              shinyjs::useShinyjs(),
              helpText(em("Predict results after linear regression model was built")),
              actionButton("pred_resultbtn","Compute Predictions", width = "100%",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              br(),
              br(),
              br(),
              helpText(em("Download prediction result, response and explanatory variable vlues.")),
              actionButton("init", "Download Predictions", icon = icon("download"), width = "100%",
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              downloadButton("pred_download", "Download Predictions", style = "visibility: hidden;")),
            
            mainPanel(
              dataTableOutput("pred_table"))
              ),
    
    tabItem(tabName = "dashboard",
            fluidRow(
              box(
                status = "info", solidHeader = TRUE,
                title = "Summary of Linear Regression Model",
                verbatimTextOutput("summary_lm_dashboard")),
              box(
                status = "primary", solidHeader = TRUE,
                title = "Scatter Plot",
                plotlyOutput("plotTest_dashboard"))
            ))
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
  #Display upload Data
  output$table1 <- renderDataTable({
    if(is.null(rawdataInput())) 
      {return ()}
    rawdataInput()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lftip',
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ))
  
  ################## Plots ######################
  #  UI for input X
  output$plotInputX <- renderUI({
    selectInput("plotInputVarX", "X axis parameter", colnameNumeric())
  })
  
  # Input X
  pltInputvarX <- reactive({
    if(is.null(input$plotInputVarX)) return(NULL)
    input$plotInputVarX
  })
  
  # variables name exclude INPUT X
  variableInputVarY <- reactive({
    if(is.null(pltInputvarX())) return(NULL)
    colnameNumeric()[colnameNumeric()!= pltInputvarX()]
  })
  
  # UI for input Y
  output$plotInputY <- renderUI({
    if(is.null(variableInputVarY())) return(NULL)
    selectInput("plotInputVarY", "Y axis parameter", variableInputVarY())
  })
  
  # Input Y aixs parameter
  pltInputvarY <- reactive({
    if(is.null(variableInputVarY())) return(NULL)
    input$plotInputVarY
  })
  
  # UI for helpinfo text
  output$helpinfo <- renderUI({
    if(is.null(colnameFactor())) return(NULL)
    helpText(em("Choose a Color/Fill parameter for color scatter plot, otherwise choose the blank option for black scatter plot."))
  })
  
  # UI for input Choice
  output$plotChoice <- renderUI({
    if(is.null(colnameFactor())) return(NULL)
    
    selectInput("plotChoiceVar", "Color & Fill Parameter", choices = c(" ", colnameFactor()))
  })
  
  # Input Choice
  pltInputChoice <- reactive({
    if(is.null(colnameFactor()) | identical(input$plotChoiceVar, " ")) return(NULL)
    input$plotChoiceVar
  })
  
  # Input Choice Parameters
  inputChoiceParamData <- reactive({
    if(is.null(pltInputChoice())) return(NULL)
    rawdataInput()[, pltInputChoice()]
  })

  # The legend/categorical of input choice parameter
  legendName <- reactive({
    if(is.null(pltInputvarX()) | 
       is.null(pltInputvarY()) | is.null(pltInputChoice())) 
       return(NULL)
    sort(unique(inputChoiceParamData()))
  })
  
  # legend selected when click any point in the plot
  legendselected <- reactive({
    # if the color/fill parameter is null, retrun NULL
    if(identical(input$plotChoiceVar, " ")) 
      return(NULL)
    
      s <- event_data("plotly_click")
      if (length(s) == 0) {
        return(NULL)
      } else {
        paste(legendName()[s[["curveNumber"]]+1])
      }
    
  })
  
  plotResult <- eventReactive(input$plot_btn, {
    if(is.null(rawdataInput())|is.null(pltInputvarX()))
      {return (NULL)}
    
    else if(identical(input$plotChoiceVar, " "))
      p <- ggplot(data = rawdataInput(),
                  aes_string(
                    x = pltInputvarX(),
                    y = pltInputvarY()
                  )) + geom_point()
    else
      p <- ggplot(data = rawdataInput(),
                  aes_string(
                    x = pltInputvarX(),
                    y = pltInputvarY(), 
                    color = pltInputChoice(),
                    fill = pltInputChoice()
                  )) + geom_point()
  })
  
  output$plotTest <- renderPlotly({
    if(is.null(plotResult()))
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't selected parameter, please upload your data in the tab of Upload Data!"
        ))
      return (NULL)
      }
    
    ggplotly(plotResult())
  })
  
  output$plotTest_dashboard <- renderPlotly({
    if(is.null(plotResult()))
    {
      return (NULL)
    }
    
    ggplotly(plotResult())
  })
  
  # filtered data
  filtered <- reactive({
    if(is.null(legendselected()) | is.null(pltInputChoice())){
      return(NULL)
    }

    rawdataInput() %>% filter(!!as.name(pltInputChoice()) == legendselected()) 
  })
  
  # plotResultParam <- eventReactive(input$plot_btn_param, {
  plotResultParam <- reactive({
    if(is.null(filtered())|is.null(legendselected()))
      {return (NULL)}
    
    p <- ggplot(data = filtered(),
                aes_string(
                  x = pltInputvarX(),
                  y = pltInputvarY()
                )) + geom_point() +
      labs(title = paste("Selected parameter is:", legendselected()))
  })
  
  output$plotParam <- renderPlotly({
    if(is.null(plotResultParam()))
    {
      return (NULL)
    }
    ggplotly(plotResultParam())
  })
  

  ################## Model ######################
  # all variables name of the input file
  varNames <- reactive({
    names(rawdataInput())
  })
  
  # find the class type of all variables of the input data
  varClass <- reactive({
    sapply(rawdataInput(),class)
  })
  
  # find the index of the factor from all class types
  colnameIndex <- reactive({
    grep("factor", varClass())
  })
  
  # find variable names with numeric/integer class
  colnameNumeric <- reactive({
    varNames()[-colnameIndex()]
  })
  
  # find variable names with factor class
  colnameFactor <- reactive({
    varNames()[colnameIndex()]
  })
  
  # target variable, numeric/integer class
  output$targetOutput <- renderUI({
    selectInput("targetInput", "Dependent Variable", colnameNumeric())
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
    selectInput("variableInput", "Independent Variable",
                variableInputVar(), multiple = TRUE)
  })

  # select Variables for linear regression model
  selected_all <- reactive({
    input$variableInput
  })
  
  # Dependent Variable data
  targetInputData <- reactive({
    rawdataInput()[,input$targetInput]
  })
  
  # selected variables data
  variableNewData <- reactive({
    rawdataInput()[,selected_all()]
  })
  
  #combine dependent variable data and selected variables data
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
  
  # Built Liner Regression Model
  linearRegressionModelText <- eventReactive(input$linear_model_btn, {
    if(is.null(rawdataInput())|is.null(variableNewData()))
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't upload data, please upload your data in the tab of Upload Data!"
        ))
      return (NULL)
    }
    else if(length(input$variableInput) == 0)
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't choose explanatory variable, please choose one or more!"
        ))
      return (NULL)
    }
    else
    {
      return(summary(newLinearMod()))
    }
  })
  
  ################## Summary ######################
  # Summary of the linear regression model
  output$summary_lm <- renderPrint({
    if(is.null(rawdataInput())) 
      {return ("You haven't upload data, please upload your data in the tab of Upload Data!")}
    else if(length(input$variableInput) == 0) 
    {
      return ("You haven't choose explanatory variable, please choose one or more!")}
    else if(is.null(linearRegressionModelText())) 
      {return ("You haven't built Linear Regression Model!")}
    
    linearRegressionModelText()
  })
  
  # Summary of the linear regression model for Dashboard
  output$summary_lm_dashboard <- renderPrint({
    if(is.null(rawdataInput())) 
    {
      return ("You haven't upload data, please upload your data in the tab of Upload Data!")}
    else if(length(input$variableInput) == 0) 
    {
      
      return ("You haven't choose explanatory variable, please choose one or more!")
      }
    else if(is.null(linearRegressionModelText())) 
      {return ("You haven't built Linear Regression Model!")}
    
    linearRegressionModelText()
  })
  
  # Built Liner Regression Model & Diagnostic
  diagnosticModel <- eventReactive(input$linear_model_diagnostic_btn, {
    if(is.null(rawdataInput())|is.null(variableNewData()))
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't upload data, please upload your data in the tab of Upload Data!"
        ))
      return (NULL)
    }
    else if(length(input$variableInput) == 0)
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't choose explanatory variable, please choose one or more!"
        ))
      return (NULL)
    }
    else
    {
      dataTable <- cbind(Target_Predict = resultPred(), 
                         Target_Input = targetInputData(), 
                         variableNewData())
      return(dataTable)
    }
  })
  
  # R diagnostic plots include:
    # Residuals vs. fitted values; Q-Q plots; Scale Location plots; Cook's distance plots
  output$diagnostic_plots <- renderPlot({
    
    if(is.null(diagnosticModel()) | length(input$variableInput) == 0)
    {
      return (NULL)
    }
    else {
      par(mfrow = c(2,2))
      plot(newLinearMod())
    }
    
  })
  
  # diagnostic plots in tab of summary
  output$diagnostic_plots_summary <- renderPlot({
    if(is.null(diagnosticModel()) | length(input$variableInput) == 0)
    {
      return (NULL)
    }
    else {
      par(mfrow = c(2,2))
      plot(newLinearMod())
    }
  })
  
  # predicted final result table
  dataResult <- eventReactive(input$pred_resultbtn, {
    if(is.null(rawdataInput())|is.null(variableNewData()))
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't upload data, please upload your data in the tab of Upload Data!"
      ))
      return (NULL)
    }
    else if(length(input$variableInput) == 0)
    {
      showModal(
        modalDialog(
          title = "Warning message",
          "You haven't choose explanatory variable, please choose one or more in tab of Linear Regression Model!"
        ))
      return (NULL)
    }
    else
    {
      dataTable <- cbind(Target_Predict = resultPred(), 
                         Target_Input = targetInputData(), 
                         variableNewData())
      
      colnames(dataTable)[1] <- paste0(targetinputvar(), "_predict")
      colnames(dataTable)[2] <- paste(targetinputvar())
      
      if (length(input$variableInput) == 1)
        colnames(dataTable)[3] <- paste(input$variableInput)
      
      return(dataTable)
    }
  })
  
  output$pred_table <- renderDataTable({
    dataResult()},
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'lftip',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
  ))
  
  ################## Download Predictions ######################
  # Download predicted data
  observeEvent(input$init, {
    if (is.null(dataResult())) {
      showModal(
        modalDialog(
          title = 'Error',
          p("You haven't prediction data, please compute your priedictions first!")
        )
      )
    } else {
      shinyjs::runjs("document.getElementById('pred_download').click();")
    }
  })
  
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
