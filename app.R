library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(aws.s3)
library(plotly)
library(shinyWidgets)
library(shinyalert)
library(binance)
library(purrr)
library(shinymanager)
library(flexdashboard)
library(stringr)
library(shinyjs)
library(riingo)

source("FXFuncs.R")

file.names = list.files('TiingoData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")

checkbox_list = setNames(file.names,file.names)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Forex",
    mainText = 'Predictor',
    badgeText = "v1.5"
  ),
  titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Overview/Backtesting", tabName = "create", icon = icon("house")),
      menuItem(text = "Predict Next Candle", tabName = "predictNext", icon = icon("chart-line"))
      
      
      # menuItem("Most Likely Outcome", tabName = "likely")
      
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyDashboardThemes(
      theme = "flat_red"
    ),
    tabItems(
      tabItem(tabName = "create",
              fluidRow(
                setBackgroundImage(
                  # color = "black",
                  src = "cropped-red.jpg",
                  shinydashboard = TRUE
                ),
                box(title = "Backtesting Inputs", solidHeader = TRUE, status = "danger",
                    selectInput("pair","Select a Currency Pair and Timeframe", choices = checkbox_list),
                    selectInput("predict","Select What to Predict", choices = list("Break High" = "BreakH",
                                                                                   "Break Low" = "BreakL",
                                                                                   "Percentage Increase" = "PercentageIncrease")),
                    sliderInput("percentIncrease","Select a Target Percent Increase",min = 0.05, max = 1, step = 0.05, value = 0.15)
                ),
                box(title = "Metrics", solidHeader = TRUE, status = "danger",
                    infoBoxOutput("overallaccuracy", width = 6),
                    infoBoxOutput("yesaccuracy", width = 6),
                    infoBoxOutput("totalnumber", width = 6),
                    infoBoxOutput("totalhits", width = 6)

                ),
                box(title = "Histogram of Predictions", solidHeader = TRUE, status = "danger",
                  plotOutput("histogram")
                )
              )
              
      ),
      tabItem(tabName = "predictNext",
              fluidRow(
                box(title = "Prediction Inputs", solidHeader = TRUE, status = "danger",
                    selectInput("predictionPair","Select a Currency Pair and Timeframe", choices = checkbox_list),
                    selectInput("predictionType","Select What to Predict", choices = list("Break High" = "BreakH",
                                                                                   "Break Low" = "BreakL",
                                                                                   "Percentage Increase" = "PercentageIncrease")),
                    sliderInput("predictionIncrease","Select a Target Percent Increase",min = 0.05, max = 1, step = 0.05, value = 0.15)
                    
                ),
                box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger",
                    plotlyOutput('candlestickPlot')
                ),
                box(title = "Predict Current Candle", solidHeader = TRUE, status = "danger",
                  actionBttn("predictConfidence",
                             label = "Predict",
                             style = "jelly",
                             color = "danger",
                             block = TRUE),
                  br(),
                  br(),
                  infoBoxOutput("prediction", width = 12),
                  
                )
                
              )
      )
    )
    
  )
)


# Define server logic
server <- function(input, output, session) {
  disable("percentIncrease")
  disable("predictionIncrease")
  
  observeEvent(input$pair, {
    if(input$predict == "BreakH" | input$predict == "BreakL"){
      GetAccuracy(input$pair, input$predict, input$percentIncrease)
      RenderInfoBoxes(output)
      output$histogram = renderPlot(hist(compare$pred))
    }else{
      GetAccuracy(input$pair, input$predict, input$percentIncrease)
      RenderInfoBoxes(output)
      
      
    }
    output$histogram = renderPlot(CreateHistogram())
    
    
  })
  
  observeEvent(input$predict, {
    if(input$predict == "BreakH" | input$predict == "BreakL"){
      disable("percentIncrease")
      
      GetAccuracy(input$pair, input$predict, input$percentIncrease)
      RenderInfoBoxes(output)
      
    }else{
      enable("percentIncrease")
      GetAccuracy(input$pair, input$predict, input$percentIncrease)
      RenderInfoBoxes(output)
      
    }
    output$histogram = renderPlot(CreateHistogram())
    
    
  })
  
  observeEvent(input$percentIncrease, {
    GetAccuracy(input$pair, input$predict, input$percentIncrease)
    RenderInfoBoxes(output)
    output$histogram = renderPlot(CreateHistogram())
    
    
  })
  
  # observeEvent(input$predictionPair, {
  #   output$candlestickPlot = renderPlotly(LivePlot(input$predictionPair))
  # })
  # 
  # observeEvent(input$predictionType, {
  #   if(input$predictionType == "BreakH" | input$predictionType == "BreakL"){
  #     disable("predictionIncrease")
  #   }else{
  #     enable("predictionIncrease")
  #   }
  # })
  # 
  # observeEvent(input$predictConfidence, {
  #   if(input$predictionType == "BreakH" | input$predictionType == "BreakL"){
  #     predict.next(input$predictionPair, input$predictionType, output)
  #   }else{
  #     predict.next(input$predictionPair, input$predictionType, output, input$predictionIncrease)
  #   }
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
