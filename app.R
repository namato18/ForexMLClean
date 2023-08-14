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
library(stringr)
library(shinyjs)
library(riingo)
library(CandleStickPattern)
library(xts)
library(shinybusy)
library(quantmod)
library(shinyjs)

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
      menuItem(text = "Predict (Simple)", tabName = "predictSimple", icon = icon("chart-line")),
      menuItem(text = "Predict (Detailed)", tabName = "predictDetail", icon = icon("chart-line"))
      
      
      
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
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                setBackgroundImage(
                  # color = "black",
                  src = "cropped-red.jpg",
                  shinydashboard = TRUE
                ),
                box(title = "Backtesting Inputs", solidHeader = TRUE, status = "danger",
                    selectInput("pair","Select a Currency Pair and Timeframe", choices = checkbox_list),
                    selectInput("predict","Select What to Predict", choices = list("Break High" = "BreakH",
                                                                                   "Break Low" = "BreakL",
                                                                                   "High" = "High",
                                                                                   "Low" = "Low",
                                                                                   "Close" = "Close",
                                                                                   "Percentage Increase" = "PercentageIncrease")),
                    sliderInput("percentIncrease","Select a Target Percent Increase",min = 0.05, max = 1, step = 0.05, value = 0.15),
                    actionButton("predictButtonBacktest","Backtest", icon = icon('chart-simple'), class = "btn-primary", style='padding:4px; width:100%')
                    
                ),
                box(title = "Metrics", solidHeader = TRUE, status = "danger",
                    valueBoxOutput("precision", width = 12),
                    valueBoxOutput("recall", width = 12),
                    valueBoxOutput("f1", width = 12),
                    
                    valueBoxOutput("rmse", width = 12),
                    valueBoxOutput("current.price", width = 12)

                ),
                box(title = "Histogram of Predictions", solidHeader = TRUE, status = "danger",
                  plotOutput("histogram")
                )
              )
              
      ),
      tabItem(tabName = "predictSimple",
              fluidRow(
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                column(width = 6,
      
                box(title = "Prediction Inputs", solidHeader = TRUE, status = "danger", width = NULL,
                    selectInput("predictionPairSimple","Select a Currency Pair and Timeframe", choices = checkbox_list),
                    actionBttn("predictConfidenceSimple",
                               label = "Predict",
                               style = "jelly",
                               color = "danger",
                               block = TRUE)
                ),

                box(title = "Predict Current Candle", solidHeader = TRUE, status = "danger",width = NULL,
                  infoBoxOutput("prediction", width = 12),
                  
                )
                ),
                box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger",
                    plotlyOutput('candlestickPlotSimple')
                )
              )
      ),
      tabItem(tabName = "predictDetail",
              fluidRow(
                add_busy_spinner(spin = "circle", color = "white", height = "100px", width="100px", position = "top-right"),
                column(width = 6,
                box(title = "Prediction Inputs", solidHeader = TRUE, status = "danger", width = NULL,
                    selectInput("predictionPairDetail","Select a Currency Pair and Timeframe", choices = checkbox_list),
                    actionBttn("predictConfidenceDetail",
                               label = "Predict",
                               style = "jelly",
                               color = "danger",
                               block = TRUE)
                ),

                box(title = "Predict High, Low, Close", solidHeader = TRUE, status = "danger",width = NULL,
                    valueBoxOutput("predictedHigh", width = 12),
                    valueBoxOutput("predictedLow", width = 12),
                    valueBoxOutput("predictedClose", width = 12),
                ),
                box(title = "Predict Break High/Low", solidHeader = TRUE, status = "danger",width = NULL,
                    valueBoxOutput("predictedBreakHigh", width = 12),
                    valueBoxOutput("predictedBreakLow", width = 12),
                ),
                box(title = "Predict Break 0.25%", solidHeader = TRUE, status = "danger",width = NULL,
                    valueBoxOutput("predictedBreak25", width = 12),
                )
                ),
                box(title = "Live Candle Chart", solidHeader = TRUE, status = "danger",
                    plotlyOutput('candlestickPlotDetail')
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
  
  output$candlestickPlotSimple = renderPlotly(LivePlot(input$predictionPairSimple))
  
  
  
  
  observeEvent(input$predict, {
    if(input$predict == "BreakH" | input$predict == "BreakL" | input$predict == "High" | input$predict == "Low" | input$predict == "Close"){
      disable("percentIncrease")
      
      # GetAccuracy(input$pair, input$predict, input$percentIncrease)
      # RenderInfoBoxes(output)
      
    }else{
      enable("percentIncrease")
      # GetAccuracy(input$pair, input$predict, input$percentIncrease)
      # RenderInfoBoxes(output)
      
    }
    output$histogram = renderPlot(CreateHistogram(input$pair, input$predict, input$targetPercentage))
    
    
  })
  

  observeEvent(input$predictionType, {
    if(input$predictionType == "BreakH" | input$predictionType == "BreakL"){
      disable("predictionIncrease")
    }else{
      enable("predictionIncrease")
    }
  })

  observeEvent(input$predictConfidenceDetail, {
    predict.next(input$predictionPairDetail, output)
    predict.next.ohlc(input$predictionPairDetail, output)
    
    output$predictedHigh = renderValueBox({
      valueBox(subtitle = "Predicted High",value = text.high, icon = icon("arrow-trend-up"), color = "green")
    })
    output$predictedLow = renderValueBox({
      valueBox(subtitle = "Predicted Low",value = text.low, icon = icon("arrow-trend-up"), color = "red")
    })
    output$predictedClose = renderValueBox({
      valueBox(subtitle = "Predicted Close",value = text.close, icon = icon("arrow-trend-up"), color = "orange")
    })
    
    output$predictedBreakHigh = renderValueBox({
      valueBox(subtitle = "Confidence to Break Previous High",value = text.bh, icon = icon("arrow-trend-up"), color = "green")
    })
    output$predictedBreakLow = renderValueBox({
      valueBox(subtitle = "Confidence to Break Previous Low",value = text.bl, icon = icon("arrow-trend-up"), color = "red")
    })
    
    output$predictedBreak25 = renderValueBox({
      valueBox(subtitle = "Confidence to Break 1% Increase",value = text.perc25, icon = icon("arrow-trend-up"), color = "green")
    })
    
    shinyalert("Success",
               "Predictions Successfully Generated!",
               type = 'success')
  })
  
  observeEvent(input$predictButtonBacktest, {
    GetAccuracy(input$pair, input$predict, input$percentIncrease)
    if(input$predict == "High" |input$predict == "Low" |input$predict == "Close"){
      hide("precision")
      hide("recall")
      hide("f1")
      shinyjs::show("rmse")
      shinyjs::show("current.price")
      hide("histogram")
    }else{
      shinyjs::show("precision")
      shinyjs::show("recall")
      shinyjs::show("f1")
      shinyjs::show("histogram")
      hide("rmse")
      hide("current.price")
    }
    
    output$precision = renderValueBox({
      valueBox(subtitle = "Precision",value = precision, icon = icon("check"), color = "green")
    })
    output$recall = renderValueBox({
      valueBox(subtitle = "recall",value = recall, icon = icon("check"), color = "green")
    })
    output$f1 = renderValueBox({
      valueBox(subtitle = "F1 Score",value = f1, icon = icon("check"), color = "green")
    })
    output$rmse = renderValueBox({
      valueBox(subtitle = "RMSE",value = rmse, icon = icon("check"), color = "green")
    })
    output$current.price = renderValueBox({
      valueBox(subtitle = "Current Price",value = current.price, icon = icon("check"), color = "green")
    })
    
    # output$histogram = renderPlot(CreateHistogram())
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
