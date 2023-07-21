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
# MINE
# secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
# api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"

credentials <- data.frame(
  user = c('gentlemam1','gentlemam2','gentlemam3','nick',"shiny", "shinymanager"),
  password = c("gentlemam1234","gentlemam1234","gentlemam1234","123","azerty", "12345"),
  stringsAsFactors = FALSE
)

#Gentlemam
# secret = "9qhPtPDePdBJnWL5zThAxqrUWXNcv37NYbyDHdkDctoJZGa0CZS6IyPqmqOdIh3i"
# api_key = "wZpij1rDxXsrnyRyuNmuaoLPsVSgJKvmmgt0rzi44GZB03za9GBFqeB6chXi1p0T"

# binance::authenticate(key = api_key,secret = secret)
# 
# binance::base_url("https://api.binance.us")

str1 = readRDS('tickers/str1.rds')
str2 = readRDS('tickers/str2.rds')

str1 = str1[-61]
str2 = str2[-61]
coin_decimals = readRDS('coin_decimals.rds')

checkbox_list = setNames(str1, str1)

possibly_spot_new_order = possibly(spot_new_order, otherwise = 'ERROR')
possibly_s3read_using = possibly(s3read_using, otherwise = 'ERROR')


# Define UI
ui <- secure_app(dashboardPage(
  dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "Crypto Currency",
    mainText = 'Predictor',
    badgeText = "v1.5"
  ),
  titleWidth = 300
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Overview/Backtesting", tabName = "create", icon = icon("house")),
      menuItem("Predict Next Candle (Multiple)", tabName = 'predictMultiple', icon = icon('money-bill-trend-up')),
      menuItem("Predict Next 7 Days/Weeks", tabName = 'predictNextWeek', icon = icon('chart-line')),
      menuItem("Build TradingView Model", tabName = 'inputCoin', icon = icon('upload')),
      menuItem("Binance", tabName = "binance", icon = icon('sack-dollar')),
      menuItem("Binance Automation", tabName = "automation", icon = icon('robot'))
      

      # menuItem("Most Likely Outcome", tabName = "likely")
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    tabItems(
      tabItem(tabName = "create",
              fluidPage(
                verbatimTextOutput("auth_output"),
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                # HTML('<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_blank">
                #        <input type="hidden" name="cmd" value="_s-xclick">
                #        <input type="hidden" name="hosted_button_id" value="2MGB68YUJEB5Q">
                #        <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_subscribeCC_LG.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!" target="_blank">
                #        <img alt="" border="0" src="https://www.paypalobjects.com/en_US/i/scr/pixel.gif" width="1" height="1" target="_blank">
                #        </form>'),
                
                strong(h1("Creating a Model:")),
                box(width = 10,
                paste0("On this tab you can use the sliders to modify how the predictive model is created. First you need to select a timeframe ",
                       "and coin that you're interested in predicting. ","The first slider is used ",
                       "to select the percentage increase in the timeframe that you've selected. The second slider is used ",
                       "to select how confident you want the model to be in order to classify a 'BUY'. The model will make a prediction on a scale from ",
                       "0-1, the closer to 1 the prediction is, the more confident the model is that your selected percentage increase will happen in your selected timeframe."),
                ),
                br(),

      
                column(width = 6,
                       box(title = "Inputs", solidHeader = TRUE, status = "primary", width = NULL,
                         selectInput("timeframe","Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                    "1 Hour" = "1hour",
                                                                                    "4 Hour" = "4hour",
                                                                                    "8 Hour" = "8hour",
                                                                                    "1 Day" = "1day",
                                                                                    "1 Week" = "7day",
                                                                                    "1 Month" = '1month')),
                         selectInput("select","Pick a crypto to predict", choices = checkbox_list),
                         br(),
                         sliderInput("slider1","Select Percentage Increase", min = 0.1, max = 1, step = 0.1, value = 0.1),
                         sliderInput("slider2", "Confidence Score 'BUY' Threshold", min = 0.1, max = 1, step = 0.02, value = 0.9),
                         # strong("Note: IT IS STRONGLY RECOMMENDED TO PLACE YOUR TAKE PROFIT TO THE SAME VALUE AS YOUR TARGET PERCENTAGE INCREASE"),
                         # br(),
                         # paste0("Metrics by default are calculated based on the candles closing value. ",
                         #        "You can use the TP (take profit) input field to specify your TP. ",
                         #        "Setting a TP can limit your gains, but can also limit your losses! ",
                         #        "Leaving the TP value at 0 will set the metrics to be calculated based on candles closing value."),
                         # numericInput('tp',"Set TP % (must be positive)", value = 0, min = 0),
                         # numericInput("sl","Set SL (must be negative)", value = 0, max = 0),
                         actionButton('action1', label = "Generate"),
                         br(),
                         br(),
                       ),
                       box(title = "Metrics", width = NULL, status = "primary", solidHeader = TRUE,
                         # infoBoxOutput("OverallAccuracy", width = 6),
                         infoBoxOutput("Buy", width = 6),
                         infoBoxOutput("SumPercentage", width = 6),
                         # infoBoxOutput("DontBuy", width = 6),
                         infoBoxOutput("Predictions", width = 6),
                         infoBoxOutput("Hits", width = 6)
                         
                       ),
                       box(title = "Histogram", width = NULL, status = "primary", solidHeader = TRUE,
                         paste0("Ideally, we'd like there to be a near 0 probability or a near 1 probability for all predictions. ",
                                "Values that are more in the middle can give us an unclear prediction."),
                         plotOutput("modelPlot")
                       )
                ),
                column(width = 6,
                box(width = NULL, title = "Backtest", status = "primary", solidHeader = TRUE,
                  strong(h4("Variable Info:")),
                  strong('Actual:'),
                  paste0("If the next candle actually hit the target percentage increase, this will be 'HIT TARGET', otherwise 'MISSED TARGET'. ",
                         "The color will be GREEN if a profit could have been made and RED if a loss could have been made."),
                  br(),
                  strong("Actual High:"),
                  paste0("This was the next candles high"),
                  br(),
                  strong("Actual Low:"),
                  paste0("This was the next candles low"),
                  br(),
                  strong("Actual Close:"),
                  paste0("This was the next candles close"),
                  br(),
                  strong("Confidence Score:"),
                  paste0("This is the confidence the model has that the next candle would reach the target percentage increase (on a scale of 0 to 1)"),
                  br(),
                  strong("Signal:"),
                  paste0("If the 'Confidence Score' is higher than the selected prediction BUY threshold, this will be 'DID BUY', otherwise 'DIDN'T BUY'"),
                  br(),
                  br(),
                  dataTableOutput("table1")
                )
              )
              )
      ),
      # tabItem(tabName = "predict",
      #         fluidRow(
      #           strong(h3("About:")),
      #           strong("Note that you must create a model on the previous tab before predicting tomorrow!"),
      #           br(),
      #           paste0("This tab will simply generate a prediction using the model you created in the 'Creating a Model' Tab. ",
      #                  "Remember that the probability is on a scale of 0-1, where a larger value represents a higher probability of your ",
      #                  "model's prediction coming true."),
      #           br(),
      #           br(),
      #           box(title = "Predict Tomorrow", status = "primary", solidHeader = TRUE,
      #             textInput("open","Open"),
      #             textInput("close","Close"),
      #             textInput("low","Low"),
      #             textInput("high","High"),
      #             actionButton("action2","Predict"),
      #             br(),
      #             br(),
      #             infoBoxOutput("predict", width = NULL)
      #           )
      #         )
      #         ),
      tabItem(tabName = "predictMultiple",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Predict Next Candle (Multiple):")),
                box(width = 10,
                paste0("On this tab you can generate predictions for multiple coins! Simply use the check boxes to select which coins you'd like to predict.",
                       " If you'd like to export these results, simply press the 'csv' button on top of the table below."),
                ),
                br(),
                br(),
                box(title = "Predict Multiple", status = "primary", solidHeader = TRUE,
                    # actionButton('selectall','Select All'),
                    selectInput('checkGroup',label = 'Select Coin(s)', choices = checkbox_list, multiple = TRUE),
                    # checkboxGroupInput('checkGroup', label = 'Select Coin(s)',
                    #                    choices = checkbox_list,
                    #                    selected = 'btcusd'),
                    selectInput("timeframePredict","Pick a Timeframe", choices = list("15 Minutes" = "15min",
                                                                                      "1 Hour" = "1hour",
                                                                                      "4 Hour" = "4hour",
                                                                                      "8 Hour" = "8hour",
                                                                                      "1 Day" = "1day",
                                                                                      "1 Week" = "7day")),
                    sliderInput("slider3", "Select Prediction 'BUY' Threshold", min = 0.1, max = 1, step = 0.05, value = 0.9),
                    actionButton("action4","Predict"),
                    br(),
                    br(),
                    strong(paste0("****** NOTE THAT I AM NOT RESPONSIBLE FOR FINANCIAL LOSS OR GAIN. PLACE TRADES AT YOUR OWN RISK. ",
                                  "IT IS GOOD TO USE THIS TOOL TO HELP YOU MAKE DECISIONS SUPPORTED BY OTHER EVIDENCE. ******")),
                    br(),
                    br()
                    

                ),
                box(title = "Candlestick Chart", status = "primary", solidHeader = TRUE,
                    br(),
                    selectInput('candlestickInput','Choose Crypto to View (options updated after predictions are made)', choices = NULL),
                    plotlyOutput('candlestickPlot')
                    
                ),

                box(title = "Predictions", status = "primary", solidHeader = TRUE, width =12,
                    br(),
                    strong(textOutput('timeRemaining')),
                    br(),
                    strong(h4("Variable Info:")),
                    strong('Coin:'),
                    paste0("The coin being predicted"),
                    br(),
                    strong('Price.Change:'),
                    paste0("The price change that's being predicted"),
                    br(),
                    strong('Confidence.Score.HIT.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL be hit next candle"),
                    br(),
                    strong('Confidence.Score.MISS.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL NOT be hit next candle"),
                    br(),
                    strong('Signal:'),
                    paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold"),
                    br(),
                  dataTableOutput("multipleOutput"),

                )

              )
      ),

      tabItem(tabName = "inputCoin",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Generate Model using TradingView Data:")),
                box(width = 10,
                paste0("On this tab you can generate a predictive model for data that you input from TradingView. You need to export TradingView data ",
                       "with no indicators on the chart. The 'Time Format' must also be set to ISO time. Name the exported file follwing the format <coinsymbol>.csv. For example, BTCUSD data would simply be BTCUSD.csv. Once you've exported the TradingView data",
                       " you simply drag that file into the input below. A timeframe must also be selected."),
                ),
                br(),
                br(),
                box(title = "Build Model from TradingView", status = "primary", solidHeader = TRUE,
                    fileInput('tvDataDrop', label = 'Input TradingView Data Here'),
                    selectInput("tvTimeFrame","Pick a Timeframe", choices = list("4 Hour" = "4hour",
                                                                               "8 Hour" = "8hour",
                                                                               "1 Day" = "1day",
                                                                               "1 Week" = "7day",
                                                                               "1 Month" = '1month')),
                    sliderInput("tvSlider","Select Percentage Increase", min = 1, max = 5, step = 1, value = 1),
                    actionButton('action6','Predict')

                ),
                box(title = "Predict Next Candle", status = "primary", solidHeader = TRUE, width =12,
                    strong(h4("Variable Info:")),
                    strong('Coin:'),
                    paste0("The coin being predicted"),
                    br(),
                    strong('Price.Change:'),
                    paste0("The price change that's being predicted"),
                    br(),
                    strong('Confidence.Score.HIT.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL be hit next candle"),
                    br(),
                    strong('Confidence.Score.MISS.TARGET:'),
                    paste0("The confidence score that the Price.Change WILL NOT be hit next candle"),
                    br(),
                    strong('Signal:'),
                    paste0("Either BUY or DON'T BUY depending on if the Confidence.Score.HIT.TARGET is above or below your selected BUY prediction threshold"),
                    br(),
                withSpinner(dataTableOutput('TVPrediction'))
                )

              )
      ),
      
      tabItem(tabName = "predictNextWeek",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Predict Next 7 Days/Weeks:")),
                box(width=10,
                paste0("On this tab you may pick a crypto to forecast for the next 7 days/weeks! The machine learning model utilizes the past 14 candles of data ",
                       "to predict the next 7 candles price movements!"),
                ),
                br(),
                br(),
                br(),
                br(),
                strong('Note: Previous data is displayed in BLUE while forecasted data is displayed in RED'),
                br(),
                br(),
                box(title = "Predict Next 7 Days/Weeks", status = "primary", solidHeader = TRUE,
                    selectInput('selectTimeFrame', 'Select a time frame', choices = list('7 Days' = 'daily',
                                                                                         '7 Weeks' = 'weekly')),
                    selectInput('selectNextWeek', "Select a Coin", choices = checkbox_list),
                    actionButton("action5", "Predict"),
                    br(),
                    br()


                ),
                plotOutput("nextWeekOutput")
              )
      ),
      
      tabItem(tabName = "binance",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Binance Integration")),
                box(width=10,
                    paste0("This tab offers you the capability of performing trades on Binance directly through this interface."),
                ),
                box(title = "Inputs", status = "primary", solidHeader = TRUE,
                    selectInput('selectCoinBinance', "Select a Coin", choices = checkbox_list, selected = 'BTCUSDT'),
                    br(),
                    selectInput('selectTypeBinance', 'Market or Limit', choices = list('Market' = 'MARKET',
                                                                                "Limit" = 'LIMIT'),
                                selected = 'Market'),
                    br(),
                    selectInput('selectSideBinance', 'Buy or Sell', choices = list("Buy" = "BUY",
                                                                            "Sell" = "SELL"),
                                selected = 'Buy'),
                    br(),
                    # sliderInput("takeProfitBinance", "Set Take Profit %",min = 0, max = 20, step = 0.1, value = 0),
                    # br(),
                    # sliderInput("stopLossBinance", "Set Stop Loss %",min = 0, max = 20, step = 0.1, value = 0),
                    # br(),
                    numericInput("tradeQuantity", "Quantity", value = 0, min = 0, step = 0.1),
                    textOutput('decimalsAllowed'),
                    br(),
                    sliderInput('percentSliderBinance', 'Percentage of USDT balance',value = 0,min = 0, max = 100, step = 0.1)
                ),
                box(title = "Live Price", status = "primary", solidHeader = TRUE,
                    actionButton(inputId = 'getLivePrice', label = 'Refresh Live Price'),
                    br(),
                    br(),
                    textOutput('livePrice')
                ),
                box(title = "Spot Account Balances", status = "primary", solidHeader = TRUE,
                  dataTableOutput('spotAccountBalances')
                ),
                actionBttn(inputId = 'submitBinance',
                           label = 'Submit',
                           icon = icon('money-bill-trend-up'),
                           style = 'pill',
                           color = 'warning',
                           size = 'lg',
                           block = TRUE),
                br(),
                dataTableOutput('binancePredictionTable')


              )
      ),
      tabItem(tabName = "automation",
              fluidRow(
                img(src='logo2.png', width = 200, height = 200, align = 'right' ),
                strong(h1("Binance Automation")),
                box(width=10,
                    paste0("This tab allows you to start and stop automation. Use the inputs to set up your automation criteria."),
                ),
                box(title = "Inputs", status = "primary", solidHeader = TRUE,width=4,
                  selectInput("timeframeAutomation","Pick a Timeframe to Automate", choices = list("4 Hour" = "4hour",
                                                                               "8 Hour" = "8hour",
                                                                               "1 Day" = "1day",
                                                                               "1 Week" = "7day",
                                                                               "1 Month" = '1month')),
                  br(),
                  selectInput('checkGroupBinance',label = 'Select Coin(s) to Automate', choices = checkbox_list, multiple = FALSE, selected = 'BTCUSDT'),
                  br(),
                  sliderInput('sliderAutomationTarget', 'Select Target Percentage Increase', min = 1, max = 15, value = 1, step = 1),
                  br(),
                  sliderInput('sliderBalanceUsed', 'Select Percentage of USDT Balance to Use', min = 1, max = 100, value = 1, step = 1),
                  br(),
                  sliderInput("takeProfitBinanceAutomation", "Set Take Profit %",min = 0, max = 20, step = 0.1, value = 0),
                  br(),
                  sliderInput("stopLossBinanceAutomation", "Set Stop Loss %",min = 0, max = 20, step = 0.1, value = 0),
                  br(),
                  sliderInput("confidenceThresholdAutomation", "Required Confidence Score to Buy", min = 0.1, max = 1, step = 0.02, value = 0.9),

                  
                ),
                box(title = "Current Automation Running", status = "primary", solidHeader = TRUE,width=8,
                    dataTableOutput("currentAutomation")
                ),
                br(),
                box(title = "Current Automation Info", status = "primary", solidHeader = TRUE,width=8,
                    selectInput('selectActiveAutomation', "Select a Coin", choices = checkbox_list),
                    dataTableOutput("activeAutomationInfo")
                ),
                box(title = "Volume % Change From Mean 5min Volume Over Past 2days", status = "primary", solidHeader = TRUE,width=4,
                  gaugeOutput("volumeGauge")
                ),
                actionBttn(inputId = 'submitBinanceAutomation',
                           label = 'Begin Automation',
                           icon = icon('robot'),
                           style = 'pill',
                           color = 'success',
                           size = 'lg',
                           block = TRUE),
                br(),
                actionBttn(inputId = 'cancelBinanceAutomation',
                           label = 'Cancel Automation',
                           icon = icon('robot'),
                           style = 'pill',
                           color = 'danger',
                           size = 'lg',
                           block = TRUE),
                br(),
                box(title = "Trades Placed", status = "primary", solidHeader = TRUE,width=12,
                    selectInput('selectTradesPlaced', "Select a Coin", choices = checkbox_list),
                    dataTableOutput("tradesPlaced")
                )

              ))
    )
  )
  
  
)
)

# Define server logic
server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials),
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)$user
  })
  
  observe({
    if(is.null(reactiveValuesToList(res_auth)$user)){
      
    }else if(reactiveValuesToList(res_auth)$user == 'nick'){
      # MINE
      
      secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
      api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"
      binance::authenticate(key = api_key,secret = secret)
      binance::base_url("https://api.binance.us")
      
    }else if(reactiveValuesToList(res_auth)$user == 'gentlemam1'){
      #Gentlemam
      secret = "9qhPtPDePdBJnWL5zThAxqrUWXNcv37NYbyDHdkDctoJZGa0CZS6IyPqmqOdIh3i"
      api_key = "wZpij1rDxXsrnyRyuNmuaoLPsVSgJKvmmgt0rzi44GZB03za9GBFqeB6chXi1p0T"
      binance::authenticate(key = api_key,secret = secret)
    }else if(reactiveValuesToList(res_auth)$user == 'gentlemam2'){
      #Gentlemam
      secret = "KECWzTynzt47MdHyFdY28l06G43odgzjXyOKf52VaiA4mEs7x68MTRHpLNl2XH0E"
      api_key = "3VSV3sbcbDS5DFnYHnpqqKZwQOjFG5hiFXEB7r6Kaev0wTBDQlvyEpOLFZgAhZZD"
      binance::authenticate(key = api_key,secret = secret)
    }else if(reactiveValuesToList(res_auth)$user == 'gentlemam3'){
      #Gentlemam
      secret = "xghtE9HU3aNHkMojdVe3jxgAzBu5Xz0EqiuAoifbM9b0rY09KjZntuSJzsCj5gvC"
      api_key = "HbKcjXOHLS0yseTvMnwX7jxltI0ugk2ZXoiYZHeDRZr9b2XWbiCBkOODsPu6xpSp"
      binance::authenticate(key = api_key,secret = secret)
    }
    output$spotAccountBalances = renderDataTable(datatable(spot_account_balances()))
    output$livePrice = renderText(round(as.numeric(binance::market_average_price(input$selectCoinBinance)$price), digits = 4))
    
    x = aws.s3::get_bucket_df("cryptomlbucket")
    
    x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
    coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
    if(length(coins.running) != 0){
      y = data.frame(Coins = coins.running)
      output$currentAutomation = renderDataTable(datatable(y))
      updateSelectInput(session = session, inputId = 'selectTradesPlaced', choices = y$Coins, selected = y$Coins[1])
      updateSelectInput(session = session, inputId = 'selectActiveAutomation', choices = y$Coins, selected = y$Coins[1])
      
    }
 
    
    
  })
  

  # user = res_auth$user

# .GlobalEnv = environment()
  # Read in functions
  source("DogeCoinML.R")
  
  output$decimalsAllowed = renderText(paste0(coin_decimals$decimals[coin_decimals$symbol == input$selectCoinBinance], " decimal places allowed."))

  output$timeRemaining = renderText(paste0("Please note there is ",getTimeRemaining(input$timeframePredict)," before the current candle closes! displayed predictions are for the current candle!"))
  output$TVPrediction = NULL
  
  observeEvent(input$action3, {
    showModal(modalDialog("Predicting Most Likely...", footer = NULL))
    on.exit(removeModal())
    all.bst.names = list.files(path = "bsts", pattern = ".rds")
    all.bst.numbers = str_match(string = all.bst.names, pattern = "bst_(.*)\\.")[,2]
    all.bst.path = list.files(path = "bsts", pattern = ".rds", full.names = TRUE)
    all.bst = lapply(all.bst.path, readRDS)
    assign('all.bst.numbers',all.bst.numbers,.GlobalEnv)
    assign('all.bst',all.bst,.GlobalEnv)
    
    predict.best(0.3, all.bst, all.bst.names)
    
    all.predictions = round(all.predictions, digits = 4)
    max.pred = which(all.predictions == max(all.predictions))
    max.bst = all.bst.numbers[max.pred]
    
    output$mostLikely = renderText(paste0("The most probable outcome over the next 24 hours is a change of ",max.bst,"% or more."))
    output$percentChance = renderText(paste0(max(all.predictions)," Probability Predicted"))
  })

  
  observeEvent(input$action1, {
    showModal(modalDialog("Generating Your Model...", footer = NULL))
    on.exit(removeModal())
    createModel(input$slider1, input$slider2, input$select, input$timeframe, input$slider1)
    # output$OverallAccuracy = renderInfoBox({
    #   infoBox("Overall Accuracy",paste0(round(overall.accuracy, digits = 2), "%"), icon = icon('check'))
    #   })
    output$Buy = renderInfoBox({infoBox("Profitable Trades", paste0(round(yes.buy.correct.perc, digits = 2), "%"), icon = icon("thumbs-up"))
    })
    output$SumPercentage = renderInfoBox({
      infoBox("Sum Percentage", paste0(round(sum.percentage, digits = 2), "%"),icon = icon("money-bill-trend-up"))
      })
    # output$DontBuy = renderInfoBox({infoBox("'Don't Buy' Correct", paste0(round(no.buy.correct.perc, digits = 2),"%"),icon = icon("thumbs-down"))
    #   })
    output$Predictions = renderInfoBox({infoBox("Number of Predictions", paste0(nrow(compare)))
    })
    output$Hits = renderInfoBox({infoBox("Number of BUYS", paste0(nrow(compare[compare$Signal == 'DID BUY',])))
    })
    
    colnames(compare) = c('Actual', 'Actual High', 'Actual Low','Actual Close', 'Confidence Score', 'Signal', 'profit')
    

    
    
    compare$`Actual High` = paste0(compare$`Actual High`,"%")
    compare$`Actual Low` = paste0(compare$`Actual Low`,"%")
    compare$`Actual Close` = paste0(compare$`Actual Close`,"%")
    
    compare$Signal[compare$Signal == 1] = "DID BUY"
    compare$Signal[compare$Signal == 0] = "DIDN'T BUY"
    
    compare$Actual[compare$Actual == 0] = 'MISSED TARGET'
    compare$Actual[compare$Actual == 1] = 'HIT TARGET'
    
    table1.colored = datatable(compare, rownames = FALSE, options = list(pageLength = 20,
      columnDefs = list(list(targets = 6, visible = FALSE))
    )) %>%
      formatStyle('Actual','profit',
                  backgroundColor = styleEqual(c(0,1), c('darkred','lightgreen'))) %>%
      formatStyle('Signal',
                  backgroundColor = styleEqual(c("DIDN'T BUY","DID BUY"), c('darkred','lightgreen')))

    
      
    output$table1 = renderDataTable(table1.colored)
    # output$modelPlot = renderPlot(hist(compare$Confidence.Score))
    output$modelPlot = renderPlot(ggplot(data = compare, aes(x = `Confidence Score`)) + geom_histogram(colour = "blue", alpha = 0.3))
  })
  
  observeEvent(input$action2, {
    predict.tomorrow(0.3, input$select)
    output$textToday = renderText(paste0("Probability of: ",round(predict.now, digits = 4)))
    output$predict = renderInfoBox({
      infoBox("Predicted Probability",round(predict.now, digits = 4))
    })
  })
  
  observeEvent(input$action4, {
    showModal(modalDialog("Generating predictions...", footer = NULL))
    on.exit(removeModal())
    x = input$checkGroup
    updateSelectInput(session = session, inputId = 'candlestickInput', choices = x, selected = head(x,1))
    
    predict.tomorrow.multiple(input$checkGroup, input$timeframePredict, input$slider3, .GlobalEnv)
    dt.colored = datatable(predictions.df.comb,
                           rownames = FALSE,
                           extensions = "Buttons",
                           options = list(paging = FALSE, searching = FALSE, dom = 'Bfrtip', buttons = c('csv'))) %>%
      formatStyle("Signal",
                  backgroundColor = styleEqual(c("DON'T BUY SIGNAL", "BUY SIGNAL"), c('darkred','lightgreen')))
    output$multipleOutput = renderDataTable(dt.colored)
    output$binancePredictionTable = renderDataTable(dt.colored)
    output$candlestickPlot = renderPlotly(createCandlePlot(input$candlestickInput))
  })

  observeEvent(input$action5, {
    showModal(modalDialog("Generating predictions...", footer = NULL))

    output$nextWeekOutput = renderPlot(predict_week(tolower(input$selectNextWeek), input$selectTimeFrame))
    on.exit(removeModal())
    
  })
  
  observeEvent(input$action6, {
    showModal(modalDialog("Generating predictions...", footer = NULL))
    on.exit(removeModal())
    
    if(is.null(input$tvDataDrop)){
      return(NULL)
    }else{
      df = input$tvDataDrop
    }
    
    output$TVPrediction = renderDataTable(build.TV.model(df, input$tvTimeFrame))
    # output$TVPrediction = renderDataTable(predictions.df.comb)
    
  })
  
  observeEvent(input$selectall, {
    updateCheckboxGroupInput(session = session, 'checkGroup',choices = checkbox_list, selected = checkbox_list)
    
  })
  
  observeEvent(input$getLivePrice, {
    output$livePrice = renderText(round(as.numeric(binance::market_average_price(input$selectCoinBinance)$price), digits = 4))
  })
  
  observeEvent(input$submitBinance, {
    x = possibly_spot_new_order(
      order_type = input$selectTypeBinance,
      symbol = input$selectCoinBinance,
      side = input$selectSideBinance,
      quantity = input$tradeQuantity,
      test = FALSE
    )
    if(x[1] == 'ERROR'){
      shinyalert("Order Not Placed",
                 "Check to see if you used to many decimals or if the minimum order requirements have not been met!",
                 type = 'error')
    }else{
      shinyalert("Success",
                 "Your order was successfully placed!",
                 type = 'success')
    }

    output$spotAccountBalances = renderDataTable(datatable(spot_account_balances()))
  })
  
  observeEvent(input$percentSliderBinance, {
    current_balance = spot_account_balances()
    free_usdt = current_balance$free[current_balance$asset == 'USDT']
    percentage = (input$percentSliderBinance / 100)
    quantity_usdt = free_usdt * percentage
    
    current_coin_price = round(as.numeric(binance::market_average_price(input$selectCoinBinance)$price), digits = 4)
    quantity_coin = round(quantity_usdt / current_coin_price, digits = coin_decimals$decimals[coin_decimals$symbol == input$selectCoinBinance])
    updateNumericInput(session = session, inputId = 'tradeQuantity',label = 'Quantity',value = quantity_coin, min = 0, step = 0.1)
  })
  
  
  observeEvent(input$checkGroupBinance, {
    vol1 = riingo_crypto_latest(input$checkGroupBinance, resample_frequency = '5min')
    vol1 = vol1[-1,]
    vol2 = riingo_crypto_prices(input$checkGroupBinance,start_date = Sys.Date() - 2,end_date = Sys.Date(), resample_frequency = '5min')
    
    vol = rbind(vol2, vol1)
    m.vol = mean(vol$volume)
    vol.now = vol$volume[(nrow(vol)-1)]
    
    vol.compare = (vol.now/m.vol * 100) - 100
    
    output$volumeGauge = renderGauge({
      gauge(vol.compare,
            min = -100,
            max = 100,
            sectors = gaugeSectors(
              success = c(20, 100), 
              warning = c(-20, 20),
              danger = c(-100, -20)))
    })
  })
  

  
  observeEvent(input$submitBinanceAutomation, {
    
    x = data.frame(User = reactiveValuesToList(res_auth)$user,
                   Timeframe = input$timeframeAutomation,
                   Coins = input$checkGroupBinance,
                   Target = input$sliderAutomationTarget,
                   Confidence = input$confidenceThresholdAutomation,
                   Percentage = input$sliderBalanceUsed,
                   TakeProfit = input$takeProfitBinanceAutomation,
                   StopLoss = input$stopLossBinanceAutomation,
                   Active = TRUE
                   )
    saveRDS(x, file = paste0(tempdir(), "/x.rds"))

    aws.s3::put_folder(reactiveValuesToList(res_auth)$user ,bucket = "cryptomlbucket/Automation")

    put_object(
      file = file.path(tempdir(), "x.rds"),
      object = paste0(input$checkGroupBinance,".rds"),
      bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user)
    )
    
    x = aws.s3::get_bucket_df("cryptomlbucket")
    
    x.sel = x[grepl(pattern = paste0("Automation/",reactiveValuesToList(res_auth)$user,"/"), x = x$Key),]
    coins.running = na.omit(str_match(string = x.sel$Key, pattern = "/.*/(.*).rds")[,2])
    if(length(coins.running) != 0){
      y = data.frame(Coins = coins.running)
      output$currentAutomation = renderDataTable(datatable(y))
    }
    
    updateSelectInput(session = session, inputId = 'selectTradesPlaced', choices = y$Coins, selected = y$Coins[1])
    updateSelectInput(session = session, inputId = 'selectActiveAutomation', choices = y$Coins, selected = y$Coins[1])
    
    shinyalert("Success",
               "Your Automation Was Successfully Started!",
               type = 'success')
  })
  
  observeEvent(input$cancelBinanceAutomation, {
    
    x = data.frame(User = reactiveValuesToList(res_auth)$user,
                   Timeframe = input$timeframeAutomation,
                   Coins = input$checkGroupBinance,
                   Target = input$sliderAutomationTarget,
                   Confidence = input$confidenceThresholdAutomation,
                   Percentage = input$sliderBalanceUsed,
                   TakeProfit = input$takeProfitBinanceAutomation,
                   StopLoss = input$stopLossBinanceAutomation,
                   Active = FALSE
    )
    saveRDS(x, file = paste0(tempdir(), "/x.rds"))
    
    aws.s3::put_folder(reactiveValuesToList(res_auth)$user ,bucket = "cryptomlbucket/Automation")
    
    put_object(
      file = file.path(tempdir(), "x.rds"),
      object = paste0(input$checkGroupBinance,".rds"),
      bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user)
    )
  })
  observeEvent(input$selectTradesPlaced, {
    y = binance::spot_trades_list(symbol=input$selectTradesPlaced)
    if(!is.null(y)){
      y = y %>%
        select(symbol, time, price, qty, commission, commission_asset, side)
      output$tradesPlaced = renderDataTable(datatable(y))
    }

  })
  
  observeEvent(input$selectActiveAutomation, {
   active = possibly_s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/Automation/",reactiveValuesToList(res_auth)$user), object = paste0(input$selectActiveAutomation,".rds"))
   if(active[1] == 'ERROR'){
     return(NULL)
   }else{
     output$activeAutomationInfo = renderDataTable(datatable(active))
   }
  })
  
  observeEvent(input$timeframe,{
    if(input$timeframe == "15min" | input$timeframe == "1hour"){
      updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min = 0.1, max = 1, step = 0.1, value = 0.1)
    }else{
      updateSliderInput(inputId = "slider1",label="Select Percentage Increase", min = 0.2, max = 3, step = 0.2, value = 0.2)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
