Sys.setenv(TZ="UTC")

GetAccuracy = function(filename, prediction, target){
  if(prediction == "BreakH" | prediction == "BreakL"){
    compare = readRDS(paste0("bsts/compare_",filename,"_",prediction,".rds"))
  }else{
    compare = readRDS(paste0("bsts/compare_",filename,target,".rds"))
  }
  
  compare$pred.value = 0
  compare$pred.value[compare$pred >= 0.5] = 1
  
  overall.accuracy = round(length(which(compare$outcome.test == compare$pred.value)) / nrow(compare) * 100, digits = 2)
  assign("overall.accuracy",overall.accuracy,.GlobalEnv)
  
  pred.yes = compare[compare$pred.value == 1,]
  
  pred.yes.accuracy = round(length(which(pred.yes$outcome.test == pred.yes$pred.value)) / nrow(pred.yes) * 100, digits = 2)
  assign("pred.yes.accuracy",pred.yes.accuracy,.GlobalEnv)
  
  assign("n.total",nrow(compare),.GlobalEnv)
  assign("n.yes",nrow(pred.yes),.GlobalEnv)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

RenderInfoBoxes = function(output){
  output$overallaccuracy = renderInfoBox({
    infoBox("Overall Accuracy", paste0(overall.accuracy, "%"),icon = icon("money-bill-trend-up"))
  })
  output$yesaccuracy = renderInfoBox({
    infoBox("Predicted Hit Accuracy", paste0(pred.yes.accuracy, "%"),icon = icon("money-bill-trend-up"))
  })
  output$totalnumber = renderInfoBox({
    infoBox("Total Number of Candles Tested", paste0(n.total),icon = icon("money-bill-trend-up"))
  })
  output$totalhits = renderInfoBox({
    infoBox("Predicted Hits", paste0(n.yes),icon = icon("money-bill-trend-up"))
  })
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
LivePlot = function(symbol){
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  
  df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 21, end_date = Sys.Date(), resample_frequency = timeframe)
  df1 = df1[-nrow(df1),]
  df2 = httr::GET(paste0("https://api.tiingo.com/tiingo/fx/",pair,"/prices?resampleFreq=",timeframe,"&token=6fbd6ce7c9e035489f6238bfab127fcedbe34ac2"))
  request_char = rawToChar(df2$content)
  request_json = jsonlite::fromJSON(request_char, flatten = TRUE)
  df2 = request_json
  
  df2$date = str_replace(string = df2$date, pattern = "T", replacement = " ")
  df2$date = str_replace(string = df2$date, pattern = "Z", replacement = "")
  df2$date = as.POSIXct(df2$date, format = "%Y-%m-%d %H:%M:%S")
  df2 = df2[,c(2,1,3:6)]

  df = rbind(df1,df2)
  
  df_candle_plot = tail(df,30) %>%
    plot_ly(x = ~date, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low)
  df_candle_plot = df_candle_plot %>% layout(title = paste0('Last 30 candles for ',toupper(pair)),
                                             xaxis = list(rangeslider = list(visible = F)))
  return(df_candle_plot)
}
