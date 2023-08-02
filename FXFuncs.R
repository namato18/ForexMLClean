Sys.setenv(TZ="UTC")

GetAccuracy = function(filename, prediction, target){
  if(prediction == "BreakH" | prediction == "BreakL"){
    # compare = readRDS(paste0("~/Desktop/R related/bsts/compare_",filename,"_",prediction,".rds"))
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,"_",prediction,".rds"))
  }else{
    # compare = readRDS(paste0("~/Desktop/R related/bsts/compare_",filename,target,".rds"))
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,target,".rds"))
    
  }
  assign("compare",compare,.GlobalEnv)
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

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
CreateHistogram = function(){
  ggp = ggplot(compare, aes(x = pred)) + geom_histogram(fill="red", alpha=0.5, color="black")
  return(ggp)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.next = function(symbol, output, target = 1){
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  predictions = c("BreakH","BreakL")
  for(i in 1:length(predictions)){
    prediction = predictions[i]
    
    # symbol = "AUDUSD_1day"
    # prediction = "BreakL"
    # pair = "AUDUSD"
    # timeframe = "1day"
    
    df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
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
    
    df = df[,-1]
    
    ###############################
    ############################### CHANGE NAMES
    colnames(df) = c("Date","Open","High","Low","Close")
    
    
    ###############################
    ############################### ADD IN MOVING AVERAGES
    df$MA10 = NA
    df$MA20 = NA
    
    for(k in 21:nrow(df)){
      df$MA10[k] = mean(df$Close[k-10:k])
      df$MA20[k] = mean(df$Close[k-20:k])
    }
    
    ###############################
    ############################### DEFINE OTHER INPUT VALUES
    df$OH = (df$High - df$Open)/df$High * 100
    df$CH = (df$Close - df$Open)/ df$Close * 100
    df$LH = (df$High - df$Low) / df$High * 100
    df$LC = (df$Close - df$Low) / df$Low * 100
    
    df$HMA = (df$High - df$MA20)/ df$High * 100
    df$LMA = (df$Low - df$MA20)/ df$Low * 100
    df$CMA = (df$Close - df$MA20)/ df$Close * 100
    
    if(grepl(pattern = "day", x = timeframe)){
      df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
    }else{
      df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
    }
    df.xts = as.xts(df)
    UpTrend = as.data.frame(up.trend(df.xts))$`Up Trend`
    DownTrend = as.data.frame(down.trend(df.xts))$`Down Trend`
    
    df$UpTrend = as.numeric(UpTrend)
    df$DownTrend = as.numeric(DownTrend)
    
    Previous1 = rep(0, nrow(df))
    Previous1[df$Close > df$Open] = 1
    Previous2 = Lag(Previous1, 1)
    Previous3 = Lag(Previous1, 2)
    
    df$Previous1 = Previous1
    df$Previous2 = Previous2
    df$Previous3 = Previous3
    
    ###############################
    ############################### DETERMINE OUTCOME VALUES
    BreakL = NA
    BreakH = NA
    
    for(k in 2:(nrow(df))){
      if(df$Low[k] <= df$Low[k-1]){
        BreakL[k] = 1
      }else{
        BreakL[k] = 0
      }
      
      if(df$High[k] >= df$High[k-1]){
        BreakH[k] = 1
      }else{
        BreakH[k] = 0
      }
    }
    
    BreakH = c(BreakH, NA)
    BreakH = BreakH[-1]
    
    BreakL = c(BreakL, NA)
    BreakL = BreakL[-1]
    
    ###############################
    ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
    df = df[-c(1:20,nrow(df)),-c(1:5)]
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    df = round(df, 2)
    
    df = df[nrow(df)-1,]
    
    df = as.matrix(df)
    assign('df',df,.GlobalEnv)
    if(prediction == "BreakH" | prediction == "BreakL"){
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_",prediction,".rds"))
    }else{
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,target,".rds"))
      assign("bst",bst,.GlobalEnv)
    }
    
    pred = predict(bst, df)
    assign(paste0("pred_",prediction),pred,.GlobalEnv)
    
  }
  output$predictBreakHigh = renderInfoBox({
    infoBox("Conf.to Break Prev High", round(pred_BreakH, digits = 3),icon = icon("arrow-trend-up"))
  })
  output$predictBreakLow = renderInfoBox({
    infoBox("Conf. to Break Prev Low", round(pred_BreakL, digits = 3),icon = icon("arrow-trend-down"))
  })
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
predict.next.ohlc = function(symbol, output){

  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  
  # symbol = "AUDUSD_1day"
  # prediction = "BreakH"
  # pair = "AUDUSD"
  # timeframe = "1day"
  
  df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 30, end_date = Sys.Date(), resample_frequency = timeframe)
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
  
  df = df[,-1]
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close")
  
  
  ###############################
  ############################### ADD IN MOVING AVERAGES
  df$MA10 = NA
  df$MA20 = NA
  
  for(k in 21:nrow(df)){
    df$MA10[k] = mean(df$Close[k-10:k])
    df$MA20[k] = mean(df$Close[k-20:k])
  }
  
  ###############################
  ############################### DEFINE OTHER INPUT VALUES
  df$OH = (df$High - df$Open)/df$Open * 100
  df$CH = (df$Close - df$Open)/ df$Open * 100
  df$LH = (df$High - df$Low) / df$Low * 100
  df$LC = (df$Close - df$Low) / df$Low * 100
  
  df$HMA = (df$High - df$MA20)/ df$MA20 * 100
  df$LMA = (df$Low - df$MA20)/ df$MA20 * 100
  df$CMA = (df$Close - df$MA20)/ df$MA20 * 100
  
  if(grepl(pattern = "day", x = timeframe)){
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
  }else{
    df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
  }
  df.xts = as.xts(df)
  UpTrend = as.data.frame(up.trend(df.xts))$`Up Trend`
  DownTrend = as.data.frame(down.trend(df.xts))$`Down Trend`
  
  df$UpTrend = as.numeric(UpTrend)
  df$DownTrend = as.numeric(DownTrend)
  
  Previous1 = rep(0, nrow(df))
  Previous1[df$Close > df$Open] = 1
  Previous2 = Lag(Previous1, 1)
  Previous3 = Lag(Previous1, 2)
  
  df$Previous1 = Previous1
  df$Previous2 = Previous2
  df$Previous3 = Previous3
  
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[nrow(df)-1,-c(1:4)]
  
  df.m = as.matrix(df)
  
  bst.open = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Open.rds"))
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Close.rds"))
  
  # bst.open = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Open.rds"))
  # bst.high = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_High.rds"))
  # bst.low = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Low.rds"))
  # bst.close = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Close.rds"))
  
  pred.open = predict(bst.open, df.m)
  pred.high = predict(bst.high, df.m)
  pred.low = predict(bst.low, df.m)
  pred.close = predict(bst.close, df.m)
  
  p.change.high = (pred.high - df$Close)/df$Close * 100
  
  assign("pred_High",pred.high,.GlobalEnv)
  assign("p.change.high",p.change.high,.GlobalEnv)
  
  output$predictPercentChangeHigh = renderInfoBox({
    infoBox("Predicted High", round(pred_High, digits = 3),icon = icon("bullseye"))
  })
  
  

}
