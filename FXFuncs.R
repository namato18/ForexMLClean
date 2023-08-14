Sys.setenv(TZ="UTC")

GetAccuracy = function(filename, prediction, target){
  # filename = "AUDUSD_1day"
  # prediction = "Open"
  # target = 0.25
  
  symbol = str_match(string = filename, pattern = "(.*)_")[,2]
  timeframe = str_match(string = filename, pattern = "_(.*)")[,2]
  
  if(prediction == "BreakH" | prediction == "BreakL" | prediction == "PercentageIncrease"){
    if(prediction == "BreakH" | prediction == "BreakL"){
      compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,"_",prediction,".rds"))
    }else{
      compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,target,".rds"))
    }
    compare$decision = 0
    compare$decision[compare$pred >= 0.5] = 1
    
    df = compare
    
    true.pos = length(which(df$outcome.test == 1 & df$decision == 1))
    false.pos = length(which(df$outcome.test == 0 & df$decision == 1))
    false.neg = length(which(df$outcome.test == 1 & df$decision == 0))
    
    
    precision = true.pos / (true.pos + false.pos) * 100
    recall = true.pos / (true.pos + false.neg) * 100
    f1 = 2*((precision * recall)/(precision + recall))
    
    precision = round(precision, digits = 4)
    recall = round(recall, digits = 4)
    f1 = round(f1, digits = 4)
    
    assign("precision",precision, .GlobalEnv)
    assign("recall",recall,.GlobalEnv)
    assign("f1",f1,.GlobalEnv)
    assign("rmse",NULL,.GlobalEnv)
    assign("current.price",NULL,.GlobalEnv)
  }else{
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,"_",prediction,".rds"))
    
    compare$error.sq = (compare$pred - compare$outcome.test)^2
    rmse = round((mean(compare$error.sq))^(1/2),digits = 5)
    
    current.price = riingo::riingo_crypto_latest(symbol, resample_frequency = timeframe)
    current.price = round(current.price$close[nrow(current.price)], digits = 5)
    
    assign("precision",NULL, .GlobalEnv)
    assign("recall",NULL,.GlobalEnv)
    assign("f1",NULL,.GlobalEnv)
    assign("rmse",rmse,.GlobalEnv)
    assign("current.price",current.price,.GlobalEnv)
  }

  
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
CreateHistogram = function(filename,prediction,target){
  # filename = "AUDUSD_1day"
  # prediction = "BreakH"
  # target = 0.25
  if(prediction == "BreakH" | prediction == "BreakL"){
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,"_",prediction,".rds"))
  }else{
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("compare_",filename,target,".rds"))
  }
  
  ggp = ggplot(compare, aes(x = pred)) + geom_histogram(fill="red", alpha=0.5, color="black")
  return(ggp)
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

predict.next = function(symbol, output, target = 0.25){
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  predictions = c("BreakH","BreakL","TargetChange")
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
    df = df[-c(1:20,nrow(df)),]
    df = df[nrow(df)-1,]
    
    prev.high.perc = round((df$High - df$Open) / df$Open * 100, digits = 3)
    prev.low.perc = round((df$Low - df$Open) / df$Open * 100, digits = 3)
    
    df = df[,-c(1:5)]
    
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    df = round(df, 2)
    
    
    df = as.matrix(df)
    assign('df',df,.GlobalEnv)
    if(prediction == "BreakH" | prediction == "BreakL"){
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_",prediction,".rds"))
    }else{
      bst = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,target,".rds"))
      assign("bst",bst,.GlobalEnv)
    }
    
    pred = round(predict(bst, df), digits = 3)
    assign(paste0("pred_",prediction),pred,.GlobalEnv)
    
    if(prediction == "BreakH"){
      text.bh = paste0(pred," (Previous High of ",round(prev.high.perc,digits = 3),"%)")
      assign("text.bh",text.bh,.GlobalEnv)
    }else if(prediction == "BreakL"){
      text.bl = paste0(pred," (Previous Low of ",round(prev.high.perc,digits = 3),"%)")
      assign("text.bl",text.bl,.GlobalEnv)
      
    }else{
      assign("text.perc25",pred,.GlobalEnv)
      
      
    }
    
  }
  
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
  df = df[nrow(df)-1,]


 
  df = df[,-c(1:4)]
  
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
  
  p.change.high = round((pred.high - df$Close)/df$Close * 100, digits = 2)
  p.change.low = round((pred.low - df$Close)/df$Close * 100, digits = 2)
  p.change.close = round((pred.close - df$Close)/df$Close * 100, digits = 2)
  
  text.high = paste0("$",round(pred.high, digits = 5), "(",p.change.high,"%)")
  text.low = paste0("$",round(pred.low, digits = 5), "(",p.change.low,"%)")
  text.close = paste0("$",round(pred.close, digits = 5), "(",p.change.close,"%)")
  
  assign("text.high",text.high,.GlobalEnv)
  assign("text.low",text.low,.GlobalEnv)
  assign("text.close",text.close,.GlobalEnv)
  
  
  
  assign("pred_High",pred.high,.GlobalEnv)
  assign("p.change.high",p.change.high,.GlobalEnv)
  assign("pred_Low",pred.low,.GlobalEnv)
  assign("p.change.low",p.change.low,.GlobalEnv)
  assign("pred_Close",pred.close,.GlobalEnv)
  assign("p.change.close",p.change.close,.GlobalEnv)
  
  output$predictPercentChangeHigh = renderInfoBox({
    infoBox("Predicted High", round(pred_High, digits = 3),icon = icon("bullseye"))
  })
  
  

}
