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
    
    
    precision = round(true.pos / (true.pos + false.pos) * 100, digits = 2)
    recall = round(true.pos / (true.pos + false.neg) * 100, digits = 2)
    f1 = round(2*((precision * recall)/(precision + recall)) / 100, digits = 2)
    
    precision = round(precision, digits = 4)
    recall = round(recall, digits = 4)
    f1 = round(f1, digits = 4)
    
    assign("precision",precision, .GlobalEnv)
    assign("recall",recall,.GlobalEnv)
    assign("f1",f1,.GlobalEnv)
    assign("rmse",NULL,.GlobalEnv)
    assign("current.price",NULL,.GlobalEnv)
  }else{
    compare = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts2", object = paste0("compare_",filename,"_",prediction,".rds"))
    # compare = readRDS(paste0("../bsts-8-31-2023/compare_",filename,"_",prediction,".rds"))
    
    
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
LivePlot = function(symbol,type){
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  
  df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 100, end_date = Sys.Date(), resample_frequency = timeframe)
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
  assign('df.date.grab',df,.GlobalEnv)
  
  df30 = tail(df,30)
  df30$open[30] = NA
  df30$close[30] = NA
  df30$high[30] = NA
  df30$low[30] = NA
  
  fig = plot_ly()
  
  fig = add_trace(
    fig,
    data = df30,
    x = ~date, type="candlestick",
    open = ~open, close = ~close,
    high = ~high, low = ~low
  )
  fig = fig %>% layout(title = paste0('Last 30 candles for ',toupper(pair)),
                                             xaxis = list(rangeslider = list(visible = F)))
  if(type == "simple"){
    return(fig)
  }else{
    
    MakePrediction(pred.close.overlay.p,pred.high.overlay.p,pred.low.overlay.p,pred.BH.overlay,pred.BL.overlay,pred.25.overlay,prev.high.overlay,prev.low.overlay,up.trend.overlay,down.trend.overlay,"multiple")
    
    i <- list(line = list(color = 'blue'))
    d <- list(line = list(color = 'orange'))
    
    
    fig = add_trace(
      fig,
      data = overlay.comb,
      x = ~date, type="candlestick",
      open = ~open, close = ~close,
      high = ~high, low = ~low,
      increasing = i, decreasing = d
    )
    # fig = fig %>% add_annotations(x = overlay.comb$date,
    #                                   y = overlay.comb$high,
    #                                   text = pred.count.overlay)
    



    return(fig)
  }


}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
CreateHistogram = function(filename,prediction,target){
  # filename = "AUDUSD_1day"
  # prediction = "TargetPercentage"
  # target = 0.3
  if(target == 0){
    return(NULL)
  }
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

predict.next = function(symbol, output, target = 0.25, type){
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  # predictions = c("BreakH","BreakL","TargetChange")
  # for(i in 1:length(predictions)){
  #   prediction = predictions[i]
    
    # symbol = "AUDUSD_1hour"
    # prediction = "BreakL"
    # pair = "AUDUSD"
    # timeframe = "1hour"
    
    df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 100, end_date = Sys.Date(), resample_frequency = timeframe)
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
    
    down.trend.df.xts = data.frame(down.trend(df.xts))
    up.trend.df.xts = data.frame(up.trend(df.xts))
    down.trend.df.xts.obj = down.trend.df.xts$Down.Trend[nrow(down.trend.df.xts)-1]
    up.trend.df.xts.obj = up.trend.df.xts$Up.Trend[nrow(up.trend.df.xts)-1]
    
    up.trend.df.xts.obj.overlay = up.trend.df.xts$Up.Trend[(nrow(up.trend.df.xts)-30):(nrow(up.trend.df.xts)-1)]
    down.trend.df.xts.obj.overlay = down.trend.df.xts$Down.Trend[(nrow(up.trend.df.xts)-30):(nrow(down.trend.df.xts)-1)]
    
    assign("up.trend.overlay",up.trend.df.xts.obj.overlay,.GlobalEnv)
    assign("down.trend.overlay",down.trend.df.xts.obj.overlay, .GlobalEnv)
    
    assign("up.trend",up.trend.df.xts.obj,.GlobalEnv)
    assign("down.trend",down.trend.df.xts.obj,.GlobalEnv)
    
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
    ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO GET LAST ROW
    
    # df = df[-c(1:20,nrow(df)),]
    df.overlay.wdate = df[(nrow(df)-30):(nrow(df)-1),]
    
    df = df[nrow(df)-1,]
    
    prev.high.perc = round((df$High - df$Open) / df$Open * 100, digits = 3)
    prev.low.perc = round((df$Low - df$Open) / df$Open * 100, digits = 3)
    
    df = df[,-c(1:5)]
    df.overlay = df.overlay.wdate[,-c(1:5)]
    
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    df = round(df, 2)
    
    
    df = as.matrix(df)
    df.m.overlay = as.matrix(df.overlay)
    assign('df',df,.GlobalEnv)
    
    
    bst.BH = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_","BreakH",".rds"))
    bst.BL = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_","BreakL",".rds"))
    bst.25 = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"0.25.rds"))
    bst.target = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,target,".rds"))
    # assign("bst",bst,.GlobalEnv)
    
    
    pred.BH = round(predict(bst.BH, df), digits = 3)
    pred.BL = round(predict(bst.BL, df), digits = 3)
    pred.target = round(predict(bst.target, df), digits = 3)
    
    pred.BH.overlay = round(predict(bst.BH, df.m.overlay), digits = 3)
    pred.BL.overlay = round(predict(bst.BL, df.m.overlay), digits = 3)
    pred.25.overlay = round(predict(bst.25, df.m.overlay), digits = 3)
    
    prev.low.overlay = (df.overlay.wdate$Low - df.overlay.wdate$Open) / df.overlay.wdate$Open * 100
    prev.high.overlay = (df.overlay.wdate$High - df.overlay.wdate$Open) / df.overlay.wdate$Open * 100
    
    # assign(paste0("pred_",prediction),pred,.GlobalEnv)
    
    if(type == "detail"){
        text.bh = paste0(pred.BH," (Previous High of ",round(prev.high.perc,digits = 3),"%)")
        assign("text.bh",text.bh,.GlobalEnv)
        text.bl = paste0(pred.BL," (Previous Low of ",round(prev.low.perc,digits = 3),"%)")
        assign("text.bl",text.bl,.GlobalEnv)
        assign("text.perc25",pred.target,.GlobalEnv)
        
        assign("pred.BH.overlay",pred.BH.overlay,.GlobalEnv)
        assign("pred.BL.overlay",pred.BL.overlay,.GlobalEnv)
        assign("pred.25.overlay", pred.25.overlay,.GlobalEnv)
        
        assign("prev.low.overlay",prev.low.overlay,.GlobalEnv)
        assign("prev.high.overlay",prev.high.overlay,.GlobalEnv)
    }else{
        assign("bh.pred.simple",pred.BH,.GlobalEnv)
        assign("bl.pred.simple",pred.BL,.GlobalEnv)
        assign("perc25.pred.simple",pred.target,.GlobalEnv)
    }
    
    
  #}
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
predict.next.ohlc = function(symbol, output, type){
  
  pair = str_match(string = symbol, pattern = "(.*)_")[,2]
  timeframe = str_match(string = symbol, pattern = "_(.*)")[,2]
  
  # symbol = "AUDUSD_1hour"
  # prediction = "BreakH"
  # pair = "AUDUSD"
  # timeframe = "1hour"
  
  df1 = riingo_fx_prices(pair, start_date = Sys.Date() - 100, end_date = Sys.Date(), resample_frequency = timeframe)
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
  assign("df.opens",df,.GlobalEnv)
  
  
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
  df.overlay.wdate = df[(nrow(df)-30):(nrow(df)-1),]
  assign("df.overlay.wdate",df.overlay.wdate,.GlobalEnv)
  df = df[nrow(df)-1,]
  
  
  prev.close = df$Close[1]
  prev.high = df$High[1]
  prev.low = df$Low[1]
  prev.open = df$Open[1]
  
  prev.high.perc = (prev.high - prev.open) / prev.open * 100
  prev.low.perc = (prev.low - prev.open) / prev.open * 100
  
  
  closes = df$Close
  df = df[,-c(1:7)]
  df.overlay = df.overlay.wdate[,-c(1:7)]
  
  df.m = as.matrix(df)
  df.m.overlay = as.matrix(df.overlay)
  
  # bst.open = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts", object = paste0("bst_",symbol,"_Open.rds"))
  # bst.high = readRDS(paste0("../bsts-8-31-2023/bst_",symbol,"_High.rds"))
  # bst.low = readRDS(paste0("../bsts-8-31-2023/bst_",symbol,"_Low.rds"))
  # bst.close = readRDS(paste0("../bsts-8-31-2023/bst_",symbol,"_Close.rds"))
  
  bst.high = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts2", object = paste0("bst_",symbol,"_High.rds"))
  bst.low = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts2", object = paste0("bst_",symbol,"_Low.rds"))
  bst.close = s3read_using(FUN = readRDS, bucket = "cryptomlbucket/FXCleanBoosts2", object = paste0("bst_",symbol,"_Close.rds"))
  
  # bst.open = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Open.rds"))
  # bst.high = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_High.rds"))
  # bst.low = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Low.rds"))
  # bst.close = readRDS(paste0("../Forex.bsts/","bst_",symbol,"_Close.rds"))
  
  # pred.open.overlay = predict(bst.open, df.m.overlay)
  pred.high.overlay.p = predict(bst.high, df.m.overlay) / 100
  pred.low.overlay.p = predict(bst.low, df.m.overlay) / 100
  pred.close.overlay.p = predict(bst.close, df.m.overlay) / 100
  
  pred.high.overlay = pred.high.overlay.p * df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))] + df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))]
  pred.low.overlay = pred.low.overlay.p * df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))] + df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))]
  pred.close.overlay = pred.close.overlay.p * df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))] + df.opens$Open[(nrow(df.opens)-29):(nrow(df.opens))]
  
  overlay.comb = data.frame(cbind(as.character(df.date.grab$date[(nrow(df.date.grab)-29):(nrow(df.date.grab))]),df.date.grab$open[(nrow(df.date.grab)-29):(nrow(df.date.grab))],pred.high.overlay,pred.low.overlay,pred.close.overlay))
  colnames(overlay.comb) = c("date","open","high","low","close")
  
  overlay.comb$high[overlay.comb$high < overlay.comb$open] = overlay.comb$open[overlay.comb$high < overlay.comb$open]
  overlay.comb$high[overlay.comb$high < overlay.comb$close] = overlay.comb$close[overlay.comb$high < overlay.comb$close]
  
  
  overlay.comb$low[overlay.comb$low > overlay.comb$close] = overlay.comb$close[overlay.comb$low > overlay.comb$close]
  overlay.comb$low[overlay.comb$low > overlay.comb$open] = overlay.comb$open[overlay.comb$low > overlay.comb$open]
  
  assign("overlay.comb",overlay.comb,.GlobalEnv)
  
  # pred.open = predict(bst.open, df.m)
  p.change.high = predict(bst.high, df.m) / 100
  p.change.low = predict(bst.low, df.m) / 100
  p.change.close = predict(bst.close, df.m) / 100
  

  
  pred.high = p.change.high * closes + closes
  pred.low = p.change.low * closes + closes
  pred.close = p.change.close * closes + closes
  
  if(pred.low >= prev.close){
    pred.low = prev.close
  }
  
  text.high = paste0("$",round(pred.high, digits = 5), "(",round(p.change.high*100,2),"%)")
  text.low = paste0("$",round(pred.low, digits = 5), "(",round(p.change.low*100,2),"%)")
  text.close = paste0("$",round(pred.close, digits = 5), "(",round(p.change.close*100,2),"%)")
  
  if(type == "detail"){
    assign("text.high",text.high,.GlobalEnv)
    assign("text.low",text.low,.GlobalEnv)
    assign("text.close",text.close,.GlobalEnv)
    
    
    
    assign("pred_High",pred.high,.GlobalEnv)
    assign("p.change.high",p.change.high,.GlobalEnv)
    assign("pred_Low",pred.low,.GlobalEnv)
    assign("p.change.low",p.change.low,.GlobalEnv)
    assign("pred_Close",pred.close,.GlobalEnv)
    assign("p.change.close",p.change.close,.GlobalEnv)
    
    assign("pred.high.overlay",pred.high.overlay, .GlobalEnv)
    assign("pred.low.overlay",pred.low.overlay, .GlobalEnv)
    # assign("pred.open.overlay",pred.open.overlay, .GlobalEnv)
    assign("pred.close.overlay",pred.close.overlay, .GlobalEnv)
    
    assign("pred.high.overlay.p",pred.high.overlay.p * 100,.GlobalEnv)
    assign("pred.low.overlay.p",pred.low.overlay.p * 100,.GlobalEnv)
    assign("pred.close.overlay.p",pred.close.overlay.p * 100,.GlobalEnv)
    
    
  }else{
    assign("pred_High.simple",pred.high,.GlobalEnv)
    assign("p.change.high.simple",p.change.high,.GlobalEnv)
    assign("pred_Low.simple",pred.low,.GlobalEnv)
    assign("p.change.low.simple",p.change.low,.GlobalEnv)
    assign("pred_Close.simple",pred.close,.GlobalEnv)
    assign("p.change.close.simple",p.change.close,.GlobalEnv)
    
    assign("prev.high.perc",prev.high.perc,.GlobalEnv)
    assign("prev.low.perc",prev.low.perc,.GlobalEnv)
  }

  
  output$predictPercentChangeHigh = renderInfoBox({
    infoBox("Predicted High", round(pred_High, digits = 3),icon = icon("bullseye"))
  })
  
  
  
}

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

MakePrediction = function(perc.close, perc.high, perc.low, pred.bh, pred.bl, pred.perc1, prev.high.perc, prev.low.perc,upTrend,downTrend,Type){
  # perc.close = p.change.close.simple
  # perc.high = p.change.high.simple
  # perc.low = p.change.low.simple
  # pred.bh = bh.pred.simple
  # pred.bl = bl.pred.simple
  # pred.perc1 = perc25.pred.simple
  # prev.high.perc = prev.high.perc
  # prev.low.perc = prev.low.perc
  
  pred.count.list = c()
  for(i in 1:length(perc.close)){
    pred.count = 0
    
    target.percentage.adjust.good = round(pred.perc1[i] * 0.8, digits = 1)
    target.percentage.adjust.bad = round(pred.perc1[i] * 0.6, digits = 1)
    
    
    #####################
    ##################### ADD CONDITIONS FOR BAD
    if((perc.low[i]*-1) > perc.high[i]){
      pred.count = pred.count - 1
    }
    if(pred.perc1[i] < 0.5){
      pred.count = pred.count - 1
    }
    if(pred.bl[i] > 0.5 & prev.low.perc[i]*-1 > target.percentage.adjust.bad){
      pred.count = pred.count - 1
    }
    if(downTrend[i] == TRUE){
      pred.count = pred.count - 1
    }
    
    #####################
    ##################### ADD CONDITIONS FOR GOOD
    if((perc.low[i]*-1) < perc.high[i] & perc.high[i] > 1){
      pred.count = pred.count + 1
    }
    if(pred.perc1[i] > 0.5){
      pred.count = pred.count + 1
    }
    if(pred.bh[i] > 0.5 & prev.high.perc[i] > target.percentage.adjust.good){
      pred.count = pred.count + 1
    }
    if(upTrend[i] == TRUE){
      pred.count = pred.count + 1
    }
    
    pred.count.list = c(pred.count.list,pred.count)
  }
  
if(Type == "single"){
  assign("pred.count",pred.count.list[1],.GlobalEnv)
}else{
  pred.count.list[pred.count.list > 0] = "BUY"
  pred.count.list[pred.count.list <= 0] = "NO BUY"
  
  assign("pred.count.overlay",pred.count.list,.GlobalEnv)
}
  
  
}

