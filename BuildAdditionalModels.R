library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(tictoc)
library(CandleStickPattern)
library(dplyr)
library(riingo)

######### EXTRA CODE TO CREATE BST MODELS
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
tictoc::tic()

str1 = readRDS('tickers/str1.rds')
# str1 = 'CKBUSDT'


# str1 = readRDS('tickers/str1')
# str2 = readRDS('tickers/str2')

# Timeframe = c("4hour","8hour","1day","7day")
Timeframe = c("15min","1hour")

x = list.files(path = '../RiingoPulledData',full.names = TRUE)
file.names = list.files('../RiingoPulledData')
file.names = str_replace(string = file.names, pattern = '\\.csv', replacement = "")
ind = grep(pattern = "1hour|15min", x = file.names)
# ind = ind[-99]
file.names = file.names[ind]
x = x[ind]
ls.files = lapply(x, read.csv)

file.names.short = str_match(string = file.names, pattern = "(.*USDT).*")[,2]


# j = 1
# df = read.csv("TVData/BTCUSDT4hour.csv")
# 
# df = readRDS("bsts/df_BTCUSDT4hour.rds")
# bst = readRDS("bsts/bst_BTCUSD4hour-1.rds")

for(i in 1:length(file.names)){
  # for(z in 1:1){
  # 
  #   # df1 = riingo_crypto_prices(str1[i], end_date = Sys.Date(), resample_frequency = Timeframe[z])
  #   # df1 = df1[-nrow(df1),]
  #   # df2 = riingo_crypto_latest(str1[i], resample_frequency = Timeframe[z])
  #   # df3 = rbind(df1,df2) %>%
  #   #   select(date, open, high, low, close)
  #   
    for(j in seq(from=-1, to=-0.1, by = 0.1)){
      df = ls.files[[i]]
      if(nrow(df) < 30){
        next()
      }
      # Testing quantmod
      # df = data.frame(getSymbols("DOGE-USD",
      #                            from = '2017-01-01',
      #                            to = Sys.Date(),
      #                            warnings = FALSE,
      #                            auto.assign = FALSE))
      # 
      # df = na.omit(df)
      
      # Remove uncecessary columns
      # df = df[,1:5]
      df = df[,3:8]
      
      # Modify data to be more useable
      df$Percent.Change = NA
      #df = df[-1,-c(1:3,10:11)]
      colnames(df) = c("Date","Open","High","Low","Close","Volume","Percent.Change")
      if(j > 0){
        df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
      }else{
        df$Percent.Change = round((((df$Low / df$Open) * 100) - 100), digits = 1)
      }
      
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
      
      #Add column for binary previouos day change+
      df$Previous = NA
      for(k in 2:nrow(df)){
        if(df$Percent.Change[k - 1] <= 0){
          df$Previous[k] = 0
        }else{
          df$Previous[k] = 1
        }
      }
      
      # Remove first row since we can't use it
      df = df[-1,]
      BreakL = BreakL[-1]
      BreakH = BreakH[-1]
      
      
      # Adding Moving Averages
      df$MA10 = NA
      df$MA20 = NA
      
      for(k in 21:nrow(df)){
        df$MA10[k] = mean(df$Close[k-10:k])
        df$MA20[k] = mean(df$Close[k-20:k])
      }
      # df$MA10 = round(df$MA10, digits = 2)
      # df$MA20 = round(df$MA20, digits = 2)
      
      # Add column for if MA10 is above or below MA20
      df$MAAB = 0

      df$MAAB[df$MA10 > df$MA20] = 1
      
      df = df[,-which(colnames(df) %in% c("MA10","MA20"))]
      # Convert to actual dates and remove year and change to numeric
      if(!grepl(pattern ="day",file.names[i])){
        df$Date = str_replace(string = df$Date, pattern = "T", replacement = " ")
        df$Date = str_replace(string = df$Date, pattern = "Z", replacement = "")
        
        df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
      }else{
        df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
      }
      
      df = df[!is.na(df$Date),]

      
      # if(Timeframe[z] == '1day' | Timeframe[z] == '7day'){
      #   df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d")
      # }else{

      # }
      
      df = as.xts(df)
      
      
      
      # Add candelstick patterns
      # candle.list = list(CSPDarkCloudCover(df),CSPDoji(df),CSPEngulfing(df),CSPGap(df),CSPHammer(df),CSPHarami(df),
      #                    CSPInsideDay(df),CSPInvertedHammer(df),CSPKicking(df),CSPLongCandle(df),CSPMarubozu(df),
      #                    CSPNLongWhiteCandles(df),CSPPiercingPattern(df),CSPStar(df),
      #                    CSPStomach(df),CSPTasukiGap(df),CSPThreeBlackCrows(df),CSPThreeInside(df),CSPThreeLineStrike(df),
      #                    CSPThreeMethods(df),CSPThreeOutside(df),CSPThreeWhiteSoldiers(df))
      candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
      
      # candle.list = list(CSPHammer(df), CSPInvertedHammer(df),CSPEngulfing(df))
      # trend = candlesticks::TrendDetectionSMA(df)
      
      
      for(k in 1:length(candle.list)){
        df = cbind(df, candle.list[[k]])
      }
      # df = cbind(df, trend$Trend)
      
      # Remove unusable rows
      df = df[-(1:20),]
      BreakL = BreakL[-(1:20)]
      BreakH = BreakH[-(1:20)]
      
      # Add lagged values
      for(k in 1:5){
        high.lag = Lag(df$High, k)
        open.lag = Lag(df$Open, k)
        percent.change.lag = round((((high.lag/open.lag) - 1) * 100), digits = 2)
        # lagging = LagOHLC(df, k)
        # ind = which(names(lagging) == paste0("High.Lag.",k))
        # ind = c(ind,which(names(lagging) == paste0("Close.Lag.",k)))
        df = cbind(df, percent.change.lag)
        
      }
      
      df = df[-c(1:5),]
      BreakL = BreakL[-(1:5)]
      BreakH = BreakH[-(1:5)]
      
      df[is.na(df)] = 0
      
      
      
      
      
      
      # Round columns to be more general
      # df$Close = round(df$Close, digits = 3)
      # df$Open = round(df$Open, digits = 3)
      # df$High = round(df$High, digits = 3)
      # df$Low = round(df$Low, digits = 3)
      
      
      
      
      
      # outcomes
      
      # outcome = nextCandlePosition(df)$HigherClose
      # outcome = data.frame(outcome)[,1]
      # outcome = as.numeric(outcome)
      
      # outcome = BreakL
      
      ######################################      
      ###################################### FOR PERCENTAGE OUTCOMES      
      outcome = rep(NA, nrow(df))
      if(j > 0){

        outcome[df$Percent.Change >= j] = 1
        outcome[df$Percent.Change < j] = 0
      }else{
        outcome[df$Percent.Change <= j] = 1
        outcome[df$Percent.Change > j] = 0
      }
      ######################################      
      ###################################### FOR PERCENTAGE OUTCOMES
      
      outcome = c(outcome, NA)
      outcome = outcome[-1]
      
      # for(z in 1:length(outcome)-1){
      #   if(df$Percent.Change[z + 1] >= j){
      #     outcome[z] = 1
      #   }else{
      #     outcome[z] = 0
      #   }
      # }
      
      
      
      
      # Remove last row from df since we can't use it
      outcome = outcome[-(length(outcome))]
      df = df[-(nrow(df)),]
      
      df = data.frame(df, row.names = NULL)
      # df = df[,c(1:11,12:20)]
      

      
      saveRDS(df, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/df_",file.names[i],".rds"))
      
      ### Remove OPEN HIGH LOW CLOSE
      df = df[,-c(1:4)]
      
      saveRDS(outcome, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/outcome_",file.names[i],j,".rds"))
      # saveRDS(outcome, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/outcome_",file.names[i],"BreakL",".rds"))
      
      
      
      # Remove Previous column for testing
      # df = df[,-ncol(df)]
      
      
      # Split data into train and test
      set.seed(123)
      sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
      
      saveRDS(sample.split, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/sample.split_",file.names[i],j,".rds"))
      # saveRDS(sample.split, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/sample.split_",file.names[i],"BreakL",".rds"))
      
      
      # Remvoe last sample int since I said so
      #sample.split = sample.split[-which(sample.split == nrow(df))]
      
      train = df[sample.split,]
      test = df[!sample.split,]
      
      train = as.matrix(train)
      test = as.matrix(test)
      
      saveRDS(train, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/train_",file.names[i],j,".rds"))
      saveRDS(test, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/test_",file.names[i],j,".rds"))
      # saveRDS(train, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/train_",file.names[i],"BreakL",".rds"))
      # saveRDS(test, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/test_",file.names[i],"BreakL",".rds"))
      
      outcome.train = outcome[sample.split]
      outcome.test = outcome[!sample.split]
      
      
      
      # Created boosted model
      bst = xgboost(data = train,
                    label = outcome.train,
                    objective = "binary:logistic",
                    max.depth = 30,
                    nrounds = 300,
                    eta = 0.3,
                    verbose = FALSE)
      saveRDS(bst, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/bst_",file.names[i],j,".rds"))
      # saveRDS(bst, file = paste0("C:/Users/xbox/Desktop/Rstuff/bsts-7-18-2023/bst_",file.names[i],"BreakL",".rds"))
      print(paste0(file.names[i],j))
    }
  #}
}
tictoc::toc()
