library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(tictoc)
library(CandleStickPattern)
library(dplyr)
library(riingo)

tic()
out.names = c("Open","High","Low","Close")

x = list.files(path = 'TiingoData/',full.names = TRUE)
file.names = list.files('TiingoData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")
ls.files = lapply(x, read.csv)

for(i in 1:length(file.names)){
  for(j in 2:5){
  df = ls.files[[i]]
  timeframe = str_match(string = file.names[i], pattern = "_(.*)")[,2]
  
  df = df[,-1]
  
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close")
  
    ###############################
    ############################### ADD VECTOR OUTPUT FOR NEXT HIGH VALUE
    NextOut = df[[j]]
    NextOut[length(NextOut)+1] = NA
    NextOut = NextOut[-1]
    
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
    df = df[-c(1:20,nrow(df)),-c(1:4)]
    BreakL = BreakL[-c(1:20,length(BreakL))]
    BreakH = BreakH[-c(1:20,length(BreakH))]
    NextOut = NextOut[-c(1:20,length(NextOut))]
    
    
    ###############################
    ############################### ROUND ALL INPUTS TO 2 DIGITS
    # df = round(df, 4)
    
    ###############################
    ############################### BREAK DATA INTO TRAIN AND TEST SETS AND MAKE INTO MATRICES
    set.seed(123)
    sample.split = sample(c(TRUE,FALSE), nrow(df), replace = TRUE, prob=c(0.8,0.2))
    
    train = df[sample.split,]
    test = df[!sample.split,]
    
    train = as.matrix(train)
    test = as.matrix(test)
    
    ###############################
    ############################### SET OUTPUT VALUE
    outcome = NextOut
    
    outcome.train = outcome[sample.split]
    outcome.test = outcome[!sample.split]
    
    ###############################
    ############################### CREATE XG BOOSTED MODLE
    bst = xgboost(data = train,
                  label = outcome.train,
                  objective = "reg:linear",
                  max.depth = 20,
                  nrounds = 200,
                  eta = 0.3,
                  verbose = FALSE)
    pred = predict(bst, test)
    
    compare = data.frame(cbind(outcome.test, pred))
    saveRDS(compare, file = paste0("../bsts-8-18-2023/","compare_",file.names[i],"_",out.names[j-1],".rds"))
    # 
    #   compare$residuals = compare$outcome.test - compare$pred
    #   dmean2 = sum((compare$outcome.test - mean(compare$outcome.test))^2)
    #   dmeanpred2 = sum((compare$pred - mean(compare$outcome.test))^2)
    # 
    #   plot(compare$pred, compare$residuals)
    # 
    #   R2 = dmeanpred2/dmean2
    # 
    #   mse = mean((compare$residuals^2))
    #   rmse = (mean((compare$residuals^2)))^(1/2)
    
    saveRDS(bst, file = paste0("../bsts-8-18-2023/","bst_",file.names[i],"_",out.names[j-1],".rds"))
    print(paste0(file.names[i],"_",out.names[j-1]))
  }
}
toc()
