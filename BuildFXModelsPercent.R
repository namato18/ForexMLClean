library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(tictoc)
library(CandleStickPattern)
library(dplyr)
library(riingo)

tic()

x = list.files(path = 'TiingoData/',full.names = TRUE)
file.names = list.files('TiingoData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")
ls.files = lapply(x, read.csv)

for(i in 1:length(file.names)){
  for(j in seq(from=0.05, to=1, by = 0.05)){
  
  df = ls.files[[i]]
  timeframe = str_match(string = file.names[i], pattern = "_(.*)")[,2]
  
  
  df = df[,-1]
  
  
  ###############################
  ############################### CHANGE NAMES
  colnames(df) = c("Date","Open","High","Low","Close")
  df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
  
  
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

  outcome = rep(NA, nrow(df))
  
  outcome[df$Percent.Change >= j] = 1
  outcome[df$Percent.Change < j] = 0
  
  outcome = c(outcome, NA)
  outcome = outcome[-1]
  
  ###############################
  ############################### NOW REMOVE PERCENT.CHANGE VALUE
  df = df[,-1]
  
  ###############################
  ############################### REMOVE FIRST 20 ROWS AND FIRST 5 COLUMNS FOR INPUT. ALSO REMOVE LAST ROW
  df = df[-c(1:20,nrow(df)),-c(1:5)]
  outcome = outcome[-c(1:20, length(outcome))]
  
  
  ###############################
  ############################### ROUND ALL INPUTS TO 2 DIGITS
  df = round(df, 2)
  
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
  
  outcome.train = outcome[sample.split]
  outcome.test = outcome[!sample.split]
  
  ###############################
  ############################### CREATE XG BOOSTED MODLE
  set.seed(123)
  bst = xgboost(data = train,
                label = outcome.train,
                objective = "binary:logistic",
                max.depth = 20,
                nrounds = 200,
                eta = 0.3,
                verbose = FALSE)
  pred = predict(bst, test)
  
  compare = data.frame(cbind(outcome.test, pred))
  saveRDS(compare, file = paste0("~/Desktop/R related/bsts-8-10-2023/","compare_",file.names[i],j,".rds"))
  
  compare$pred.value = 0
  compare$pred.value[compare$pred >= 0.5] = 1
  
  overall.accuracy = length(which(compare$outcome.test == compare$pred.value)) / nrow(compare) * 100
  
  pred.yes = compare[compare$pred.value == 1,]
  
  pred.yes.accuracy = length(which(pred.yes$outcome.test == pred.yes$pred.value)) / nrow(pred.yes) * 100
  
  saveRDS(bst, file = paste0("~/Desktop/R related/bsts-8-10-2023/","bst_",file.names[i],j,".rds"))
  print(file.names[i])
  }
}

toc()
