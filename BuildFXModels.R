library(stringr)
library(lubridate)
library(xgboost)
library(quantmod)
library(tictoc)
library(CandleStickPattern)
library(dplyr)
library(riingo)

x = list.files(path = 'TiingoData/',full.names = TRUE)
file.names = list.files('TiingoData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")
ls.files = lapply(x, read.csv)

for(i in 1:length(file.names)){
  
df = ls.files[[i]]

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
df = round(df, 4)

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
outcome = BreakL

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
saveRDS(compare, file = paste0("~/Desktop/R related/bsts/","compare_",file.names[i],"_BreakL.rds"))

compare$pred.value = 0
compare$pred.value[compare$pred >= 0.5] = 1

overall.accuracy = length(which(compare$outcome.test == compare$pred.value)) / nrow(compare) * 100

pred.yes = compare[compare$pred.value == 1,]

pred.yes.accuracy = length(which(pred.yes$outcome.test == pred.yes$pred.value)) / nrow(pred.yes) * 100

saveRDS(bst, file = paste0("~/Desktop/R related/bsts/","bst_",file.names[i],"_BreakL.rds"))
print(file.names[i])
}
