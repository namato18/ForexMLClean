library(riingo)
library(dplyr)
library(tictoc)
library(purrr)

tictoc::tic()
Sys.setenv(TZ='UTC')

possibly_riingo_fx_prices = possibly(riingo_fx_prices, otherwise = "ERROR")

########################################## READ IN NAMES OF COINS WERE INTERESTED IN AND REMOVE BAD COINS
########################################## 
tickers = c("EURUSD","GBPUSD","AUDUSD","USDJPY","USDCAD")

########################################## SET TIMEFRAMES
########################################## 
timeframes = c("1hour","4hour","8hour","1day")
timeframes.n = c(60, 240, 480, 1440)

########################################## SET UP LOOPS, ONCE FOR EACH TIMEFRAME
########################################## 
for(i in 1:length(timeframes)){
  days.back = floor(50 * (timeframes.n[i] / 15))
  if(days.back > 1000){
    days.back = 1000
  }
  for(j in 1:length(tickers)){
    
    # GRAB TIINGO DATA
    df1 = possibly_riingo_fx_prices(ticker = tickers[j], resample_frequency = timeframes[i], end_date = Sys.Date(), start_date = Sys.Date() - days.back)
    df2 = possibly_riingo_fx_prices(ticker = tickers[j], resample_frequency = timeframes[i], end_date = Sys.Date() - days.back, start_date = Sys.Date() - days.back*2)
    if(df2[1] == 'ERROR'){
      df = df1
    }else{
      # REMOVE FIRST ROW FROM DF1 TO AVOID DUPLICATE
      df1 = df1[-1,]
      
      # COMBINE DF1 AND DF2
      df = rbind(df2, df1)
    }
    
    df = df[,-c(1)]
    
    # WRITE .CSV
    write.csv(df, paste0("TiingoData/", tickers[j],"_",timeframes[i],".csv"))
    
    print(j)
  }
}

tictoc::toc()
