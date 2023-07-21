library(riingo)
library(dplyr)
library(tictoc)
library(purrr)

tictoc::tic()
Sys.setenv(TZ='UTC')

possibly_riingo_crypto_prices = possibly(riingo_crypto_prices, otherwise = "ERROR")

########################################## READ IN NAMES OF COINS WERE INTERESTED IN AND REMOVE BAD COINS
########################################## 
str1 = readRDS('tickers/str1.rds')
str1 = str1[-61]

########################################## SET TIMEFRAMES
########################################## 
timeframes = c("4hour","8hour","1day")
timeframes.n = c(240,480,1440)

########################################## SET UP LOOPS, ONCE FOR EACH TIMEFRAME
########################################## 
for(i in 1:length(timeframes)){
  days.back = floor(50 * (timeframes.n[i] / 15))
  for(j in 1:length(str1)){
    
    # GRAB TIINGO DATA
    df1 = possibly_riingo_crypto_prices(ticker = str1[j], resample_frequency = timeframes[i], end_date = Sys.Date(), start_date = Sys.Date() - days.back)
    df2 = possibly_riingo_crypto_prices(ticker = str1[j], resample_frequency = timeframes[i], end_date = Sys.Date() - days.back, start_date = Sys.Date() - days.back*2)
    if(df2[1] == 'ERROR'){
      df = df1
    }else{
      # REMOVE FIRST ROW FROM DF1 TO AVOID DUPLICATE
      df1 = df1[-1,]
      
      # COMBINE DF1 AND DF2
      df = rbind(df2, df1)
    }

    df = df[,-c(2,3)]
    
    # WRITE .CSV
    write.csv(df, paste0("C:/Users/xbox/Desktop/Rstuff/RiingoPulledData/", str1[j],"_",timeframes[i],".csv"))
    
    print(j)
  }
}

tictoc::toc()

