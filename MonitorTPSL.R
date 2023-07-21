library(aws.s3)
library(binance)
library(stringr)
library(lubridate)
library(purrr)
############################################# 
############################################# CREATE SELL FUNCTION
possibly_spot_new_order = possibly(spot_new_order, otherwise = 'ERROR')

sell.crypto <- function(df){
  out = possibly_spot_new_order(spot_new_order(
    order_type = "MARKET",
    symbol = df$Coin[j],
    side = "SELL",
    quantity = df$Quantity[j],
    test = FALSE
  ))
  
  if(out[1] == 'ERROR'){
    out = possibly_spot_new_order(spot_new_order(
      order_type = "MARKET",
      symbol = df$Coin[j],
      side = "SELL",
      quantity = df$step_down[j],
      test = FALSE
    ))
  }
  df$Active[j] = FALSE
}


############################################# 
############################################# SET TIMEZONE TO UTC
Sys.setenv(TZ='UTC')

############################################# 
############################################# END OF CANDLE TIME CONDITIONS
cond1 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 23:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 23:59:30")))
cond2 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 03:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 03:59:30")))
cond3 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 07:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 07:59:30")))
cond4 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 11:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 11:59:30")))
cond5 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 15:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 15:59:30")))
cond6 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 19:58:30"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 19:59:30")))

############################################# 
############################################# PREPARE SOME THINGS


secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"
binance::authenticate(key = api_key,secret = secret)
binance::base_url("https://api.binance.us")

coin_decimals = readRDS('coin_decimals.rds')
readRenviron(".Renviron")


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

credentials = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/APIKeys"), object = "credentials.rds")

############################################# 
############################################# READ IN ACTIVE AUTOMATION

x = aws.s3::get_bucket_df("cryptomlbucket")

x.sel = x[grepl(pattern = "ActiveAutomation/.*", x = x$Key),]
user.files = unique(str_match(string = x.sel$Key, pattern = "/(.*)")[,2])
user.files = user.files[user.files != ""]

if(length(user.files) == 0){
  stop("NO AUTOMATION FILES PRESENT")
}

############################################# 
############################################# RUN THIS CODE FOR EACH USERS ACTIVE AUTOMATION
for(i in 1:length(user.files)){
  df = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/ActiveAutomation"), object = user.files[i])
  if(nrow(df) == 0){
    print('no purchases for this user')
    next()
  }
  df$TakeProfitPrice = df$Price * (1 + (df$TakeProfit / 100))
  df$StopLossPrice = df$Price * (1 - (df$StopLoss / 100))
  
  for(j in 1:nrow(df)){
    if(df$Active[j] == TRUE){
      ############################################# 
      ############################################# GET CURRENT PRICE AND STEP DOWN FOR EACH COIN
      current_price = binance::market_average_price(df$Coin[j])$price
      
      if(coin_decimals$decimals[coin_decimals$symbol == df$Coin[j]] == 0){
        df$step_down[j] = df$Quantity[j] - 1
      }else if(coin_decimals$decimals[coin_decimals$symbol == df$Coin[j]] == 1){
        df$step_down[j] = df$Quantity[j] - 0.1
      }else if(coin_decimals$decimals[coin_decimals$symbol == df$Coin[j]] == 2){
        df$step_down[j] = df$Quantity[j] - 0.01
      }else if(coin_decimals$decimals[coin_decimals$symbol == df$Coin[j]] == 3){
        df$step_down[j] = df$Quantity[j] - 0.001
      }else if(coin_decimals$decimals[coin_decimals$symbol == df$Coin[j]] == 4){
        df$step_down[j] = df$Quantity[j] - 0.0001
      }
      
      ############################################# 
      ############################################# CHECK FOR TAKE PROFIT AND STOP LOSS SELLS
      if(current_price >= df$TakeProfitPrice){
        sell.crypto(df)
      }
      
      if(current_price <= df$StopLossPrice[j]){
        sell.crypto(df)
      }
      
      ############################################# 
      ############################################# CLOSE OUR POSITION IF END OF CANDLE
      if(cond1 | cond2 | cond3 | cond4 | cond5 | cond6){
        sell.crypto(df)
      }
    }
  }

}
