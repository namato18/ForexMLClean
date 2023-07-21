library(binance)
library(aws.s3)
library(stringr)
library(dplyr)
library(riingo)
library(xts)
library(CandleStickPattern)
library(quantmod)
library(purrr)

############################################# 
############################################# INITIALIZE TABLE

purchases_made = data.frame(User = character(),
                            Coin = character(),
                            Price = numeric(),
                            Quantity = numeric(),
                            TakeProfit = numeric(),
                            StopLoss = numeric(),
                            Active = logical(),
                            APIKey = character(),
                            APISecret = character())

############################################# 
############################################# PREPARE SOME THINGS
possibly_spot_new_order = possibly(spot_new_order, otherwise = 'ERROR')
coin_decimals = readRDS('coin_decimals.rds')
readRenviron(".Renviron")


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

credentials = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/APIKeys"), object = "credentials.rds")




############################################# 
############################################# GET INITIAL DATA FROM AWS
x = aws.s3::get_bucket_df("cryptomlbucket")

x.sel = x[grepl(pattern = "Automation/.*/", x = x$Key),]
users = unique(str_match(string = x.sel$Key, pattern = "/(.*)/")[,2])

if(length(users) == 0){
  stop('NO ACTIVE AUTOMATION')
}

############################################# 
############################################# RUN FOR EACH USER
for(i in 1:length(users)){
  x.sel.usr = x.sel[grepl(pattern=users[i], x.sel$Key),]
  path = str_match(string = x.sel.usr$Key[1], pattern = "(.*/.*)/")[,2]
  
  ############################################# 
  ############################################# RUN FOR EACH AUTOMATION FILE WITHIN USER
  for(j in 2:nrow(x.sel.usr)){
    file.name = str_match(string = x.sel.usr$Key[j], pattern = ".*/.*/(.*)")[,2]
    file.usr = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/",path), object = file.name)
    file.usr = left_join(file.usr, credentials, by = 'User')
    
    if(file.usr$Active == TRUE){
      
      ############################################# 
      ############################################# LOGIN TO BINANCE ACCOUNT
      secret = file.usr$APISecret
      api_key = file.usr$APIKey
      binance::authenticate(key = api_key,secret = secret)
      binance::base_url("https://api.binance.us")
      
      ############################################# 
      ############################################# CALCULATE AUTOMATION PURCHASE AMOUNT
      orders = binance::spot_trades_list(file.usr$Coins)
      balances = spot_account_balances()
      free_usdt = balances$free[balances$asset == 'USDT']
      
      usdt_to_use = free_usdt * (file.usr$Percentage / 100)
      
      current_coin_price = round(as.numeric(binance::market_average_price(file.usr$Coins)$price), digits = 4)
      quantity_coin = round(usdt_to_use / current_coin_price, digits = coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins])
      
      ############################################# 
      ############################################# CALCULATE STEP DOWN VALUE TO ENSURE SUCCESSFUL SELL
      # quantity_coin = 20
      
      if(coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins] == 0){
        step_down = quantity_coin - 1
      }else if(coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins] == 1){
        step_down = quantity_coin - 0.1
      }else if(coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins] == 2){
        step_down = quantity_coin - 0.01
      }else if(coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins] == 3){
        step_down = quantity_coin - 0.001
      }else if(coin_decimals$decimals[coin_decimals$symbol == file.usr$Coins] == 4){
        step_down = quantity_coin - 0.0001
      }
      
      ############################################# 
      ############################################# LOOK FOR RECENT TRADE MATCHING AN AUTOMATED PURCHASE
      similar.to.order = which(orders$qty == quantity_coin)
      similar.to.order = orders[similar.to.order,]
      
      
      ############################################# 
      ############################################# WANT TO CLOSE OUR POSITION AFTER 4 HOURS EACH TIME
      # if(similar.to.order$side[nrow(similar.to.order)] == 'BUY'){
      #   
      #   out = possibly_spot_new_order(spot_new_order(
      #     order_type = "MARKET",
      #     symbol = file.usr$Coins,
      #     side = "SELL",
      #     quantity = quantity_coin,
      #     test = TRUE
      #   ))
      #   
      #   if(out[1] == 'ERROR'){
      #     out = possibly_spot_new_order(spot_new_order(
      #       order_type = "MARKET",
      #       symbol = file.usr$Coins,
      #       side = "SELL",
      #       quantity = step_down,
      #       test = TRUE
      #     ))
      #   }
      #   
      #   if(out[1] == 'ERROR'){
      #     print("STILL CAN'T SELL")
      #     stop()
      #   }
      #   
      # }
      
      ############################################# 
      ############################################# STARTING OUR PREDICTION CODE
      ############################################# 
      ############################################# LOAD IN BOOSTED MODEL
      bst = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/bsts"),
                         object = paste0("bst_",file.usr$Coins,file.usr$Timeframe,file.usr$Target,".rds"))
      
      if(file.usr$Timeframe == '4hour' | file.usr$Timeframe == '8hour'){
        df1 = riingo_crypto_prices(file.usr$Coins, end_date = Sys.Date(), resample_frequency = file.usr$Timeframe)
        df1 = df1[-nrow(df1),]
        df2 = riingo_crypto_latest(file.usr$Coins, resample_frequency = file.usr$Timeframe)
        df = rbind(df1,df2)
      }else{
        df = riingo_crypto_prices(file.usr$Coins, end_date = Sys.Date(), resample_frequency = file.usr$Timeframe)
      }
      # Modify data to be more useable
      df = df[,4:8]
      df$Percent.Change = NA
      
      colnames(df) = c("Date","Open","High","Low","Close","Percent.Change")
      df$Percent.Change = round((((df$High / df$Open) * 100) - 100), digits = 1)
      
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
      
      
      # Adding Moving Averages
      df$MA10 = NA
      # df$MA20 = NA
      
      for(k in 21:nrow(df)){
        df$MA10[k] = mean(df$Close[k-10:k])
      }
      
      
      df$Date = as.POSIXct(df$Date, format = "%Y-%m-%d %H:%M:%S")
      
      df = as.xts(df)
      
      
      candle.list = list(hammer(df), inverted.hammer(df), bearish.engulf(df), bullish.engulf(df), up.trend(df), down.trend(df))
      
      # Remove unusable rows
      for(k in 1:length(candle.list)){
        df = cbind(df, candle.list[[k]])
      }
      df = df[-(1:20),]
      
      
      # Add lagged values
      for(k in 1:5){
        high.lag = Lag(df$High, k)
        close.lag = Lag(df$Close, k)
        percent.change.lag = ((high.lag/close.lag) - 1) * 100
        df = cbind(df, percent.change.lag)
        
      }
      
      df = df[-c(1:5),]
      
      df[is.na(df)] = 0
      
      # grab second to last entry since that is the most recent closed candle
      df = df[nrow(df)-1,]
      
      ############################################# 
      ############################################# PREDICT CURRENT CANDLE
      predict.next = predict(bst, df)
      
      ############################################# 
      ############################################# IF WE MEET OUR CONDITIONS, BUY
      if(predict.next >= file.usr$Confidence){
        
        n.ord = possibly_spot_new_order(
          order_type = "MARKET",
          symbol = file.usr$Coins,
          side = "BUY",
          quantity = quantity_coin,
          test = FALSE
        )
        
        ############################################# 
        ############################################# CREATE OUR TABLE FOR TP/SL MONITORING
        if(n.ord == 'ERROR'){
          print("Purchase did not go through")
          stop()
        }else{
          order.df = data.frame(User = file.usr$User,
                                Coin = file.usr$Coins,
                                Price = n.ord$fills[[1]]$price,
                                # Price = 0.0668,
                                Quantity = quantity_coin,
                                TakeProfit = file.usr$TakeProfit,
                                StopLoss = file.usr$StopLoss,
                                Active = TRUE,
                                APIKey = file.usr$APIKey,
                                APISecret = file.usr$APISecret)
          purchases_made = rbind(purchases_made, order.df)
          
        }
        
        
        
        
      }
    }else{
      order.df = data.frame(User = NA,
                                  Coin = file.usr$Coins,
                                  Price = NA,
                                  Quantity = NA,
                                  TakeProfit = NA,
                                  StopLoss = NA,
                                  Active = FALSE,
                                  APIKey = NA,
                                  APISecret = NA)
      purchases_made = rbind(purchases_made, order.df)
      
    }
    
  }
  saveRDS(purchases_made, paste0(tempdir(),'/testing.rds'))
  put_object(
    file = (paste0(tempdir(),'/testing.rds')), 
    object = paste0(users[i],'.rds'), 
    bucket = "cryptomlbucket/ActiveAutomation"
  )
}
