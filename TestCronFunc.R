library(lubridate)



cond11 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 00:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 00:01:00")))
cond21 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 04:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 04:01:00")))
cond31 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 08:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 08:01:00")))
cond41 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 12:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 12:01:00")))
cond51 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 16:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 16:01:00")))
cond61 = (Sys.time() >= as_datetime(paste0(Sys.Date(), " 20:00:00"))) & (Sys.time() < as_datetime(paste0(Sys.Date(), " 20:01:00")))


if(cond11 | cond21 | cond31 | cond41 | cond51 | cond61){
  source('C:/Users/xbox/Desktop/Rstuff/CryptoMLClean/TestCronFunc.R')
}

source('C:/Users/xbox/Desktop/Rstuff/CryptoMLClean/MonitorTPSL.R')
