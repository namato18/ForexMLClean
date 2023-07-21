library(binance)
library(dplyr)
library(stringr)

str1 = readRDS('tickers/str1.rds')

x = market_exchange_info()$symbols

ind = which(x$symbol %in% str1)

x.ind = x[ind, c("symbol","filters")]

x.ind$step.size = NA
for(i in 1:nrow(x.ind)){
  x.ind$step.size[i] = x.ind$filters[[i]]$stepSize[3]
}

ind.uni = which(duplicated(x.ind$symbol))

x.ind = x.ind[-ind.uni,]  

x.ind = x.ind[,c(1,3)]

x.ind$cleaned.dec = NA
x.ind$decimals = NA

x.ind$cleaned.dec = str_extract(string = x.ind$step.size, pattern = ".*?1")
x.ind$cleaned.dec = str_replace(string = x.ind$cleaned.dec, pattern = "0.", replacement = "")

x.ind$decimals = nchar(x.ind$cleaned.dec)
x.ind$decimals = as.numeric(x.ind$decimals - 1)

x.ind = x.ind[,-c(2,3)]

saveRDS(x.ind, file = "coin_decimals.rds")
