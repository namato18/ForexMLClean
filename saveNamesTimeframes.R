library(stringr)

file.names = list.files('TiingoData/')
file.names = str_replace(string = file.names, pattern = '.csv', replacement = "")

pair.names = str_match(string = file.names, pattern = "(.*)_")[,2]
pair.names = unique(pair.names)

pair.timeframes = str_match(string = file.names, pattern = "_(.*)")[,2]
pair.timeframes = unique(pair.timeframes)

saveRDS(pair.names,"tickers/fx.pair.names.rds")
saveRDS(pair.timeframes,"tickers/fx.pair.timeframes.rds")
