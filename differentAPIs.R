library(httr)
library(jsonlite)

request_tiingo = httr::GET("https://api.tiingo.com/tiingo/fx/usdjpy/prices?startDate=2023-07-17&resampleFreq=1hour&token=6fbd6ce7c9e035489f6238bfab127fcedbe34ac2")
request_polygon = httr::GET("https://api.polygon.io/v2/aggs/ticker/C:USDJPY/range/4/hour/2022-07-21/2023-07-21?adjusted=true&sort=asc&limit=50000&apiKey=DaDI9JX6nlje9VmOLJG_4XJnlYHnMzGf")

# TIINGO
# request_char = rawToChar(request_tiingo$content)
# 
# request_json = jsonlite::fromJSON(request_char, flatten = TRUE)
# 
# df_tiingo = request_json

# POLYGON
request_char = rawToChar(request_polygon$content)

request_json = jsonlite::fromJSON(request_char, flatten = TRUE)

df_polygon = request_json$results

colnames(df_polygon) = c("Volume","Volume Weighted", "Open","Close","High","Low","Time (miliseconds)","Transactions")

