library(binance)
library(dplyr)


## TestingR

secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"

# secret = "9qhPtPDePdBJnWL5zThAxqrUWXNcv37NYbyDHdkDctoJZGa0CZS6IyPqmqOdIh3i"
# api_key = "wZpij1rDxXsrnyRyuNmuaoLPsVSgJKvmmgt0rzi44GZB03za9GBFqeB6chXi1p0T"

binance::authenticate(key = api_key,secret = secret)

binance::base_url("https://api.binance.us")
# binance::base_url("https://api.binance")


binance::market_ping()

binance::spot_account()

df = binance::market_average_price('DOGEUSDT')
df$price

# check if supported
market_exchange_info()$symbols %>% filter(symbol == "BTCUSDT") %>% pull(order_types)
x = market_exchange_info()$symbols %>% filter(symbol == "BTCUSDT")

x = spot_new_order(
  order_type = "MARKET",
  symbol = "DOGEUSDT",
  side = "BUY",
  quantity = 20,
)


spot_new_order(
  order_type = "MARKET",
  symbol = "DOGEUSDT",
  side = "SELL",
  quantity = 20
  )

spot_new_order(
  order_type = 'STOP_LOSS_LIMIT',
  symbol = 'BTCUSDT',
  side = 'SELL',
  quantity = 1,
  price = 25000,
  stopPrice = 24000,
  time_in_force = 'GTC',
  test = TRUE,
)

order <- spot_new_order(
  order_type = "STOP_LOSS_LIMIT",
  symbol = "BTCUSDT",
  side = "SELL",
  quantity = 1,
  price = 22000,
  time_in_force = "GTC",
  stop_price = 17000,
  test = TRUE
)

order <- spot_new_order(
  order_type = "STOP_LOSS_LIMIT",
  symbol = "DOGEUSDT",
  side = "SELL",
  quantity = 20,
  price = 0.055,
  time_in_force = "GTC",
  stop_price = 0.050,
)

order2 <- spot_new_order(
  order_type = "TAKE_PROFIT_LIMIT",
  symbol = "DOGEUSDT",
  side = "SELL",
  quantity = 20,
  price = 0.075,
  time_in_force = "GTC",
  stop_price = 0.073,
)

