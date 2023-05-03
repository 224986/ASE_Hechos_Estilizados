library(forecast)
forecsted <- function(ticker, start_date, end_date, test_df) {
  
  #nos volamos el codigo paar calcular los retonos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$adjusted))
  sarima_model <- arima(log_returns$log_returns, order=c(1,1,1), seasonal=c(0,1,1))
  forecast_values <- sarima.for(sarima_model, h=10)
  plot(forecast_values)
}

