mis_predicciones <- function(ticker, start_date = "2013-01-01",
                             end_date = "2022-12-31") {
  
  # Calculamos los retornos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker,
           date >= start_date & date <= end_date)
  
  log_returns <- diff(log(stock_prices$adjusted))
  
  # Determinamos el modelo ARIMA que mejor ajusta a los datos de log returns
  arima_model <- auto.arima(log_returns)
  
  # Obtener los residuos
  residuos <- arima_model
  
  # Ajustar modelo GARCH a los residuos
  modelo_garch <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                             variance.model = list(model = "gjrGARCH"),
                             distribution.model = "sged")
  ajuste_garch <- ugarchfit(data = log_returns, spec = modelo_garch)
  
  # Obtener las predicciones
  n.ahead <- 1000
  pred <- ugarchforecast(ajuste_garch, n.ahead = n.ahead)
  
  # Obtener los intervalos de confianza con bootstrap
  bootp <- ugarchboot(ajuste_garch, method = c("Partial", "Full")[1], 
                      n.ahead = 500, n.bootpred = 500)
  print(bootp)
  print(ticker)
  plot(bootp)
  #ci <- ugarchbootforecast(pred, bootp, n.bootpred = n.ahead)
  
  # Devolver las predicciones y los intervalos de confianza
  #return(list(pred = pred, ci = ci))
}


mis_predicciones("AAPL")
resultados <- lapply(companies, mis_predicciones)
