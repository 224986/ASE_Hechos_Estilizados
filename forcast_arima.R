# Cargar la serie de tiempo
ticker<-"HLT"
start_date<-"2022-01-01"
end_date<-"2022-01-01"

arima_forecast <- function(ticker, start_date = "2022-01-01" , end_date = "2022-01-01") {
stock_prices <- sp_500 %>%
  filter(symbol == ticker) %>%
  filter(date >= start_date & date <= end_date)
#Calcualmos los retornos logratimicos con esta base filtarda
stock_returns <- diff(log(stock_prices$close))

# Ajustar un modelo ARIMA con la función auto.arima()
fit_arima <- auto.arima(stock_returns)
print(fit_arima)
# Generar una predicción de 12 meses con el modelo ARIMA ajustado
forecast_arima <- forecast(fit_arima, h = 6)

# Imprimir los resultados
print(ticker)
print(forecast_arima)

# Graficar la predicción del modelo ARIMA
plot(forecast_arima, main = ticker)
}
# Graficar la predicción del modelo ARIMA con observaciones entre los índices 2400 y 2500
plot(forecast_arima, xlim = c(time(stock_returns)[2400], time(stock_returns)[2500 + 10]), 
     main = "Predicción de ventas minoristas con modelo ARIMA (observaciones entre los índices 2400 y 2500)")

resultados <- lapply(companies, arima_forecast)
