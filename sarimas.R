# Cargar la serie de tiempo
ticker<-"SYY"
start_date<-"2013-01-01"
end_date<-"2023-01-01"
SYY
5
3
0

stock_prices <- sp_500 %>%
  filter(symbol == ticker) %>%
  filter(date >= start_date & date <= end_date)
#Calcualmos los retornos logratimicos con esta base filtarda
stock_returns <- diff(log(stock_prices$close))

ts_data <- diff(log(stock_prices$adjusted))

# Ajustar un modelo SARIMA
sarima_model <- arima(ts_data, order = c(5, 3, 0))

# Hacer predicciones para los próximos 10 períodos
sarima_pred <- sarima.for(model = sarima_model, n.ahead = 10, x = ts_data, level = 0.95, pred.var = "residuals", ci = TRUE)

#prosemeito para saber la frecuencia
sp500_ts <- ts(stock_returns, frequency = 252)


arima_model <- arima(sp500_ts, order = c(5, 3, 0))
arima_resid <- resid(arima_model)

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                         mean.model = list(armaOrder = c(0,0)), 
                         distribution.model = "norm")

garch_fit <- ugarchfit(spec = garch_spec, data = arima_resid)
summary(garch_fit)
garch_forecast <- ugarchforecast(garch_fit, n.ahead = 10)
ugarchboot()
plot(garch_forecast)
plot(garch_fit )
