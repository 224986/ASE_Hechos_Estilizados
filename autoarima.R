library(tidyquant)
library(ggplot2)
library(forecast)
library(plyr)
library(tseries)

sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")
models_df <- data.frame(name = character(), AR = integer(), I = integer(),  MA = integer(), stringsAsFactors = FALSE)

model_log_returns <- function(ticker, 
                              start_date = "2013-01-01",
                              end_date = "2022-12-31") {
  
  # Calculamos los retornos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$close))
  
  # Determinamos el modelo ARIMA que mejor ajusta a los datos de log returns
  arima_model <- auto.arima(log_returns)
  
  
  # Creamos un vector con los resultados del modelo ajustado
  c(AR = arima_model$arma[1], I = arima_model$arma[2], MA = arima_model$arma[6])
}

arimass<-laply(companies,model_log_returns )

