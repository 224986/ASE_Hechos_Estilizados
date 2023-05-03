library(tidyquant)
library(ggplot2)
library(forecast)
library(dplyr)
library(tseries)

sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")

plot_log_returns_tsdisplay <- function(ticker, start_date, end_date) {
  
  # Calculamos los retornos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$close))
  
  # Graficamos la autocorrelación
  tsdisplay(log_returns, lag.max = 9, main = paste(ticker, "Autocorrelation"))
  
}

plot_log_returns_tsdisplay("NXPI", "2022-01-01", "2022-12-31")

