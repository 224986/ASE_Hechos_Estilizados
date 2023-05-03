library(tidyquant)
library(ggplot2)
library(forecast)
library(dplyr)

sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")
plot_log_returns_coralation <- function(ticker, start_date, end_date) {
  
  #nos volamos el codigo paar calcular los retonos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$adjusted))
  stock_returns <- diff(log(stock_prices$adjusted))
  
  ggAcf(log_returns, lag.max = 30) + #Funcion que grafica la cutocorelacion con lag hastas de 30 
    labs(x = "Lag", y = "Autocorrelation", title = paste(ticker, "Autocorrelation")) +
    theme_bw()
  
}
models_df <- data.frame(model_name = character(), model_count = integer(), stringsAsFactors = FALSE)

plot_log_returns_coralation("AAPL", "2022-01-01", "2022-12-31")
