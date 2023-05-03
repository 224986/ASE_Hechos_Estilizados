library(tidyquant)
library(ggplot2)


#sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")
plot_log_returns <- function(ticker, start_date, end_date) {

#nos volamos el codigo paar calcular los retonos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$close))
  stock_returns <- diff(log(stock_prices$close))
#usamos u gg plot para el histograma de los retornos 
  ggplot(data.frame(log_returns), aes(x = log_returns)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    labs(x = "Log Returns", y = "Frequency", title = paste(ticker, "Log Returns")) +
    theme_bw()
  
}

plot_log_returns("KR", "2022-01-01", "2022-12-31")




