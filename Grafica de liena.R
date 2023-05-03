library(tidyquant)
library(ggplot2)


#sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")
plot_log_returns <- function(ticker, start_date, end_date) {
  
  #nos volamos el codigo paar calcular los retonos
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  stock_returns <- diff(log(stock_prices$close))
 
  ggplot(data = data.frame(date = stock_prices$date[-1], returns = stock_returns),
              aes(x = date, y = returns)) +
    geom_line(color = "blue") +
    ggtitle(paste0(ticker, " Daily Log Returns (", start_date, " to ", end_date, ")")) +
    xlab("Date") + ylab("Log Returns")
  
  
  
}

plot_log_returns("KR", "2022-01-01", "2022-12-31")
