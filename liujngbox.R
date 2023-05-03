library(tidyquant)
library(ggplot2)
library(forecast)
library(dplyr)
library(tidyverse)
library(purr)


sp_500 <- readRDS("/Users/ljeg/Documents/GitHub/ASE_Ciencia_de_Datos/Base_SP500.rds")
extract_ticker <- logreturns(ticker, data = sp_500, 
                           start_date = "2010-01-01",
                           end_date = "2022-12-31", log_returns = TRUE)
{
  
  
  
  
  ticker_data <- ticker_data$adjusted
  
  if(log_returns){
    ticker_data <- diff(log(ticker_data))
  }
  return(ticker_data)
} 



test_df <- data.frame( lag1 = integer(), lag5 = integer(),  lag10 = integer(), stringsAsFactors = FALSE)
testaplay <- function(ticker, start_date, end_date, test_df) {
  
  #nos volamos el codigo paar calcular los retonos
  stock_prices <- sp_500 %>%filter(symbol == ticker) %>% filter(date >= start_date & date <= end_date)
  log_returns <- diff(log(stock_prices$adjusted))
  l1=Box.test(log_returns, lag=1, type="Ljung-Box")$p.value
  l5=Box.test(log_returns, lag=5, type="Ljung-Box")$p.value
  l10=Box.test(log_returns, lag=10, type="Ljung-Box")$p.value
  
  # Creamos un vector con los resultados del modelo ajustado
  test <- c(Lag1 = l1, Lag5 = l5, Lag10 = l10)
  
  # Agregamos el vector al data frame de modelos
  if (!is.null(test_df)) {
    test_df <- rbind(test_df, test)
    return(test_df)
  } else {
    print('No way man!')
  }
}
companies <- unique(sp_500$symbol)
#testaplay("AAPL", "2022-01-01", "2022-12-31")
i=0
for (ticker in companies) {
  test_df <- testaplay(ticker, "2013-01-01", "2023-01-01", test_df)
  print(i)
  i<-i+1
  print(test_df)

}

mean_vector <- c(mean(as.numeric(test_df[, 1])), mean(as.numeric(test_df[, 2])), mean(as.numeric(test_df[, 3])))
print(mean_vector )

colSums((test_df>.05)/502)

test_df$Lag_10 <- replace(test_df$Lag_10, is.na(test_df$Lag_10), mean(as.numeric(test_df[, 3]), na.rm = TRUE))



#----
# 





sp_SDA %>% group_by(symbol) %>%
  summarise(median = median(adjusted),
            mean = mean(adjusted))

log_rets <- function(x) {
  n <- length(x)
  c(NA, log(x[2:n]) - log(x[1:(n-1)]))
}

sp_SDA %>% group_by(symbol) %>%
  mutate(log_adjusted = log_rets(adjusted))


