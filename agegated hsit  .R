library(tidyquant)
library(ggplot2)
library(dplyr)

calculate_returns <- function(ticker, start_date, end_date, interval) {
  
  sp_500_returns_daily <- sp_500 %>%
    filter(symbol == ticker) %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "yearly", type = "log")
  
  sp_500_returns_grouped <- sp_500_returns_daily %>%
    tq_transmute(select = yearly.returns, 
                 mutate_fun = to_period,
                 period = "years",
                 col_rename = "period_return") %>%
    group_by(date) %>%
    summarize(mean_return = mean(period_return), sd_return = sd(period_return))
  
  sp_500_returns_grouped %>%
    ggplot(aes(x = mean_return, y = ..density..)) +
    geom_histogram(binwidth = .5, alpha = 0.5, color = "black", fill = "gray") +
    geom_density(color = "red") +
    labs(title = paste("Distribución de retornos logarítmicos de", ticker),
         x = "Rendimiento logarítmico", y = "Densidad") +
    theme_minimal()
}

# Ejemplo de uso
calculate_returns("AAPL", "2013-01-01", "2022-12-31", "yearly")
start_date <- "2013-01-01"
end_date <- "2022-12-31"
interval <- "months"
ticker <- "AAPL"
