library(tidyquant)
library(ggplot2)
library(gridExtra)

# Descargamos toda la informacion disponible de todos los activos dentro del SP500
# sp_500 <- tq_index("SP500") %>% tq_get(get="stock.prices")

# Creamos una función que grafique el qqplot e histogarama de los retornos logaritmicos 
plot_log_returns <- function(ticker, start_date, end_date) {
  
  # Primero firltramos para encontar el activo y las fechas deseadas
  stock_prices <- sp_500 %>%
    filter(symbol == ticker) %>%
    filter(date >= start_date & date <= end_date)
  
  # Calcualmos los retornos logratimicos con esta base filtarda
  stock_returns <- diff(log(stock_prices$close))
  
  # Creamos las graficas de qq plot e hitorgrama, e igualamos a otra variable para poder organizar las graficas
  p1 <- ggplot(data.frame(stock_returns), aes(sample = stock_returns)) +
    stat_qq() +
    stat_qq_line(color="red") +
    ggtitle(paste0(ticker, " Daily Log Returns (", start_date, " to ", end_date, ")")) 
  
  p2 <- ggplot(data.frame(stock_returns), aes(x = stock_returns)) +
    geom_histogram(fill = "blue", alpha = 0.5, bins = seq(min(stock_returns), max(stock_returns), by = 0.01)) +
    labs(x = "Log Returns", y = "Frequency")
  
  # Utilizamos la libreria grid extra para poder organizar en las garfocas una alado de la otra 
  grid.arrange(p1, p2, nrow = 1) 
  
  # Para poder usar y visualizar las garfcas con mayor comidadi se descaegan en formato png
  ext <- ".png"
  # ggsave(filename = paste(ticker, ext), plot = grid.arrange(p1, p2, nrow = 1), width = 15, height = 8)
  
}

# Llamamos solo 1 vez la funcion con la compañia y fecha desada
plot_log_returns("AAPL", "2013-01-01", "2023-01-01")

# Llamamsos a la función para todas las empresas en las fechas deseadas
i <- 1
# Sacamos todos lo tickets de las empresas 
companies <- unique(sp_500$symbol)
# Llamamos la función para todas las empresas en las fechas deseadas
for (ticker in companies) {
  plot_log_returns(ticker, "2013-01-01", "2023-01-01")
  print(i)
  i <- i + 1
}
