library(dplyr)
library(moments)

# Función para calcular momentos de log returns
calculate_log_return_moments <- function(df) {
  
  # Crear data frame vacío para almacenar los momentos
  moments_logR <- data.frame(symbol = character(),
                             mean = numeric(),
                             median = numeric(),
                             mode = numeric(),
                             skewness = numeric(),
                             kurtosis = numeric(),
                             stringsAsFactors = FALSE)
  
  # Recorrer el data frame de entrada
  for (ticker in unique(df$symbol)) {
    
    # Filtrar por símbolo
    stock_prices <- df %>% filter(symbol == ticker)
    
    # Calcular log returns
    log_returns <- diff(log(stock_prices$adjusted))
    
    # Calcular momentos
    median <- median(log_returns)
    mode <- mode(log_returns)
    skewness <- skewness(log_returns)
    kurtosis <- kurtosis(log_returns)
    mean <- mean(log_returns)
    
    # Agregar resultados al data frame de momentos
    moments_logR <- rbind(moments_logR,
                          data.frame(symbol = ticker,
                                     mean = mean,
                                     median = median,
                                     mode = mode,
                                     skewness = skewness,
                                     kurtosis = kurtosis,
                                     stringsAsFactors = FALSE))
  }
  
  return(moments_logR)
}

moments_logR <- calculate_log_return_moments(sp_SDA)