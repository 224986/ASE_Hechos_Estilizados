library(tidyquant)
library(ggplot2)
library(dplyr)

# Obtener datos de Microsoft
MSFT <- tq_get("MSFT",
               get = "stock.prices",
               from = "2010-01-01",
               to = "2021-04-23")

# Calcular volatilidad
MSFT %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            type = "log") %>%
  tq_mutate(select = adjusted,
            mutate_fun = runSD,
            n = 20,
            col_rename = "volatility") %>%
  ggplot(aes(x = date, y = volatility)) +
  geom_line() +
  ggtitle("Volatilidad agrupada de Microsoft") +
  ylab("Volatilidad") +
  xlab("Fecha")
