

#adjusted <- sp500$adjusted

# Normalizar los datos
normalized <- (adjusted - min(adjusted)) / (max(adjusted) - min(adjusted))

# Calcular los retornos logarítmicos
returns <- diff(log(normalized))

keep_cols <- c("symbol", "date","adjusted")

# Nuevo data frame con las columnas seleccionadas
sp_500 <- subset(sp_500, select = keep_cols)

saveRDS(sp_SDA, "sp_SDA.rds")

moments_logR <- readRDS("moments_logR.rds")
saveCSV(models_df, "models_df_bueno.csv" )
write.csv(models_df, file = "all_models_arima.csv", row.names = FALSE)

all_models_arima <- read.csv("all_models_arima.csv", header = TRUE)
moments_logR <- reacolnames(datos) <- c("Nombre1", "Nombre2", "Nombre3")

for (col in names(all_models_arima)) {
  # Crear el histograma
  hist(all_models_arima[[col]], main = col, xlab = col, breaks=2)
}
