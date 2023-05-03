sum_vector <- c(728, 11, 698)

# Crear un vector para etiquetar las barras
labels <- c("AR", "I", "MA")

# Crear la gráfica de barras
barplot(sum_vector,main = "Resulatdos auto.arima|", names.arg = labels, ylab = "Frecuencia")
par(mar = c(5, 4, 4, 4) + 0.1)
colSums(all_models > 0)
