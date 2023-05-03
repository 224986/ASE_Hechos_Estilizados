par(mfrow=c(2,2))
y <- rnorm(1000, mean = 0, sd = 1)
qqplot(qnorm(ppoints(500), sd = 3), y,
       main = expression("Q-Q plot para normal"))
qqline(y, distribution = function(p) qnorm(p, sd = 3))

norm_dist <- rnorm(1000, mean = 0, sd = 1)

ggplot(data.frame(x = norm_dist)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()
#grid libreria para acomodar