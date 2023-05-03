library(tidyverse)

#Normal Baja Kurtosis and Skewness
norm_dist <- rnorm(1000, mean = 0, sd = 1)

ggplot(data.frame(x = norm_dist)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()

#Log-Normal Distribution Alta Kurtosis and Skewness
lognorm_dist <- rlnorm(1000, meanlog = 0, sdlog = 1)

ggplot(data.frame(x = lognorm_dist)) + 
  geom_qq(aes(sample = x)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()

# Student's t  
t_dist <- rt(1000, df = 5)

ggplot(data.frame(x = t_dist)) + 
  geom_qq(aes(sample = x)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()

# Cauchy  
cauchy_dist <- rcauchy(1000, location = 0, scale = 1)

ggplot(data.frame(x = cauchy_dist)) + 
  geom_qq(aes(sample = x)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()

# Exponential 
exp_dist <- rexp(1000, rate = 1)

ggplot(data.frame(x = exp_dist)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()

# Skewed Exponential Distribution (High Kurtosis and Skewness)
skew_exp_dist <- rsexp(1000, location = 0, shape = 1)

ggplot(data.frame(x = skew_exp_dist)) + 
  geom_qq(aes(sample = x)) + 
  geom_histogram(aes(x), binwidth = 0.5, right = TRUE, fill = "blue", alpha = 0.5) + 
  theme_classic()


