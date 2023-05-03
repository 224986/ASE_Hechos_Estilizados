library(ggplot2)

#con datafarmes
#Normal baja kurtosis y skewness:

x <- rnorm(1000, mean = 0, sd = 1)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Normal ")
norm_dist <- rnorm(1000, mean = 0, sd = 1)

#Exponential alta kurtosis y low skewness:
x <- rexp(1000, rate = 1)
ggplot(data.frame(x), aes(sample = x)) + stat_qq(color='steelblue') + stat_qq_line(col="tomato") + ggtitle("Exponential ")

#Log-normal baja kurtosis y alta skewness:
x <- rlnorm(1000, meanlog = 0, sdlog = 1)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Log-Normal ")

#Poisson alta kurtosis y alta skewness:
x <- rpois(1000, lambda = 5)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Poisson ")

#Gamma baja kurtosis y baja skewness:
x <- rgamma(1000, shape = 1, rate = 1)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Gamma ")

#Student's t alta kurtosis y negativa skewness:
x <- rt(1000, df = 5)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Student's t")

#Chi-square alta kurtosis y positivs skewness:
x <- rweibull(1000, shape = 1, scale = 1)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Weibull ")

#Beta baja kurtosis y negativa skewness:
x <- rbeta(1000, shape1 = 2, shape2 = 5)
ggplot(data.frame(x), aes(sample = x)) + stat_qq() + stat_qq_line() + ggtitle("Beta ")

x <- rnorm(1000, mean = 0, sd = 1)
qqplot(x,1000, main = "Normal")

#con puero qq plots
y <- rnorm(1000, mean = 0, sd = 1)
qqplot(qnorm(ppoints(500), sd = 3), y,
      main = expression("Q-Q plot para normal"))
qqline(y, distribution = function(p) qnorm(p, sd = 3))
       
y <-rexp(1000, rate = 1)
qqplot(qexp(ppoints(500), rate = 1), y,
       main = expression("Q-Q plot para exp"))
qqline(y, distribution = function(p) qexp(p, rate = 1))
