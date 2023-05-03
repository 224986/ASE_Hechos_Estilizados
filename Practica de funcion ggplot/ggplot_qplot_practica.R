#install.packages("tidyverse")
#install.packages("stringi")

library(tidyverse)
set.seed(0) 

#q plot de normal
normal<- rexp(1000, rate = 1)
qplot(sample=normal)

#pasamos a un data farme para usar ggplot
df<-data.frame(normal)
ggplot(df, aes(sample=normal))+
    geom_qq()+
    geom_qq_line()


qplot(x=normal)

#agregamos otra columna a el data farme 
df$group<-factor(sample(c(0,1), 100, replace = TRUE))#100 exp bernuli
ggplot(df, aes(sample=normal), cols=group)+
  geom_qq()+ #se genrea el qqplot
  geom_qq_line()+ #la linea del plot 
  facet_wrap(~group)+#secuencia de paneles
  theme_minimal()#displey visual minimo

  
  
