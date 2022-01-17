install.packages("ggplot2")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)


getwd()
setwd("C/Users/user/Desktop/Base")

select_base<-read.csv("C:/Users/user/Desktop/Base/select_base.csv", sep = ",")
select_base$X <- NULL

### create boxplot

boxplot_cadernos<-select_base %>% 
  ggplot(aes(x = Caderno, y = Idade, group = Caderno)) +
  geom_boxplot(fatten = NULL)+
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                             width = .75, linetype = "dashed")

### compute mean for the cadernos


tapply(select_base$Idade, select_base$Caderno, mean)
tapply(select_base$Idade, select_base$Caderno, median)
