#########################compute anova######################################

library(tidyverse)

select_base<-read.csv("C:/Users/user/Desktop/Base/select_base.csv", sep = ",")
select_base$X <- NULL

### é necessário ter uma tabela com duas variáveis. A primeira é a variável de grupo e a segunda é o valor que será testado


anova<-aov(lm(Idade ~ Caderno, select_base))
anova1 <- aov(Idade ~ Caderno, select_base)
summary(anova1)

###compute post-hoc analysis
install.packages("DescTools")
library(DescTools)
PostHocTest(anova1, method = "duncan", conf.level = 0.95)
