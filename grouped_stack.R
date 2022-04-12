# Gera um gráfico que mostra a dsitribuição de uma determinada variável para diferentes grupos 



library(ggplot2)

cor <- c(rep("Amarelo", 3) ,
            rep("Branco" , 3), 
            rep("Indígena", 3),
            rep("Parda", 3),
            rep("Preta", 3),
            rep("Não declarada",3)
            )
  condition <- rep(c("F" , "E" , "D", "C", "B", "A"), 3)
# value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(cor,condition)

# Grouped
ggplot(data, aes(fill=as.factor(condition), y=condition, x=cor)) + 
    geom_bar(aes(y = ..prop.., group = condition), position = "dodge") +
    theme(
    legend.position = "none",  
    panel.background = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
    )

  
