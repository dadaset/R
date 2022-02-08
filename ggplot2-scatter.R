library(ggplot2)

iris <- iris

grafico <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point()

grafico

#ênfase para a diferença quando adiciona color = variavel

grafico <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point()

grafico

#adicionando linha de regressão
grafico <- ggplot2::ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = lm, se = T, fullrange = F) +
  ggplot2::labs(title = "Dispersão entre Comprimento e Largura das Pétalas no gênero Iris",
       x = "Comprimento da Pétala",
       y = "Largura da Pétala"
       )
grafico


