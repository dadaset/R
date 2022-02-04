library(readxl)
library(stringr)
renomeacao<-read_excel("C:/Users/dadaset/Desktop/LEPES/renomeacao20220203.xlsx")

# Para todas as linhas em que a coluna renomeacao$nomes_originais termina com 
# "extra", substituir inserir na coluna seguinte F depois do primeiro dígito

vetor_extra<-str_detect(renomeacao$nomes_originais, "extra")
tibble_extras<- renomeacao[vetor_extra,]

tibble_extras$nomes_padronizados <- gsub(
  "V",
  "VF",
  tibble_extras$nomes_padronizados
  )

tibble_extras$nomes_padronizados <- gsub(
  "FF",
  "F",
  tibble_extras$nomes_padronizados
)

renomeacao_1 <- merge(
  renomeacao,
  tibble_extras,
  by = "nomes_originais",
  all.x = T
)

renomeacao_1[!is.na(renomeacao_1$nomes_padronizados.y),"nomes_padronizados.x"] <- renomeacao_1[!is.na(renomeacao_1$nomes_padronizados.y),"nomes_padronizados.y"]

renomeacao_1 <- renomeacao_1[,c("nomes_originais","nomes_padronizados.x")]
