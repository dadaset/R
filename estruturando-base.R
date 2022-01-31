#----Estruturando a base-----------------------------------------------------------------------

# Colocando as observacoes em caixa alta
## Colocando em caixa alta pois Lorem ipsum dolor sit amet consectetur adipiscing, 
## elit semper turpis vivamus ultrices molestie volutpat (justificando)
for(coluna in 1:ncol(base_dados) ){
  
  try( for(linha in 1:nrow(base_dados) ){ 
    base_dados[linha,coluna] <- toupper(base_dados[linha, coluna]) 
  }
  )
  
}
rm(linha, coluna)
