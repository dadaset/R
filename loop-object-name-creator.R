 #criando o vetor de cadernos
vector_Cadernos<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")

for(i in vector_Cadernos) { 
  nam <- paste("Caderno_", i, sep = "")
  assign(nam, subset(select_base, Caderno == i) )
}
