got = read.csv("planilha_got.csv")

moda = function(nota){
  aux = as.numeric(names(which.max(table(nota))))
  return(aux)
}

#Primeira Questão:
print(got)

#Segunda e Terceira Questão:
result2 = moda(got$Nota)
print(result2)

#Terceira Questão:
result3 = moda(got$Audiencia.Em.milhoes.)
print(result3)

#Quarta Questâo:
got = read.csv("planilha_got.csv")
nome = c()
for (i in 1:length(got$Nota)){
  if (got$Nota[i] >= 9){
    nome = c(nome, toString(got$Episodio[i]))
  }
}

#Quinta Questão:
got = read.csv("planilha_got.csv")
len = table(got$Temporada)
t = 1
x = 1
for(i in 1:(t-1)){
  x = x + as.numeric(len[i])
}

print(x)







