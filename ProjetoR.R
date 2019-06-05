got = read.csv("planilha_got.csv")

moda = function(nota){
  aux = as.numeric(names(which.max(table(nota))))
  return(aux)
}

#Primeira Questão:
print(got)

#Segunda e Terceira Questão:
result2 = moda(got$Nota)
result3 = moda(got$Audiencia.Em.milhoes.)
print(result2)
print(result3)
