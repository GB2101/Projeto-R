#Projeto de Estatística
#Gabriel Braz Cavalcante Silva (gbcs)
#Eduardo Guimarães Medeiros (egm3)

got = read.csv("planilha_got.csv")

#Primeira Questão:
print(got)

#Segunda e Terceira Questão:
#  Utilizamos a função table() para calcular a moda, tal função retorna a frequência de cada elemento do vetor,
# depois obtemos o elemento de maior frequência.
moda = function(nota){
  aux = as.numeric(names(which.max(table(nota))))
  return(aux)
}
result1 = mean(got$Nota)
result2 = sd(got$Nota)
result3 = moda(got$Nota)
print(result1)
print(result2)
print(result3)

#Terceira Questão:
# Todas as funções já estavam implementadas na linguagem R
result1 = mean(got$Audiencia.Em.milhoes.)
result2 = sd(got$Audiencia.Em.milhoes.)
result3 = median(got$Audiencia.Em.milhoes.)
print(result1)
print(result2)
print(result3)

#Quarta Questâo:
# Itera pelo vetor de notas, e armazena o nome dos episódios de nota >= a 9
nota_9 = function(){
  nome = c()
  for (i in 1:length(got$Nota)){
    if (got$Nota[i] >= 9){
      nome = c(nome, toString(got$Episodio[i]))
    }
  }
  return(nome)
}
res = nota_9()
print(res)

#Quinta Questão:
# A função minimax() encontra onde começa e termina a temporada t; e retorna o episódio de menor e maior nota da temporada.
# A função notas() utiliza a função minimax() para todas as temporadas, armazenando em um dataframe os episódios e as notas.

b = length(table(got$Temporada))
minimax = function(t){
  len = table(got$Temporada)
  s = 0
  m = data.frame(nm = c(), nt = c())
  if (t > 1){
    for(i in 1:(t-1)){
      s = s + as.numeric(len[i])
    }
  }
  x = s + 1
  for (i in 1:len[t]){
    m = data.frame(nm = c(as.character(m$nm), as.character(got$Episodio[x])), nt = c(m$nt, got$Nota[x]))
    x = x + 1
  }
  a = which.max(m$nt) + s
  b = which.min(m$nt) + s
  maximo = data.frame(ep_max = got$Episodio[a], nt_max = got$Nota[a])
  minimo = data.frame(ep_min = got$Episodio[b], nt_min = got$Nota[b])
  resp = data.frame(minimo, maximo)
  return(resp)
}

notas = function(){
  ep = c()
  nt = c()
  te = c()
  for (i in 1:b) {
    aux = minimax(i)
    ep = c(ep, toString(aux$ep_min), toString(aux$ep_max))
    nt = c(nt, aux$nt_min, aux$nt_max)
    te = c(te, i, i)
  }
  resp = data.frame(TÍTULO = ep, NOTA = nt, TEMPORADA = te)
  return(resp)
}

res = notas()
print(res)

#Sexta Questão:
# Equanto iteramos pelo vetor de temporadas, armazenamos os elementos de cada temporada.
# Quando mudamos de temporada, calculamos o desvio padrão da temporada anterior e o armazenamos em um vetor.
# Por fim retornamos a temporada de menor desvio padrão.
desivo_padrao = function(){
  c = length(got$Temporada)
  t = 1
  dp = c()
  au = c()
  for (i in 1:c){
    if (got$Temporada[i] == t){
      au = c(au, got$Audiencia.Em.milhoes.[i])
    }else {
      dpt = sd(au)
      dp = c(dp, dpt)
      t = t + 1
      au = c(got$Audiencia.Em.milhoes.[i])
    }
  }
  return(which.min(dp))
}
res = desivo_padrao()
print(res)


#Sétima Questão
# Itera pelo vetor de personagens e armazena os episódios em que ele aparece.
personagem = function(nome){
  c = length(got$Temporada)
  m = c()
  for (i in 1:c){
    a = got$Personagens[i]
    p = unlist(strsplit(as.character(a), ","))
    if(any(p == nome)) {
      m = c(m, got$Nota[i])
    }
  }
  me = mean(m)
  return(me)
}
nome = "Brienne of Tarth(Gwendoline Christie)"
res = personagem(nome)
print(res)


#Oitava Questão:
# Primeiramente, encontramos os episódios em que a temporada t começa e termina.
# Então armazenamos todos os personagens de cada episódio em um vetor. Em seguida usamos a função table() para termos
# a frequência de cada personagem. Por fim, retornamos os personagens que possuem frequência = 1.
unico_episodio = function(t){
  len = table(got$Temporada)
  s = 1
  if (t > 1){
    for(i in 1:(t-1)){
      s = s + as.numeric(len[i])
    }
  }
  p = c()
  for (i in 1:len[t]){
    a = got$Personagens[s]
    au = unlist(strsplit(as.character(a), ","))
    p = c(p, au)
    s = s + 1
  }
  g = table(p)
  p= c()
  for(i in 1:length(g)) {
    if (g[i] == 1){
      p = c(p, names(g[i]))
    }
  }
  return(p)
}

t = 4
res = unico_episodio(t)
print(res)


#Nona Questão:
# De início, calculamos a frequência do personagem em cada temporada, para isso utilizamos um metódo
# semelhante ao da sexta questão. Dessa forma, utilizamos um inteiro, indicando a temporada, para  
# cada episódio que o personagem aparece. Por fim, passamos esse vetor para o histograma.
histograma = function(nome) {
  c = length(got$Temporada)
  t = 1
  i = 1
  qq = c()
  while (i <= c){
    if (got$Temporada[i] == t){
      a = got$Personagens[i]
      au = unlist(strsplit(as.character(a), ","))
      if (any(au == nome)){
        qq = c(qq, t)
      }
      i = i + 1
    }else {
      t = t + 1
    }
  }
  brk = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  color = c("hot pink","red","orange","yellow","green", "turquoise", "blue", "purple")
  xl = "Temporada"
  yl = "Ocorrência"
  graph = hist(qq, main = nome, xlab = xl, ylab = yl, xlim = c(0, 9), ylim = c(0, 10), right = TRUE, breaks = brk, col = color, border = "black")
} 
#Cersei Lannister(Lena Headey)
#Bran Stark(Isaac Hempstead)
#"Eddard 'Ned' Stark(Sean Bean)"
nome = "Cersei Lannister(Lena Headey)"
histograma(nome)
