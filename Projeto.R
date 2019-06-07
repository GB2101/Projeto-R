#Projeto de Estatística
#Gabriel Braz Cavalcante Silva (gbcs)
#Eduardo Guimarães Medeiros (egm3)

got = read.csv("planilha_got.csv")

moda = function(nota){
  aux = as.numeric(names(which.max(table(nota))))
  return(aux)
}

#Primeira Questão:
print(got)

#Segunda e Terceira Questão:
result1 = mean(got$Nota)
result2 = sd(got$Nota)
result3 = moda(got$Nota)
print(result1)
print(result2)
print(result3)

#Terceira Questão:
result1 = mean(got$Audiencia.Em.milhoes.)
result2 = sd(got$Audiencia.Em.milhoes.)
result3 = median(got$Audiencia.Em.milhoes.)
print(result1)
print(result2)
print(result3)

#Quarta Questâo:
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
  color = c("blue", "gray")
  xl = "Temporada"
  yl = "Ocorrência"
  graph = hist(qq, main = nome, xlab = xl, ylab = yl, xlim = c(0, 9), ylim = c(0, 10), right = TRUE, breaks = brk, col = color, border = "black")
} 
#Cersei Lannister(Lena Headey)
#Bran Stark(Isaac Hempstead)
#"Eddard 'Ned' Stark(Sean Bean)"
nome = "Bran Stark(Isaac Hempstead)"
histograma(nome)
