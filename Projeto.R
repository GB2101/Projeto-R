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
got = read.csv("planilha_got.csv")
nome = c()
for (i in 1:length(got$Nota)){
  if (got$Nota[i] >= 9){
    nome = c(nome, toString(got$Episodio[i]))
  }
}
print(nome)

#Quinta Questão:
got = read.csv("planilha_got.csv")
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

mother = function(){
  resp = c()
  for (i in 1:b) {
    aux = minimax(i)
    resp = c(resp, toString(aux$ep_min), toString(aux$ep_max))
  }
  return(resp)
}

nome = c()
nota = c()
temp = c()
for (i in 1: b){
  aux = minimax(i)
  nome = c(nome, toString(aux$ep_min), toString(aux$ep_max))
  nota = c(nota, aux$nt_min, aux$nt_max)
  temp = c(temp, i, i)
}

final = data.frame(TÍTULO = nome, NOTA = nota, TEMPORADA = temp)
print(final)


#Sexta Questão:
got = read.csv("planilha_got.csv")
index = function(){
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
a = index()
print(a)


#Sétima Questão
got = read.csv("planilha_got.csv")
zeta = function(nome){
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
gg = zeta(nome)
print(gg)


#Oitava Questão:
got = read.csv("planilha_got.csv")
z = function(t){
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
x = z(t)
print(x)


#Nona Questão:
got = read.csv("planilha_got.csv")
graphic = function(nome) {
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
  graph = hist(qq, main = nome, xlab = "Temporada", ylab = "Ocorrência", xlim = c(0, 9), ylim = c(0, 10), right = TRUE, breaks = brk, col = "yellow", border = "black")
} 
#Cersei Lannister(Lena Headey)
#Bran Stark(Isaac Hempstead)
nm = "Eddard 'Ned' Stark(Sean Bean)"
graphic(nm)
