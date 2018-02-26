###### Aula 3: Funções e Loops. ######

##Exemplo 1: Criando a função x^2-5*x+6
equacaoSegundoGrau<-function(x){
  #Calcula a função
  y<-x^2-5*x+6
  #Retorna o resultado
  return(y)
}

#Aplica a função no ponto 3
equacaoSegundoGrau(3)

#Aplica a função no ponto 2
equacaoSegundoGrau(2)

##Exemplo 2: Criando a função Mexican hat
mexicanHat1<-function(args){
  #Passo 0: Guarda os argumentos
  x<-args[1]
  y<-args[2]
  #Passo 1: Calcula a primeira parte
  p1<-1-x^2-y^2
  #Passo 2: Calcula a segunda parte
  p2<-exp(-0.5*(x^2+y^2))
  #Retorna o resultado
  return(p1*p2)
}

#Acha a função para alguns pontos
ponto<-c(0,3)
mexicanHat1(ponto)
mexicanHat1(c(2,4))


##Exemplo 2: Criando a função Mexican hat
#Maneira 2
mexicanHat2<-function(x,y){
  #Passo 1: Calcula a primeira parte
  p1<-1-x^2-y^2
  #Passo 2: Calcula a segunda parte
  p2<-exp(-0.5*(x^2+y^2))
  #Retorna o resultado
  return(p1*p2)
}

#Acha a função para alguns pontos
mexicanHat2(0,3)
mexicanHat2(x=2,y=4)

##Solução do exercício
#Item 1:
library(rgl)

#Item 2:
open3d(windowRect=c(50,50,800,800))

#Item 3:
x<-seq(-5,5,length.out=100)
y<-seq(-5,5,length.out=100)

#Item 4:
z <- outer(x,y, mexicanHat2)

#Item 5:
palette <- colorRampPalette(c("blue", "green", "yellow", "red")) 
col.table <- palette(256)
col.ind <- cut(z, 256)
persp3d(x, y, z, col=col.table[col.ind])

##Exemplo 3: Criando a função condicional
funcaoExemplo3<-function(x){
  if(x>=0){
    return(x^2-4*x+6)
  }
  else{
    return(sqrt(log(abs(x)))+exp(2*x))
  }
}

#Aplica a função
funcaoExemplo3(4)
funcaoExemplo3(-2)

##Exemplo 4: Criando a função condicional
funcaoExemplo4<-function(x){
  if(x>=4){
    return(x^2-4*x+6)
  }
  else if(x>=0){
    return(x^3-x^2 )
  }
  else{
    return(sqrt(log(abs(x)))+exp(2*x))
  }
}

#Aplica a função
funcaoExemplo4(4)
funcaoExemplo4(1.5)
funcaoExemplo4(-2)

##Exemplo 5: Usando a função ifelse
setwd("C:\\Users\\Introdução à Computação Científica\\Dados")
#Lê os dados (Item 1)
df<-read.csv2("CRIMES.csv")
#Mostra os nomes das variáveis (Item 2)
colnames(df)
#Usa as funções head e tail (Item 3)
head(df)
tail(df)
#Usa o comando str (Item 4)
str(df)
#Converte para numeric (Item 5)
df$CRIME<-as.numeric(levels(df$CRIME))
str(df$CRIME)
#Faz a análise descritiva (Item 6)
summary(df$CRIME)
#Cria as classes (Item 7)
df$TIPO<-ifelse(df$CRIME>48,"Muito Violento",
                ifelse(df$CRIME>35,"Violento ",
                       ifelse(df$CRIME>20,"Normal","Pacífico ")))
table(df$TIPO)

#Exemplo 6: Mostra todos os números de 1 a 6.
for(i in 1:100){
  #Mostra o valor de i
  print(i)
}

#Exemplo 7 (maneira 1): 
valores<-seq(0, 10, 2)
for(i in valores){
  print(i)
}
#Exemplo 7 (maneira 2): 
for(i in seq(0, 10, 2)){
  print(i)
}

#Exemplo 8:
library(numbers)
nprimos<-0
numero<-1
while(nprimos<25){
  if(isPrime(numero))
  {
    print(numero)
    nprimos<-nprimos+1
  }
  numero<-numero+1
}




