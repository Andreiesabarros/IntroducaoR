###### Aula 1: Introdução ao R e RStudio. ######

###Exemplo 01:

##Item 1:
2^(1/4)*3/5^3-4/3+2

##Item 2:
(3/4)/5-(5^-3)/4+10-1/4


##Item 1:
(12/3)+4-((24/3)*8)
##Item 2:
10-((50/5)*10)

###Exemplo 02:
resultado<-(2/4)-(3*4)^(-1/4)
resultado

##Exemplo 03:
objeto1<-34.4635
objeto2<-"Curso de R"
objeto3<-FALSE
objeto4<-2

#Verifica o tipo do objeto:
typeof(objeto1)
typeof(objeto2)
typeof(objeto3)
typeof(objeto4)

###Exemplo 04:
##Item 1:
sqrt(3/4)*exp(-2/4)+log(34/pi)-abs(-32)

##Item 2:
sqrt(exp(2)+log(43^2))

###Exemplo 05:
aluno<-c('Ana','Paula','José','João','Sérgio')
notas<-c(8.75,3.65,9.21,7.34,6.48)

###Exemplo 06:
#Acessa o segundo elemento
notas[2]

###Exemplo 07:
#Mostra todos menos o terceiro
notas[-3]

###Exemplo 08:
##Maneira 1 de acessar os elementos 1, 4 e 5:
#Passo 1:
posicoes<-c(1,4,5)
#Passo 2:
notas[posicoes]

##Maneira 2 de acessar os elementos 1, 4 e 5:
#Passo 1:
notas[c(1,4,5)]

###Exemplo 09:
indice<-c(TRUE,FALSE,FALSE,TRUE,TRUE)
notas[indice]

###Exemplo 10:
#Cria uma matriz de dados (data.frame)
mat.df<-cbind(aluno,notas)
mat.df

###Exemplo 11: Pega as três primeiras linhas e a segunda coluna:
##Maneira 1: 
mat.df[1:3,2]

##Maneira 2: 
mat.df[c(1,2,3),2]

##Maneira 3:
pos<-c(1,2,3)
mat.df[pos,2]

###Exemplo 12: Cria data.frame
##Maneira 1: 
mydata <- data.frame(aluno=c('Ana','Paula','José','João','Sérgio'),
                     notas=c(8.75,3.65,9.21,7.34,6.48))
mydata

##Maneira 2: 
mydata <- data.frame(aluno,notas)
mydata

###Exemplo 13: Cria data.frame
##Acessa os elementos 1, 4 e 5 da segunda coluna
#Maneira 1:
mydata[c(1,2,3),2]

#Maneira 1:
mydata[1:3,2]

#Maneira 3:
names(mydata)
mydata[1:3,"notas"]

#Maneira 4:
mydata$notas[1:3]

###Exemplo 14:
#Item 1:
mydata$nota2<-mydata$notas^2

#Item 2:
mydata$notaZ<-(mydata$notas-mean(mydata[,2]))/sd(mydata[,"notas"])

#Item 3:
mydata[,"notaN"]<-(mydata$notas-min(mydata$notas))/(max(mydata[,2])-min(mydata[,2]))











