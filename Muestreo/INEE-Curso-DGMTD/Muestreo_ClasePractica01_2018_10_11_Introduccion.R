#################################################################
#################################################################
#################################################################
###
### Curso de Muestreo - INEE
###
### Clase Practica 01 (Introduccion a R)
###
### Emilio Lopez Escobar (http://www.info-Emilio.NET)
### emilio@numerika.mx
### Ciudad de México. Octubre 11, 2018
###
#################################################################
#################################################################
#################################################################



# Basta con copiar y pegar cualquier linea en la consola de R.



# Ojo: Es quizas necesario copiar, pegar y correr las lineas anteriores a la linea de interes.



# Precisamente para que pudieran copiar y pegar no estoy utilizando acentos ni letras hispanas.



# Para ir aprendiendo y que tenga chiste, hay que ir viendo lo que pasa con cada linea.



# Si quiero comentar algo sin que lo ejecute R, utilizo el signo # antes
getwd()
setwd("C:/Emilio/R")
setwd("C:\\Emilio\\R")


help(sum)
?sum
??sum
# Si de plano no encuentro, entonces utilizo Google tecleando por ejemplo: R sum of values



c(1, 2.5, 3)


x <- c(1, 2.5, 3)


x


length(x)


x <- c(x, 4)


x


length(x)
mean(x)
var(x)
mean(x^2)


# Entonces estas dos lineas:
sum(x)/length(x)
sum(  (x-mean(x))^2  ) / (length(x)-1)


# me tienen que dar lo mismo, respectivamente, que estas dos lineas:
mean(x)
var(x)


Varianza.Que.Me.Interesa <- var(x) # Crea una variable que guarde la varianza de x


sqrt(Varianza.Que.Me.Interesa)


sd(x)


n <- 5


c(1:n)


rep(x, times=2)


rep(x, each=2)


z <- c(1:6)^2


z


EsMenorADos   <- z<2


EsMenorADos


EsIgualACuatro <- z==4


EsIgualACuatro


z


z[3]


z[c(1,3)]


z[z<2]


z[EsMenorADos]


z[-3]


summary(z)     # Dependiendo de lo que sea z (datos, vector, matrix,.. arroja estadisticos basicos
