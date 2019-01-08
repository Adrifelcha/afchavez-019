rm(list=ls())
setwd("C:/Users/Alejandro/Desktop/Felisa/Proyectos/Uri Tesis/Sabado_21OCtubre")
library('R2jags')



datos <- read.csv('S9Bloque5A.csv')
nTrials <- length(datos$tonos)
respuestas <- datos$respuesta
resp_binaria <- datos$resp_bin
correcto <- datos$correcto
x <- datos$duraciones
s <- median(x)

data <- matrix(c(resp_binaria, x), nrow=nTrials, ncol=2)  #Ordenamos los datos


#De acuerdo con la matriz creada (con el csv de datos seleccionado)
y <- data[,1]    #La primera columna son las FA(A)
stimulus <- data[,2]    #La primera columna son las FA(B)
standard <- s

######################################
#Preparamos y Corremos el modelo
######################################
data <- list("y", "stimulus", "nTrials", "standard") # Datos a analizar con JAGS
myinits <- list(
  list(z = rep(0,nTrials), phi = 0, psi = 0, alpha = 0,  beta = 0))


parameters <- c('alpha', 'beta', 'psi', 'phi', 'z')

niter <- 200000     #Iteraciones
burnin <- 2000      #Numero de extracciones iniciales ignoradas


samples <- jags(data, inits=myinits, parameters,
                model.file ="UriModel.txt",
                n.chains=1, n.iter=niter, n.burnin=burnin, n.thin=1)