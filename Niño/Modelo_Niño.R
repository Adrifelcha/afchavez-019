##############################################################
##############################################################
#Diferencias en Rates
##############################################################
##############################################################
setwd("C:/Users/Alejandro/Desktop/afchavez19/Niño")
rm(list=ls())
dir()
library(R2jags)



  datos <- read.csv('Ganancias_binomial.csv')      #Archivo con los datos
  
  FrecG <- datos$Ganancias   #Frecuencia absoluta en Condicion Ganancias
  FrecP <- datos$Perdidas   #frecuencias absolutas en condicion perdidas
  k <- 18     #Participantes
  r <- 200   #Repeticiones
  



######################################
#Escribimos el modelo
######################################
  write('
model{
  for (i in 1:k){
  # Observed counts
    FrecG[i] ~ dbin(thetaG[i],r)
    FrecP[i] ~ dbin(thetaP[i],r)
  # Reparameterization Using Equal-Variance Gaussian SDT
    thetaG[i] ~ dbeta(1,1)
    thetaP[i] ~ dbeta(1,1)
  #Differences on thetas
      Tau[i] <- thetaG[i]/thetaP[i]
      Gamma[i] <- thetaG[i]-thetaP[i]
      }
}', 'Tau.bug')

######################################
# Preparamos el modelo
######################################

#Identificamos los datos (nodos sombreados)
data <- list("FrecG", "FrecP", "r", "k")

myinits <- list(
list(thetaG = rep(0.5,k), thetaP = rep(0.5,k)))      #Valores iniciales para las cadenas de Markov


#Parámetros a inferir (nodos no sombreados y NO determinísticos)
parameters <- c("thetaG", "thetaP", "Tau", "Gamma")

niter <- 100000    #Iteraciones
burnin <- 1000     #No. de primeros sampleos en ignorarse

#Corremos el modelo
samples <- jags(data, inits=myinits, parameters,
                model.file ="Tau.bug",
                n.chains=1, n.iter=niter, n.burnin=burnin, n.thin=1)
#La variable 'samples' contiene los parámetros monitoreados por el modelo. (Las extracciones)

####################################################################
# Jalamos la cadena de inferencias hechas por el modelo para cada parametro monitoreado
####################################################################

  tetaG <- samples$BUGSoutput$sims.list$thetaG
  tetaP <- samples$BUGSoutput$sims.list$thetaP

  tau <- samples$BUGSoutput$sims.list$Tau
  gamma <- samples$BUGSoutput$sims.list$Gamma
  
  
  
  
  
  
  