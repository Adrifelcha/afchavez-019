rm(list=ls())
setwd("~/Desktop/Ordenado")
library('R2jags')
datos <- read.csv('S9Bloque5A.csv')
nTrials <- length(datos$tonos)
respuestas <- datos$respuesta
x <- datos$duraciones
s <- median(x)

parameters <- c('alpha', 'beta', 'psi', 'phi', 'z')

write('# Logistic psychophysical model for single subject with informative priors
# and trial-level contamination
      model{
      # Data
      for (trial in 1:nTrials){
      theta[trial,1] = 1/(1+exp(-(stimulus[trial]-standard-alpha)/beta))
      theta[trial,2] ~ dbern(psi)
      y[trial] ~ dbern(theta[trial,z[trial]+1])
      }
      # Priors
      alpha ~ dnorm(0,1/50^2)
      beta ~ dnorm(0,1/100^2)T(0,)
      # Trial-level contaminants
      for (trial in 1:nTrials){
      z[trial] ~ dbern(phi)
      }
      # Base-rate of contamination
      phi ~ dunif(0,1)
      # Bias in contamination
      psi ~ dunif(0,1)
      }')

samples <- jags(data, inits=myinits, parameters,
                model.file ="Psychophysical_1.txt",
                n.chains=3, n.iter=10000, n.burnin=5000, n.thin=1)