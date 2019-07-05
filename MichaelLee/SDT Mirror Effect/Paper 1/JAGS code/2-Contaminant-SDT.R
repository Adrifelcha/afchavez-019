setwd("C:/Users/Alejandro/Desktop/Adrifelcha/Tesis/Datos_CSVs")
rm(list=ls())
dir()
library(R2jags)
##############################################################
##############################################################
#Diferencias en D'
##############################################################
#Modelo 2 :  Diferencia en el promedio de D' (parametro Delta)
##############################################################




######################################################
#Especificamos el Experimento y los Datos a analizar
experimento <- 2
#####################################################

if (experimento == 1) 
{
  archive <-'Ex_1Ebb_TODOS.csv'      #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)          #Jalamos los datos del archivo
  Hits_Facil <- datos$A_H             #Hits(A)
  Hits_Dificil <- datos$B_H           #Hits(B)
  FA_Facil <- datos$A_FA              #FA(A)
  FA_Dificil <- datos$B_FA            #FA(B)
  k <- nrow(datos)                 #Total Participantes
  data <- matrix(c(FA_Facil, FA_Dificil, Hits_Dificil, Hits_Facil), nrow=k, ncol=4)  #Ordenamos los datos
}

if (experimento == 2)
{
  archive <-'Ex_2Ebb_TODOS.csv'          #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)              #Jalamos los datos
  Hits_Facil <- datos$A_H                 #H(A)
  Hits_Dificil <- datos$B_H               #H(B)
  FA_Facil <- datos$A_FA                  #FA(A)
  FA_Dificil <- datos$B_FA                #FA(B)
  k <- nrow(datos)    #Total de participantes
  data <- matrix(c(FA_Facil, FA_Dificil, Hits_Dificil, Hits_Facil), nrow=k, ncol=4)  #Ordenamos los datos
}

#De acuerdo con la matriz creada (con el csv de datos seleccionado)
fa_A <- data[,1]    #La primera columna son las FA(A)
fa_B <- data[,2]    #La primera columna son las FA(B)
h_B <- data[,3]     #La primera columna son las H(B)
h_A <- data[,4]     #La primera columna son las H(A)
s <- 160    # Número Total de ensayos con Señal
n <- 160    # Número Total de ensayos con Ruido




######################################
######################################
#Escribimos el modelo
######################################
write('
# Signal Detection Theory
model{
  for (i in 1:k){
  # Observed counts
    z[i] ~ dbern(0.5)
    h_A[i] ~ dbin(thetah_A[i],s)
    fa_A[i] ~ dbin(thetaf_A[i],n)
    h_B[i] ~ dbin(thetah_B[i],s)
    fa_B[i] ~ dbin(thetaf_B[i],n)
    pi[i] ~ dbeta(1,1)
    # Reparameterization Using Equal-Variance Gaussian SDT
    thetah_A[i] <- equals(z[i],1)*phi((d_A[i]/2)-c_A[i])+equals(z[i],0)*pi[i]
    thetaf_A[i] <- equals(z[i],1)*phi((-d_A[i]/2)-c_A[i])+equals(z[i],0)*pi[i]
    thetah_B[i] <- equals(z[i],1)*phi((d_B[i]/2)-c_B[i])+equals(z[i],0)*pi[i]
    thetaf_B[i] <- equals(z[i],1)*phi((-d_B[i]/2)-c_B[i])+equals(z[i],0)*pi[i]
    # These Priors over Discriminability and Bias Correspond 
    # to Uniform Priors over the Hit and False Alarm Rates
    d_A[i] ~ dnorm(mud_A,lambdad_A)
    c_A[i] ~ dnorm(muc_A,lambdac_A)
    d_B[i] ~ dnorm(mud_B,lambdad_B)
    c_B[i] ~ dnorm(muc_B,lambdac_B)
    } 
  #Hierarchical Structure
  muc_A ~ dnorm(0,0.7)
  mud_A ~ dnorm(0,1)T(0,6)
  muc_B ~ dnorm(0,0.7)
  mud_B ~ dnorm(0,1)T(0,6)
  lambdac_A ~ dgamma(.001,.001)
  lambdad_A ~ dgamma(.001,.001)
  lambdac_B ~ dgamma(.001,.001)
  lambdad_B ~ dgamma(.001,.001)
  sigmac_A <- 1/sqrt(lambdac_A)
  sigmad_A <- 1/sqrt(lambdad_A)
  sigmac_B <- 1/sqrt(lambdac_B)
  sigmad_B <- 1/sqrt(lambdad_B)
}
      ','ContaminantSDT.bug')

######################################
######################################
# Definimos los elementos de trabajo
######################################
data <- list("fa_A", "fa_B", "h_B", "h_A", "s", "n", "k") # Datos a analizar con JAGS
myinits <- list(
  list(d_A = rep(0,k), d_B = rep(0,k), c_A = rep(0,k), c_B = rep(0,k),  muc_A = 0, lambdac_A = 1, muc_B = 0, lambdac_B = 1, mud_A = 0, lambdad_A = 1, mud_B = 0, lambdad_B = 1))

# Parametros monitoreados
parameters <- c("c_A", "c_B", "d_A", "d_B", 
                "thetah_A", "thetah_B", "thetaf_A", "thetaf_B", 
                "muc_A", "muc_B", "mud_A", "mud_B", 
                "sigmac_A", "sigmac_B", "sigmad_A", "sigmad_B",
                "z", "pi")

niter <- 200000     #Iteraciones
burnin <- 2000      #Numero de extracciones iniciales ignoradas

# Corremos el modelo
samples <- jags(data, inits=myinits, parameters,
                model.file ="ContaminantSDT.bug",
                n.chains=1, n.iter=niter, n.burnin=burnin, n.thin=1)

#La variable 'samples' contiene los parámetros monitoreados por el modelo. (Las extracciones)


####################################################################
# Jalamos los resultados de correr el modelo (Inferencias)
# a.k.a.:
#Le ponemos una etiqueta a cada elemento contenido en Samples
####################################################################

d_a <- samples$BUGSoutput$sims.list$d_A
d_b <- samples$BUGSoutput$sims.list$d_B

c_a <- samples$BUGSoutput$sims.list$c_A
c_b <- samples$BUGSoutput$sims.list$c_B

tetaH_a <- samples$BUGSoutput$sims.list$thetah_A
tetaH_b <- samples$BUGSoutput$sims.list$thetah_B
tetaFA_a <- samples$BUGSoutput$sims.list$thetaf_A
tetaFA_b <- samples$BUGSoutput$sims.list$thetaf_B

muDA <- samples$BUGSoutput$sims.list$mud_A
muDB <- samples$BUGSoutput$sims.list$mud_B
muCA <- samples$BUGSoutput$sims.list$muc_A
muCB <- samples$BUGSoutput$sims.list$muc_B

Pi <- samples$BUGSoutput$sims.list$pi

Z <- samples$BUGSoutput$sims.list$z




######################################################
######################################################
######################################################
######### Dibujamos los plots
######################################################
layout(matrix(1:1,ncol=1))

if (experimento == 1){
  no <- 1
}else{
  no <- 2
}


Zetas <- NULL
Col_Z <- NULL
for(a in 1:k){
  Zetas[a] <- round(mean(Z[,a]),0)
  ifelse(Zetas[a]==1, Col_Z[a] <- "seagreen4", Col_Z[a] <- "red4")
}
Zetas


#########################Z PLOTTING
numero <- 0
linea <- 1.5
plot(c(1:k),Zetas, ann=F, axes=F,cex=3, pch=18, col=Col_Z, ylim=c(0,1.2))
for(u in 1:k){
  numero <- numero + 1
  lines(c(linea,linea),c(0,1), lty=2)
  linea <- linea+1
}
mtext(side=2, text = "Z", line=1.2, cex=2.1, srt=90)
mtext(side=1, text = "Participants", line=2.2, cex=1.5)
mtext(side=3, paste("Experiment No.", no), line=0.5, cex=1.5)
axis(1,c(1.25:(k+.25)),c(1:k))
axis(2,seq(0,1,1),c("Pi", "SDT"), line=-1)
legend(4,5.5, legend=c("A stimuli", "B stimuli"),
       col=c("deepskyblue3", "darkorchid3"), lty=1, cex=0.8)
mtext(side=3,"Z estimates per Participant", cex=2, line=2)