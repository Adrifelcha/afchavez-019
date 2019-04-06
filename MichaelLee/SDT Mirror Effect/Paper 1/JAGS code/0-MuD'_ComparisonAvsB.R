##############################################################
##############################################################
#Modelo 0: Comprobando diferencias entre las medias de las D'
#          a lo largo de las clases A y B
#          (Asegurando que estamos replicando bien la tarea)
##############################################################
# En este modelo se incluyen dos parámetros Mu_i y Delta que
# inciden en las medias observadas por cada clase de estímulos
##############################################################
setwd("C:/Users/Alejandro/Desktop/afchavez19/MichaelLee/SDT Mirror Effect/Data")
rm(list=ls())
dir()
library(R2jags)


######################################################
#Especificamos el Experimento y los Datos a analizar
experimento <- 2
#####################################################

if (experimento == 1) {
  archive <-'Ex_1Ebb_TODOS.csv'      #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)         #Jalamos los datos del archivo
  h_A <- datos$A_H       #Hits(A)
  h_B <- datos$B_H       #Hits(B)
  fa_A <- datos$A_FA     #FA(A)
  fa_B <- datos$B_FA     #FA(B)
  k <- 20     #Total Participantes
  s <- 160    #Número de Ensayos con Señal
  n <- 160    #Número de Ensayos con Ruido
}
if (experimento == 2){
  archive <-'Ex_2Ebb_TODOS_Sin1.csv'  #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)          #Jalamos los datos
  h_A <- datos$A_H       #H(A)
  h_B <- datos$B_H       #H(B)
  fa_A <- datos$A_FA     #FA(A)
  fa_B <- datos$B_FA     #FA(B)
  k <- 20     #Total de participantes
  s <- 160    #Número de Ensayos con Señal
  n <- 160    #Número de Ensayos con Ruido
}


######################################
######################################
#Escribimos el modelo
######################################
write('
model{
for (i in 1:k){
      # Observed counts
      h_A[i] ~ dbin(thetah_A[i],s)
      fa_A[i] ~ dbin(thetaf_A[i],n)
      h_B[i] ~ dbin(thetah_B[i],s)
      fa_B[i] ~ dbin(thetaf_B[i],n)
      # Reparameterization Using Equal-Variance Gaussian SDT
      thetah_A[i] <- phi((d_A[i]/2)-c[i])
      thetaf_A[i] <- phi((-d_A[i]/2)-c[i])
      thetah_B[i] <- phi((d_B[i]/2)-c[i])
      thetaf_B[i] <- phi((-d_B[i]/2)-c[i])
      # These Priors over Discriminability and Bias Correspond 
      # to Uniform Priors over the Hit and False Alarm Rates
      d_A[i] ~ dnorm(mud_A,lambdad_A)
      c[i] ~ dnorm(0,1)
      d_B[i] ~ dnorm(mud_B,lambdad_B)
      } 
      #Priors
      mud_A <- MuD + delta/2
      mud_B <- MuD - delta/2
      lambdad_A ~ dgamma(.001,.001)
      lambdad_B ~ dgamma(.001,.001)
      sigmad_A <- 1/sqrt(lambdad_A)
      sigmad_B <- 1/sqrt(lambdad_B)
      delta ~ dnorm(0,1)
      MuD ~ dnorm(0,1)
      delta_prior ~ dnorm(0,1)
      MuD_prior ~ dnorm(0,1)}
      ','0-diff_dprime.bug')

######################################
######################################
# Definimos los elementos de trabajo
######################################

# Especificamos cuáles son los DATOS del modelo (Nodos sombreados)
data <- list("fa_A", "fa_B", "h_B", "h_A", "s", "n", "k") 

#Asignamos valores iniciales para las Cadenas de Markov
myinits <- list(
  list(d_A = rep(0,k), d_B = rep(0,k), c = rep(0,k), lambdad_A = 1,  lambdad_B = 1, delta = 0, MuD = 0))

# Identificamos los parámetros a inferir
parameters <- c("c", "d_A", "d_B", "thetah_A", "thetah_B", "thetaf_A", "mud_A", "mud_B", 
                "thetaf_B", "sigmad_A", "sigmad_B", "delta", "MuD", "delta_prior", "MuD_prior")

niter <- 200000     #No. Iteraciones
burnin <- 2000      #No.  de extracciones iniciales "quemadas"

# Corremos el modelo
samples <- jags(data, inits=myinits, parameters,
                model.file ="0-diff_dprime.bug",
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

Delta <- samples$BUGSoutput$sims.list$delta




######################################################
######################################################
######################################################
######### Dibujamos los plots
######################################################

###################################################################################
# Cuatro Panels
# Las posteriores de los parámetros INDVIDUALES estimados (D'; C; ThetaH y ThetaFA)
###################################################################################
#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) 
layout(matrix(1:1,ncol=1))

if (experimento == 1){   ### EXPERIMENTO 1
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
     font.lab = 2, cex.axis = 1.3, bty = "n", las=1, cex.main=3)
  
############### D':    
  soporte_d <- c(0,2.8)
  plot(soporte_d, axes=F, main="Experimento 1", ylab="", xlab="", xlim=c(0,6.5), col='white')
  for(a in 1:k){
    lines(density(d_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(d_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  lines(density(muDB), lwd=5, col="darkorchid4", lty=1)
  lines(density(muDA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,210))
  #mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
  mtext("d'", side=1, line = 3, cex=2.5, f=2)
  mtext("Densidad posterior", side=2, line = 2, cex=2.1, las=0, f=2)

################ C:    
  soporte_c <- c(0,5.5)
  plot(soporte_c, axes=F, main="Experimento 1", ylab="", xlab="", xlim=c(-2,2), col='white')
  for(a in 1:k){
    lines(density(c_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(c_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  lines(density(muCB), lwd=5, col="darkorchid4", lty=1)
  lines(density(muCA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,210))
  mtext("C", side=1, line = 3, cex=2.5, f=2)
  mtext("Densidad posterior", side=2, line = 2, cex=2.1, las=0, f=2) 

############## Theta Hits:
soporte_t <- c(0,90)
plot(soporte_t, axes=F, main="Experimento 1", ylab="", xlab="", xlim=c(0.4,1), col='white')
for(a in 1:k){
  lines(density(tetaH_a[,a]), lwd=1, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
  lines(density(tetaH_b[,a]), lwd=1, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
axis(1)
axis(2, labels=F, at=c(0,210))
#mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
mtext(expression(paste(theta, "H")), side=1, line = 3, cex=1.5)
mtext("Densidad", side=2, line = 2, cex=1.5, las=0)

lines(c(0, 0.1),c(60,60), lwd=2, lty=1, col="deepskyblue3")
lines(c(0, 0.1),c(50,50), lwd=2, lty=1, col="darkorchid3")
text(0.15, 60, labels="Estímulos A", offset=0, cex = 0.8, pos=4)
text(0.15, 50, labels="Estímulos B", offset=0, cex = 0.8, pos=4)

############### Theta F.A.
plot(soporte_t, axes=F, main="Experimento 1", ylab="", xlab="", xlim=c(0,0.7), col='white')
for(a in 1:k){
  lines(density(tetaFA_a[,a]), lwd=1, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
  lines(density(tetaFA_b[,a]), lwd=1, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
axis(1)
axis(2, labels=F, at=c(0,200))
#mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
mtext(expression(paste(theta, "F")), side=1, line = 3, cex=1.5)
mtext("Densidad", side=2, line = 2, cex=1.5, las=0)
}


if (experimento == 2){ ######### EXPERIMENTO 2
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1, cex.main=3)
  
############### D'
  soporte_d <- c(0,2.8)
  plot(soporte_d, axes=F, main="Experimento 2", ylab="", xlab="", xlim=c(-0.5,5), col='white')
  for(a in 1:k){
    lines(density(d_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(d_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
    lines(density(muDB), lwd=5, col="darkorchid4", lty=1)
    lines(density(muDA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,210))
  #mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
  mtext("d'", side=1, line = 3, cex=2.5, f=2)
  mtext("Densidad posterior", side=2, line = 2, cex=2.1, las=0, f=2) 
  

################ C:
  soporte_c <- c(0,5.5)
  
  plot(soporte_c, axes=F, main="Experimento 2", ylab="", xlab="", xlim=c(-2,2), ylim = c(0,6), col='white')
  for(a in 1:k){
    lines(density(c_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(c_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  lines(density(muCB), lwd=5, col="darkorchid4", lty=1)
  lines(density(muCA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,250))
  mtext("C", side=1, line = 3, cex=2.5, f=2)
  mtext("Densidad posterior", side=2, line = 2, cex=2.1, las=0, f=2) 
  
################ Theta Hits:
  soporte_t <- c(0,50)
  
  plot(soporte_t, axes=F, main="Experimento 2", ylab="", xlab="", xlim=c(0.2,1), col='white')
  for(a in 1:k){
    lines(density(tetaH_a[,a]), lwd=1, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(tetaH_b[,a]), lwd=1, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  axis(1)
  axis(2, labels=F, at=c(0,210))
  #mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
  mtext(expression(paste(theta, "H")), side=1, line = 3, cex=1.5)
  mtext("Densidad", side=2, line = 2, cex=1.5, las=0)
  
  lines(c(.2, 0.3),c(45,45), lwd=2, lty=1, col="deepskyblue3")
  lines(c(.2, 0.3),c(35,35), lwd=2, lty=1, col="darkorchid3")
  text(0.4, 45, labels="Estímulos A", offset=0, cex = 0.8, pos=4)
  text(0.4, 35, labels="Estímulos B", offset=0, cex = 0.8, pos=4)
  
################### Theta FA:    
  plot(soporte_t, axes=F, main="Experimento 2", ylab="", xlab="", ylim=c(0,25), xlim=c(0,0.7), col='white')
  for(a in 1:k){
    lines(density(tetaFA_a[,a]), lwd=1, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(tetaFA_b[,a]), lwd=1, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  axis(1)
  axis(2, labels=F, at=c(0,200))
  #mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
  mtext(expression(paste(theta, "F")), side=1, line = 3, cex=1.5)
  mtext("Densidad", side=2, line = 2, cex=1.5, las=0)
}



##########
############################### Interaccion entre parametros
######## Marginales y Densidad
keep_ <- (1000)
keep <- sample(niter, keep_)
d.FA_a <- density(tetaFA_a)
d.FA_b <- density(tetaFA_b)
d.H_a <- density(tetaH_a)
d.H_b <- density(tetaH_b)
mu.Da <- density(muDA)
mu.Db <- density(muDB)
mu.Ca <- density(muCA)
mu.Cb <- density(muCB)

layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

if (experimento ==1)
{
 # D' y C
  
  par(mar=c(0.7,1,3,0))
  plot(muDA[keep],muCA[keep], col="deepskyblue3", xlab="", main="Experimento 1", cex.main=2, ylab="", axes=F,xlim=c(0,5), ylim=c(-1,1))
  points(muDB[keep],muCB[keep], col="darkorchid3")
  lines(c(0.2, 0.6),c(0.9,0.9), lwd=3, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.6),c(0.7,0.7), lwd=3, lty=1, col="darkorchid3")
  text(0.65, 0.9, labels="Estímulos A", offset=0, cex = 2, pos=4)
  text(0.65, 0.7, labels="Estímulos B", offset=0, cex = 2, pos=4)
  box(lty=1)
  
  par(mar=c(0.7,0.5,3,6))
  plot(mu.Ca$y, mu.Ca$x, xlim=rev(c(0,5)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(-1,1), lwd=2)
  lines(mu.Cb$y, mu.Cb$x, col="darkorchid3", lwd=2)
  
  axis(4)
  mtext(expression(paste(mu, "C")), side=4,line=5, cex=1.5, font=2, las=0)
  box(lty=1)
  
  par(mar=c(6,1,0,0))
  plot(density(muDA),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,5),ylim=c(0,3), lwd=2)
  lines(density(muDB), col="darkorchid3", lwd=2)
  axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5))
  mtext(expression(paste(mu, "D")), side=1.2,line=4, cex=1.5, font=2)
  box(lty=1)
}


if (experimento ==2)
{
  # D' y C
  
  par(mar=c(0.7,1,3,0))
  plot(muDA[keep],muCA[keep], col="deepskyblue3", xlab="", main="Experimento 2", cex.main=2, ylab="", axes=F,xlim=c(0,5), ylim=c(-1,1))
  points(muDB[keep],muCB[keep], col="darkorchid3")
  lines(c(0.2, 0.6),c(0.9,0.9), lwd=3, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.6),c(0.7,0.7), lwd=3, lty=1, col="darkorchid3")
  text(0.65, 0.9, labels="Estímulos A", offset=0, cex = 2, pos=4)
  text(0.65, 0.7, labels="Estímulos B", offset=0, cex = 2, pos=4)
  box(lty=1)
  
  par(mar=c(0.7,0.5,3,6))
  plot(mu.Ca$y, mu.Ca$x, xlim=rev(c(0,6)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(-1,1), lwd=2)
  lines(mu.Cb$y, mu.Cb$x, col="darkorchid3", lwd=2)
  axis(4)
  mtext(expression(paste(mu, "C")), side=4,line=5, cex=1.5, font=2, las=0)
  box(lty=1)
  
  par(mar=c(6,1,0,0))
  plot(density(muDA),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,5),ylim=c(0,3), lwd=2)
  lines(density(muDB), col="darkorchid3", lwd=2)
  axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5))
  mtext(expression(paste(mu, "D")), side=1.2,line=4, cex=1.5, font=2)
  box(lty=1)
}

############################
######### DIFFERENCES ON D'

layout(matrix(1:1,ncol=1))
if (experimento ==1)
{
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  
  plot(density(Delta), col='blue4', main="Experimento 1", cex.main=2, lwd=3.5, ylab="", xlab="", axes=F, xlim=c(-0.5,2))
  #text(1.5, 1.2, labels="Delta", offset=0, cex = 1, col='red', pos=4)
  axis(1)
  axis(2, labels=F, at=c(0,24))
  mtext("Densidad de probabilidad", side=2, line=2, cex=2, las=0, font=2)
  mtext("Delta", side=1, line=2.5, cex=2, font=2)
  points(0,0.03032, pch=16, type='p', col='red', cex=1.5)
}

if (experimento ==2)
{
  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  
  plot(density(Delta), col='blue4', main="Experimento 2", cex.main=2, lwd=3.5, ylab="", xlab="", axes=F, xlim=c(-0.5,2))
  lines(c(mean(Delta), mean(Delta)),c(0,2), col='red')
#  text(1.5, 1.2, labels="Delta", offset=0, cex = 1, col='red', pos=4)
  axis(1)
  axis(2, labels=F, at=c(0,24))
  mtext("Densidad de probabilidad", side=2, line=2, cex=2, las=0, font=2)
  mtext("Delta", side=1, line=2.5, cex=2, font=2)
  points(0,0.007229, pch=16, type='p', col='red', cex=1.5)
}



############################################
############ ROC CURVES


############### ROC Curves

layout(matrix(1:1,ncol=1))

hits_A <- c()
falarm_A <- c()
hits_B <- c()
falarm_B <- c()
hits_na <- c()
falarm_na <- c()
c <- seq(-10,10,0.1)
d_null <- 0

  for (i in 1:length(c)){
    hits_A[i] <- pnorm((-muDA/2)-c[i])
    falarm_A[i] <- pnorm((muDA/2)-c[i])
    hits_B[i] <- pnorm((-muDB/2)-c[i])
    falarm_B[i] <- pnorm((muDB/2)-c[i])
    hits_na[i] <- pnorm((d_null/2)-c[i])
    falarm_na[i] <- pnorm((-d_null/2)-c[i])
}

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)

plot(hits_na,falarm_na, type='o', col='white', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
lines(hits_na,falarm_na,lwd=1,col='black', lty=2)
lines(hits_A,falarm_A,lwd=3,col='deepskyblue3')
lines(hits_B,falarm_B,lwd=3,col='darkorchid3')
lines(c(0.58, 0.68),c(0.3,0.3), lwd=3, lty=1, col="deepskyblue3")
lines(c(0.58, 0.68),c(0.2,0.2), lwd=3, lty=1, col="darkorchid3")
text(0.7, 0.3, labels="D' para A", offset=0, cex = 1.8, pos=4)
text(0.7, 0.2, labels="D' para B", offset=0, cex = 1.8, pos=4)
title('ROC por Clase de Estímulo en el Experimento 1')
#mtext("Experimento 1",3,cex=1)
