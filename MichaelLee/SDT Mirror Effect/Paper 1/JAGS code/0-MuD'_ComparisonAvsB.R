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
experimento <- 1
#####################################################

if (experimento == 1) {
  exp <- 1
  archive <-'Ex_1Ebb_TODOS.csv'      #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)         #Jalamos los datos del archivo
}
if (experimento == 2){
  exp <- 2
  archive <-'Ex_2Ebb_TODOS_Sin1.csv'  #Especificamos el nombre del archivo que contiene los datos
  datos <- read.csv(archive)          #Jalamos los datos
}

h_A <- datos$A_H       #Hits(A)
h_B <- datos$B_H       #Hits(B)
fa_A <- datos$A_FA     #FA(A)
fa_B <- datos$B_FA     #FA(B)
k <- 20     #Total Participantes
s <- 160    #Número de Ensayos con Señal
n <- 160    #Número de Ensayos con Ruido

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
      thetah_A[i] <- phi((d_A[i]/2)-c_A[i])
      thetaf_A[i] <- phi((-d_A[i]/2)-c_A[i])
      thetah_B[i] <- phi((d_B[i]/2)-c_B[i])
      thetaf_B[i] <- phi((-d_B[i]/2)-c_B[i])
      # These Priors over Discriminability and Bias Correspond 
      # to Uniform Priors over the Hit and False Alarm Rates
      d_A[i] ~ dnorm(mud_A,sigmad_A)
      c_A[i] ~ dnorm(0,1)
      c_B[i] ~ dnorm(0,1)
      d_B[i] ~ dnorm(mud_B,sigmad_B)
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
      MuD_prior ~ dnorm(0,1)T(0,6)}
      ','0-diff_dprime.bug')

######################################
######################################
# Definimos los elementos de trabajo
######################################

# Especificamos cuáles son los DATOS del modelo (Nodos sombreados)
data <- list("fa_A", "fa_B", "h_B", "h_A", "s", "n", "k") 

#Asignamos valores iniciales para las Cadenas de Markov
myinits <- list(
  list(d_A = rep(0,k), d_B = rep(0,k), c_A = rep(1,k),c_B = rep(1,k), lambdad_A = 1,  lambdad_B = 1, 
       delta = 0, MuD = 0))

# Identificamos los parámetros a inferir
parameters <- c("c_A", "c_B", "d_A", "d_B", "thetah_A", "thetah_B", "thetaf_A", "mud_A", "mud_B", 
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

muD <- samples$BUGSoutput$sims.list$MuD
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

if (experimento == 1){
  no <- 1
}else{
  no <- 2
}

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
     font.lab = 2, cex.axis = 1.3, bty = "n", las=1, cex.main=3)
  
############### D':    
  soporte_d <- c(0,2.8)
  plot(soporte_d, axes=F, ann=F, ylab="", xlab="", xlim=c(0,6.5), col='white')
  for(a in 1:k){
    lines(density(d_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(d_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  lines(density(muDB), lwd=5, col="darkorchid4", lty=1)
  lines(density(muDA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,210))
  #mtext("Differences on Hit Rates", side=3, line = 0.2, cex=1.2, font=1)
  mtext("D'", side=1, line = 3, cex=1.5, f=2)
  mtext("Posterior density", side=2, line = 2, cex=1.5, las=0, f=2)
  mtext(paste("Experiment No.", no, ": D' estimates"), cex=2, f=2)
  legend(4,2.8, legend=c("A Class", "B Class", "Mean of A", "Mean of B"),
         col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))

################ C:    
  soporte_c <- c(0,5.5)
  plot(soporte_c, axes=F, main="", ylab="", xlab="", xlim=c(-2,2), col='white')
  for(a in 1:k){
    lines(density(c_a[,a]), lwd=2, col="dodgerblue2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
    lines(density(c_b[,a]), lwd=2, col="darkorchid2", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
  lines(density(muCB), lwd=5, col="darkorchid4", lty=1)
  lines(density(muCA), lwd=5, col="dodgerblue4", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,210))
  mtext("C", side=1, line = 3, cex=1.5, f=2)
  mtext("Posterior density", side=2, line = 2, cex=1.5, las=0, f=2)
  mtext(paste("Experiment No.", no, ": C estimates"), cex=2, f=2)
  legend(-1.5,5.3, legend=c("A Class", "B Class"),
         col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))

############## Theta Hits:
soporte_t <- c(0,90)
plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(0.4,1), col='white')
for(a in 1:k){
  lines(density(tetaH_a[,a]), lwd=1.5, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
  lines(density(tetaH_b[,a]), lwd=1.5, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
axis(1)
axis(2, labels=F, at=c(0,210))
mtext(expression(paste(theta, " - Hits")), side=1, line = 3, cex=1.5, f=2)
mtext("Posterior density", side=2, line = 2, cex=1.5, las=0, f=2)
mtext(paste("Experiment No.", no), cex=2, f=2)
mtext(expression(paste(theta, "(Hits) estimates")), cex=1.5, f=2, line=-2)
legend(.5,70.3, legend=c("A Class", "B Class"),
       col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))


############### Theta F.A.
plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(0,0.7), col='white')
for(a in 1:k){
  lines(density(tetaFA_a[,a]), lwd=1, col="dodgerblue3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)
  lines(density(tetaFA_b[,a]), lwd=1, col="darkorchid3", ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}  
axis(1)
axis(2, labels=F, at=c(0,200))
mtext(expression(paste(theta, " - False Alarms")), side=1, line = 3, cex=1.5, f=2)
mtext("Posterior density", side=2, line = 2, cex=1.5, las=0, f=2)
mtext(paste("Experiment No.", no), cex=2, f=2)
mtext(expression(paste(theta, "(F.A.) estimates")), cex=1.5, f=2, line=-2)
legend(.5,70.3, legend=c("A Class", "B Class"),
       col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))










############################
######### DIFFERENCES ON D'
layout(matrix(1:1,ncol=1))

  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
  
  prior_delta <- rnorm((niter-burnin),0,1)
  SavageDickey <- dnorm(0,0,1)/dnorm(0,mean(Delta),sd(Delta))
  dot_prior <- max(density(prior_delta)$y)
  dot_x <- max(density(Delta)$y[which(density(Delta)$x<0.01&density(Delta)$x>-0.01)])
  
  layout(matrix(1:1,ncol=1))
  plot(density(Delta), col='blue4', lwd=3.5, ann=F, axes=F, xlim=c(-0.5,2))
  #lines(seq(-100,100,.05), dnorm(seq(-100,100,.05), 0,1), lwd=1, col="darkorchid3")
  lines(density(prior_delta), lwd=1, col="red")
  axis(1)
  axis(2, line=.5)
  mtext("Density", side=2, line=3.5, cex=1.5, las=0, font=1)
  mtext("Delta", side=1, line=2.5, cex=2)
  title("Bayes Factor for the prior and posterior densities of Delta", line=2.2, cex=2)
  mtext(side=3, paste("Experiment No.", exp), cex=1,line=0.5)
  points(0,dot_x, pch=16, type='p', col='red', cex=1.5)
  points(0,dot_prior, pch=16, type='p', col='red', cex=1.5)
  lines(c(0,0), c(dot_x, dot_prior), lwd=1, col="red", lty=2)
  legend(1.2,1.25, legend=c("Prior", "Posterior"),
         col=c("red", "blue4"), lty=1, cex=1.2, lwd=2)
  text(0,0.84,paste(round(SavageDickey,3)), f=2)  
  text(0,0.9,paste("Bayes Factor"),f=2)



############################################
############ ROC CURVES
############################################
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
text(0.7, 0.3, labels="Mean D' for stimuli A", offset=0, cex = 1.8, pos=4)
text(0.7, 0.2, labels="Mean D' for stimuli B", offset=0, cex = 1.8, pos=4)
title("ROC curve per class of stimuli (based on mean D')")
mtext(side=3, paste("Experiment No.", exp), cex=1,line=0.2)









#######################################
#  Density plots
#######################################
x_axis_a <- NULL
y_axis_a <- NULL
x_axis_b <- NULL
y_axis_b <- NULL



numero <- 0
media_post_a <- NULL
for(i in 1:ncol(d_a)){
  numero <- numero+1
  a <- sample(d_a[,i],1000)
  espacio_init <- ((numero-1) * length(a)) + 1
  espacio_fin <- numero * length(a)
  y_axis_a[espacio_init:espacio_fin] <- a
  media_post_a[i] <- mean(d_a[,i])
}

numero <- 0
for(i in 1:ncol(d_a)){
  numero <- numero+1
  b <- rep(numero, 1000)
  espacio_init <- ((numero-1) * length(b)) + 1
  espacio_fin <- numero * length(b)
  x_axis_a[espacio_init:espacio_fin] <- b
}

numero <- 0
media_post_b <- NULL
for(i in 1:ncol(d_b)){
  numero <- numero+1
  a <- sample(d_b[,i],1000)
  espacio_init <- ((numero-1) * length(a)) + 1
  espacio_fin <- numero * length(a)
  y_axis_b[espacio_init:espacio_fin] <- a
  media_post_b[i] <- mean(d_b[,i])
}

numero <- 0.5
num <- 0
for(i in 1:ncol(d_b)){
  numero <- numero+1
  num <- num+1
  b <- rep(numero, 1000)
  espacio_init <- ((num-1) * length(b)) + 1
  espacio_fin <- num * length(b)
  x_axis_b[espacio_init:espacio_fin] <- b
}


numero <- 0
linea <- 1.75
plot(x_axis_a, y_axis_a, ann=F, axes=F,cex=0.5, pch=1, col="deepskyblue3", ylim=c(0,6))
points(x_axis_b, y_axis_b, col="darkorchid3", pch=1, cex=0.5)
for(u in 1:20){
  numero <- numero + 1
  points(numero,media_post_a[u], col="black", pch=16, cex=1, type="p")
  points(numero+.5,media_post_b[u], col="black", pch=16, cex=1, type="p")
  lines(c(linea,linea),c(-0.3,6), lty=2)
  linea <- linea+1
  }
mtext(side=2, text = "Density", line=2, cex=1.5, srt=90)
mtext(side=1, text = "Participants", line=2.2, cex=1.5)
mtext(side=3, paste("Experiment No.", exp), line=0.5, cex=1)
axis(1,c(1.25:20.25),c(1:20))
axis(2,seq(0,6,0.5),seq(0,6,0.5), line=-1)
legend(4,5.5, legend=c("A stimuli", "B stimuli"),
       col=c("deepskyblue3", "darkorchid3"), lty=1, cex=0.8)
title("Posterior densities of d' estimates per participant", cex=2)