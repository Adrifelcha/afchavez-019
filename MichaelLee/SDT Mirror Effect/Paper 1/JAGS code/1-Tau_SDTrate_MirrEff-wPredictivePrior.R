setwd("C:/Users/Sandra/Desktop/afchavez-019/MichaelLee/SDT Mirror Effect/Data")
rm(list=ls())
dir()
library(R2jags)
##############################################################
##############################################################
#Diferencias en Hits y Falsas Alarmas
##############################################################
#Modelo 1 :  Diferencias entre las Tasas H y FA (parametros TauH y TauF)
##############################################################



######################################################
#Especificamos el Experimento y los Datos a analizar
experimento <- 2
#####################################################

if (experimento == 1)    #Una Figura de Ebbinghaus
{
  exp <- 1
  archive <-'Ex_1Ebb_TODOS.csv'         #El archivo que contiene los datos
  datos <- read.csv(archive)          #Jalamos los datos del archivo
}
if (experimento == 2)   # Dos Figuras de Ebbinghaus
{
  exp <- 2
  archive <-'Ex_2Ebb_TODOS_Sin1.csv'  #El archivo que contiene los datos
  datos <- read.csv(archive)          #Jalamos los datos
}

h_A <- datos$A_H       #Hits(A)
h_B <- datos$B_H     #Hits(B)
fa_A <- datos$A_FA        #FA(A)
fa_B <- datos$B_FA      #FA(B)
k <- 20                       #Participantes
s <- 160       #Ensayos con Señal
n <- 160       #Ensayos con Ruido

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
    thetah_A[i] <- phi(d_A[i]/2-c_A[i])
    thetaf_A[i] <- phi(-d_A[i]/2-c_A[i])
    thetah_B[i] <- phi(d_B[i]/2-c_B[i])
    thetaf_B[i] <- phi(-d_B[i]/2-c_B[i])
    # These Priors over Discriminability and Bias Correspond 
    # to Uniform Priors over the Hit and False Alarm Rates
    d_A[i] ~ dnorm(0,1)T(0,6)
    c_A[i] ~ dnorm(0,0.7)
    d_B[i] ~ dnorm(0,1)T(0,6)
    c_B[i] ~ dnorm(0,0.7)
    #Differences on dprime
      Tau_H[i] <- thetah_A[i]-thetah_B[i]
      Tau_F[i] <- thetaf_B[i]-thetaf_A[i]
  }
  #Predictive Priors
  for (i in 1:k){
  # Observed counts
      prior_h_A[i] ~ dbin(Pr_thetah_A[i],s)
      prior_fa_A[i] ~ dbin(Pr_thetaf_A[i],n)
      prior_h_B[i] ~ dbin(Pr_thetah_B[i],s)
      prior_fa_B[i] ~ dbin(Pr_thetaf_B[i],n)
      # Reparameterization Using Equal-Variance Gaussian SDT
      Pr_thetah_A[i] <- phi(Pr_d_A[i]/2-Pr_c_A[i])
      Pr_thetaf_A[i] <- phi(-Pr_d_A[i]/2-Pr_c_A[i])
      Pr_thetah_B[i] <- phi(Pr_d_B[i]/2-Pr_c_B[i])
      Pr_thetaf_B[i] <- phi(-Pr_d_B[i]/2-Pr_c_B[i])
      # These Priors over Discriminability and Bias Correspond 
      # to Uniform Priors over the Hit and False Alarm Rates
      Pr_d_A[i] ~ dnorm(0,1)T(0,6)
      Pr_c_A[i] ~ dnorm(0,0.7)
      Pr_d_B[i] ~ dnorm(0,1)T(0,6)
      Pr_c_B[i] ~ dnorm(0,0.7)
      #Differences on dprime
      PRIOR_Tau_H[i] <- Pr_thetah_A[i]-Pr_thetah_B[i]
      PRIOR_Tau_F[i] <- Pr_thetaf_B[i]-Pr_thetaf_A[i]
      }}','Tau.bug')

######################################
######################################
# Definimos los elementos de trabajo
######################################
data <- list("fa_A", "fa_B", "h_B", "h_A", "s", "n", "k")                    #Los datos que vamos a utilizar para nuestro modelo
myinits <- list(
  list(d_A = rep(0,k), c_A = rep(0,k), d_B = rep(0,k), c_B = rep(0,k),
       Pr_d_A = rep(0,k), Pr_c_A = rep(0,k), Pr_d_B = rep(0,k), Pr_c_B = rep(0,k),
       prior_h_A = rep(0.5,k), prior_h_B = rep(0.5,k), prior_fa_A = rep(0.5,k),prior_fa_B = rep(0.5,k)))      #Valores iniciales para las extracciones de las cadenas de Markov

#Parámetros monitoreados
parameters <- c("d_A", "c_A", "thetah_A", "thetaf_A", "d_B", "c_B", "thetah_B", "thetaf_B","Tau_H", "Tau_F",
                "Pr_d_A", "Pr_c_A", "Pr_thetah_A", "Pr_thetaf_A", "Pr_d_B", "Pr_c_B", "Pr_thetah_B", "Pr_thetaf_B",
                "PRIOR_Tau_H", "PRIOR_Tau_F","prior_h_A", "prior_h_B", "prior_fa_A", "prior_fa_B")

niter <- 100000    #Iteraciones
burnin <- 1000     #No. de primeros sampleos en ignorarse

#Corremos el modelo
samples <- jags(data, inits=myinits, parameters,
                model.file ="Tau.bug",
                n.chains=1, n.iter=niter, n.burnin=burnin, n.thin=1)

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
  
  tauH <- samples$BUGSoutput$sims.list$Tau_H
  tauF <- samples$BUGSoutput$sims.list$Tau_F
  
  
  #Prior predictiva
  PPr_d_a <- samples$BUGSoutput$sims.list$Pr_d_A
  PPr_d_b <- samples$BUGSoutput$sims.list$Pr_d_B
  
  PPr_c_a <- samples$BUGSoutput$sims.list$Pr_c_A
  PPr_c_b <- samples$BUGSoutput$sims.list$Pr_c_B
  
  PPr_tetaH_a <- samples$BUGSoutput$sims.list$Pr_thetah_A
  PPr_tetaH_b <- samples$BUGSoutput$sims.list$Pr_thetah_B
  PPr_tetaFA_a <- samples$BUGSoutput$sims.list$Pr_thetaf_A
  PPr_tetaFA_b <- samples$BUGSoutput$sims.list$Pr_thetaf_B
  
  PPr_tauH <- samples$BUGSoutput$sims.list$PRIOR_Tau_H
  PPr_tauF <- samples$BUGSoutput$sims.list$PRIOR_Tau_F


##########################################################
##########################################################
##########################################################
################## DRAWING PLOTS
##########################################################

  
  ###################################################################################
  # Paneles separados
  # Las posteriores de los parámetros INDVIDUALES estimados (D'y C; ThetaH y ThetaFA)
  ###################################################################################
  
layout(matrix(1,ncol=1))  #Dos paneles
  soporte_d <- c(0,3)      
  soporte_c <- c(0,6)

  
if (experimento ==1){  
soporte_h <- c(0,70)
soporte_f <- c(0,60)
Exp <- 1}else{
  soporte_h <- c(0,62)
  soporte_f <- c(0,25)
  Exp <- 2
}

    
###############################
  # DISCRIMINABBILITY (D'):  
        # Predictive Prior
  plot(soporte_d, axes=F, main="", ylab="", xlab="", xlim=c(0,6), ylim=c(0,1), col='white')
  for(a in 1:k){                                                      
    lines(density(PPr_d_a[,a]), lwd=2.5, col="deepskyblue3")
    lines(density(PPr_d_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
    axis(1)
    axis(2, labels=F, at=c(0,24))
    mtext("Predictive Prior density", side=2, line = 2, cex=1.5, las=0)
    mtext("D-prime", side=1, line = 2.5, cex=1, font=2)}
    mtext(paste("Predictive prior for D' - Experiment No.", Exp), font=2, cex=2, side=3)
            #Posterior density
    plot(soporte_d, axes=F, main="", ylab="", xlab="", xlim=c(0,6), ylim=c(0,3), col='white')
    for(a in 1:k){                                                      
      lines(density(d_a[,a]), lwd=2.5, col="deepskyblue3")
      lines(density(d_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
      axis(1)
      axis(2, labels=F, at=c(0,24))
      mtext("Posterior density", side=2, line = 2, cex=1.5, las=0)
      mtext("D-prime", side=1, line = 2.5, cex=1, font=2)}
    mtext(paste("Posterior estimates for D' - Experiment No.", Exp), font=2, cex=2, side=3)
    
  
###############################
  # BIAS (C):   
        # Predictive Prior
  plot(soporte_c, main="", ylab="", xlab="", col='white', xlim=c(-1.5,1.5), axes=F,
       ylim=c(0,0.5))
    for(a in 1:k){
      axis(1)
      axis(2, labels=F, at=c(0,24))
      lines(density(PPr_c_a[,a]), lwd=2.5, col="deepskyblue3")
      lines(density(PPr_c_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
      mtext("Predictive Prior Density", side=2, line = 2, cex=1.5, las=0)
      mtext("Bias - C", side=1, line = 2.5, cex=1, font=2)}
    mtext(paste("Predictive Priors for C - Experiment No.", Exp), font=2, cex=2, side=3)
        # Posterior density
  plot(soporte_c, main="", ylab="", xlab="", col='white', xlim=c(-1.5,1.5), axes=F)
  for(a in 1:k){
  axis(1)
  axis(2, labels=F, at=c(0,24))
  lines(density(c_a[,a]), lwd=2.5, col="deepskyblue3")
  lines(density(c_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
  mtext("Posterior Density", side=2, line = 2, cex=1.5, las=0)
  mtext("Bias - C", side=1, line = 2.5, cex=1, font=2)}
  mtext(paste("Posterior estimates for C - Experiment No.", Exp), font=2, cex=2, side=3)
  
 
  
  ###############################
    # THETA HITS:    
          #Predictive Prior
  plot(soporte_h, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0.3,1), axes=F,
       ylim=c(0,3))
  for(a in 1:k){
    axis(1)
    axis(2, labels=F, at=c(0,94))
    lines(density(PPr_tetaH_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(PPr_tetaH_b[,a]), lwd=2, col="darkorchid3", lty=1)
    legend(0.4,2, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Posterior density", 2, line = 2, cex=2.1, las=0)
    mtext(expression(paste(theta, "H")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Predictive prior for the Hit rates - Experiment No.", Exp), font=2, cex=2, side=3)
          #Posterior Density
  plot(soporte_h, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0.3,1), axes=F)
  for(a in 1:k){
  axis(1)
  axis(2, labels=F, at=c(0,94))
  lines(density(tetaH_a[,a]), lwd=2, col="deepskyblue3")
  lines(density(tetaH_b[,a]), lwd=2, col="darkorchid3", lty=1)
  legend(0.4,56, legend=c("A Class", "B Class"),
         col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Posterior density", 2, line = 2, cex=2.1, las=0)
  mtext(expression(paste(theta, "H")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Posterior estimates for the Hit rates - Experiment No.", Exp), font=2, cex=2, side=3)
  
  
  
  ###############################
    # THETA F.A:
            #Prior predictive
  plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0,0.7), axes=F,
       ylim=c(0,3))
  for(a in 1:k){
    lines(density(PPr_tetaFA_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(PPr_tetaFA_b[,a]), lwd=2, col="darkorchid3", lty=1)
    axis(1)
    axis(2, labels=F, at=c(0,94))
    legend(0.4,2, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Posterior density", side=2, line = 2.1, cex=2, las=0)
    mtext(expression(paste(theta, "F")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Predictive prior for the F.A rates - Experiment No.", Exp), font=2, cex=2, side=3)
            #Posterior density
  plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0,0.7), axes=F)
  for(a in 1:k){
  lines(density(tetaFA_a[,a]), lwd=2, col="deepskyblue3")
  lines(density(tetaFA_b[,a]), lwd=2, col="darkorchid3", lty=1)
  axis(1)
  axis(2, labels=F, at=c(0,94))
  legend(0.4,56, legend=c("A Class", "B Class"),
         col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
  mtext("Posterior density", side=2, line = 2.1, cex=2, las=0)
  mtext(expression(paste(theta, "F")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Posterior estimates for the F.A rates - Experiment No.", Exp), font=2, cex=2, side=3)
  
  
  
  
  
  
  
  ###################################################################################
  # Gráficos de Dispersión
  # Interacción entre parámetros (TauH y TauFA; D' y C)
  ###################################################################################

#Preparamos los datos
keep_ <- (1000)   #Numero de extracciones a incluir en el Gráfico
keep <- sample(niter, keep_)    #De las 'niter' extracciones, sacamos 'keep' muestras
#
d.FA_a <- density(tetaFA_a)
d.FA_b <- density(tetaFA_b)
d.H_a <- density(tetaH_a)
d.H_b <- density(tetaH_b)
d.D_a <- density(d_a)
d.D_b <- density(d_b)
d.C_a <- density(c_a)
d.C_b <- density(c_b)
d.TauH <- density(tauH)
d.TauF <- density(tauF)


layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

if (experimento ==1)
{ soporte_d <- c(0,3)
  soporte_c <- c(0,6)
  soporte_h <- c(0,62)
  soporte_f <- c(0,25)
  par(mar=c(2,2,1,0))
  plot(tetaFA_a[keep],tetaH_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,0.5), ylim=c(0.5,1))
  points(tetaFA_b[keep],tetaH_b[keep], col="darkorchid3")
  lines(c(0.36, 0.41),c(0.60,0.60), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.36, 0.41),c(0.55,0.55), lwd=2, lty=1, col="darkorchid3")
  text(0.42, 0.60, labels="A Condition", offset=0, cex = 0.8, pos=4)
  text(0.42, 0.55, labels="B Condition", offset=0, cex = 0.8, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(d.H_a$y, d.H_a$x, xlim=rev(c(0,16)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(0.5,1))
  lines(d.H_b$y, d.H_b$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste("Hits")), side=4,line=2.3, cex=0.9, las=0)
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(tetaFA_a),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,0.5),ylim=c(0,26))
  lines(density(tetaFA_b), col="darkorchid3")
  axis(1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
  mtext(expression(paste("False Alarms")), side=1.2,line=2, cex=0.9)
  box(lty=1)
  
  # D' y C
  
  par(mar=c(2,2,1,0))
  plot(d_a[keep],c_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,5), ylim=c(-1,1))
  points(d_b[keep],c_b[keep], col="darkorchid3")
  lines(c(0.2, 0.6),c(0.9,0.9), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.6),c(0.7,0.7), lwd=2, lty=1, col="darkorchid3")
  text(0.65, 0.9, labels="A Condition", offset=0, cex = 0.8, pos=4)
  text(0.65, 0.7, labels="B Condition", offset=0, cex = 0.8, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(d.C_a$y, d.C_a$x, xlim=rev(c(0,5)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(-1,1))
  lines(d.C_b$y, d.C_b$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste("C (Bias)")), side=4,line=2.3, cex=0.9, font=2, las=0)
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(d_a),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,5),ylim=c(0,3))
  lines(density(d_b), col="darkorchid3")
  axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5))
  mtext(expression(paste("D prime")), side=1.2,line=2, cex=0.9, font=2)
  box(lty=1)
}


if (experimento ==2)
{
  soporte_d <- c(0,3)
  soporte_c <- c(0,6)
  soporte_h <- c(0,62)
  soporte_f <- c(0,25)

  #Density Plot
  par(mar=c(2,2,1,0))  
  plot(tetaFA_a[keep],tetaH_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,0.5), ylim=c(0.45,1))
  points(tetaFA_b[keep],tetaH_b[keep], col="darkorchid3")
  lines(c(0.02, 0.07),c(0.90,0.90), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.02, 0.07),c(0.85,0.85), lwd=2, lty=1, col="darkorchid3")
  text(0.08, 0.90, labels="A Condition", offset=0, cex = 0.8, pos=4)
  text(0.08, 0.85, labels="B Condition", offset=0, cex = 0.8, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(d.H_a$y, d.H_a$x, xlim=rev(c(0,17)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(0.45,1))
  lines(d.H_b$y, d.H_b$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste("Hits")), side=4,line=2.3, cex=0.9, las=0)
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(tetaFA_a),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,0.5),ylim=c(0,16))
  lines(density(tetaFA_b), col="darkorchid3")
  axis(1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
  mtext(expression(paste("False Alarms")), side=1.2,line=2, cex=0.9)
  box(lty=1)
  
  # D' y C
  
  par(mar=c(2,2,1,0))
  plot(d_a[keep],c_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,3), ylim=c(-1,1))
  points(d_b[keep],c_b[keep], col="darkorchid3")
  lines(c(0.2, 0.6),c(0.90,0.90), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.6),c(0.80,0.80), lwd=2, lty=1, col="darkorchid3")
  text(0.7, 0.90, labels="A Condition", offset=0, cex = 0.8, pos=4)
  text(0.7, 0.80, labels="B Condition", offset=0, cex = 0.8, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(d.C_a$y, d.C_a$x, xlim=rev(c(0,6)),type='l', col="deepskyblue3", axes=F, xlab="", ylab="",ylim=c(-1,1))
  lines(d.C_b$y, d.C_b$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste(mu, "C (Bias)")), side=4,line=2.3, cex=0.9, font=2, las=0)
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(d_a),zero.line=F ,main="", col="deepskyblue3", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,3),ylim=c(0,3))
  lines(density(d_b), col="darkorchid3")
  axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0))
  mtext(expression(paste("D prime")), font=2, side=1.2,line=2, cex=0.9)
  box(lty=1)
}


###################################################################################
# Tau
# Posteriores individuales para Tau por sujeto
# ###################################################################################
layout(matrix(1:1,ncol=1))
prior_tau <- rnorm((niter-burnin),0,.1)
plot(density(prior_tau), main="Prior Tau ~ Normal(0,0.1)")

layout(matrix(1:2,ncol=1))
####Un color diferente por sujeto
coltaufa <- c('chocolate', 'chocolate1', 'chocolate2', 'chocolate3', 'chocolate4', 'firebrick4', 'coral1', 'coral2', 'coral3', 'coral4','darkgoldenrod', 'brown', 'brown4', 'darkgoldenrod3', 'darkgoldenrod4','darkorange','coral4', 'darkorange2', 'darkorange3', 'darkorange4', 'goldenrod3')
coltauh <- c('darkolivegreen', 'darkolivegreen1', 'darkolivegreen2', 'darkolivegreen3', 'darkolivegreen4', 'darkseagreen', 'darkseagreen1', 'darkseagreen2', 'darkseagreen3', 'darkseagreen4','chartreuse4', 'chartreuse3', 'chartreuse2', 'aquamarine4', 'aquamarine3','aquamarine2','darkgreen', 'forestgreen', 'darkcyan', 'darkgoldenrod4', 'darkkhaki')
soporte_t<- c(0,35)
#####Tres colores diferentes por Tau: Ayuda a distinguir los colores sin cargar demasiado la gráfica
taucolfa <- c('chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2')
taucolh <- c('darkgreen','forestgreen','chartreuse3', 'darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3')

  par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
      font.lab = 2, cex.axis = 1.3, bty = "n", las=1, cex.main=2)
  
  plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(-0.15,0.3), col='white')
  for(a in 1:k){
  title(paste("Experiment No.", exp), line=2.2, cex=1)
  lines(density(tauH[,a]), lwd=2.5, col=taucolh[a], ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}
  lines(density(prior_tau), col="blue", lwd=3)
  axis(1)
  abline(v=0, col='black', lty=2, lwd=3)
  #axis(2, labels=F, at=c(0,24))
  #mtext("Density", side=2, line = 0, cex=1, las=0)
  mtext("Differences on Hit Rates across classes of stimuli", side=3, line = 0.2, cex=1.5, font=1)
  mtext("Tau-H", side=1, line = 3, cex=2, font=2)

  
  plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(-0.1,0.35), col='white')
  for (a in 1:k){
  lines(density(tauF[,a]), lwd=2.5, col=taucolfa[a], ylab="", main="", xlab="", xlim=c(-0.5,0.5), axes=F)
  }
  lines(density(prior_tau), col="blue", lwd=3)
  axis(1) 
  abline(v=0, col='black', lty=2, lwd=3)
  #axis(2, labels=F, at=c(0,24))
  #mtext("Density", side=2, line = 2, cex=1, las=0)
  mtext("Tau-F", side=1, line = 3, cex=2, font=2)
  mtext("Differences on F.A. Rates across classes of stimuli", side=3, line = 0.2, cex=1.5, font=1)




############################
# Posterior Densities for individual TauH and TauF



#plotear <- "Tau(Hits)"
plotear <- "Tau(FA)"


layout(matrix(1:1,ncol=1))
ifelse(plotear=="Tau(Hits)", datos <- tauH, datos <- tauF)
if(experimento ==1){
  if(plotear=="Tau(Hits)"){
  Leg <- c(4.5,0.3)  
  }else{
    Leg <- c(3.5,0.35)  
  }
}else{
    if(plotear=="Tau(Hits)"){
      Leg <- c(3.5,0.53)  
    }else{
      Leg <- c(3.5,0.34)  
    }
}


x_axis <- NULL
y_axis <- NULL
numero <- 0
media_post <- NULL
Savage_Dickey_0 <- NULL
Savage_Dickey_mean <- NULL
color_SD_0 <- NULL
color_SD_mean <- NULL
for(i in 1:ncol(datos)){
  numero <- numero+1
  a <- sample(datos[,i],1000)
  espacio_init <- ((numero-1) * length(a)) + 1
  espacio_fin <- numero * length(a)
  y_axis[espacio_init:espacio_fin] <- a
  media_post[i] <- mean(datos[,i])
  Savage_Dickey_0[i] <- dnorm(0,0,0.1)/dnorm(0,mean(datos[,i]),sd(datos[,i]))
    ifelse(Savage_Dickey_0[i] == 0, color_SD_0[i] <- "black",
         ifelse(Savage_Dickey_0[i] >= 1, color_SD_0[i] <- "cyan4",
                color_SD_0[i]<- "darkgoldenrod2"))
  Savage_Dickey_mean[i] <- dnorm(mean(datos[,i]),0,0.1)/dnorm(mean(datos[,i]),mean(datos[,i]),sd(datos[,i]))
  ifelse(Savage_Dickey_mean[i] == 0, color_SD_mean[i] <- "black",
         ifelse(Savage_Dickey_mean[i] >= 1, color_SD_mean[i] <- "cyan4",
                color_SD_mean[i]<- "darkgoldenrod2"))
}

numero <- 0
for(i in 1:ncol(datos)){
  numero <- numero+1
  b <- rep(numero, 1000)
  espacio_init <- ((numero-1) * length(b)) + 1
  espacio_fin <- numero * length(b)
  x_axis[espacio_init:espacio_fin] <- b
}



numero <- 0
plot(x_axis, y_axis, ann=F, axes=F,cex=0.9)
for(u in 1:20){
  numero <- numero + 1
  points(numero,0, col=color_SD_0[u], pch=16, cex=2, type="p")}
mtext(side=2, text = plotear, line=2.2, cex=1.5)
mtext(side=1, text = "Participants", line=2.2, cex=1.5)
legend(Leg[1],Leg[2], legend=c("BF01 < 1", "BF01 => 1"),
       col=c("darkgoldenrod3", "cyan4"), pch=16, cex=1.2)
axis(1,c(1:20),c(1:20))
axis(2,seq(-0.2,0.5,0.1),seq(-0.2,0.5,0.1))
title(paste(plotear, "individual posterior densities; BF at 0"))
mtext(side=3, paste("Experiment No.", exp), line=-0.5,f=2)


numero <- 0
plot(x_axis, y_axis, ann=F, axes=F,cex=0.9)
for(u in 1:20){
  numero <- numero + 1
  points(numero,media_post[u], col=color_SD_mean[u], pch=16, cex=2, type="p")}
mtext(side=2, text = plotear, line=2.2, cex=1.5)
mtext(side=1, text = "Participants", line=2.2, cex=1.5)
legend(Leg[1],Leg[2], legend=c("BF01 < 1", "BF01 => 1"),
       col=c("darkgoldenrod3", "cyan4"), pch=16, cex=1.2)
axis(1,c(1:20),c(1:20))
axis(2,seq(-0.2,0.5,0.1),seq(-0.2,0.5,0.1))
title(paste(plotear, "individual posterior densities; BF at mean value"))
mtext(side=3, paste("Experiment No.", exp), line=-0.5,f=2)





