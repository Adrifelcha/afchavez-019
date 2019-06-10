setwd("C:/Users/Alejandro/Desktop/afchavez19/MichaelLee/SDT Mirror Effect/Data")
rm(list=ls())
dir()
library(R2jags)
##############################################################
##############################################################
#Diferencias en Hits y Falsas Alarmas
#Modelo 1 :  Diferencias entre las Tasas H y FA (parametros TauH y TauF)
#            contra datos generados usando las priors
##############################################################




######################################################
#Especificamos el Experimento y los Datos a analizar
experimento <- 1
###################

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



############################################################################
############################################################################
#Escribimos el modelo Inicial
############################################################################
write('
model{
  #Posterior estimations (based on the data collected)
  for (i in 1:k){
  # Posterior density based on our data
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
  #Plain priors for drawing prior predictions
  for (i in 1:k){
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

niter <- 50000    #Iteraciones
burnin <- 5000     #No. de primeros sampleos en ignorarse

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
  Pr_d_a <- samples$BUGSoutput$sims.list$Pr_d_A
  Pr_d_b <- samples$BUGSoutput$sims.list$Pr_d_B
  
  Pr_c_a <- samples$BUGSoutput$sims.list$Pr_c_A
  Pr_c_b <- samples$BUGSoutput$sims.list$Pr_c_B
  
  Pr_Ha <- samples$BUGSoutput$sims.list$prior_h_A
  Pr_Hb <- samples$BUGSoutput$sims.list$prior_h_B
  Pr_Fa <- samples$BUGSoutput$sims.list$prior_fa_A
  Pr_Fb <- samples$BUGSoutput$sims.list$prior_fa_B
  
  
  Pr_tetaH_a <- samples$BUGSoutput$sims.list$Pr_thetah_A
  Pr_tetaH_b <- samples$BUGSoutput$sims.list$Pr_thetah_B
  Pr_tetaFA_a <- samples$BUGSoutput$sims.list$Pr_thetaf_A
  Pr_tetaFA_b <- samples$BUGSoutput$sims.list$Pr_thetaf_B
  
  Pr_tauH <- samples$BUGSoutput$sims.list$PRIOR_Tau_H
  Pr_tauF <- samples$BUGSoutput$sims.list$PRIOR_Tau_F

Predicted_Ha <- NULL
Predicted_Hb <- NULL
Predicted_Fa <- NULL
Predicted_Fb <- NULL

for(i in 1:k){
  Predicted_Ha[i] <- round(median(density(Pr_Ha[,i])$x[which(density(Pr_Ha[,i])$y==max(density(Pr_Ha[,i])$y))-rbinom(1,10,.5)]),0)
  Predicted_Hb[i] <- round(median(density(Pr_Hb[,i])$x[which(density(Pr_Hb[,i])$y==max(density(Pr_Hb[,i])$y))-rbinom(1,10,.5)]),0)
  Predicted_Fa[i] <- round(median(density(Pr_Fa[,i])$x[which(density(Pr_Fa[,i])$y==max(density(Pr_Fa[,i])$y))+rbinom(1,10,.5)]),0)
  Predicted_Fb[i] <- round(median(density(Pr_Fb[,i])$x[which(density(Pr_Fb[,i])$y==max(density(Pr_Fb[,i])$y))+rbinom(1,10,.5)]),0) 
  }
  

############################################################################
############################################################################
#Escribimos nuevamente el modelo para obtener estimados basados en las Priors
############################################################################
write('
      model{
      for (i in 1:k){
      # Prior predicted counts
      Predicted_Ha[i] ~ dbin(PPr_thetah_A[i],s)
      Predicted_Fa[i] ~ dbin(PPr_thetaf_A[i],n)
      Predicted_Hb[i] ~ dbin(PPr_thetah_B[i],s)
      Predicted_Fb[i] ~ dbin(PPr_thetaf_B[i],n)
      # Reparameterization Using Equal-Variance Gaussian SDT
      PPr_thetah_A[i] <- phi(PPr_d_A[i]/2-PPr_c_A[i])
      PPr_thetaf_A[i] <- phi(-PPr_d_A[i]/2-PPr_c_A[i])
      PPr_thetah_B[i] <- phi(PPr_d_B[i]/2-PPr_c_B[i])
      PPr_thetaf_B[i] <- phi(-PPr_d_B[i]/2-PPr_c_B[i])
      # These Priors over Discriminability and Bias Correspond 
      # to Uniform Priors over the Hit and False Alarm Rates
      PPr_d_A[i] ~ dnorm(0,1)T(0,6)
      PPr_c_A[i] ~ dnorm(0,0.7)
      PPr_d_B[i] ~ dnorm(0,1)T(0,6)
      PPr_c_B[i] ~ dnorm(0,0.7)
      #Differences on dprime
      PPRIOR_Tau_H[i] <- PPr_thetah_A[i]-PPr_thetah_B[i]
      PPRIOR_Tau_F[i] <- PPr_thetaf_B[i]-PPr_thetaf_A[i]
      }}','Tau_predictive.bug')

######################################
######################################
# Definimos los elementos de trabajo
######################################
data <- list("Predicted_Ha", "Predicted_Hb", "Predicted_Fa", "Predicted_Fb", "s", "n", "k")                    
#Los datos que vamos a utilizar para nuestro modelo
myinits <- list(
  list(PPr_d_A = rep(0,k), PPr_c_A = rep(0,k), PPr_d_B = rep(0,k), PPr_c_B = rep(0,k)))      
#Valores iniciales para las extracciones de las cadenas de Markov

#Parámetros monitoreados
parameters <- c("PPr_d_A", "PPr_c_A", "PPr_thetah_A", "PPr_thetaf_A", "PPr_d_B", "PPr_c_B", "PPr_thetah_B", 
                "PPr_thetaf_B","PPRIOR_Tau_H", "PPRIOR_Tau_F")

niter <- 100000    #Iteraciones
burnin <- 5000     #No. de primeros sampleos en ignorarse

#Corremos el modelo
samples <- jags(data, inits=myinits, parameters,
                model.file ="Tau_predictive.bug",
                n.chains=1, n.iter=niter, n.burnin=burnin, n.thin=1)

####################################################################
# Jalamos los resultados de correr el modelo (Inferencias)
# a.k.a.:
#Le ponemos una etiqueta a cada elemento contenido en Samples
####################################################################

#Prior predictiva
PPr_d_a <- samples$BUGSoutput$sims.list$PPr_d_A
PPr_d_b <- samples$BUGSoutput$sims.list$PPr_d_B

PPr_c_a <- samples$BUGSoutput$sims.list$PPr_c_A
PPr_c_b <- samples$BUGSoutput$sims.list$PPr_c_B

PPr_tetaH_a <- samples$BUGSoutput$sims.list$PPr_thetah_A
PPr_tetaH_b <- samples$BUGSoutput$sims.list$PPr_thetah_B
PPr_tetaFA_a <- samples$BUGSoutput$sims.list$PPr_thetaf_A
PPr_tetaFA_b <- samples$BUGSoutput$sims.list$PPr_thetaf_B

PPr_tauH <- samples$BUGSoutput$sims.list$PPRIOR_Tau_H
PPr_tauF <- samples$BUGSoutput$sims.list$PPRIOR_Tau_F






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

  ##########################
  # Predicted vs Observed Total Counts
  ################################
  # Total number of Hits
    plot(c(1:k), h_A, col="white", pch=18, cex=2, ylim=c(80,160),
       axes=F, xlab="", ylab="")
  points(c(1:k),Predicted_Ha, col="deepskyblue1", pch=15, cex=3)
  points(c(1:k),Predicted_Hb, col="mediumorchid1", pch=15, cex=3)
  points(c(1:k),h_B, col="magenta4", pch=18, cex=2)
  points(c(1:k),h_A, col="royalblue3", pch=18, cex=2)
  axis(1,c(1:20),c(1:20))
  axis(2,seq(80,160,5),seq(80,160,5), line=-1)
  mtext("Total number of Hits", side=2, line = 2, cex=1.5, las=0)
  mtext("Participant", side=1, line = 2.5, cex=1, font=2)
  mtext(paste("Predicted vs Observed number of Hits - Experiment No.", Exp), font=2, cex=2, side=3)
  numero <- 0
  linea <- 1.5
  for(u in 1:k){
    numero <- numero + 1
    if(u<9){
    lines(c(linea,linea),c(101,160), lty=2)  
    }else{
    lines(c(linea,linea),c(80,160), lty=2)}
    linea <- linea+1
  }
  points(1.8,97, col="deepskyblue1", pch=15, cex=3)
  points(1.8,92, col="mediumorchid1", pch=15, cex=3)
  points(1.8,87, col="magenta4", pch=18, cex=2)
  points(1.8,82, col="royalblue3", pch=18, cex=2)
  text(5.5,97, "Prior prediction for the number of Hits - A Class", f=2)
  text(5.5,92, "Prior prediction for the number of Hits - B Class", f=2)
  text(4.5,87, "Observed number of Hits - A Class", f=2)
  text(4.5,82, "Observed number of Hits - B Class", f=2)
  
  # Total number of False Alarms
  plot(c(1:k), h_A, col="white", pch=18, cex=2, ylim=c(0,80),
       axes=F, xlab="", ylab="")
  points(c(1:k),Predicted_Fa, col="deepskyblue1", pch=15, cex=3)
  points(c(1:k),Predicted_Fb, col="mediumorchid1", pch=15, cex=3)
  points(c(1:k),fa_B, col="magenta4", pch=18, cex=2.5)
  points(c(1:k),fa_A, col="royalblue3", pch=18, cex=2)
  axis(1,c(1:20),c(1:20))
  axis(2,seq(0,80,5),seq(0,80,5), line=-1)
  mtext("Total number of False Alarms", side=2, line = 2, cex=1.5, las=0)
  mtext("Participant", side=1, line = 2.5, cex=1, font=2)
  mtext(paste("Predicted vs Observed number of False Alarms - Experiment No.", Exp), font=2, cex=2, side=3)
  numero <- 0
  linea <- 1.5
  for(u in 1:k){
    numero <- numero + 1
    if(u>12){
    lines(c(linea,linea),c(0,60), lty=2)  
    }else{
    lines(c(linea,linea),c(0,80), lty=2)}
    linea <- linea+1
  }
  points(12.8,78, col="deepskyblue1", pch=15, cex=3)
  points(12.8,73, col="mediumorchid1", pch=15, cex=3)
  points(12.8,68, col="magenta4", pch=18, cex=2)
  points(12.8,63, col="royalblue3", pch=18, cex=2)
  text(16.5,78, "Prior prediction for the number of Hits - A Class", f=2)
  text(16.5,73, "Prior prediction for the number of Hits - B Class", f=2)
  text(15.5,68, "Observed number of Hits - A Class", f=2)
  text(15.5,63, "Observed number of Hits - B Class", f=2)
  
  
  
  ##########################
  # Predicted vs Observed Rates
  ################################
  Hrate_A <- h_A/160
  Hrate_B <- h_B/160
  Frate_A <- fa_A/160
  Frate_B <- fa_B/160
  
  Predicted_HRate_A <- Predicted_Ha/160
  Predicted_HRate_B <- Predicted_Hb/160
  Predicted_FRate_A <- Predicted_Fa/160
  Predicted_FRate_B <- Predicted_Fb/160
  
  # Hit Rates
  plot(c(1:k), Hrate_A, col="white", pch=18, cex=2, ylim=c(.5,1),
       axes=F, xlab="", ylab="")
  points(c(1:k),Predicted_HRate_A, col="deepskyblue1", pch=15, cex=3)
  points(c(1:k),Predicted_HRate_B, col="mediumorchid1", pch=15, cex=3)
  points(c(1:k),Hrate_B, col="magenta4", pch=18, cex=2)
  points(c(1:k),Hrate_A, col="royalblue3", pch=18, cex=2)
  axis(1,c(1:20),c(1:20))
  axis(2,seq(.5,1,.05),seq(.5,1,.05), line=-1)
  mtext("Hit Rates", side=2, line = 2, cex=1.5, las=0)
  mtext("Participant", side=1, line = 2.5, cex=1, font=2)
  mtext(paste("Predicted vs Observed Hit Rates - Experiment No.", Exp), font=2, cex=2, side=3)
  numero <- 0
  linea <- 1.5
  for(u in 1:k){
    numero <- numero + 1
    if(u<8){
      lines(c(linea,linea),c(0.62,1), lty=2)  
    }else{
      lines(c(linea,linea),c(0.5,1), lty=2)}
    linea <- linea+1
  }
  points(1.8,.6, col="deepskyblue1", pch=15, cex=3)
  points(1.8,.57, col="mediumorchid1", pch=15, cex=3)
  points(1.8,.54, col="magenta4", pch=18, cex=2)
  points(1.8,.51, col="royalblue3", pch=18, cex=2)
  text(5.1,.6, "Prior prediction for the Hit rates - A Class", f=2)
  text(5.1,.57, "Prior prediction for the Hit rates - B Class", f=2)
  text(4.5,.54, "Observed number of Hits - A Class", f=2)
  text(4.5,.51, "Observed number of Hits - B Class", f=2)
  
  # Total number of False Alarms
  plot(c(1:k), Frate_B, col="white", pch=18, cex=2, ylim=c(0,.5),
       axes=F, xlab="", ylab="")
  points(c(1:k),Predicted_FRate_A, col="deepskyblue1", pch=15, cex=3)
  points(c(1:k),Predicted_FRate_B, col="mediumorchid1", pch=15, cex=3)
  points(c(1:k),Frate_B, col="magenta4", pch=18, cex=2)
  points(c(1:k),Frate_A, col="royalblue3", pch=18, cex=2)
  axis(1,c(1:20),c(1:20))
  axis(2,seq(0,0.5,.05),seq(0,0.5,.05), line=-1)
  mtext("False Alarm Rate", side=2, line = 2, cex=1.5, las=0)
  mtext("Participant", side=1, line = 2.5, cex=1, font=2)
  mtext(paste("Predicted vs Observed number of False Alarms - Experiment No.", Exp), font=2, cex=2, side=3)
  numero <- 0
  linea <- 1.5
  for(u in 1:k){
    numero <- numero + 1
    if(u>12){
      lines(c(linea,linea),c(0,0.4), lty=2)  
    }else{
      lines(c(linea,linea),c(0,0.5), lty=2)}
    linea <- linea+1
  }
  points(13.8,.49, col="deepskyblue1", pch=15, cex=3)
  points(13.8,.47, col="mediumorchid1", pch=15, cex=3)
  points(13.8,.45, col="magenta4", pch=18, cex=2)
  points(13.8,.43, col="royalblue3", pch=18, cex=2)
  text(17.1,.49, "Prior prediction for the F.A. rates - A Class", f=2)
  text(17.1,.47, "Prior prediction for the F.A. rates - B Class", f=2)
  text(16.5,.45, "Observed number of Hits - A Class", f=2)
  text(16.5,.43, "Observed number of Hits - B Class", f=2)
  
  
  
    
###############################
  # DISCRIMINABBILITY (D'):  
         # Prior Distribution
  plot(soporte_d, axes=F, main="", ylab="", xlab="", xlim=c(0,6), ylim=c(0,1), col='white')
  for(a in 1:k){                                                      
    lines(density(Pr_d_a[,a]), lwd=2.5, col="deepskyblue3")
    lines(density(Pr_d_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
    axis(1)
    axis(2, labels=F, at=c(0,24))
    mtext("Prior density", side=2, line = 2, cex=1.5, las=0)
    mtext("D-prime", side=1, line = 2.5, cex=1, font=2)}
  mtext(paste("Prior distribution for D' - Experiment No.", Exp), font=2, cex=2, side=3)
        # Predictive Prior
  plot(soporte_d, axes=F, main="", ylab="", xlab="", xlim=c(0,6), ylim=c(0,3), col='white')
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
         # Prior Distribution
    plot(soporte_c, main="", ylab="", xlab="", col='white', xlim=c(-1.5,1.5), axes=F,
         ylim=c(0,0.5))
    for(a in 1:k){
      axis(1)
      axis(2, labels=F, at=c(0,24))
      lines(density(Pr_c_a[,a]), lwd=2.5, col="deepskyblue3")
      lines(density(Pr_c_b[,a]), lwd=2.5, col="darkorchid3", lty=1)
      mtext("Prior Density", side=2, line = 2, cex=1.5, las=0)
      mtext("Bias - C", side=1, line = 2.5, cex=1, font=2)}
    mtext(paste("Prior distribution for C - Experiment No.", Exp), font=2, cex=2, side=3)
        # Predictive Prior
  plot(soporte_c, main="", ylab="", xlab="", col='white', xlim=c(-1.5,1.5), axes=F)
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
          #Prior Distribution
  plot(soporte_h, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0.3,1), axes=F,
       ylim=c(0,3))
  for(a in 1:k){
    axis(1)
    axis(2, labels=F, at=c(0,94))
    lines(density(Pr_tetaH_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(Pr_tetaH_b[,a]), lwd=2, col="darkorchid3", lty=1)
    legend(0.4,2, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Prior density", 2, line = 2, cex=2.1, las=0)
    mtext(expression(paste(theta, "H")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Prior distribution for the Hit rates - Experiment No.", Exp), font=2, cex=2, side=3)
          #Predictive Prior
  plot(soporte_h, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0.3,1), axes=F, ylim=c(0,27))
  for(a in 1:k){
    axis(1)
    axis(2, labels=F, at=c(0,94))
    lines(density(PPr_tetaH_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(PPr_tetaH_b[,a]), lwd=2, col="darkorchid3", lty=1)
    legend(0.35,8, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Predictive Prior density", 2, line = 2, cex=2.1, las=0)
    mtext(expression(paste(theta, "H")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Predictive prior for the Hit rates - Experiment No.", Exp), font=2, cex=2, side=3)
          #Posterior Density
  plot(soporte_h, col="white", main="", cex.main=3, ylab="", xlab="", 
       xlim=c(0.3,1), axes=F, ylim=c(0,80))
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
            #Prior distributions
  plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0,0.7), axes=F,
       ylim=c(0,3))
  for(a in 1:k){
    lines(density(Pr_tetaFA_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(Pr_tetaFA_b[,a]), lwd=2, col="darkorchid3", lty=1)
    axis(1)
    axis(2, labels=F, at=c(0,94))
    legend(0.4,2, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Prior density", side=2, line = 2.1, cex=2, las=0)
    mtext(expression(paste(theta, "F")), side=1, line = 2.8, cex=2.5, font=2)}
  mtext(paste("Prior distribution for the F.A rates - Experiment No.", Exp), font=2, cex=2, side=3)
            #Prior predictive
  plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(0,0.7), axes=F,
       ylim=c(0,27))
  for(a in 1:k){
    lines(density(PPr_tetaFA_a[,a]), lwd=2, col="deepskyblue3")
    lines(density(PPr_tetaFA_b[,a]), lwd=2, col="darkorchid3", lty=1)
    axis(1)
    axis(2, labels=F, at=c(0,94))
    legend(0.1,8, legend=c("A Class", "B Class"),
           col=c("dodgerblue2", "darkorchid2","dodgerblue4", "darkorchid4"), lty=1, cex=1.2, lwd=c(2,2,5,5))
    mtext("Predictive prior density", side=2, line = 2.1, cex=2, las=0)
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
keep_ <- (4000)   #Numero de extracciones a incluir en el Gráfico
keep <- sample(length(tetaFA_a),keep_)    #De las 'niter' extracciones, sacamos 'keep' muestras
#

layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

if (experimento ==1)
{}else{
  } 

soporte_d <- c(0,3)
soporte_c <- c(0,6)
soporte_h <- c(0,62)
soporte_f <- c(0,25)

#######################################
# Theta Hits y Falsas Alarmas

  #PREDICTED
  par(mar=c(2,2,1,0))
  plot(PPr_tetaFA_a[keep],PPr_tetaH_a[keep], col="deepskyblue3", pch=16,
       xlab="", ylab="", axes=F,xlim=c(0,0.6), ylim=c(0.5,1))
  points(PPr_tetaFA_b[keep],PPr_tetaH_b[keep], col="darkorchid3")
  lines(c(0.48, 0.51),c(0.59,0.59), lwd=2.5, lty=2, col="deepskyblue3")
  lines(c(0.48, 0.51),c(0.56,0.56), lwd=2.5, lty=2, col="darkorchid3")
  text(0.52, 0.59, labels="A Condition", offset=0, cex = 1.2, pos=4)
  text(0.52, 0.56, labels="B Condition", offset=0, cex = 1.2, pos=4)
  box(lty=1)

  par(mar=c(2,1,1,4))
  plot(soporte_h, xlim=rev(c(0,27)),type='l', col="white", axes=F, xlab="", 
     ylab="",ylim=c(0.5,1))
  for(a in 1:k){  
  lines(density(PPr_tetaH_a[,a])$y,density(PPr_tetaH_a[,a])$x, col="deepskyblue3")
  lines(density(PPr_tetaH_b[,a])$y,density(PPr_tetaH_b[,a])$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste("Hits")), side=4,line=2.3, cex=1.3, las=0)}
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(PPr_tetaFA_a),zero.line=F ,main="", col="white", ylab="", 
     xlab="", cex.lab=1.3, axes=F, xlim=c(0,0.6),ylim=c(0,30))
for(a in 1:k){  
  lines(density(PPr_tetaFA_a[,a]), col="deepskyblue3")
  lines(density(PPr_tetaFA_b[,a]), col="darkorchid3")
  axis(1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
  mtext(expression(paste("False Alarms")), side=1.2,line=2, cex=1.3)}
box(lty=1)

  # OBSERVED
  par(mar=c(2,2,1,0))
  plot(tetaFA_a[keep],tetaH_a[keep], col="deepskyblue3", xlab="", ylab="", 
       axes=F,xlim=c(0,0.6), ylim=c(0.5,1))
  points(tetaFA_b[keep],tetaH_b[keep], col="darkorchid3")
  lines(c(0.48, 0.51),c(0.59,0.59), lwd=2.5, lty=2, col="deepskyblue3")
  lines(c(0.48, 0.51),c(0.56,0.56), lwd=2.5, lty=2, col="darkorchid3")
  text(0.52, 0.59, labels="A Condition", offset=0, cex = 1.2, pos=4)
  text(0.52, 0.56, labels="B Condition", offset=0, cex = 1.2, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(soporte_h, xlim=rev(c(0,86)),type='l', col="deepskyblue3", axes=F, xlab="", 
       ylab="",ylim=c(0.5,1))
  for(a in 1:k){  
  lines(density(tetaH_a[,a])$y,density(tetaH_a[,a])$x, col="deepskyblue3")
  lines(density(tetaH_b[,a])$y,density(tetaH_b[,a])$x, col="darkorchid3")
  axis(4)
  mtext(expression(paste("Hits")), side=4,line=2.3, cex=1.3, las=0)}
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(tetaFA_a),zero.line=F ,main="", col="white", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,0.6),ylim=c(0,46))
  for(a in 1:k){  
  lines(density(tetaFA_a[,a]), col="deepskyblue3")
  lines(density(tetaFA_b[,a]), col="darkorchid3")
  axis(1, at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5))
  mtext(expression(paste("False Alarms")), side=1.2,line=2, cex=1.3)}
  box(lty=1)
  
  

  #######################################
  # D' y C
  
  
  #Predicted
  par(mar=c(2,2,1,0))
  plot(PPr_d_a[keep],PPr_c_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,5.5), ylim=c(-1.1,1.1))
  points(PPr_d_b[keep],PPr_c_b[keep], col="darkorchid3")
  lines(c(0.2, 0.45),c(0.9,0.9), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.45),c(0.7,0.7), lwd=2, lty=1, col="darkorchid3")
  text(0.5, 0.9, labels="A Condition", offset=0, cex = 1.2, pos=4)
  text(0.5, 0.7, labels="B Condition", offset=0, cex = 1.2, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(soporte_c, xlim=rev(c(0,4)),type='l', col="white", axes=F, 
       xlab="", ylab="",ylim=c(-1,1))
  for(a in 1:k){
    lines(density(PPr_c_a[,a])$y, density(PPr_c_a[,a])$x, col="darkorchid3")
    lines(density(PPr_c_b[,a])$y, density(PPr_c_b[,a])$x, col="deepskyblue3")
    axis(4)
    mtext(expression(paste("C (Bias)")), side=4,line=2.4, cex=1.3, las=0)}
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(d_a[,a]),zero.line=F ,main="", col="white", ylab="", xlab="", cex.lab=1.3, 
       axes=F, xlim=c(0,5),ylim=c(0,2))
  for(a in 1:k){
    lines(density(PPr_d_a[,a]), col="deepskyblue3")
    lines(density(PPr_d_b[,a]), col="darkorchid3")
    axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5))
    mtext(expression(paste("D prime")), side=1.2,line=2.5, cex=1.3)}
  box(lty=1)
  
  #OBSERVED
  par(mar=c(2,2,1,0))
  plot(d_a[keep],c_a[keep], col="deepskyblue3", xlab="", ylab="", axes=F,xlim=c(0,5.5), ylim=c(-1.1,1.1))
  points(d_b[keep],c_b[keep], col="darkorchid3")
  lines(c(0.2, 0.45),c(0.9,0.9), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.2, 0.45),c(0.7,0.7), lwd=2, lty=1, col="darkorchid3")
  text(0.5, 0.9, labels="A Condition", offset=0, cex = 1.2, pos=4)
  text(0.5, 0.7, labels="B Condition", offset=0, cex = 1.2, pos=4)
  box(lty=1)
  
  par(mar=c(2,1,1,4))
  plot(soporte_c, xlim=rev(c(0,5)),type='l', col="white", axes=F, xlab="", ylab="",ylim=c(-1,1))
  for(a in 1:k){
  lines(density(c_a[,a])$y, density(c_a[,a])$x, col="darkorchid3")
  lines(density(c_b[,a])$y, density(c_b[,a])$x, col="deepskyblue3")
  axis(4)
  mtext(expression(paste("C (Bias)")), side=4,line=2.4, cex=1.3, las=0)}
  box(lty=1)
  
  par(mar=c(6,2,0,0))
  plot(density(d_a[,a]),zero.line=F ,main="", col="white", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(0,5),ylim=c(0,3))
  for(a in 1:k){
  lines(density(d_a[,a]), col="deepskyblue3")
  lines(density(d_b[,a]), col="darkorchid3")
  axis(1, at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4, 4.5, 5))
  mtext(expression(paste("D prime")), side=1.2,line=2.5, cex=1.3)}
  box(lty=1)






###################################################################################
# Tau
# Posteriores individuales para Tau por sujeto
# ###################################################################################
layout(matrix(1:1,ncol=1))
prior_tauH <- as.vector(PPr_tauH)
prior_tauF <- as.vector(PPr_tauF)
plot(density(prior_tau), main="Prior Tau ~ Normal(0,0.1)")

layout(matrix(1:2,ncol=1))
soporte_t<- c(0,35)
taucolfa <- c('chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2','chocolate3','firebrick4','goldenrod2')
taucolh <- c('darkgreen','forestgreen','chartreuse3', 'darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3','darkgreen','forestgreen','chartreuse3')


if (experimento ==1){  
YLIM <- c(0,30)
  }else{
YLIM <- c(0,30)
}

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1, cex.main=2)

###############################
#Prior predictive - Tau
plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(-1,1), axes=F,
     ylim=c(0,20))
for(a in 1:k){
  lines(density(PPr_tauH[,a]), lwd=2, col="deepskyblue3")
  axis(1)
  axis(2, labels=F, at=c(0,94))
  mtext("Predictive prior density", side=2, line = 1.5, cex=1.2, las=0)
  mtext(expression(paste(tau, "Hits")), side=1, line = 2.8, cex=1.8, font=2)}
mtext(paste("Predictive prior for the differences between F.A rates"), font=2, cex=1.2, side=3, line=0.5)
mtext(paste("Experiment No.", Exp), font=2, cex=2, side=3, line=1.8)

plot(soporte_f, col="white", main="", cex.main=3, ylab="", xlab="", xlim=c(-1,1), axes=F,
     ylim=c(0,20))
for(a in 1:k){
  lines(density(PPr_tauF[,a]), lwd=2, col="deepskyblue3")
  axis(1)
  axis(2, labels=F, at=c(0,94))
  mtext("Predictive prior density", side=2, line = 1.5, cex=1.2, las=0)
  mtext(expression(paste(tau, "F.A")), side=1, line = 2.8, cex=1.8, font=2)}
mtext(paste("Predictive prior for the differences between Hit rates"), font=2, cex=1.2, side=3, line=0.5)


#Posteriors 
plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(-0.15,0.3), ylim=c(0,21), col='white')
for(a in 1:k){
title(paste("Experiment No.", exp), line=2.2, cex=1)
lines(density(tauH[,a]), lwd=2.5, col=taucolh[a], ylab="", xlab="", xlim=c(-0.5,0.5), axes=F)}
lines(density(PPr_tauH[,a]), col="blue", lwd=3)
axis(1)
abline(v=0, col='black', lty=2, lwd=3)
mtext("Differences on Hit Rates across classes of stimuli", side=3, line = 0.2, cex=1.5, font=1)
mtext(expression(paste(tau, "Hits")), side=1, line = 2.8, cex=1.8, font=2)

plot(soporte_t, axes=F, main="", ylab="", xlab="", xlim=c(-0.1,0.35), col='white')
for (a in 1:k){
lines(density(tauF[,a]), lwd=2.5, col=taucolfa[a], ylab="", main="", xlab="", xlim=c(-0.5,0.5), axes=F)}
lines(density(PPr_tauF[,a]), col="blue", lwd=3)
axis(1) 
abline(v=0, col='black', lty=2, lwd=3)
mtext(expression(paste(tau, "F.A.")), side=1, line = 2.8, cex=1.8, font=2)
mtext("Differences on F.A. Rates across classes of stimuli", side=3, line = 0.2, cex=1.5, font=1)




###########################  
###########################
###########################  
###########################
# Posterior Densities for individual TauH and TauF



plotear <- "Tau(Hits)"
#plotear <- "Tau(FA)"


layout(matrix(1:1,ncol=1))
ifelse(plotear=="Tau(Hits)", datos <- tauH, datos <- tauF)
ifelse(plotear=="Tau(Hits)", predictive <- PPr_tauF, predictive <- PPr_tauF)
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
  Savage_Dickey_0[i] <- dnorm(0,mean(predictive[,i]),sd(predictive[,i]))/dnorm(0,mean(datos[,i]),sd(datos[,i]))
    ifelse(Savage_Dickey_0[i] == 0, color_SD_0[i] <- "black",
         ifelse(Savage_Dickey_0[i] >= 1, color_SD_0[i] <- "cyan4",
                color_SD_0[i]<- "darkgoldenrod2"))
  Savage_Dickey_mean[i] <- dnorm(mean(datos[,i]),mean(predictive[,i]),sd(predictive[,i]))/dnorm(mean(datos[,i]),mean(datos[,i]),sd(datos[,i]))
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





