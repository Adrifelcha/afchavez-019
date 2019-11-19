#############################################################
# Codigo para generar plot de respuestas individuales
#
# Proyecto Transitividad en Pérdidas y Ganancias
# Autor: J.M. Niño García
#############################################################
#
# GANANCIAS
#
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/asus/Desktop/afchavez-019/Niño/Datos/Exp1/Ganancias")

Participantes <- 0
for(archive in dir()){
  Participantes <- Participantes+1 
}

colores <- c()
v <-0
b <-0
w <- 0.5
base <-0
plot(2,3, col="white", xlim=c(0,10), ylim=c(0,18), ann=FALSE, axes=F)
mtext(side=1, text = "Pair presented", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Participants", line=1, cex=1.3, f=2)
mtext(side=3, text = "Preference for the bigger alternative for each pair presented", line=1.5, cex=1.5, f=2)
mtext(side=3, text = "(All participants - Experiment 1 - GAINS)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0.5,17.5,1),labels=seq(1,18,1),las=1, line=-1.4, col = NA, col.ticks = 1)
axis(1,at=seq(0.5,9.5,1),labels=c(1,5,8,10,2,6,9,3,7,4), line=-0.8, f=2, col = NA, col.ticks = 1)
for(archive in dir()){
  Fel <- read.csv(archive)
  Par <- NULL
  Rate <- NULL
  Repet <- NULL
  Prop <- NULL
  left <- 0
  
  Par[(Fel$money_left==463&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==463)] <- '10' #4
  Par[(Fel$money_left==579&Fel$money_right==521)|(Fel$money_left==521&Fel$money_right==579)] <- '7' #9
  Par[(Fel$money_left==550&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==550)] <- '4' #10
  Par[(Fel$money_left==550&Fel$money_right==492)|(Fel$money_left==492&Fel$money_right==550)] <- '6' #6
  Par[(Fel$money_left==492&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==492)] <- '9' #7
  Par[(Fel$money_left==521&Fel$money_right==550)|(Fel$money_left==550&Fel$money_right==521)] <- '3' #8
  Par[(Fel$money_left==521&Fel$money_right==463)|(Fel$money_left==463&Fel$money_right==521)] <- '5' #2
  Par[(Fel$money_left==463&Fel$money_right==492)|(Fel$money_left==492&Fel$money_right==463)] <- '1' #1
  Par[(Fel$money_left==463&Fel$money_right==550)|(Fel$money_left==550&Fel$money_right==463)] <- '8' #3
  Par[(Fel$money_left==492&Fel$money_right==521)|(Fel$money_left==521&Fel$money_right==492)] <- '2' #5
  
  for(a in 1:10){
  Repet[a] <- length(which(Par==a))
  Rate[a] <- (sum(Fel$biggerchosenP[which(Par==a)]))/20
  Prop[a] <- Rate[a]/.05
  
  Pol_width <- c(left,left+1,left+1,left)
  Pol_prop <- c(base,base,(base+Rate[a]),(base+Rate[a]))
  polygon(Pol_width, Pol_prop,
            density = NA, col="lightblue2")
  left <- left+1
  }
  
  print(Rate)
  #print(unique(Repet))  #Sólo para asegurarme que se registraron bien los pares
  base <- base+1
}
for(vert in 1:11){
  lines(c(v,v),c(0,18),col='black', lty=1,lwd=2) 
  v <- v+1
}
for(vert in 1:19){
  lines(c(0,10),c(b,b),col='black', lty=1,lwd=2) 
  b <- b+1
}
for(vert in 1:18){
  lines(c(0,10),c(w,w),col='black', lty=3,lwd=1) 
  w <- w+1
}



#############################################################
#
# Pérdidas
#
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Jaime/Desktop/afchavez-019/Niño/Datos/Exp1/Perdidas")

Participantes <- 0
for(archive in dir()){
  Participantes <- Participantes+1 
}

colores <- c()
v <-0
w <-0.5
b <-0
base <-0
plot(2,3, col="white", xlim=c(0,10), ylim=c(0,18), ann=FALSE, axes=F)
mtext(side=1, text = "Pair presented", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Participants", line=1, cex=1.3, f=2)
mtext(side=3, text = "Preference for the bigger alternative for each pair presented", line=1.5, cex=1.5, f=2)
mtext(side=3, text = "(All participants - Experiment 1 - LOSSES)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0.5,17.5,1),labels=seq(1,18,1),las=1, line=-1.4, col = NA, col.ticks = 1)
axis(1,at=seq(0.5,9.5,1),labels=c(1,5,8,10,2,6,9,3,7,4), line=-0.8, f=2, col = NA, col.ticks = 1)
for(archive in dir()){
  Fel <- read.csv(archive)
  Par <- NULL
  Rate <- NULL
  Repet <- NULL
  Prop <- NULL
  left <- 0
  
  Par[(Fel$money_left==463&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==463)] <- '10' #4
  Par[(Fel$money_left==579&Fel$money_right==521)|(Fel$money_left==521&Fel$money_right==579)] <- '7' #9
  Par[(Fel$money_left==550&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==550)] <- '4' #10
  Par[(Fel$money_left==550&Fel$money_right==492)|(Fel$money_left==492&Fel$money_right==550)] <- '6' #6
  Par[(Fel$money_left==492&Fel$money_right==579)|(Fel$money_left==579&Fel$money_right==492)] <- '9' #7
  Par[(Fel$money_left==521&Fel$money_right==550)|(Fel$money_left==550&Fel$money_right==521)] <- '3' #8
  Par[(Fel$money_left==521&Fel$money_right==463)|(Fel$money_left==463&Fel$money_right==521)] <- '5' #2
  Par[(Fel$money_left==463&Fel$money_right==492)|(Fel$money_left==492&Fel$money_right==463)] <- '1' #1
  Par[(Fel$money_left==463&Fel$money_right==550)|(Fel$money_left==550&Fel$money_right==463)] <- '8' #3
  Par[(Fel$money_left==492&Fel$money_right==521)|(Fel$money_left==521&Fel$money_right==492)] <- '2' #5
  
  for(a in 1:10){
    Repet[a] <- length(which(Par==a))
    Rate[a] <- (sum(Fel$biggerchosenP[which(Par==a)]))/20
    Prop[a] <- Rate[a]/.05
    
    Pol_width <- c(left,left+1,left+1,left)
    Pol_prop <- c(base,base,(base+Rate[a]),(base+Rate[a]))
    polygon(Pol_width, Pol_prop,
            density = NA, col="lightblue3")
    left <- left+1
  }
  
  print(Rate)
  #print(unique(Repet))  #Sólo para asegurarme que se registraron bien los pares
  base <- base+1
}
for(vert in 1:11){
  lines(c(v,v),c(0,18),col='black', lty=1,lwd=2) 
  v <- v+1
}
for(vert in 1:19){
  lines(c(0,10),c(b,b),col='black', lty=1,lwd=2) 
  b <- b+1
}
for(vert in 1:18){
  lines(c(0,10),c(w,w),col='black', lty=3,lwd=1) 
  w <- w+1
}