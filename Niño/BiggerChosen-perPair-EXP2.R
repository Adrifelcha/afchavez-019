#############################################################
# Codigo para generar plot de respuestas individuales
#
# Proyecto Transitividad en Pérdidas y Ganancias
# Autor: J.M. Niño García
#############################################################
#
# Ganancias
#
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Jaime/Desktop/afchavez-019/Niño/Datos/Exp2/Ganancias")

Participantes <- 0
for(archive in dir()){
  Participantes <- Participantes+1 
}

colores <- c()
v <- 0
b <- 0
base <- 0
w <- 0.5
plot(2,3, col="white", xlim=c(0,36), ylim=c(0,22), ann=FALSE, axes=F)
mtext(side=1, text = "Pair presented", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Participants", line=1, cex=1.3, f=2)
mtext(side=3, text = "Preference for the bigger alternative for each pair presented", line=1.5, cex=1.5, f=2)
mtext(side=3, text = "(All participants - Experiment 2 - GAINS)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0.5,21.5,1),labels=seq(1,22,1),las=1, line=-1.4, col = NA, col.ticks = 1)
axis(1,at=seq(0.5,35.5,1),labels=c(1,9,16,22,27,31,34,36,2,10,17,23,28,32,35,3,11,18,24,29,33,4,12,19,25,30,5,13,20,26,6,14,21,7,15,8), line=-0.8, f=2, col = NA, col.ticks = 1)
for(archive in dir()){
  Fel <- read.csv(archive)
  Par <- NULL
  Rate <- NULL
  Repet <- NULL
  Prop <- NULL
  left <- 0
  
  Par[(Fel$money_left==197&Fel$money_right==394)|(Fel$money_left==394&Fel$money_right==197)] <- '1' #4
  Par[(Fel$money_left==394&Fel$money_right==591)|(Fel$money_left==591&Fel$money_right==394)] <- '2' #9
  Par[(Fel$money_left==591&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==591)] <- '3' #10
  Par[(Fel$money_left==788&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==788)] <- '4' #6
  Par[(Fel$money_left==985&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==985)] <- '5' #7
  Par[(Fel$money_left==1182&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==1182)] <- '6' #8
  Par[(Fel$money_left==1379&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==1379)] <- '7' #2
  Par[(Fel$money_left==1576&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1576)] <- '8' #1
  Par[(Fel$money_left==197&Fel$money_right==591)|(Fel$money_left==591&Fel$money_right==197)] <- '9' #3
  Par[(Fel$money_left==394&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==394)] <- '10' #5
  Par[(Fel$money_left==591&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==591)] <- '11' #9
  Par[(Fel$money_left==788&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==788)] <- '12' #10
  Par[(Fel$money_left==985&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==985)] <- '13' #6
  Par[(Fel$money_left==1182&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==1182)] <- '14' #7
  Par[(Fel$money_left==1379&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1379)] <- '15' #8
  Par[(Fel$money_left==197&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==197)] <- '16' #2
  Par[(Fel$money_left==394&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==394)] <- '17' #1
  Par[(Fel$money_left==591&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==591)] <- '18' #3
  Par[(Fel$money_left==788&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==788)] <- '19' #5
  Par[(Fel$money_left==985&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==985)] <- '20' #9
  Par[(Fel$money_left==1182&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1182)] <- '21' #10
  Par[(Fel$money_left==197&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==197)] <- '22' #6
  Par[(Fel$money_left==394&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==394)] <- '23' #7
  Par[(Fel$money_left==591&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==591)] <- '24' #8
  Par[(Fel$money_left==788&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==788)] <- '25' #2
  Par[(Fel$money_left==985&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==985)] <- '26' #1
  Par[(Fel$money_left==197&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==197)] <- '27' #3
  Par[(Fel$money_left==394&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==394)] <- '28' #5
  Par[(Fel$money_left==591&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==591)] <- '29' #9
  Par[(Fel$money_left==788&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==788)] <- '30' #10
  Par[(Fel$money_left==197&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==197)] <- '31' #6
  Par[(Fel$money_left==394&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==394)] <- '32' #7
  Par[(Fel$money_left==591&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==591)] <- '33' #8
  Par[(Fel$money_left==197&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==197)] <- '34' #2
  Par[(Fel$money_left==394&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==394)] <- '35' #1
  Par[(Fel$money_left==197&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==197)] <- '36' #3

  for(a in 1:36){
  Repet[a] <- length(which(Par==a))
  Rate[a] <- (sum(Fel$biggerchosenP[which(Par==a)]))/10
  
  Pol_width <- c(left,left+1,left+1,left)
  Pol_prop <- c(base,base,(base+Rate[a]),(base+Rate[a]))
  polygon(Pol_width, Pol_prop,
            density = NA, col="lightcyan2")
  left <- left+1
  }
  
  print(Rate)
  #print(unique(Repet))  #Sólo para asegurarme que se registraron bien los pares
  base <- base+1
}
for(vert in 1:37){
  lines(c(v,v),c(0,22),col='black', lty=1,lwd=2) 
  v <- v+1
}
for(vert in 1:23){
  lines(c(0,36),c(b,b),col='black', lty=1,lwd=2) 
  b <- b+1
}
for(vert in 1:22){
  lines(c(0,36),c(w,w),col='black', lty=3,lwd=1) 
  w <- w+1
}
#############################################################
#
# Perdidas
#
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Jaime/Desktop/afchavez-019/Niño/Datos/Exp2/Perdidas")

Participantes <- 0
for(archive in dir()){
  Participantes <- Participantes+1 
}

colores <- c()
v <- 0
b <- 0
base <- 0
w <- 0.5
plot(2,3, col="white", xlim=c(0,36), ylim=c(0,22), ann=FALSE, axes=F)
mtext(side=1, text = "Pair presented", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Participants", line=1, cex=1.3, f=2)
mtext(side=3, text = "Preference for the bigger alternative for each pair presented", line=1.5, cex=1.5, f=2)
mtext(side=3, text = "(All participants - Experiment 2 - LOSSES)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0.5,21.5,1),labels=seq(1,22,1),las=1, line=-1.4, col = NA, col.ticks = 1)
axis(1,at=seq(0.5,35.5,1),labels=c(1,9,16,22,27,31,34,36,2,10,17,23,28,32,35,3,11,18,24,29,33,4,12,19,25,30,5,13,20,26,6,14,21,7,15,8), line=-0.8, f=2, col = NA, col.ticks = 1)
for(archive in dir()){
  Fel <- read.csv(archive)
  Par <- NULL
  Rate <- NULL
  Repet <- NULL
  Prop <- NULL
  left <- 0
  
  Par[(Fel$money_left==197&Fel$money_right==394)|(Fel$money_left==394&Fel$money_right==197)] <- '1' #4
  Par[(Fel$money_left==394&Fel$money_right==591)|(Fel$money_left==591&Fel$money_right==394)] <- '2' #9
  Par[(Fel$money_left==591&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==591)] <- '3' #10
  Par[(Fel$money_left==788&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==788)] <- '4' #6
  Par[(Fel$money_left==985&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==985)] <- '5' #7
  Par[(Fel$money_left==1182&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==1182)] <- '6' #8
  Par[(Fel$money_left==1379&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==1379)] <- '7' #2
  Par[(Fel$money_left==1576&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1576)] <- '8' #1
  Par[(Fel$money_left==197&Fel$money_right==591)|(Fel$money_left==591&Fel$money_right==197)] <- '9' #3
  Par[(Fel$money_left==394&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==394)] <- '10' #5
  Par[(Fel$money_left==591&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==591)] <- '11' #9
  Par[(Fel$money_left==788&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==788)] <- '12' #10
  Par[(Fel$money_left==985&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==985)] <- '13' #6
  Par[(Fel$money_left==1182&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==1182)] <- '14' #7
  Par[(Fel$money_left==1379&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1379)] <- '15' #8
  Par[(Fel$money_left==197&Fel$money_right==788)|(Fel$money_left==788&Fel$money_right==197)] <- '16' #2
  Par[(Fel$money_left==394&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==394)] <- '17' #1
  Par[(Fel$money_left==591&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==591)] <- '18' #3
  Par[(Fel$money_left==788&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==788)] <- '19' #5
  Par[(Fel$money_left==985&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==985)] <- '20' #9
  Par[(Fel$money_left==1182&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==1182)] <- '21' #10
  Par[(Fel$money_left==197&Fel$money_right==985)|(Fel$money_left==985&Fel$money_right==197)] <- '22' #6
  Par[(Fel$money_left==394&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==394)] <- '23' #7
  Par[(Fel$money_left==591&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==591)] <- '24' #8
  Par[(Fel$money_left==788&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==788)] <- '25' #2
  Par[(Fel$money_left==985&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==985)] <- '26' #1
  Par[(Fel$money_left==197&Fel$money_right==1182)|(Fel$money_left==1182&Fel$money_right==197)] <- '27' #3
  Par[(Fel$money_left==394&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==394)] <- '28' #5
  Par[(Fel$money_left==591&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==591)] <- '29' #9
  Par[(Fel$money_left==788&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==788)] <- '30' #10
  Par[(Fel$money_left==197&Fel$money_right==1379)|(Fel$money_left==1379&Fel$money_right==197)] <- '31' #6
  Par[(Fel$money_left==394&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==394)] <- '32' #7
  Par[(Fel$money_left==591&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==591)] <- '33' #8
  Par[(Fel$money_left==197&Fel$money_right==1576)|(Fel$money_left==1576&Fel$money_right==197)] <- '34' #2
  Par[(Fel$money_left==394&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==394)] <- '35' #1
  Par[(Fel$money_left==197&Fel$money_right==1773)|(Fel$money_left==1773&Fel$money_right==197)] <- '36' #3
  
  for(a in 1:36){
    Repet[a] <- length(which(Par==a))
    Rate[a] <- (sum(Fel$biggerchosenP[which(Par==a)]))/10
    
    Pol_width <- c(left,left+1,left+1,left)
    Pol_prop <- c(base,base,(base+Rate[a]),(base+Rate[a]))
    polygon(Pol_width, Pol_prop,
            density = NA, col="lightcyan3")
    left <- left+1
  }
  
  print(Rate)
  #print(unique(Repet))  #Sólo para asegurarme que se registraron bien los pares
  base <- base+1
}
for(vert in 1:37){
  lines(c(v,v),c(0,22),col='black', lty=1,lwd=2) 
  v <- v+1
}
for(vert in 1:23){
  lines(c(0,36),c(b,b),col='black', lty=1,lwd=2) 
  b <- b+1
}
for(vert in 1:22){
  lines(c(0,36),c(w,w),col='black', lty=3,lwd=1) 
  w <- w+1
}