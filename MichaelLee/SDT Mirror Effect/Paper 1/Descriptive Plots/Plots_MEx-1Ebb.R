####################################
# Ploteo de Datos
# EXPERIMENTO 1 (Una Figura Ebbinghaus)
####################################

setwd("C:/Users/Alejandro/Desktop/afchavez19/MichaelLee/SDT Mirror Effect/Data/Datos_MirrExp_1Ebb")
rm(list=ls())
dir()

#####################################
#####################################
#     Evaluando/ Graficando         #
#        Mirror Effect              #
#####################################
#####################################
#####################################


#####################################
####            HITS Y FALSAS ALARMAS
#####################################


rm(list=ls())
#layout(matrix(1:2,ncol=1))
for(archive in dir()){
jaime <- read.csv(archive)
print(c(archive))
fa <- NULL
hits <- NULL
conf <- NULL
for(nce in sort(unique(jaime$tipo))){
  fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$tipo==nce]=='True'))
  hits <- append(hits, sum(jaime$Hits[jaime$tipo==nce]=='True'))
  rate <- (fa+hits)/160
  total <- (fa+hits)
  print(c(nce,
          'Tasa:', rate[length(rate)],
          'Total:', total[length(total)]))
}

barplot(total, main = "", ann=F, xlab = "", ylab = "", font.lab=2, ylim = c(0, 160), axes = FALSE, col =c("deepskyblue3", "darkorchid3", "darkorchid2", "deepskyblue2"))
axis(1,at=c(0.72,1.9,3.1,4.3),labels=c("Fa(AN)", "Fa(BN)", "H(BS)", "H(AS)"), font=2)
axis(2,at=c(0, 20, 40, 60, 80, 100, 120, 140, 160),labels=c("0","20","40","60", "80", "100", "120", "140", "160"),las=1)
text(0.72,total[1]+6,paste(total[1]),cex=1.1,col='black',f=2)
text(1.9,total[2]+6,paste(total[2]),cex=1.1,col='black',f=2)
text(3.1,total[3]-6,paste(total[3]),cex=1.1,col='black',f=2)
text(4.3,total[4]-6,paste(total[4]),cex=1.1,col='black',f=2)
text(1.1, 100,paste('Hits & Falsas Alarmas'),cex=2,col='black',f=2)
mtext(archive,3,cex=3, line=1, f=2)
mtext(side=2, text = "Frecuencia Absoluta", line=2.2, cex=2)
}  


#####################################
####            Confidence Rating
#####################################
  
rm(list=ls())
layout(matrix(1,ncol=1))
for(archive in dir()){
  jaime <- read.csv(archive)
  
  C_AS <- NULL
  C_AN <- NULL
  C_BS <- NULL
  C_BN <- NULL
  
  for(nce in sort(jaime$Estimulo)){
    C_AS <- sum(jaime$Confidence[jaime$tipo=='4 AS'])/160
    C_AN <- sum(jaime$Confidence[jaime$tipo=='1 AN'])/160
    C_BS <- sum(jaime$Confidence[jaime$tipo=='3 BS'])/160
    C_BN <- sum(jaime$Confidence[jaime$tipo=='2 BN'])/160
    C_AS <- round(C_AS,3)
    C_AN <- round(C_AN,3)
    C_BS <- round(C_BS,3)
    C_BN <- round(C_BN,3)
    Confidence <- c(C_AN, C_BN, C_BS, C_AS)
  }

  print(c(archive))
  print(c(Confidence))
  
  barplot(Confidence, col=c('deeppink', 'deeppink1', 'deeppink2', 'deeppink3'),ylim=c(0,6),axes=F , ann=F,  ylab="", xlab="", font.lab=2)
  axis(1,at=c(0.8,1.9,3.1,4.3),labels=c("R(AN)", "R(BN)", "R(BS)", "R(AS)"), font=2)
  axis(2,at=c(0, 1, 2, 3, 4, 5, 6),labels=c("0","1", "2","3","4","5","6"),las=1)
  text(0.8,C_AN+.5,paste(C_AN),cex=1,col='black',f=2)
  text(1.9,C_BN+.5,paste(C_BN),cex=1,col='black',f=2)
  text(3.1,C_BS-.6,paste(C_BS),cex=1,col='black',f=2)
  text(4.3,C_AS-.5,paste(C_AS),cex=1,col='black',f=2)
  mtext(archive,3,cex=3,f=2)
  mtext(side=2, text = "Puntaje de Confianza promedio", line=2.2, cex=2)
}


#####################################
#####################################
#     Explorando/Graficando         #
#             Datos                 #
#####################################
#####################################
#####################################


#############################################
####            Aciertos y Errores Por Ensayo
#############################################
rm(list=ls())
layout(matrix(1:2,ncol=1, byrow=TRUE))  #Sin Registro Acumulativo
#layout(matrix(1:3,ncol=1, byrow=TRUE)) # Con Resgistro Acumulativo
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  
  a <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  b <- c(1,30,60,90,120,150,180,210,240,270,300,330,360,390,420,450,480,510,540,570,600,630)
  
  
  #REGISTRO ACUMULATIVO
  #plot(jaime$Aciertos,type='o',pch=16, col='green', lwd=.5, ylim=c(0,640),axes=F , ylab="Frecuencia acumulada", font.lab=2, line=2, xlab='Ensayo' )
  #axis(1,at=a,labels=a)
  #axis(2,at=b, labels=b, tck=0, line=-1.3, font=2)
  #points(jaime$Errores,type='o', lty=1, lwd=.5, pch=16, col='red')
  #mtext(archive,3,cex=1, f=2)
  #text(70,500,paste('Aciertos'),cex=1,col='chartreuse4',f=2)
  #text(70,400,paste('Errores'),cex=1,col='red',f=2)
  #title("Performance across time", outer = TRUE, line = -2)
  
  
  plot(jaime$Exito[1:320],type='o',pch=16, col='darkgreen',ylim=c(0,1),axes=F , ann = F )
  axis(1,at=1:320,labels=c(1:320))
  axis(2,at=c(0,1), labels=c('Error', 'Acierto'), font=2)
  mtext(side=1, text = "Ensayos 1-320", line=2.5, cex=2)
  mtext(archive,3,cex=3, f=2, line=1)   #Comentar SI se incluye Registro Acumulativo
  
  
  plot(jaime$Exito[321:640],type='o',pch=16, col='darkgreen',ylim=c(0,1),axes=F , ann = F )
  axis(1,at=1:320,labels=c(321:640))
  axis(2,at=c(0,1), labels=c('Error', 'Acierto'), font=2)
  mtext(side=1, text = "Ensayos 321-640", line=2.5, cex=2)}


##################################
####            Output  Por Ensayo
##################################

rm(list=ls())
number <- 0
for(archive in dir()){
  number <- number+1
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  layout(matrix(1:1,ncol=1))
  plot(jaime$ContadorH,type='o',pch=16, col='blue',ylim=c(0,320),axes=F , xlab="", ylab="", font.lab=2, line.lab=1)
  axis(1,at=seq(1,640,20),labels=seq(1,640,20))
  points(jaime$ContadorF,type='o', lty=3, pch=16, col='red')
  points(jaime$ContadorM,type='o', lty=3, pch=16, col='purple')
  points(jaime$ContadorR,type='o', lty=3, pch=16, col='chartreuse4')
  text(30,300,paste("False Alarms"),cex=1,col='red',f=2)
  text(30,230,paste("Misses"),cex=1,col='purple',f=2)
  text(30,160,paste("Rejections"),cex=1,col='chartreuse4',f=2)
  text(30,90,paste("Hits"),cex=1,col='blue',f=2)
  mtext(paste("Participant No.", number, "; Experiment 1"), 3, line=1, col='black', cex=2, font=2)
  mtext(side=2, text = "Cumulative frequencies", line=1, cex=2)
  
  layout(matrix(1:2,ncol=1))
  
  plot(jaime$outcome[1:320],type='o',pch=16, col='deepskyblue4',ylim=c(1,4),axes=F , xlab="", ylab="", font.lab=2)
  mtext(side=1, text = "Trials", line=2.5, cex=2)
  axis(1,at=seq(1,320,5),labels=seq(1,320,5))
  axis(2,at=c(1,2,3,4), labels=c('FA', 'M', 'R', 'H'),f=2)
  mtext(paste("Participant No.", number, "; Experiment 1"), 3, line=1, col='black', cex=2, font=2)
  mtext("Results obtained per trial", 3, line=0, col='black', cex=1.2, font=2)
  
  plot(jaime$outcome[321:640],type='o',pch=16, col='deepskyblue4',ylim=c(1,4),axes=F , xlab="", ylab="", font.lab=2)
  mtext(side=1, text = "Trials", line=2.5, cex=2)
  axis(1,at=seq(1,320,5),labels=seq(321,640,5))
  axis(2,at=c(1,2,3,4), labels=c('FA', 'M', 'R', 'H'),f=2)
  
  print(archive)}

############################################
############################################
####            Response Times Across Trials
############################################
############################################
# RT 1 y 2                            ######
############################################
############################################


rm(list=ls())
layout(matrix(1:1,ncol=1))
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  a <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  
  plot(jaime$RTime1,type='o',pch=16, col='purple',ylim=c(0,16),axes=F , ylab='Segundos', xlab='Ensayo', font.lab=2 )
  axis(1,at=a,labels=a)
  axis(2,at=c(0,2,4,6,8,10,12,14,16),labels=c("0","2","4","6","8","10","12","14","16"))
  points(jaime$RTime2,type='o', lty=3, pch=16, col='brown')
  text(150,14,paste('Tiempo de Respuesta a la Tarea Binaria'),cex=1,col='purple',f=2)
  text(150,13,paste('Tiempo de Respuesta a la Escala de Confianza'),cex=1,col='brown',f=2)
  mtext(archive,3,cex=1.2, f=2)
  #title("Response Times per trial", outer = TRUE, line = -2)
}

#########################################################
#  Response Time per half the trials
#########################################################
rm(list=ls())

rm(list=ls())
layout(matrix(1:2,ncol=1))
for(archive in dir()){
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  
  a <- c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320)
  b <- c(321,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640)
  
  
  {plot(jaime$RTime1[1:320],type='o',pch=16, col='purple',ylim=c(1,10),axes=F , ylab='Segundos', xlab='Ensayos 1-320', font.lab=2)
  axis(1,at=a,labels=a)
  axis(2,at=0:10,labels=c(0:10))
  mtext(archive, 3, line=1, col='black', cex=1.2, font=2)
  #title("Response time per Trial (to the Stimulus)",line = 3)  
  
  plot(jaime$RTime1[321:640],type='o',pch=16, col='purple',ylim=c(1,10),axes=F , ylab='Segundos', xlab='Ensayos 321-640', font.lab=2)
  axis(1,at=a,labels=b)
  axis(2,at=0:10,labels=c(0:10))

  }  
  {plot(jaime$RTime2[1:320],type='o',pch=16, col='brown',ylim=c(0,10),axes=F , ylab='Segundos', xlab='Ensayos 1-320', font.lab=2)
  axis(1,at=a,labels=a)
  axis(2,at=0:10,labels=c(0:10))
  mtext(archive, 3, line=1, col='black', cex=1.2, font=2)
  #title( "Response Time per Trial (to the Scale)", line = 3)
  
  plot(jaime$RTime2[321:640],type='o',pch=16, col='brown',ylim=c(0,10),axes=F , ylab='Segundos', xlab='Ensayos 321-640', font.lab=2)
  axis(1,at=a,labels=b)
  axis(2,at=0:10,labels=c(0:10))
  }
  }

###############################################
####            Confidence Rating on each trial
###############################################

rm(list=ls())
numero <- 0
for(archive in dir()){
  
  a <- c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320)
  b <- c(321,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640)
  m <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  numero <- numero + 1
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  #640 trials in one plot
  #layout(matrix(1:1,ncol=1))
  #plot(jaime$Confidence[1:640],type='o',pch=16, col='darkorchid',ylim=c(1,6),axes=F , ylab='', xlab='', font.lab=2)
  #axis(1,at=m,labels=m)
  #axis(2,at=1:6,labels=c("1","2","3","4","5","6"))
  #text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  #mtext(paste("Participant No.", numero, "; Experiment 1"), 3, line=1, col='black', cex=2, font=2)
  #mtext(side=1, text = "Trials (all)", line=3, cex=2.4)
  #mtext(side=2, text = "Confidence Rating", line=2, cex=2)
  
  layout(matrix(1:2,ncol=1))
  plot(jaime$Confidence[1:320],type='o',pch=16, col='darkorchid',ylim=c(1,6),axes=F , ylab='', xlab='', font.lab=2)
  axis(1,at=a,labels=a)
  axis(2,at=1:6,labels=c("1","2","3","4","5","6"))
  text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  mtext(paste("Participant No.", numero, "; Experiment 1"), 3, line=1, col='black', cex=2.5, font=2)
  mtext("Confidence rating chosen per trial", 3, line=0, col='black', cex=1.2, font=2)
  mtext(side=1, text = "Trials", line=3, cex=1.8)
  mtext(side=2, text = "Rating", line=2, cex=1.8)
    
  plot(jaime$Confidence[321:640],type='o',pch=16, col='darkorchid2',ylim=c(1,6), ylab='',xlab='', axes=F, font.lab=2 )
  axis(1,at=a,labels=b)
  axis(2,at=1:6,labels=c("1","2","3","4","5","6"))
  text(140,8.5,paste("321-480"),cex=1,col='darkorchid2',f=2)
  mtext(side=1, text = "Trials", line=3, cex=1.8)
  mtext(side=2, text = "Rating", line=2, cex=1.8)
 
}

###########################################
####            Respuesta (Y/NN) por ensayo
###########################################

rm(list=ls())
numero <- 0
for(archive in dir()){
  numero <- numero + 1
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  m <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  n <- c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320)
  o <- c(321,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640)
  
  #640 trials in a single plot
  #layout(matrix(1:1,ncol=1))
  #plot(jaime$choice[1:640],type='o',pch=16, col='dodgerblue3',ylim=c(0,1),axes=F , ann=F,  ylab='', xlab='', font.lab=2 )
  #axis(1,at=m,labels=m)
  #axis(2,at=0:1,labels=c("No","Sí"))
  #text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  #title( "Response per Trial", line = 3)  
  #mtext(archive, 3, line=1, col='black', cex=3, font=2)
  #mtext(side=1, text = "Ensayos 1 - 640", line=3, cex=2.4)
  #mtext(side=2, text = "Respuesta", line=1, cex=2.4)
  
  
  layout(matrix(1:2,ncol=1))
  plot(jaime$choice[1:320],type='o',pch=16, col='dodgerblue3',ylim=c(0,1),axes=F , ann=F,  ylab='', xlab='', font.lab=2 )
  axis(1,at=n,labels=n)
  axis(2,at=0:1,labels=c("No","Yes"))
  text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  #title( "Response per Trial", line = 3)  
  mtext(paste("Participant No. ", numero, "; Experiment 1"), 3, line=1.5, col='black', cex=2, font=2)
  mtext("Yes/No response given per trial", 3, line=0, col='black', cex=1.2, font=2)
  mtext(side=1, text = "Trial", line=3, cex=1.6)
  
  plot(jaime$choice[321:640],type='o',pch=16, col='dodgerblue3',ylim=c(0,1),axes=F , ylab='', xlab='', font.lab=2 )
  axis(1,at=n,labels=o)
  axis(2,at=0:1,labels=c("No","Yes"))
  #text(140,8.5,paste("321-480"),cex=1,col='darkorchid2',f=2)
  mtext(side=1, text = "Trial", line=3, cex=1.6)
  

colp <- c('deepskyblue3','darkorchid3', 'green', 'red', 'orange', 'firebrick3')

### Choice por FACIL DIFICIL

layout(matrix(1:2,ncol=1))

plot(jaime$choice[1:640],type='o',pch=16, col='black',ylim=c(0,1), xlim=c(0,700),axes=F , ann=F, ylab='', xlab='', font.lab=2 )
axis(1,at=m,labels=m)
axis(2,at=0:1,labels=c("No","Sí"))
for (a in 1:640){
  if (jaime$facilidad[a] == 'pocos'){
    points(a,jaime$choice[a],pch=16,col=colp[1])}
  if (jaime$facilidad[a] == 'muchos'){
    points(a,jaime$choice[a],pch=16,col=colp[2])}}
text(670,0.8,"Fácil",cex=1,col=colp[1],f=2)
text(670,0.3,"Difícil",cex=1,col=colp[2],f=2)
mtext("Respuesta por Clase",3,cex=1.5, f=3)
mtext(archive, 3, line=2, cex=3, f=2)
mtext(side=1, text = "Ensayos 1 - 640", line=3, cex=2.4)
mtext(side=2, text = "Respuesta", line=1, cex=2.4)

##### Choice por SENAL

plot(jaime$choice[1:640],type='o',pch=16, col='black',ylim=c(0,1), xlim=c(0,700),axes=F , ann=F, ylab='', xlab='', font.lab=2 )
axis(1,at=m,labels=m)
axis(2,at=0:1,labels=c("No","Sí"))
for (a in 1:640){
  if (jaime$senal[a] == 'senal'){
    points(a,jaime$choice[a],pch=16,col=colp[3])}
  if (jaime$senal[a] == 'ruido'){
    points(a,jaime$choice[a],pch=16,col=colp[4])}}
text(670,0.8,"Señal",cex=1,col='forestgreen',f=2)
text(670,0.3,"Ruido",cex=1,col=colp[4],f=2)
text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
mtext("Respuesta por Estímulo",3,cex=1.5, f=3)
mtext(side=1, text = "Ensayos 1 - 640", line=3, cex=2.4)
mtext(side=2, text = "Respuesta", line=1, cex=2.4)

##### Choice por COLOR

#plot(jaime$choice[1:640],type='o',pch=16, col='black',ylim=c(0,1), xlim=c(0,700),axes=F , ylab='Respuesta', xlab='Trial', font.lab=2 )
#axis(1,at=m,labels=m)
#axis(2,at=0:1,labels=c("No","Sí"),f=2)
#for (a in 1:640){
#  if (jaime$color[a] == 'purpura'){
#    points(a,jaime$choice[a],pch=16,col=colp[2])}
#  if (jaime$color[a] == 'naranja'){
#    points(a,jaime$choice[a],pch=16,col=colp[5])}
#  if (jaime$color[a] == 'azul'){
#    points(a,jaime$choice[a],pch=16,col=colp[1])}
#  if (jaime$color[a] == 'verde'){
#    points(a,jaime$choice[a],pch=16,col=colp[3])}
#  if (jaime$color[a] == 'guinda'){
#    points(a,jaime$choice[a],pch=16,col=colp[6])}}
#text(670,0.9,"Púrpura",cex=1,col=colp[2],f=2)
#text(670,0.7,"Naranja",cex=1,col=colp[5],f=2)
#text(670,0.5,"Azul",cex=1,col=colp[1],f=2)
#text(670,0.3,"Verde",cex=1,col=colp[3],f=2)
#text(670,0.1,"Guinda",cex=1,col=colp[6],f=2)
#text(670,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
#mtext("Respuesta Registrada por Color",3,cex=.8, f=3)
}

############################################
############################################
###   Influencia de Variables           ####
############################################
############################################



############################################
#           H y FA por No. Circulos Externos
############################################

rm(list=ls())
layout(matrix(1:2,ncol=2))
for(archive in dir()){

  jaime <- read.csv(archive)
  jaime$num_circulos_externos <- as.character(jaime$num_circulos_externos)
  cafe <- strsplit(as.character(jaime$num_circulos_externos),split='-')
  print(c(archive))
  
  fa <- NULL
  hits <- NULL
  for(nce in sort(unique(jaime$num_circulos_externos))){
    fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$num_circulos_externos==nce]=='True'))
    hits <- append(hits,sum(jaime$Hits[jaime$num_circulos_externos==nce]=='True'))
    print(c(nce,
            'F.A.', fa[length(fa)],
            'Hits', hits[length(hits)]))}
  
  barplot(hits, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col = c("dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("2","3", "7", "8"))
  mtext("Número de Círculos Externos", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Hits", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('Hits por Círculos Externos',3,cex=1.2, font=2)
  mtext(archive, outer = TRUE, line = -2, cex=3, font=2)

  barplot(fa, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col = c("firebrick1","firebrick2","firebrick3", "firebrick4"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("2","3", "7", "8"))
  mtext("Número de Círculos Externos", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Falsas Alarmas", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('F. Alarmas por Círculos Externos',3,cex=1.2, font=2)
  }

#############################################
#               Hits y Falsas Alarmas x Color
#############################################

rm(list=ls())
layout(matrix(1:2,ncol=2, byrow = TRUE))
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$color <- as.character(jaime$color)
  cafe <- strsplit(as.character(jaime$color),split='-')

  print(c(archive))
  
  fa <- NULL
  hits <- NULL
  for(nce in sort(unique(jaime$color))){
    fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$color==nce]=='True'))
    hits <- append(hits,sum(jaime$Hits[jaime$color==nce]=='True'))
    print(c(nce,
            'F.A.', fa[length(fa)],
            'Hits', hits[length(hits)]))}
  
  barplot(hits, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col = c("dodgerblue3","firebrick3", "chocolate3", "darkorchid4", "forestgreen"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3,5.5),labels=c("Azul","Rojo", "Anaranjado", "Púrpura", "Verde"))
  text(0.7,hits[1]+5,paste(hits[1]),cex=1.2,col='black',f=2)
  text(1.9,hits[2]+5,paste(hits[2]),cex=1.2,col='black',f=2)
  text(3.1,hits[3]+5,paste(hits[3]),cex=1.2,col='black',f=2)
  text(4.3,hits[4]+5,paste(hits[4]),cex=1.2,col='black',f=2)
  text(5.5,hits[5]+5,paste(hits[5]),cex=1.2,col='black',f=2)
  mtext("Color", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Hits", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('Hits por color',3,cex=1.5, font=2)
  mtext(archive, outer = TRUE, line = -2, cex=3, font=2)
   
  barplot(fa, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col =c("dodgerblue3","firebrick3", "chocolate3", "darkorchid4", "forestgreen"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3,5.5),labels=c("Azul","Rojo", "Naranja", "Púrpura", "Verde"))
  text(0.7,fa[1]+5,paste(fa[1]),cex=1.5,col='black',f=2)
  text(1.9,fa[2]+5,paste(fa[2]),cex=1.5,col='black',f=2)
  text(3.1,fa[3]+5,paste(fa[3]),cex=1.5,col='black',f=2)
  text(4.3,fa[4]+5,paste(fa[4]),cex=1.5,col='black',f=2)
  text(5.5, fa[5]+5,paste(fa[5]), cex=1.5,col='black',f=2)
  mtext("Color", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Falsas Alarmas", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('F. Alarmas por Color',3,cex=1.5, font=2)
  }

####################################################
#               Yes/No por Color
####################################################
rm(list=ls())
layout(matrix(1:1,ncol=1, byrow=TRUE))
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$color <- as.character(jaime$color)
  cafe <- strsplit(as.character(jaime$color),split='-')
  print(c(archive))
  
  yes <- NULL
  no <- NULL
  for(nce in sort(unique(jaime$color))){
    yes<- append(yes, sum(jaime$Respuesta[jaime$color==nce]=='s'))
    no <- append(no,sum(jaime$Respuesta[jaime$color==nce]=='n'))
    print(c(nce,
            'Sí', yes[length(yes)],
            'No', no[length(no)]))}
  
  barplot(yes, main = "", xlab = "", horiz=T, ylab = " ", ylim = c(0, 6),xlim = c(0,128), axes = FALSE, col = c("deepskyblue3","firebrick2", "darkorange2", "darkorchid1", "chartreuse3"))
  lines(c(yes[1], 128),c(0.7,0.7), lwd=2, lty=1, col="deepskyblue3")
  lines(c(yes[2], 128),c(1.9,1.9), lwd=2, lty=1, col="firebrick2")
  lines(c(yes[3], 128),c(3.1,3.1), lwd=2, lty=1, col="darkorange2")
  lines(c(yes[4], 128),c(4.3,4.3), lwd=2, lty=1, col="darkorchid1")
  lines(c(yes[5], 128),c(5.5,5.5), lwd=2, lty=1, col="chartreuse3")
  axis(1,at=c(0, 32, 64, 96, 128),labels=c("0", "32", "64", "96","128"), las=1)
  axis(2,at=c(0.7,1.9,3.1,4.3,5.5),labels=c("Azul","Rojo", "Naranja", "Púrpura", "Verde"), las=3)
  text(yes[1]-5, 0.7,paste(yes[1]),cex=1,col='black',f=2)
  text(yes[2]-5, 1.9,paste(yes[2]),cex=1,col='black',f=2)
  text(yes[3]-5,3.1,paste(yes[3]),cex=1,col='black',f=2)
  text(yes[4]-5,4.3,paste(yes[4]),cex=1,col='black',f=2)
  text(yes[5]-5,5.5,paste(yes[5]),cex=1,col='black',f=2)
  text(yes[1]+5, 0.85,paste(no[1]),cex=1,col='black',f=2)
  text(yes[2]+5, 2.05,paste(no[2]),cex=1,col='black',f=2)
  text(yes[3]+5,3.25,paste(no[3]),cex=1,col='black',f=2)
  text(yes[4]+5,4.45,paste(no[4]),cex=1,col='black',f=2)
  text(yes[5]+5,5.65,paste(no[5]),cex=1,col='black',f=2)
  text(yes[3]/2,3.1,'Sí',cex=3,col='black',f=3)
  text(yes[3]+no[3]/2,3.1,'No',cex=3,col='black',f=3)
  mtext("Total de ensayos por color", side = 1, line = 2.5, cex = 2, font = 2)
  mtext("Color", side = 4, line=0.8, cex = 2.2, font = 2, las = 3)
  #mtext('Sí por color',3,cex=1.5, font=2)
  mtext(archive, outer = TRUE, line = -3.5, cex=3, font=2)}



####################################################
#               Hits y FA por Tamaño Circulo Central
####################################################

rm(list=ls())
layout(matrix(1:2,ncol=2, byrow=TRUE))
for(archive in dir()){
  jaime <- read.csv(archive)
  fa <- NULL
  hits <- NULL
  print(c(archive))
  
  for(nce in sort(unique(jaime$tamano_central))){
    fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$tamano_central==nce]=='True'))
    hits <- append(hits,sum(jaime$Hits[jaime$tamano_central==nce]=='True'))
    yes <- fa+hits
    print(c(nce,
            'Yes:', yes[length(yes)]))}

  barplot( hits[c(4,5,6,7)], main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, ann= FALSE,  col =c("dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("Over-1", "Over-2", "Under-1", "Under-2"))
  text(0.7,hits[4]+5,paste(hits[4]),cex=1,col='blue4',f=1)
  text(1.9,hits[5]+5,paste(hits[5]),cex=1,col='blue4',f=1)
  text(3.1,hits[6]+5,paste(hits[6]),cex=1,col='blue4',f=1)
  text(4.3,hits[7]+5,paste(hits[7]),cex=1,col='blue4',f=1)
  mtext("Tipo de ilusión presentada", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Hits", side = 2, line = 3, cex = 1, font = 2, las = 0)
  mtext('Hits por illusión',3,cex=1.2, font=2)
  
  barplot(fa[c(1,2,3,8)], main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col =c("firebrick1", "firebrick2", "firebrick3", "firebrick4"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("1.0", "1,5", "2.5", "3.0"))
  text(0.7,fa[1]+5,paste(fa[1]),cex=1,col='brown4',f=1)
  text(1.9,fa[2]+5,paste(fa[2]),cex=1,col='brown4',f=1)
  text(3.1,fa[3]+5,paste(fa[3]),cex=1,col='brown4',f=1)
  text(4.3,fa[8]+5,paste(fa[8]),cex=1,col='brown4',f=1)
  mtext("Tamaño del círculo central", side = 1, line = 2.5, cex = 1, font = 2)
  mtext("Falsas Alarmas", side = 2, line = 3, cex = 1, font = 2, las = 0)
  title(archive, outer = TRUE, font=1, line = -1)
  mtext('F.A. por tamaño de Círculo Central',3,cex=1.2, font=2)
}

####################################
####################################
######       ROC curves     ########
####################################
####################################


rm(list=ls())
layout(matrix(1:20,ncol=5, byrow=TRUE))
for(archive in dir()){
  jaime <- read.csv(archive)
  fa_AN <- NULL
  hits_AN <- NULL
  fa_AS <- NULL
  fa_AS <- NULL
  hits_AN <- NULL
  hits_AS <- NULL
  hits_BS <- NULL
  hits_BN <- NULL
  { fa_AN <- sum(jaime$Falsas.alarmas[jaime$Estimulo>=161&jaime$Estimulo<=320]=='True')
  fa_AS <- sum(jaime$Falsas.alarmas[jaime$Estimulo>=1&jaime$Estimulo<=160]=='True')
  hits_AS <- sum(jaime$Hits[jaime$Estimulo>=1&jaime$Estimulo<=160]=='True')
  hits_AN <- sum(jaime$Hits[jaime$Estimulo>=161&jaime$Estimulo<=320]=='True')
  fa_BN <- sum(jaime$Falsas.alarmas[jaime$Estimulo>=481&jaime$Estimulo<=640]=='True')
  fa_BS <- sum(jaime$Falsas.alarmas[jaime$Estimulo>=321&jaime$Estimulo<=480]=='True')
  hits_BS <- sum(jaime$Hits[jaime$Estimulo>=321&jaime$Estimulo<=480]=='True')
  hits_BN <- sum(jaime$Hits[jaime$Estimulo>=481&jaime$Estimulo<=640]=='True')
  FAr_an <- fa_AN/160 
  Hr_as <- hits_AS/160
  FAr_bn <- fa_BN/160
  Hr_bs <- hits_BS/160
  print(c(archive))
  print(c(fa_AN[length(fa_AN)], 
          FAr_an[length(FAr_an)], 
          fa_BN[length(fa_BN)], 
          FAr_bn[length(FAr_bn)], 
          hits_BS[length(hits_BS)], 
          Hr_bs[length(Hr_bs)], 
          hits_AS[length(hits_AS)], 
          Hr_as[length(Hr_as)]))
  k_A <- qnorm(1-FAr_an,0,1)
  d_A <- qnorm(Hr_as,0,1)-qnorm(FAr_an,0,1)
  c_A <- k_A-(d_A/2)                    
  beta_A <- dnorm(k_A,d_A,1)/dnorm(k_A,0,1)
  k_B <- qnorm(1-FAr_bn,0,1)
  d_B <-qnorm(Hr_bs,0,1)-qnorm(FAr_bn,0,1)
  c_B <-k_B-(d_B/2)                    
  beta_B <-dnorm(k_B,d_B,1)/dnorm(k_B,0,1)
  }
  
  hits_A <- c()
  falarm_A <- c()
  hits_B <- c()
  falarm_B <- c()
  hits_na <- c()
  falarm_na <- c()
  c <- seq(-10,10,0.1)
  d_null <- 0
  
  for (i in 1:length(c)){
    hits_A[i] <- pnorm((-d_A/2)-c[i])
    falarm_A[i] <- pnorm((d_A/2)-c[i])
    hits_B[i] <- pnorm((-d_B/2)-c[i])
    falarm_B[i] <- pnorm((d_B/2)-c[i])
    hits_na[i] <- pnorm((d_null/2)-c[i])
    falarm_na[i] <- pnorm((-d_null/2)-c[i])
  }
  
  plot(FAr_an,Hr_as, pch=16, col='deepskyblue4', xlim=c(0,1), ylim=c(0,1), xlab='Tasa F.A.', ylab='Tasa Hits', font.lab=2, cex.lab=0.9)
    points(FAr_bn,Hr_bs, lty=3, pch=16, col='darkorchid4')
    lines(hits_A,falarm_A,lwd=2,col='deepskyblue3')
    lines(hits_B,falarm_B,lwd=2,col='darkorchid3')
    lines(hits_na,falarm_na,lwd=1,col='black', lty=2)
    lines(c(0.58, 0.68),c(0.3,0.3), lwd=2, lty=1, col="deepskyblue3")
    lines(c(0.58, 0.68),c(0.2,0.2), lwd=2, lty=1, col="darkorchid3")
    text(FAr_an, Hr_as+.04, paste("D'(A)=", round(d_A,2)), offset=0, cex = 0.7, pos=4, col='deepskyblue4', font=2)
    text(FAr_bn, Hr_bs-.04, paste("D'(B)=", round(d_B,2)), offset=0, cex = 0.7, pos=4, col='darkorchid4', font=2)
    text(0.7, 0.3, labels="D' para condición Fácil", offset=0, cex = 0.7, pos=4)
    text(0.7, 0.2, labels="D' para condición Difícil", offset=0, cex = 0.7, pos=4)
    title('ROC por Condition; Experimento 1',outer = TRUE, line = -2)
    mtext(archive,3,cex=.7)
    }
  