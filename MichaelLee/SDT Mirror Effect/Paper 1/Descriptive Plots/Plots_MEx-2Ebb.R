####################################
# Ploteo de Datos
# EXPERIMENTO 2 (Dos Ebbinghaus)
####################################

setwd("C:/Users/Sandra/Desktop/afchavez-019/MichaelLee/SDT Mirror Effect/Data/Datos_MirrExp_2Ebb")
rm(list=ls())
dir()


#####################################
#####################################
#     Plotting evidence for         #
#        Mirror Effect              #
#####################################
#####################################
#####################################


#####################################
####            HITS & FALSE ALARMS
#####################################
rm(list=ls())
number <- 0
for(archive in dir()){
  number <- number + 1
  jaime <- read.csv(archive)
  print(c(archive))
  fa <- NULL
  hits <- NULL
  conf <- NULL
  for(nce in sort(unique(jaime$tipo))){
    fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$tipo==nce]=='True'))
    hits <- append(hits, sum(jaime$Hits[jaime$tipo==nce]=='True'))
    rate <- (fa+hits)/160
    rate <- round(rate,3)
    total <- (fa+hits)
    print(c(nce,
            'Tasa:', rate[length(rate)],
            'Total:', total[length(total)]))
  }
  
  barplot(total, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0, 160), axes = FALSE, col =c("dodgerblue4", "deeppink4", "deeppink3", "dodgerblue3"))
  axis(1,at=c(0.72,1.9,3.1,4.3),labels=c("FA(AN)", "FA(BN)", "H(BS)", "H(AS)"), font=2)
  axis(2,at=c(0, 20, 40, 60, 80, 100, 120, 140, 160),labels=c("0","20","40","60", "80", "100", "120", "140", "160"),las=1, line=-1.2)
  text(0.72,total[1]+6,paste(total[1], "(",rate[1],")"),cex=1.1,col='black',f=2)
  text(1.9,total[2]+6,paste(total[2], "(",rate[2],")"),cex=1.1,col='black',f=2)
  text(3.1,total[3]-6,paste(total[3], "(",rate[3],")"),cex=1.1,col='black',f=2)
  text(4.3,total[4]-6,paste(total[4], "(",rate[4],")"),cex=1.1,col='black',f=2)
  text(1.1, 100,paste('Hits & False Alarms'),cex=1.5,col='black',f=2)
  mtext(paste("Participant No.", number, "; Experiment 2"),3,cex=2, line=1, f=2)
  mtext(side=2, text = "Absolute Frequency", line=2.2, cex=2)
}  



#####################################
####            Confidence Rating
#####################################
rm(list=ls())
number <- 0
layout(matrix(1,ncol=1))
for(archive in dir()){
  number <- number + 1
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
  
  barplot(Confidence,type='o',pch=16,col=c('lightpink', 'lightpink1', 'lightpink2', 'lightpink3'),ylim=c(0,6),axes=F , ylab="", xlab="", font.lab=2)
  axis(1,at=c(0.8,1.9,3.1,4.3),labels=c("R(AN)", "R(BN)", "R(BS)", "R(AS)"), font=2)
  axis(2,at=c(0, 1, 2, 3, 4, 5, 6),labels=c("0","1", "2","3","4","5","6"),las=1, line=-0.8)
  text(0.8,C_AN+.2,paste(C_AN),cex=1.4,col='black',f=2)
  text(1.9,C_BN+.2,paste(C_BN),cex=1.4,col='black',f=2)
  text(3.1,C_BS-.2,paste(C_BS),cex=1.4,col='black',f=2)
  text(4.3,C_AS-.2,paste(C_AS),cex=1.4,col='black',f=2)
  mtext(paste("Participant No.",number, "; Experiment 2"),3,cex=2,f=2)
  mtext(side=2, text = "Mean Confidence Rating", line=2, cex=2)
}


#####################################
#####################################
#     Plotting patterns of          #
#           Response               #
#####################################
#####################################
#####################################


#############################################
####            Right and Wrong per trial
#############################################
rm(list=ls())
number <- 0
for(archive in dir()){
  number <- number + 1
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  
  a <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  b <- c(1,30,60,90,120,150,180,210,240,270,300,330,360,390,420,450,480,510,540,570,600,630)
  
  layout(matrix(1:1,ncol=1, byrow=TRUE))    
  plot(jaime$Aciertos,type='o',pch=16, col='green', lwd=.5, ann=F, axes=F , ylab="Cumulative Frequency", 
       font.lab=2, line=2, xlab='Trial', ylim=c(0,640))
  axis(1,at=a,labels=a)
  axis(2,at=b, labels=b, tck=0, line=-1.3)
  points(jaime$Errores,type='o', lty=1, lwd=.5, pch=16, col='red')
  text(70,500,paste('Right reponses'),cex=1,col='chartreuse4',f=2)
  text(70,400,paste('Wrong responses'),cex=1,col='red',f=2)
  text(630,jaime$Aciertos[640]+15,paste(jaime$Aciertos[640]),cex=1,col='chartreuse4',f=2)
  text(630,jaime$Errores[640]+15,paste(jaime$Errores[640]),cex=1,col='red',f=2)
  mtext(side=3,paste("Participant No.",number,"; Experiment 2"), line = -2, cex=2, f=2)
  mtext(side=1, text = "Trial", line=2.5, cex=1.5)
  mtext(side=2, text = "Cumulative Frequency", line=1.5, cex=1.5)
  
  layout(matrix(1:2,ncol=1, byrow=TRUE))  
  plot(jaime$Exito[1:320],type='o',pch=16, col='darkgreen',ylim=c(0,1),axes=F , ann = F )
  axis(1,at=1:320,labels=c(1:320))
  axis(2,at=c(0,1), labels=c('Wrong', 'Right'), font=2, line=-1)
  mtext(side=1, text = "Trials 1-320", line=2.5, cex=1.5)
  mtext(paste("Participant No.",number,"; Experiment 2"),3,cex=2, f=2, line=1)  
  
  
  plot(jaime$Exito[321:640],type='o',pch=16, col='darkgreen',ylim=c(0,1),axes=F , ann = F )
  axis(1,at=1:320,labels=c(321:640))
  axis(2,at=c(0,1), labels=c('Wrong', 'Right'), font=2, line=-1)
  mtext(side=1, text = "Trials 321-640", line=2.5, cex=1.5)
}
#############################################
####                   Contadores por Ensayo
#############################################

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
  mtext(paste("Participant No.", number, "; Experiment 2"), 3, line=1, col='black', cex=2, font=2)
  mtext(side=2, text = "Cumulative frequencies", line=1, cex=2)
  
  layout(matrix(1:2,ncol=1))
  
  plot(jaime$outcome[1:320],type='o',pch=16, col='deepskyblue4',ylim=c(1,4),axes=F , xlab="", ylab="", font.lab=2)
  mtext(side=1, text = "Trials", line=2.5, cex=2)
  axis(1,at=seq(1,320,5),labels=seq(1,320,5))
  axis(2,at=c(1,2,3,4), labels=c('FA', 'M', 'R', 'H'),f=2)
  mtext(paste("Participant No.", number, "; Experiment 2"), 3, line=1, col='black', cex=2, font=2)
  mtext("Results obtained per trial", 3, line=0, col='black', cex=1.2, font=2)
  
  plot(jaime$outcome[321:640],type='o',pch=16, col='deepskyblue4',ylim=c(1,4),axes=F , xlab="", ylab="", font.lab=2)
  mtext(side=1, text = "Trials", line=2.5, cex=2)
  axis(1,at=seq(1,320,5),labels=seq(321,640,5))
  axis(2,at=c(1,2,3,4), labels=c('FA', 'M', 'R', 'H'),f=2)
  
  print(archive)}

#############################################
####                  Respuesta por Ensayo
#############################################

rm(list=ls())
#layout(matrix(1:1,ncol=1))
numero <- 0
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  numero <- numero + 1
  
  m <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  n <- c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320)
  o <- c(321,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640)
  
  
  colp <- c('deepskyblue3','darkorchid3', 'green', 'red', 'orange')

#Choice sola 640 ensayos
  #layout(matrix(1:1,ncol=1))
  #plot(jaime$choice[1:640],type='o',pch=16, col='dodgerblue',ylim=c(0,1),axes=F , ann=F, ylab='', xlab='', font.lab=2 )
  #axis(1,at=m,labels=m)
  #axis(2,at=0:1,labels=c("No","Sí"))
  #text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  #mtext(archive,3,cex=3, f=2, line=2)
  #mtext(side=1, text = "Ensayos 1 - 640", line=3, cex=2.4)
  #mtext(side=2, text = "Respuesta", line=1, cex=2.4)
  
  
  
#Choice sola
  layout(matrix(1:2,ncol=1))
  plot(jaime$choice[1:320],type='o',pch=16, col='dodgerblue',ylim=c(0,1),axes=F , ann=F, ylab='', xlab='', font.lab=2 )
  axis(1,at=n,labels=n)
  axis(2,at=0:1,labels=c("No","Yes"))
  text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  mtext(paste("Participant No.", numero, "; Experiment 2"), 3, line=1.5, col='black', cex=2, font=2)
  mtext("Yes/No response given per trial", 3, line=0, col='black', cex=1.2, font=2)
  mtext(side=1, text = "Trials", line=3, cex=1.6)
  #mtext(side=2, text = "Respuesta", line=1, cex=2.4)

  plot(jaime$choice[321:640],type='o',pch=16, col='dodgerblue',ylim=c(0,1),axes=F , ann=F, ylab='', xlab='', font.lab=2 )
  axis(1,at=n,labels=o)
  axis(2,at=0:1,labels=c("No","Yes"))
  text(140,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  mtext(side=1, text = "Trials", line=3, cex=1.6)
  #mtext(side=2, text = "Respuesta", line=1, cex=2.4)


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
  
  plot(jaime$choice[1:640],type='o',pch=16, col='black',ylim=c(0,1), xlim=c(0,700),axes=F , ann=F,  ylab='', xlab='', font.lab=2 )
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
  #  if (jaime$Color[a] == 'Purpura'){
  #    points(a,jaime$choice[a],pch=16,col=colp[2])}
  #  if (jaime$Color[a] == 'Naranja'){
  #    points(a,jaime$choice[a],pch=16,col=colp[5])}
  # if (jaime$Color[a] == 'Azul'){
  #    points(a,jaime$choice[a],pch=16,col=colp[1])}
  #  if (jaime$Color[a] == 'Verde'){
  #    points(a,jaime$choice[a],pch=16,col=colp[3])}}
  #text(670,0.9,"Púrpura",cex=1,col=colp[2],f=2)
  #text(670,0.7,"Naranja",cex=1,col=colp[5],f=2)
  #text(670,0.5,"Azul",cex=1,col=colp[1],f=2)
  #text(670,0.3,"Verde",cex=1,col=colp[3],f=2)
  #text(670,9.5,paste("1-160"),cex=1,col='darkorchid',f=2)
  #mtext("Respuesta Registrada por Color",3,cex=.8, f=3)
  }
  


##################################
######### Confidence por ENSAYO
##########################################

rm(list=ls())
numero <- 0
layout(matrix(1:2,ncol=1))
for(archive in dir()){
  
  a <- c(1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320)
  b <- c(321,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640)
  m <- c(1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640)
  numero <- numero + 1
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  plot(jaime$Confidence[1:320],type='o',pch=16, col='hotpink3',ylim=c(1,6),axes=F , ylab='', xlab='', font.lab=2)
  axis(1,at=a,labels=a)
  axis(2,at=1:6,labels=c("1","2","3","4","5","6"))
  mtext(paste("Participant No.", numero, "; Experiment 2"), 3, line=1, col='black', cex=2, font=2)
  mtext("Confidence rating chosen per trial", 3, line=0, col='black', cex=1.2, font=2)
  mtext(side=1, text = "Trials", line=3, cex=1.8)
  mtext(side=2, text = "Rating", line=2, cex=1.8)
  
  plot(jaime$Confidence[321:640],type='o',pch=16, col='hotpink3',ylim=c(1,6), ylab='',xlab='', axes=F, font.lab=2 )
  axis(1,at=a,labels=b)
  axis(2,at=1:6,labels=c("1","2","3","4","5","6"))
  mtext(side=1, text = "Trials", line=3, cex=1.8)
  mtext(side=2, text = "Rating", line=2, cex=1.8)
  }

######################
######################
#Response Time (1 y 2) across trials


rm(list=ls())
#layout(matrix(1:4,ncol=2))
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  plot(jaime$RTime1,type='o',pch=16, col='purple',ylim=c(0,16),axes=F , ann = F )
  axis(1,at=1:640,labels=sort(unique(jaime$Ensayo)))
  axis(2,at=c(0,2,4,6,8,10,12,14,16),labels=c("0","2","4","6","8","10","12","14","16"))
  points(jaime$RTime2,type='o', lty=3, pch=16, col='brown')
  text(150,14,paste('Tiempo de Respuesta a la Tarea Binaria'),cex=1,col='purple',f=2)
  text(150,10,paste('Tiempo de Respuesta a la Escala de Confianza'),cex=1,col='brown',f=2)
  mtext(archive,3,cex=.8)
  title("Tiempo de Respuesta por Ensayo", outer = TRUE, line = -2)
}

################################
################################
#RT en Intervalos de 150 ensayos.

rm(list=ls())

rm(list=ls())
layout(matrix(1:4,ncol=2))
for(archive in dir()){
  jaime <- read.csv(archive)
  jaime$Ensayo <- as.character(jaime$Ensayo)
  cafe <- strsplit(as.character(jaime$Ensayo),split='-')
  
  {plot(jaime$RTime1[1:160],type='o',pch=16, col='darkorange',ylim=c(1,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(1:160))
  axis(2,at=0:20,labels=c(0:20))
  text(145,9.5,paste("1-160"),cex=1,col='darkorange',f=2)
  mtext(archive,3,cex=.8)
  
  
  plot(jaime$RTime1[161:320],type='o',pch=16, col='darkorange1',ylim=c(1,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(161:320))
  axis(2,at=0:20,labels=c(0:20))
  text(145,9,paste("161-320"),cex=1,col='darkorange1',f=2)
  
  
  plot(jaime$RTime1[321:480],type='o',pch=16, col='darkorange2',ylim=c(1,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(321:480))
  axis(2,at=0:20,labels=c(0:20))
  text(145,8.5,paste("321-480"),cex=1,col='darkorange2',f=2)
  
  
  plot(jaime$RTime1[481:640],type='o',pch=16, col='darkorange3',ylim=c(1,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(481:640))
  axis(2,at=0:20,labels=c(0:20))
  text(145,8,paste("481-640"),cex=1,col='darkorange3',f=2)
  # text(170,7.5,paste("5"),cex=1,col='violet',f=2)
  title("Tiempo de Respuesta al Estimulo", outer = TRUE, line = -2)
  
  }  
  {plot(jaime$RTime2[1:160],type='o',pch=16, col='violetred1',ylim=c(0,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(1:160))
  axis(2,at=0:20,labels=c(0:20))
  text(145,9.5,paste("1-160"),cex=1,col='violetred1',f=2)
  mtext(archive,3,cex=.8)
  
  
  plot(jaime$RTime2[161:320],type='o',pch=16, col='violetred2',ylim=c(0,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(161:320))
  axis(2,at=0:20,labels=c(0:20))
  text(145,9,paste("161-320"),cex=1,col='violetred2',f=2)
  
  
  plot(jaime$RTime2[321:480],type='o',pch=16, col='violetred3',ylim=c(0,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(321:480))
  axis(2,at=0:20,labels=c(0:20))
  text(145,8.5,paste("321-480"),cex=1,col='violetred3',f=2)
  
  
  plot(jaime$RTime2[481:640],type='o',pch=16, col='violetred4',ylim=c(0,10),axes=F , ann = F )
  axis(1,at=1:160,labels=c(481:640))
  axis(2,at=0:20,labels=c(0:20))
  text(145,8,paste("481-640"),cex=1,col='violetred4',f=2)
  # text(170,7.5,paste("5"),cex=1,col='violet',f=2)
  title( "Tiempo de Respuesta a la Escala", outer = TRUE, line = -2)
  
  }}

###############
############################3
#Hits y Falsas Alarmas x Color

rm(list=ls())
layout(matrix(1:2,ncol=2, byrow=TRUE))
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Color <- as.character(jaime$Color)
  cafe <- strsplit(as.character(jaime$Color),split='-')
  #index <- which(jaime$facilidad=='muchos')
  
  fa <- NULL
  hits <- NULL
  for(nce in sort(unique(jaime$Color))){
    fa <- append(fa, sum(jaime$Falsas.alarmas[jaime$Color==nce]=='True'))
    hits <- append(hits,sum(jaime$Hits[jaime$Color==nce]=='True'))
    print(c(nce,
            fa[length(fa)],
            hits[length(hits)]))}
  
  barplot(hits, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col = c("dodgerblue3", "chocolate3", "darkorchid4", "forestgreen"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("Azul","Naranja", "Morado", "Verde"))
  text(0.7,hits[1]+5,paste(hits[1]),cex=1.5,col='black',f=2)
  text(1.9,hits[2]+5,paste(hits[2]),cex=1.5,col='black',f=2)
  text(3.1,hits[3]+5,paste(hits[3]),cex=1.5,col='black',f=2)
  text(4.3,hits[4]+5,paste(hits[4]),cex=1.5,col='black',f=2)
  mtext("Color", side = 1, line = 2.5, cex = 2, font = 2)
  mtext("Hits", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('Hits por color',3,cex=2, font=2)
  mtext(archive, outer = TRUE, line = -2, cex=3, font=2)
  
  
  barplot(fa, main = "", xlab = "", ylab = " ", ylim = c(0, 100), axes = FALSE, col =c("dodgerblue3", "chocolate3", "darkorchid4", "forestgreen"))
  axis(2,at=c(0, 20, 40, 60, 80, 100),labels=c("0", "20", "40","60","80","100"),las=1)
  axis(1,at=c(0.7,1.9,3.1,4.3),labels=c("Azul","Naranja", "Morado", "Verde"))
  text(0.7,fa[1]+5,paste(fa[1]),cex=1.5,col='black',f=2)
  text(1.9,fa[2]+5,paste(fa[2]),cex=1.5,col='black',f=2)
  text(3.1,fa[3]+5,paste(fa[3]),cex=1.5,col='black',f=2)
  text(4.3,fa[4]+5,paste(fa[4]),cex=1.5,col='black',f=2)
  mtext("Color", side = 1, line = 2.5, cex = 2, font = 2)
  mtext("Falsas Alarmas", side = 2, line = 2, cex = 2, font = 2, las = 0)
  mtext('F. Alarmas por color',3,cex=2, font=2)
  }

###############
############################3
#Yes/No x Color

rm(list=ls())
for(archive in dir()){
  
  jaime <- read.csv(archive)
  jaime$Color <- as.character(jaime$Color)
  cafe <- strsplit(as.character(jaime$Color),split='-')
  print(c(archive))
  
  yes <- NULL
  no <- NULL
  for(nce in sort(unique(jaime$Color))){
    yes<- append(yes, sum(jaime$Respuesta[jaime$Color==nce]=='s'))
    no <- append(no,sum(jaime$Respuesta[jaime$Color==nce]=='n'))
    print(c(nce,
            'Sí', yes[length(yes)],
            'No', no[length(no)]))}
  
  barplot(yes, main = "", xlab = "", horiz=T, ylab = " ", ylim = c(0, 5),xlim = c(0,160), axes = FALSE, col = c("deepskyblue3", "darkorange2", "darkorchid1", "chartreuse3"))
  lines(c(yes[1], 160),c(0.7,0.7), lwd=2, lty=1, col="deepskyblue3")
  lines(c(yes[2], 160),c(1.9,1.9), lwd=2, lty=1, col="darkorange2")
  lines(c(yes[3], 160),c(3.1,3.1), lwd=2, lty=1, col="darkorchid1")
  lines(c(yes[4], 160),c(4.3,4.3), lwd=2, lty=1, col="chartreuse3")
  axis(1,at=c(0, 32, 64, 96, 128, 160),labels=c("0", "32", "64", "96","128", "160"), las=1)
  axis(2,at=c(0.7,1.9,3.1,4.3),labels=c("Azul","Naranja", "Purpura", "Verde"), las=3)
  text(yes[1]-5, 0.7,paste(yes[1]),cex=1,col='black',f=2)
  text(yes[2]-5, 1.9,paste(yes[2]),cex=1,col='black',f=2)
  text(yes[3]-5,3.1,paste(yes[3]),cex=1,col='black',f=2)
  text(yes[4]-5,4.3,paste(yes[4]),cex=1,col='black',f=2)
  text(yes[1]+5, 0.85,paste(no[1]),cex=1,col='black',f=2)
  text(yes[2]+5, 2.05,paste(no[2]),cex=1,col='black',f=2)
  text(yes[3]+5,3.25,paste(no[3]),cex=1,col='black',f=2)
  text(yes[4]+5,4.45,paste(no[4]),cex=1,col='black',f=2)
  text(yes[3]/2,3.1,'Sí',cex=3,col='black',f=3)
  text(yes[3]+no[3]/2,3.1,'No',cex=3,col='black',f=3)
  mtext("Total de ensayos por color", side = 1, line = 2.5, cex = 2, font = 2)
  mtext("Color", side = 4, line=0.8, cex = 2.2, font = 2, las = 3)
  #mtext('Sí por color',3,cex=1.5, font=2)
  mtext(archive, outer = TRUE, line = -3.5, cex=3, font=2)}


####################################
####################################
######       ROC curves     ########
####################################
####################################


rm(list=ls())
layout(matrix(1:1,ncol=1))
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
  
  plot(FAr_an,Hr_as, pch=16, col='deepskyblue4', xlim=c(0,1), ylim=c(0,1), xlab='Tasa F.A.', ylab='Tasa Hits', font.lab=2)
  points(FAr_bn,Hr_bs, lty=3, pch=16, col='darkorchid4')
  lines(hits_A,falarm_A,lwd=2,col='deepskyblue3')
  lines(hits_B,falarm_B,lwd=2,col='darkorchid3')
  lines(hits_na,falarm_na,lwd=1,col='black', lty=2)
  lines(c(0.58, 0.68),c(0.3,0.3), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.58, 0.68),c(0.2,0.2), lwd=2, lty=1, col="darkorchid3")
  text(FAr_an, Hr_as+.04, paste("D'(A)=", round(d_A,2)), offset=0, cex = 0.8, pos=4, col='deepskyblue4', font=2)
  text(FAr_bn, Hr_bs-.04, paste("D'(B)=", round(d_B,2)), offset=0, cex = 0.8, pos=4, col='darkorchid4', font=2)
  text(0.7, 0.3, labels="D' en la Condición Facil (A)", offset=0, cex = 0.8, pos=4)
  text(0.7, 0.2, labels="D' en la Condición Difícil (B)", offset=0, cex = 0.8, pos=4)
  title('ROC por Condición')
  mtext(archive,3,cex=.8)
}





#################################
####################################
###       Z-ROC curves     ########
####################################
####################################


rm(list=ls())
layout(matrix(1:1,ncol=1))
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
  fa_AN <- sum(jaime$Falsas.alarmas[jaime$Estimulo>=161&jaime$Estimulo<=320]=='True')
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
    #print(c(fa_AN[length(fa_AN)], 
    #        FAr_an[length(FAr_an)], 
    #        fa_BN[length(fa_BN)], 
    #        FAr_bn[length(FAr_bn)], 
    #        hits_BS[length(hits_BS)], 
    #        Hr_bs[length(Hr_bs)], 
    #        hits_AS[length(hits_AS)], 
    #        Hr_as[length(Hr_as)]))
    k_A <- qnorm(1-FAr_an,0,1)
    d_A <- qnorm(Hr_as,0,1)-qnorm(FAr_an,0,1)
    c_A <- k_A-(d_A/2)                    
    beta_A <- dnorm(k_A,d_A,1)/dnorm(k_A,0,1)
    k_B <- qnorm(1-FAr_bn,0,1)
    d_B <-qnorm(Hr_bs,0,1)-qnorm(FAr_bn,0,1)
    c_B <-k_B-(d_B/2)                    
    beta_B <-dnorm(k_B,d_B,1)/dnorm(k_B,0,1)
  
  hits_A <- c()
  falarm_A <- c()
  hits_B <- c()
  falarm_B <- c()
  hits_na <- c()
  falarm_na <- c()
  c <- seq(-10,10,0.1)
  d_null <- 0
  
  for (i in 1:length(c)){
    hits_A[i] <- qnorm(pnorm((-d_A/2)-c[i]))
    falarm_A[i] <- qnorm(pnorm((d_A/2)-c[i]))
    hits_B[i] <- qnorm(pnorm((-d_B/2)-c[i]))
    falarm_B[i] <- qnorm(pnorm((d_B/2)-c[i]))
    hits_na[i] <- qnorm(pnorm((d_null/2)-c[i]))
    falarm_na[i] <- qnorm(pnorm((-d_null/2)-c[i]))
  }
  
  plot(FAr_an,Hr_as, pch=16, col='firebrick2', xlim=c(0,6), ylim=c(0,6), xlab='Tasa F.A.', ylab='Tasa Hits', font.lab=2)
  points(FAr_bn,Hr_bs, lty=3, pch=16, col='firebrick4')
  lines(hits_A,falarm_A,lwd=2,col='firebrick2')
  lines(hits_B,falarm_B,lwd=2,col='firebrick4')
  lines(hits_na,falarm_na,lwd=1,col='black', lty=2)
  lines(c(0.58, 0.68),c(0.3,0.3), lwd=2, lty=1, col="deepskyblue3")
  lines(c(0.58, 0.68),c(0.2,0.2), lwd=2, lty=1, col="darkorchid3")
  text(FAr_an, Hr_as+.04, paste("D'(A)=", round(d_A,2)), offset=0, cex = 0.8, pos=4, col='deepskyblue4', font=2)
  text(FAr_bn, Hr_bs-.04, paste("D'(B)=", round(d_B,2)), offset=0, cex = 0.8, pos=4, col='darkorchid4', font=2)
  text(0.7, 0.3, labels="D' en la Condición Facil (A)", offset=0, cex = 0.8, pos=4)
  text(0.7, 0.2, labels="D' en la Condición Difícil (B)", offset=0, cex = 0.8, pos=4)
  title('ZROC por Condición')
  mtext(archive,3,cex=.8)
}
