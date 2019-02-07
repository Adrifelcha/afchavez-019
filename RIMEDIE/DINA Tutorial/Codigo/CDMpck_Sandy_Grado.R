#############################################################
# Codigo Prueba Paquete CDM
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Alejandro/Desktop/afchavez19/RIMEDIE/DINA Tutorial/Codigo")
library(CDM)

Respuestas <- read.csv("MatrizAciertos.csv")       #Archivo con matriz ParticipantexItem que señala con 0 y 1 los aciertos y errores
Respuestas$Nombre <- NULL
Respuestas$Forma.A <- NULL
Respuestas$Forma.B <- NULL
Respuestas$Total <- NULL
Respuestas$Sexo <- NULL
Respuestas$Grupo <- NULL                           #Se eliminan las columnas innecesarias
View(Respuestas)
RespQuinto <- Respuestas[Respuestas$Grado==1,] 
RespSexto <- Respuestas[Respuestas$Grado==2,]
RespQuinto$Grado <- NULL
RespSexto$Grado <- NULL
Qmat <- read.csv("QMatriz_Own.csv")                #Matriz Q:  ItemxHabilidad con valores 0 y 1 que señalan cuáles de las 35 habilidades se requiern por item
Qmat[is.na(Qmat)] <- 0                             #Se sustituyen los espacios en blanco con valores 0
Eje1 <- NULL
Eje2 <- NULL
Eje3 <- NULL
e1 <- 0
e2 <- 0
e3 <- 0
for(i in 1:nrow(Qmat)){                            #Se identifica a los ítems que pertenecen a cada Eje temático
  if(Qmat$Eje[i]=="1-EFM"){
    e1 <- e1 + 1
    Eje1[e1] <- Qmat$Item[i]
  }else{
    if(Qmat$Eje[i]=="2 - MI"){
      e2 <- e2+1
      Eje2[e2] <- Qmat$Item[i]
    }else{
      e3 <- e3+1
      Eje3[e3] <- Qmat$Item[i]
    }}}
Qmat$Item <- NULL
Qmat$Cod_Ram <- NULL                               #Se cancelan las columnas innecesarias
Qmat$Eje <- NULL
Q_E1 <- Qmat[Eje1,1:12]
Q_E2 <- Qmat[Eje2,13:22]
Q_E3 <- Qmat[Eje3,23:35]
R5_E1 <- RespQuinto[,Eje1]
R5_E2 <- RespQuinto[,Eje2]
R5_E3 <- RespQuinto[,Eje3]
R6_E1 <- RespSexto[,Eje1]
R6_E2 <- RespSexto[,Eje2]
R6_E3 <- RespSexto[,Eje3]
#View(Qmat)



###########################################################
###########################################################
# QUINTO AÑO

DINA_51 <- din(R5_E1, Q_E1, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R5_E1)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R5_E1)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_52 <- din(R5_E2, Q_E2, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R5_E2)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R5_E2)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_53 <- din(R5_E3, Q_E3, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R5_E3)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R5_E3)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

m_sk51 <- as.vector(DINA_51$skill.patt)
m_sk52 <- as.vector(DINA_52$skill.patt)
m_sk53 <- as.vector(DINA_53$skill.patt)

col_sk51 <- ifelse(m_sk51>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk52 <- ifelse(m_sk52>0.5, "lightsalmon1", "lightsalmon4")
col_sk53 <- ifelse(m_sk53>0.5, "palegreen3", "palegreen4")

q1 <- c("H101","H102","H103","H104","H105","H106","H107","H108","H109","H110","H111","H112")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210","H211","H212")
q3 <- c("H301","H302","H303","H304","H305","H306","H307","H308","H309","H310","H311","H312", "H313")

valor_label <- 12.8
barplot(rev(m_sk51), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk51),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk51[i]-.03,valor_label, paste(round(m_sk51[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}


valor_label <- 10.5
barplot(rev(m_sk52), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk52),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 2: Manejo de la Información", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk52[i]-.03,valor_label, paste(round(m_sk52[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q2[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}



valor_label <- 13.8
barplot(rev(m_sk53), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk53),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 3: Sentido Numérico y Pensamiento Algebraico", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,13,1))){
  text(m_sk53[i]-.03,valor_label, paste(round(m_sk53[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q3[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}




##########################################################
###########################################################
# Sexto AÑO

DINA_61 <- din(R6_E1, Q_E1, skillclasses=NULL,
               conv.crit=0.001, dev.crit=10^(-2), maxit=500,
               constraint.guess=NULL, constraint.slip=NULL,
               guess.init= rep(0.25, ncol(R6_E1)), slip.init=guess.init,
               guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
               weights=rep(1, nrow(R6_E1)), rule="DINA",
               wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
               seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_62 <- din(R6_E2, Q_E2, skillclasses=NULL,
               conv.crit=0.001, dev.crit=10^(-2), maxit=500,
               constraint.guess=NULL, constraint.slip=NULL,
               guess.init= rep(0.25, ncol(R6_E2)), slip.init=guess.init,
               guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
               weights=rep(1, nrow(R6_E2)), rule="DINA",
               wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
               seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_63 <- din(R6_E3, Q_E3, skillclasses=NULL,
               conv.crit=0.001, dev.crit=10^(-2), maxit=500,
               constraint.guess=NULL, constraint.slip=NULL,
               guess.init= rep(0.25, ncol(R6_E3)), slip.init=guess.init,
               guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
               weights=rep(1, nrow(R6_E3)), rule="DINA",
               wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
               seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

m_sk61 <- as.vector(DINA_61$skill.patt)
m_sk62 <- as.vector(DINA_62$skill.patt)
m_sk63 <- as.vector(DINA_63$skill.patt)

col_sk61 <- ifelse(m_sk61>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk62 <- ifelse(m_sk62>0.5, "lightsalmon1", "lightsalmon4")
col_sk63 <- ifelse(m_sk63>0.5, "palegreen3", "palegreen4")

q1 <- c("H101","H102","H103","H104","H105","H106","H107","H108","H109","H110","H111","H112")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210","H211","H212")
q3 <- c("H301","H302","H303","H304","H305","H306","H307","H308","H309","H310","H311","H312", "H313")

valor_label <- 12.8
barplot(rev(m_sk61), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk61),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Sexto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk61[i]-.03,valor_label, paste(round(m_sk61[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}


valor_label <- 10.5
barplot(rev(m_sk62), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk62),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 2: Manejo de la Información", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Sexto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk62[i]-.03,valor_label, paste(round(m_sk62[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q2[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}



valor_label <- 13.8
barplot(rev(m_sk63), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk63),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 3: Sentido Numérico y Pensamiento Algebraico", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Sexto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,13,1))){
  text(m_sk63[i]-.03,valor_label, paste(round(m_sk63[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q3[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}