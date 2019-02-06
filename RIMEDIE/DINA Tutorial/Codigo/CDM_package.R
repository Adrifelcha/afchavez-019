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
Respuestas$Grado <- NULL
Respuestas$Grupo <- NULL                           #Se eliminan las columnas innecesarias
#View(Respuestas)
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
#View(Qmat)



###########################################################
# DINA

DINA_1 <- din(Respuestas[,Eje1], Q_E1, skillclasses=NULL,
    conv.crit=0.001, dev.crit=10^(-2), maxit=500,
    constraint.guess=NULL, constraint.slip=NULL,
    guess.init= rep(0.25, ncol(Respuestas[,Eje1])), slip.init=guess.init,
    guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
    weights=rep(1, nrow(Respuestas[,Eje1])), rule="DINA",
    wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
    seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_2 <- din(Respuestas[,Eje2], Q_E2, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(Respuestas[,Eje2])), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(Respuestas[,Eje2])), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

DINA_3 <- din(Respuestas[,Eje3], Q_E3, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(Respuestas[,Eje3])), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(Respuestas[,Eje3])), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

##########################
DINA_1$skill.patt #El grado en que la MUESTRA domina cada habilidad

m_sk1 <- as.vector(DINA_1$skill.patt)
m_sk2 <- as.vector(DINA_2$skill.patt)
m_sk3 <- as.vector(DINA_3$skill.patt)

barplot(rev(m_sk1), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), col="deepskyblue3")
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.9, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)

barplot(rev(m_sk2), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), col="chocolate2")
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 2: MAnejo de la Información", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.9, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)

barplot(rev(m_sk3), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), col="seagreen4")
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 3: Sentido Numérico y Pensamiento Algebráico", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.9, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)