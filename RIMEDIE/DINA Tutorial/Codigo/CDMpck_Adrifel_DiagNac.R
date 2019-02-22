#############################################################
# Codigo Prueba Paquete CDM
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/sandra/Desktop/afchavez-019/RIMEDIE/DINA Tutorial/Codigo")
library(CDM)

Datos <- read.table("m_dat_q0.txt",header = T)
Respuestas <- read.table("m_dat_q0.txt",header = T)       #Archivo con matriz ParticipantexItem que señala con 0 y 1 los aciertos y errores
Respuestas$SEXO <- NULL
Respuestas$MARGINC <- NULL 
Respuestas$SERV <- NULL
Respuestas$ID_SERV <- NULL    #Se eliminan las columnas innecesarias
Respuestas$NOM_ENT <- NULL
Respuestas$ESCUELA <- NULL
Respuestas$ID_ENT <- NULL
Respuestas$ALUMNO <- NULL

R_E1<-Respuestas[,c(1:8,26:34)]
R_E2<-Respuestas[,c(9:14,35:39)]
R_E3<-Respuestas[,c(15:25,40:50)]

#View(Respuestas)
Qmat <- read.table("q_matrices_mdatq_modificada.txt")                #Matriz Q:  ItemxHabilidad con valores 0 y 1 que señalan cuáles de las 35 habilidades se requiern por item

Q_E1<-Qmat[1:17,1:12]
Q_E2<-Qmat[18:28,1:10]
Q_E3<-Qmat[29:50,]
#View(Qmat)



###########################################################
# DINA

DINA_1 <- din(R_E1, Q_E1, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E1)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E1)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

write.csv(DINA_1$item, "E1_FEM_itemparameters.csv")
write.csv(DINA_1$skill.patt, "E1_FEM_skilpatterns.csv")
write.csv(data.frame(DINA_1$pattern), "E1_FEM_postpattern.csv",row.names = F)

DINA_2 <- din(R_E2, Q_E2, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E2)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E2)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

write.csv(DINA_2$item, "E2_MI_itemparameters.csv")
write.csv(DINA_2$skill.patt, "E2_MI_skilpatterns.csv")
write.csv(data.frame(DINA_2$pattern), "E2_MI_postpattern.csv",row.names = F)


DINA_3 <- din(R_E3, Q_E3, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E3)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E3)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

write.csv(DINA_3$item, "E3_SNPA_itemparameters.csv")
write.csv(DINA_3$skill.patt, "E3_SNPA_skilpatterns.csv")
write.csv(data.frame(DINA_3$pattern), "E3_SNPA_postpattern.csv",row.names = F)

##########################
DINA_1$skill.patt #El grado en que la MUESTRA domina cada habilidad

m_sk1 <- as.vector(DINA_1$skill.patt)
m_sk2 <- as.vector(DINA_2$skill.patt)
m_sk3 <- as.vector(DINA_3$skill.patt)

col_sk1 <- ifelse(m_sk1>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk2 <- ifelse(m_sk2>0.5, "lightsalmon1", "lightsalmon4")
col_sk3 <- ifelse(m_sk3>0.5, "palegreen3", "palegreen4")

q1 <- c("H101","H102","H103","H104","H105","H106","H107","H108","H109","H110","H111","H112")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210","H211","H212")
q3 <- c("H301","H302","H303","H304","H305","H306","H307","H308","H309","H310","H311","H312", "H313")

valor_label <- 12.8
barplot(rev(m_sk1), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk1[i]-.03,valor_label, paste(round(m_sk1[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}


valor_label <- 10.5
barplot(rev(m_sk2), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk2),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 2: Manejo de la Información", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,12,1))){
  text(m_sk2[i]-.03,valor_label, paste(round(m_sk2[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q2[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}



valor_label <- 13.8
barplot(rev(m_sk3), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk3),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 3: Sentido Numérico y Pensamiento Algebraico", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,13,1))){
  text(m_sk3[i]-.03,valor_label, paste(round(m_sk3[i],3)), f=2, cex=1.2)
  text(0.03,valor_label, paste(q3[i]), f=2, cex=1.2)
  valor_label <- valor_label - 1.1}