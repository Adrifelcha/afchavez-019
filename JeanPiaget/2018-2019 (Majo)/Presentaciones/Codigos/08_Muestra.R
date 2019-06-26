#######################################
#######################

setwd("C:/Users/Alejandro/Desktop/afchavez19/JeanPiaget/Presentaciones/Codigos")
Datos <- read.csv("08_Muestra_Calificaciones.csv")
Calificaciones <- Datos$Calificación

grupo <- "4B"
Cal_grupo <- Calificaciones[Datos$Grupo==grupo]

hist(Cal_grupo, breaks = seq(0,10,1), ann=F, axes=F, xlim=c(-1,10), col="lightgoldenrod1",
     ylim=c(0,10), 
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.9,0.9),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.8,.8),lwd=2,lty=3, col="black"),lines(c(0,1),c(.7,.7),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.6,.6),lwd=2,lty=3, col="black"),lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.4,.4),lwd=2,lty=3, col="black"),lines(c(0,1),c(.3,.3),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.2,.2),lwd=2,lty=3, col="black"),lines(c(0,1),c(.1,.1),lwd=2,lty=3, col="black")))
axis(2,at=seq(0,10,1),labels=seq(0,10,1),las=1, line=-1.5)
axis(1,at=c(-0.5:9.5),labels=c(0:10), line=-0.5, f=2)
mtext(side=3, text = "Calificaciones obtenidas en el 2do Parcial", line=1.5, cex=2, f=2)
mtext(side=3, text = paste("Por los estudiantes del grupo", grupo), line=0, cex=1.2, f=2)
text(2,8.3, paste("Promedio: ", round(mean(Cal_grupo),2)), f=2)
text(2,8.8, paste("No. Estudiantes: ", length(Cal_grupo)), f=2)
