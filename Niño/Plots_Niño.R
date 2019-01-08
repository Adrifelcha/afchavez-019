rm(list=ls())
setwd("D:/afchavez/Desktop/Adrifelcha_Lab25/Proyectos/Niño")
dir()

Datos <- read.csv("Datos_JM.csv")
Participantes <- Datos$Participante
Ganancias <- Datos$Ganancias
Perdidas <- Datos$Pérdidas

plot(Ganancias, pch=15, col="forestgreen", ylim=c(0,1), cex=1.2, 
     xlab="Participantes", ylab="P")
for(b in 1:length(Perdidas)){
  lines(c(Participantes[b], Participantes[b]), c(0,1), lty=3)
}
points(Perdidas, pch=17, col="red", cex=1.2)
points(Ganancias, pch=15, col="forestgreen", cex=1.2)
legend(15, 0.8, c("Ganancias", "Perdidas"))
axis(1, Participantes)
points(15.3,0.725, pch=16, col="forestgreen")
points(15.3,0.64, pch=16, col="red")



plot(Ganancias, pch=15, col="chartreuse4", type="l", ylim=c(0,1), cex=1.2, xlab= "Participantes",
     ylab= "P")
for(b in 1:length(Perdidas)){
  lines(c(Participantes[b], Participantes[b]), c(0,1), lty=3, col="gray")
}
points(Ganancias, pch=15, col="forestgreen")
axis(1, Participantes)
lines(Perdidas, col="indianred")
points(Perdidas, pch=17, col="red")
legend(11.5, 0.7, c("Ganancias", "Perdidas"), lty=c(1,1), lwd=2, col=c("forestgreen", "red"))
points(15.3,0.725, pch=16, col="forestgreen")
points(15.3,0.64, pch=16, col="red")




P_matriz <- matrix(data=c(Ganancias,Perdidas), nrow=2, ncol=18, byrow=TRUE)
matplot(P_matriz, type="b", lty=1, lwd=3, pch=21, col=c("indianred"),
        cex=1, ylim=c(0,1), xlim=c(0.8,2.25), xlab='', ylab='', font.lab=2, cex.lab=1,
        las=1, labels=F, ann=F)
mtext('¿Título?',3,cex=2, col='orange4',line=1.2)
mtext("Condición",1,cex=1, line=2.5, f=2)
mtext("P",2,cex=1, line=2.5, f=2)
axis(1,at=c(1,2),labels=c("Ganancias","Pérdidas"),las=1)
axis(2,at=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5),labels=c("0","0.5","1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0","5.5"),las=1)


plot(NULL, type="b", lty=1, lwd=3, pch=21, col=c("indianred"),
        cex=1, ylim=c(0,1), xlim=c(1,2.3), xlab='', ylab='', font.lab=2, cex.lab=1,
        las=1, labels=F, ann=F, axes=FALSE)
for(a in 1:length(Perdidas)){
  if(Ganancias[a]>Perdidas[a]){
    lines(c(1,2),c(Ganancias[a],Perdidas[a]), col="brown2", lwd=2)
  }else{
    lines(c(1,2),c(Ganancias[a],Perdidas[a]), col="goldenrod4", lwd=2)
  }
}
#mtext('Título',3,cex=2, col='orange4',line=1.2)
mtext("Condición",1,cex=1, line=2.5, f=2)
mtext("P",2,cex=1, line=2.5, f=2)
axis(1,at=c(1,2),labels=c("Wins","Loss"),las=1)
axis(2,at=c(0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5),labels=c("0","0.5","1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0","5.5"),las=1)
legend(2.05, 0.8, c("Wins > Loss", "Wins > Loss"), lty=c(1,1), lwd=2, col=c("brown2", "goldenrod4"))
