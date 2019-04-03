setwd("C:/Users/sandra/Desktop/afchavez-019/Niño")
Datos <- read.csv("Exp1_binomial.csv")
#View(datos)

Participantes <- Datos[,1]
Ganancias <- Datos$Ganancias
Perdidas <- Datos$Perdidas
Razon <- Ganancias-Perdidas
Datos$Razon <- Razon

write.csv(Datos, "Exp1_binomial_razones.csv")


plot(Ganancias, pch=15, col="forestgreen", ylim=c(0,50), cex=1.2, 
     xlab="Participantes", ylab="P")
for(b in 1:length(Perdidas)){
  lines(c(Participantes[b], Participantes[b]), c(0,50), lty=3)
}
points(Perdidas, pch=17, col="red", cex=1.2)
#points(Ganancias, pch=15, col="forestgreen", cex=1.2)
legend(3, 30, c("Ganancias", "Perdidas"))
axis(1, Participantes)
points(3.5,27.8, pch=16, col="forestgreen")
points(3.5,25.8, pch=16, col="red")



plot(Razon, pch=15, col="forestgreen", ylim=c(-50,50), cex=1.2, 
     ann=F,axes=F)
for(b in 1:length(Perdidas)){
  lines(c(Participantes[b], Participantes[b]), c(-50,50), lty=3)
}
#points(Perdidas, pch=17, col="red", cex=1.2)
#points(Ganancias, pch=15, col="forestgreen", cex=1.2)
legend(3, 33, c("Ganancias", "Perdidas"))
axis(1, Participantes)
axis(2,c(-30,0,30), c("Más varianza en Pérdidas","<- Neutro ->","Más Varianza en Ganancias"))
points(3.5,29, pch=16, col="forestgreen")
points(3.5,25, pch=16, col="red")
lines(c(0, 50), c(0,0), lty=1, lwd=3,col="red")


