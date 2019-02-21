alumnos <- c("Mariana", "Viridiana", "Sergio", "Laura", "Guadalupe", "Chester", "Agustín")
semana_1 <- c(135, 125, 118, 110, 108, 106, 100)
semana_2 <- c(131, 130, 125, 112, 110, 105, 101)

plot(semana_1,semana_2, pch=15, xlim= c(100,135), ylim=c(100,135), col="black", cex=2,
     xlab="Semana 1", ylab="Semana 2")
lines(c(0,140),c(0,140),col='black', lty=4,lwd=2) 
for(i in 1:6){
  text(semana_1[i], semana_2[i]+1, paste(alumnos[i]), cex=1.2)
}


#Correlación perfecta positiva
plot(semana_1,semana_1, pch=15, xlim= c(100,135), ylim=c(100,135), col="black", cex=2,
     xlab="Semana 1", ylab="Semana 2")
lines(c(0,140),c(0,140),col='black', lty=4,lwd=2) 
for(i in 1:6){
  text(semana_1[i], semana_1[i]+1, paste(alumnos[i]), cex=1.2)
}


#Correlación perfecta negativa
plot(-(semana_1),-(semana_1), pch=15, xlim= c(-135,-100), ylim=c(-135,-100), col="black", cex=2,
     xlab="Semana 1", ylab="Semana 2")
lines(c(0,-140),c(0,-140),col='black', lty=4,lwd=2) 
for(i in 1:6){
  text(-(semana_1[i]), -(semana_1[i]-1), paste(alumnos[i]), cex=1.2)
}