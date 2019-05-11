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
x <- c(2,4,6,8,10,12,14)
y <- rev(x)
plot(x,y, pch=15, cex=2, xlab="Prueba de felicidad", ylab="Prueba de depresión", cex.lab=1.5)
lines(c(2,14),c(14,2), col="black", lty=4, lwd=2)


#Correlación perfecta nula
y <- rep(120,7)
x <- rnorm(7,60,5)
plot(x,y, pch=15, cex=2, xlab="Peso", ylab="Prueba de inteligencia")
lines(c(0,100),c(120,120), col="black", lty=4, lwd=2)
