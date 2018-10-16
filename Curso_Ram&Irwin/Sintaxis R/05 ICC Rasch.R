# Sintaxis para ilustrar CCI del modelo de Rasch en la IRT 

library(mirt)

# Ejemplos ilustrativos de diveros modelos dicotómicos de la IRT

theta <- seq(-3, 3, .1)
#assign("theta", seq(-3,3,.1))
#seq(-3, 3, .1) -> theta

bmedium <- 0
P <- 1 / (1 + exp(-(theta - bmedium)))
plot(theta, P) 
plot(theta, P, type="l")

help("plot")
?plot

##### Figura mejorada

par(lab=c(10,10,2000)) #Asigna en los par?metros la separación que tendrán (eje x, eje y, extra) (x, y, len)
theta <- seq(-3, 3, .1)
b <- 0
P <- 1 / (1 + exp(-(theta - b)))
plot(theta, P, type="o", xlim=c(-3,3), ylim=c(0,1),  # type="l"
     xlab=expression(paste("Habilidad, ",theta)),
     ylab=expression(paste("Probabilidad de responder correctamente, P (",theta,")")))


#### Figura más mejorada!

par(lab=c(10,10,1))
theta <- seq(-3, 3, .1)
b <- 0
P <- 1 / (1 + exp(-(theta - b)))
plot(theta, P, type="o", xlim=c(-3,3), ylim=c(0,1), 
     main="Figura 1. Curva característica de un item con dificultad media
     y discriminación moderada",
      sub="Ver Baker y Kim (2016).",
     xlab=expression(paste("Habilidad, ",theta)),
     ylab=expression(paste("Probabilidad de responder correctamente, P (",theta,")")))

#### Su primer funci??n "ICC"

iccplot <- function(b) {
  par(lab=c(7,3,3))
  theta <- seq(-7, 7, .1)
  P <- 1 / (1 + exp(-1 * (theta - b)))
  plot(theta, P, type="l", xlim=c(-7,7), ylim=c(0,1),
       xlab=expression(paste("Habilidad, ",theta)), 
       ylab=expression(paste("Probabilidad de responder correctamente, P (",theta,")")))
}


iccplot(0)

iccplot(b=0) # Si cambian los valores de los parámetros a y b se modifica la forma general de la figura



icc.prob <- function(b) {
  theta <- seq(-7, 7, .1)
  P <- 1 / (1 + exp(-(theta - b)))
  as.vector(return(P))
}

icc.prob(1)

theta <- seq(-7, 7, .1)

prob1 <- as.data.frame(icc.prob(1)); prob1
arreglo <- cbind(theta, prob1); arreglo


#Dificultades
bmuyfacil <- -2.625
bfacil <- -1.5
bmedia <- 0
bdificil <- 1.5
bmuydificil <- 2.625


#Plots simultáneos

iccplot(bmedia)
par(new=T)
iccplot(bfacil)
par(new=T)
iccplot(bdificil)
par(new=T)
iccplot(bmuyfacil)
par(new=T)
iccplot(bmuydificil)



## END!
