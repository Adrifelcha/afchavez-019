#############################################################
# Tarea Aleatoria:                                          #
# Ejercicios en R con el modelo de Rasch                    #
#############################################################
rm(list=ls())           #Limpiamos variables
setwd("D:/afchavez/Desktop/Adrifelcha_PsicometriaYEvaluacion/Curso_Ram&Irwin/Tareas/Examen_Rasch")  #working directory
#############################################################


#############################################################
######################        I N S T R U C C I O N E S 
###################### 1. Elabora las siguientes funciones en R:
###################### a. ProbAcertar, la cual calcula para cierto valor del parámetro theta p y cierto valor del parámetro beta i 
######################   la probabilidad según el modelo de Rasch de que la persona p acierte el ítem i.
##############   Permite al usuario especificar los valores de theta p y beta i.

 # Creamos una función "ProbAcertar" que dependa de dos parámetros: "theta_p" y "beta_i"
 # La función "ProbAcertar" devuelve la probabilidad de acertar, de acuerdo con la ecuación planteada por el modelo de Rasch:

ProbAcertar <- function(theta_p,beta_i) { (exp(theta_p - beta_i) / (1 + (exp(theta_p - beta_i)))) }

#Comprobamos que ProbAcertar funciona, utilizando los ejemplos de la diapositiva 9. Los resultados que obtenemos son los mismos.
ProbAcertar(-1,1)
ProbAcertar(2,1)
ProbAcertar(0,0)
ProbAcertar(1,0)

###################### b. ProbFallar, la cual hace lo mismo que la función anterior salvo que calcula la probabilidad de fallar el ítem.

# ProbFallar es el complemento de ProbAcertar. 
 
ProbFallar <- function(theta_p,beta_i) { 1 - (exp(theta_p - beta_i) / (1 + (exp(theta_p - beta_i)))) }

###################### c. CCIAcertar, la cual genera el tabulado de las probabilidades en el modelo de Rasch asociadas con 
######################    la curva característica de un ítem con cierto grado de dificultad beta_i.
######################    Es decir, el tabulado tiene que dar, para un rango de valores de theta que el usuario puede especificar, 
######################    la probabilidad de acertar el ítem.
###############  Permite al usuario especificar el valor de beta_i, el valor mínimo y máximo de theta 
###############  y el tamaño de los pasos intermedios para este rango de theta.

CCIAcertar <- function(beta_i,theta_min,theta_max,theta_salto) {
  rango_theta <- seq(from=theta_min, to=theta_max, by=theta_salto)
  fi_theta <- rango_theta
  for(p in 1:length(rango_theta)){
    fi_theta[p] <- (exp(rango_theta[p] - beta_i) / (1 + (exp(rango_theta[p] - beta_i))))
  }
  Tabprob <- matrix(c(rango_theta,fi_theta),ncol=2)
  return(Tabprob)
}

###################### d. CCIFallar, la cual hace lo mismo que la función anterior salvo que genera las probabilidades de fallar el ítem.

CCIFallar <- function(beta_i,theta_min,theta_max,theta_salto) {
  rango_theta <- seq(from=theta_min, to=theta_max, by = theta_salto)
  fi_theta <- rango_theta
  for(p in 1:length(rango_theta)){
    fi_theta[p] <- 1 - (exp(rango_theta[p] - beta_i) / (1 + (exp(rango_theta[p] - beta_i))))
  }
  Tabprob <- matrix(c(rango_theta,fi_theta),ncol=2)
  return(Tabprob)
}

#############################################################
###################### Ejercicio 2
###################### a) Utiliza la función CCIAcertar para ítems con los siguientes grados de dificultad: 
###################### -2.34, -1.23, +0.15 y +3.34.
###################### Especifica como valores de theta en el tabulado: (-6.00, -5.99, -5.98, ..., +5.99, +6.00); 
###################### es decir, queremos para cada uno de estos ítems las probabilidades de acertarlo para 
###################### los valores de theta en el rango de -6 a +6 con pasos de 0.01.

 #Se usa la función CCIAcertar con los valores indicados y se guarda el tabulado obtenido para cada ítem.
tabla_acertar_1 <- CCIAcertar(-2.34,-6,6,.01)  
tabla_acertar_2 <- CCIAcertar(-1.23,-6,6,.01)
tabla_acertar_3 <- CCIAcertar(0.15,-6,6,.01)
tabla_acertar_4 <- CCIAcertar(3.34,-6,6,.01)

###################### b) Aplica el mismo procedimiento para la función CCIFallar.

#Igual que en (a) pero usando CCIFallar.

tabla_fallar_1 <- CCIFallar(-2.34,-6,6,.01)
tabla_fallar_2 <- CCIFallar(-1.23,-6,6,.01)
tabla_fallar_3 <- CCIFallar(0.15,-6,6,.01)
tabla_fallar_4 <- CCIFallar(3.34,-6,6,.01)

#############################################################
###################### Ejercicio 3
###################### 3. Una vez generado estos tabulados, guarda el resultado para cada ítem en un archivo csv. 
###################### (Da nombres claros a estos archivos.)

write.csv(tabla_acertar_1,"tab_acertar_item1.csv",row.names = FALSE)
write.csv(tabla_acertar_2,"tab_acertar_item2.csv",row.names = FALSE)
write.csv(tabla_acertar_3,"tab_acertar_item3.csv",row.names = FALSE)
write.csv(tabla_acertar_4,"tab_acertar_item4.csv",row.names = FALSE)

write.csv(tabla_fallar_1,"tab_fallar_item1.csv",row.names = FALSE)
write.csv(tabla_fallar_2,"tab_fallar_item2.csv",row.names = FALSE)
write.csv(tabla_fallar_3,"tab_fallar_item3.csv",row.names = FALSE)
write.csv(tabla_fallar_4,"tab_fallar_item4.csv",row.names = FALSE)

#############################################################
###################### Ejercicio 3
###################### 4. Genera las siguientes figuras, leyendo directamente la información en los archivos .csv creados 
###################### en el ejercicio anterior.

###################### a) Una figura PARA CADA UNO de los cuatro ítems que represente simultáneamente (es decir, en la misma 
###################### figura) las curvas características tanto de acertar como de fallar el ítem.

 #Se grafican las cuatro gráficas.
 #Como datos a graficar se especifican los archivos .csv. creados.
plot(read.csv("tab_acertar_item1.csv"), type="l", col="red", main="Item 1", lwd=2)
lines(c(-7, 6), c(0.5,0.5), lty=8, col="gray")
lines(read.csv("tab_fallar_item1.csv"),type="l",col="indianred3", lwd=2)
legend(-6,0.6, c("p(Acierto)", "p(Error)"), lty=c(1,1), col=c("red", "indianred3"), lwd=2, cex=0.8)
legend(3.5,0.9, expression(paste(beta, " = -2.34   ")), lty=c(2,2),  cex=1.5)
text(5.5,0.45, "p(Acertar)= 0.5", col="gray", cex=0.8)
lines(c(-2.34, -2.34), c(0,1), lty=3)


plot(read.csv("tab_acertar_item2.csv"),type="l",col="cyan3", main="Item 2", lwd=2)
lines(c(-7, 6), c(0.5,0.5), lty=8, col="gray")
lines(read.csv("tab_fallar_item2.csv"),type="l",col="cyan4", lwd=2)
legend(-6,0.6, c("p(Acierto)", "p(Error)"), lty=c(1,1), col=c("cyan3", "cyan4"), lwd=2, cex=0.8)
legend(3.5,0.9, expression(paste(beta, " = -1.23  ")), lty=c(2,2),  cex=1.5)
text(5.5,0.45, "p(Acertar)= 0.5", col="gray", cex=0.8)
lines(c(-1.23, -1.23), c(0,1), lty=5)

plot(read.csv("tab_acertar_item3.csv"),type="l",col="forestgreen", main="Item 3", lwd=2)
lines(c(-7, 6), c(0.5,0.5), lty=8, col="gray")
lines(read.csv("tab_fallar_item3.csv"),type="l",col="darkgreen", lwd=2)
legend(-6,0.6, c("p(Acierto)", "p(Error)"), lty=c(1,1), col=c("forestgreen", "darkgreen"), lwd=2, cex=0.8)
legend(3.5,0.9, expression(paste(beta, " = 0.15  ")), lty=c(2,2),  cex=1.5)
text(5.5,0.45, "p(Acertar)= 0.5", col="gray", cex=0.8)
lines(c(0.15, 0.15), c(0,1), lty=2)

plot(read.csv("tab_acertar_item4.csv"),type="l",col="orange", main="Item 4", lwd=2)
lines(c(-7, 6), c(0.5,0.5), lty=8, col="gray")
lines(read.csv("tab_fallar_item4.csv"),type="l",col="darkorange", lwd=2)
legend(-6,0.6, c("p(Acierto)", "p(Error)"), lty=c(1,1), col=c("orange", "darkorange"), lwd=2, cex=0.8)
legend(3.5,0.9, expression(paste(beta, " = 3.34  ")), lty=c(2,2),  cex=1.5)
text(5.5,0.45, "p(Acertar)= 0.5", col="gray", cex=0.8)
lines(c(3.34, 3.34), c(0,1), lty=4)

###################### b) Una sola figura que integre las curvas características de acertar para los cuatro reactivos.

plot(read.csv("tab_acertar_item1.csv"),type="l",col="red", lwd=2, main=expression(paste("Probabilidad de acertar como una función de ", theta)),
     xlab=expression(paste(theta)), ylab="Probabilidad de acierto")
lines(c(-7, 6), c(0.5,0.5), lty=8, col="gray")
lines(read.csv("tab_acertar_item2.csv"), col="blue", lwd=2)
lines(read.csv("tab_acertar_item3.csv"), col="darkgreen", lwd=2)
lines(read.csv("tab_acertar_item4.csv"), col="orange",lwd=2)
text(5.5,0.45, "p(Acertar)= 0.5", col="gray", cex=0.8)
lines(c(-2.34, -2.34), c(0,0.5), lty=2, col="darkgray")
lines(c(-1.23, -1.23), c(0,0.5), lty=2, col="darkgray")
lines(c(0.15, 0.15), c(0,0.5), lty=2, col="darkgray")
lines(c(3.34, 3.34), c(0,0.5), lty=2, col="darkgray")
legend(-5.5,0.9, c(expression(paste("Item 1  ", beta, " = -2.34   " )),
                   expression(paste("Item 2  ", beta, " = -1.23" )),
                 expression(paste("Item 3  ", beta, " = 0.15" )),
                 expression(paste("Item 4  ", beta, " = 3.34" ))),  lty=c(1,1), lwd=2, col=c("red", "blue", "darkgreen","orange"))
#############################################################
###################### Ejercicio 5
###################### a) A partir de los tabulados y/o las figuras, estima para cada uno de los cuatro reactivos el nivel de 
###################### theta para el cual la probabilidad de acertar el ítem es igual a 0.80.

 #De acuerdo con las indicaciones, 
 #1) Es posible determinar según las CCI graficadas el valor de theta en el que la probabilidad de acertar es de 0.80.
plot(read.csv("tab_acertar_item1.csv"),type="l",col="red", lwd=2, main=expression(paste("Probabilidad de acertar como una función de ", theta)),
     xlab=expression(paste(theta)), ylab="Probabilidad de acierto")
lines(read.csv("tab_acertar_item2.csv"),type="l",col="blue", lwd=2)
lines(read.csv("tab_acertar_item3.csv"),type="l",col="darkgreen", lwd=2)
lines(read.csv("tab_acertar_item4.csv"),type="l",col="orange",lwd=2)
text(5.5,0.75, "p(Acertar)= 0.8", col="indianred", cex=0.8)
lines(c(-.95, -.95), c(0,0.8), lty=3, col="darkgray", lwd=2)
lines(c(0.16, 0.16), c(0,0.8), lty=3, col="darkgray", lwd=2)
lines(c(1.54, 1.54), c(0,0.8), lty=3, col="darkgray", lwd=2)
lines(c(4.73, 4.73), c(0,0.8), lty=3, col="darkgray", lwd=2)
lines(c(-7, 6), c(0.8,0.8), lty=8, col="red", lwd=2)
legend(-5.5,0.9, c(expression(paste("Item 1  ")),
                   expression(paste("Item 2  ")),
                   expression(paste("Item 3  ")),
                   expression(paste("Item 4  "))),  lty=c(1,1), lwd=2, col=c("red", "blue", "darkgreen","orange"))

 #2) También es posible rastrear en las tablas creadas, el valor de theta más cercano a fi_theta = 0.8.

theta1 <- which(abs(tabla_acertar_1[,2] - 0.80) == min(abs(tabla_acertar_1[,2] - 0.80)))
tabla_acertar_1[theta1,1]

theta2 <- which(abs(tabla_acertar_2[,2] - 0.80) == min(abs(tabla_acertar_2[,2] - 0.80)))
tabla_acertar_2[theta2,1]

theta3 <- which(abs(tabla_acertar_3[,2] - 0.80) == min(abs(tabla_acertar_3[,2] - 0.80)))
tabla_acertar_3[theta3,1]

theta4 <- which(abs(tabla_acertar_4[,2] - 0.80) == min(abs(tabla_acertar_4[,2] - 0.80)))
tabla_acertar_4[theta4,1]

#   (b) DERIVA con base en la ecuación básica del modelo de Rasch (la cual da la probabilidad de acertar el ítem para valores de theta p y beta i)
#       la función inversa (la cual permite conocer el valor de theta p a partir del valor de beta i y la probabilidad Ppi de que la persona p acierte
#       el ítem i.
#       (Recuerda que la función inversa de la exponencial ex es la función logarítmica log x.)

 #La función inversa de Rasch es:
 #theta_p - beta_i = log base e (Ppi / 1 - Ppi)

 #Se pide que a partir de beta_i y Ppi se obtenga theta_p, por lo que se despeja theta_p:
 #theta_p = log base e (Ppi / 1 - Ppi) + beta_i

 #Se crea la función "Obtener_theta_p" con parámetros beta_i y PPi.
 #La función log() utiliza la base 'e' por default

Obtener_theta_p <- function(beta_i,Ppi) {
  theta_p <- (log((Ppi/(1-Ppi))) + beta_i)
  return(theta_p)
}

#   (c) Utiliza la función anterior para conocer los valores exactos de theta en el ejercicio (a).

 #En el valor de beta_i se especifica la dificultad de los cuatro ítems.
 #El valor de Ppi es 0.80 la probabilidad en la que nos interesa conocer el valor de theta_p.
 #Obtenemos el valor de theta_p para cada ítem.
 #En todos los casos, el valor obtenido es cercano, pero más preciso, que el que habíamos extraído de las tabulaciones.
Obtener_theta_p(-2.34,0.80)
Obtener_theta_p(-1.23,0.80)
Obtener_theta_p(0.15,0.80)
Obtener_theta_p(3.34,0.80)