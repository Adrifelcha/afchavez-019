rm(list=ls())                                         #función para limpiar
setwd("C:/Users/Neli/Documents/R")                    #define tu working directory

#####################################################################################################################################################

#1. Elabora las siguientes funciones en R:

#a. ProbAcertar, la cual calcula para cierto valor del parámetro theta p y cierto valor del parámetro beta i la probabilidad según el modelo de Rasch 
#   de que la persona p acierte el ítem i.

#   Permite al usuario especificar los valores de theta p y beta i.


 #Vamos a crear la función ProbAcertar con los parámetros theta_p (rasgo de la persona) y beta_i (dificultad del ítem)
 #Pr es la probabilidad de acertar, y se calcula usando la ecuación básica de rasch, que es:

 # Pr = (e^(theta_p - beta_i)) / (1+e^(theta_p - beta_i))

 #la base 'e' que es la base de los logaritmos naturales existe como la función exp() en R.

ProbAcertar <- function(theta_p,beta_i) {
  Pr <- (exp(theta_p - beta_i) / (1 + (exp(theta_p - beta_i))))
  return(Pr)
}

 #Para comprobar que ProbAcertar funciona, utilizamos los ejemplos de las diapositivas. Los resultados que obtenemos son los mismos.

ProbAcertar(-1,1)
ProbAcertar(2,1)
ProbAcertar(0,0)
ProbAcertar(1,0)

#b. ProbFallar, la cual hace lo mismo que la función anterior salvo que calcula la probabilidad de fallar el ítem.

 #ProbFallar es el complemento de ProbAcertar. Para que cada función sea independiente, usamos la misma fórmula pero le restamos el resultado a 1.

ProbFallar <- function(theta_p,beta_i) {
  Pf <- 1 - (exp(theta_p - beta_i) / (1 + (exp(theta_p - beta_i))))
  return(Pf)
}

 #Con los mismos ejemplos, podemos ver que el resultado es el complemento de ProbAcertar.

ProbFallar(-1,1)
ProbFallar(2,1)
ProbFallar(0,0)
ProbFallar(1,0)

#c. CCIAcertar, la cual genera el tabulado de las probabilidades en el modelo de Rasch asociadas con la curva característica de un ítem con cierto
#   grado de dificultad beta_i.
#   Es decir, el tabulado tiene que dar, para un rango de valores de theta que el usuario puede especificar, la probabilidad de acertar el ítem.
#   Permite al usuario especificar el valor de beta_i, el valor mínimo y máximo de theta y el tamaño de los pasos intermedios para este rango de theta.

 #Creamos la función CCIAcertar cuyos parámetros son beta_i, valores minimo, máximo y pasos intermedios del rango de theta.
 #La función seq() nos permite crear el rango de theta (rango_theta) con los tres parámetros correspondientes de theta.
 #Declaramos fi_theta (la función theta) como un vector con la misma longitud que rango_theta.
 #Usamos un ciclo for() para calcular el valor de fi_theta en cada uno de los valores de rango_theta de acuerdo con el valor de beta_i especificado
 #y la ecuación básica del modelo de Rasch.
 #Los valores de rango_theta y fi_theta se almacenan en una matriz llamada Tabprob que contiene los valores de theta y las probabilidades de acertar.

CCIAcertar <- function(beta_i,theta_min,theta_max,theta_paso) {
  rango_theta <- seq(from=theta_min, to=theta_max, by=theta_paso)
  fi_theta <- rango_theta
  for(p in 1:length(rango_theta)){
    fi_theta[p] <- (exp(rango_theta[p] - beta_i) / (1 + (exp(rango_theta[p] - beta_i))))
  }
  Tabprob <- matrix(c(rango_theta,fi_theta),ncol=2)
  return(Tabprob)
}

#d. CCIFallar, la cual hace lo mismo que la función anterior salvo que genera las probabilidades de fallar el ítem.

 #Igual que la función CCIAcertar, pero se le resta a 1 para obtener la probabilidad complementaria.

CCIFallar <- function(beta_i,theta_min,theta_max,theta_paso) {
  rango_theta <- seq(from=theta_min, to=theta_max, by = theta_paso)
  fi_theta <- rango_theta
  for(p in 1:length(rango_theta)){
    fi_theta[p] <- 1 - (exp(rango_theta[p] - beta_i) / (1 + (exp(rango_theta[p] - beta_i))))
  }
  Tabprob <- matrix(c(rango_theta,fi_theta),ncol=2)
  return(Tabprob)
}

#####################################################################################################################################################

#2. (a) Utiliza la función CCIAcertar para ítems con los siguientes grados de dificultad: -2.34, -1.23, +0.15 y +3.34.
#   Especifica como valores de theta en el tabulado: (-6.00, -5.99, -5.98, ..., +5.99, +6.00); es decir, queremos para cada uno de estos ítems las
#   probabilidades de acertarlo para los valores de theta en el rango de -6 a +6 con pasos de 0.01.

 #Se usa la función CCIAcertar con los valores indicados y se guarda el tabulado obtenido para cada ítem.

tabla_acertar_1 <- CCIAcertar(-2.34,-6,6,.01)
tabla_acertar_2 <- CCIAcertar(-1.23,-6,6,.01)
tabla_acertar_3 <- CCIAcertar(0.15,-6,6,.01)
tabla_acertar_4 <- CCIAcertar(3.34,-6,6,.01)

#   (b) Aplica el mismo procedimiento para la función CCIFallar.

  #Igual que en (a) pero usando CCIFallar.

tabla_fallar_1 <- CCIFallar(-2.34,-6,6,.01)
tabla_fallar_2 <- CCIFallar(-1.23,-6,6,.01)
tabla_fallar_3 <- CCIFallar(0.15,-6,6,.01)
tabla_fallar_4 <- CCIFallar(3.34,-6,6,.01)

#####################################################################################################################################################

#3. Una vez generado estos tabulados, guarda el resultado para cada ítem en un archivo csv. (Da nombres claros a estos archivos.)

 #Se guardan los tabulados generados con el mismo nombre, en formato .csv.

write.csv(tabla_acertar_1,"tabla_acertar_1.csv",row.names = FALSE)
write.csv(tabla_acertar_2,"tabla_acertar_2.csv",row.names = FALSE)
write.csv(tabla_acertar_3,"tabla_acertar_3.csv",row.names = FALSE)
write.csv(tabla_acertar_4,"tabla_acertar_4.csv",row.names = FALSE)

write.csv(tabla_fallar_1,"tabla_fallar_1.csv",row.names = FALSE)
write.csv(tabla_fallar_2,"tabla_fallar_2.csv",row.names = FALSE)
write.csv(tabla_fallar_3,"tabla_fallar_3.csv",row.names = FALSE)
write.csv(tabla_fallar_4,"tabla_fallar_4.csv",row.names = FALSE)

#####################################################################################################################################################

#4. Genera las siguientes figuras, leyendo directamente la información en los archivos .csv creados en el ejercicio anterior.

#   (a) Una figura para cada una de los cuatro ítems que represente simultáneamente (es decir, en la misma figura) las curvas características tanto de
#   acertar como de fallar el ítem.

 #Se grafican las cuatro gráficas.
 #Como datos a graficar se especifican los archivos .csv. creados.

plot(read.csv("tabla_acertar_1.csv"),type="l",col="red")
lines(read.csv("tabla_fallar_1.csv"),type="l",col="red")

plot(read.csv("tabla_acertar_2.csv"),type="l",col="blue")
lines(read.csv("tabla_fallar_2.csv"),type="l",col="blue")

plot(read.csv("tabla_acertar_3.csv"),type="l",col="darkgreen")
lines(read.csv("tabla_fallar_3.csv"),type="l",col="darkgreen")

plot(read.csv("tabla_acertar_4.csv"),type="l",col="orange")
lines(read.csv("tabla_fallar_4.csv"),type="l",col="orange")

#   (b) Una sola figura que integre las curvas características de acertar para los cuatro reactivos.

 #Se grafica en una sola figura la información solicitada.

plot(read.csv("tabla_acertar_1.csv"),type="l",col="red")
lines(read.csv("tabla_acertar_2.csv"),type="l",col="blue")
lines(read.csv("tabla_acertar_3.csv"),type="l",col="darkgreen")
lines(read.csv("tabla_acertar_4.csv"),type="l",col="orange")

#####################################################################################################################################################

#5. (a) A partir de los tabulados y/o las figuras, estima para cada uno de los cuatro reactivos el nivel de theta para el cual la probabilidad de
#       acertar el ítem es igual a 0.80.

 #De acuerdo con las indicaciones, es posible determinar visualmente en la figura el valor de theta en el que la probabilidad de acertar es de 0.80.
 #También es posible inspeccionar las tablas manualmente. Una forma automatizada de obtener el resultado, sería la siguiente:
 #Obtener la pocisión en la que theta está mas cercana a 0.80. (pos_theta), buscando el valor en la columna de probabilidades que tiene la menor
 #diferencia respecto de 0.80 (encontrar la diferencia con el valor abosluto mínimo).
 #Se puede hacer una comprobación manual de que el valor es el más cercano a 0.80.
 #Se realiza el proceso para los cuatro ítems.

pos_theta1 <- which(abs(tabla_acertar_1[,2] - 0.80) == min(abs(tabla_acertar_1[,2] - 0.80)))
tabla_acertar_1[pos_theta1,1]

pos_theta2 <- which(abs(tabla_acertar_2[,2] - 0.80) == min(abs(tabla_acertar_2[,2] - 0.80)))
tabla_acertar_2[pos_theta2,1]

pos_theta3 <- which(abs(tabla_acertar_3[,2] - 0.80) == min(abs(tabla_acertar_3[,2] - 0.80)))
tabla_acertar_3[pos_theta3,1]

pos_theta4 <- which(abs(tabla_acertar_4[,2] - 0.80) == min(abs(tabla_acertar_4[,2] - 0.80)))
tabla_acertar_4[pos_theta4,1]

#   (b) Deriva con base en la ecuación básica del modelo de Rasch (la cual da la probabilidad de acertar el ítem para valores de theta p y beta i)
#       la función inversa (la cual permite conocer el valor de theta p a partir del valor de beta i y la probabilidad Ppi de que la persona p acierte
#       el ítem i.
#       (Recuerda que la función inversa de la exponencial ex es la función logarítmica log x.)

 #La función inversa de Rasch es es:

 #theta_p - beta_i = log base e (Ppi / 1 - Ppi)

 #Se pide que a partir de beta_i y Ppi se obtenga theta_p, por lo que se despeja theta_p:

 #theta_p = log base e (Ppi / 1 - Ppi) + beta_i

 #Se crea la función Obtener_theta_p con parámetros beta_i y PPi.
 #La función log() utiliza la base 'e' por default, pero se especifica de todas formas.

Obtener_theta_p <- function(beta_i,Ppi) {
  theta_p <- (log((Ppi/(1-Ppi)),base=exp(1)) + beta_i)
  return(theta_p)
}

#   (c) Utiliza la función anterior para conocer los valores exactos de theta en el ejercicio (a).

 #En el valor de beta_i se especifica la dificultad de los cuatro ítems.
 #El valor de Ppi es 0.80 la probabilidad en la que nos interesa conocer el valor de theta_p.
 #Obtenemos el valor de theta_p para cada ítem.
 #En todos los casos, el valor obtenido es cercano, pero más preciso, que el que habíamos extraído de las tabulaciones.

Obtener_theta_p(-1.23,0.80)
Obtener_theta_p(-2.34,0.80)
Obtener_theta_p(0.15,0.80)
Obtener_theta_p(3.34,0.80)