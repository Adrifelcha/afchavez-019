#############################################################
# Tarea 1 : TCT vs TRI 
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("D:/afchavez/Desktop/Adrifelcha_PsicometriaYEvaluacion/Curso_Ram&Irwin/Tareas/Tarea1_IRTvsTCT")
#############################################################
Datos <- read.csv("Tarea1_Datos.csv")    #Datos a trabajar
Datos$Id <- NULL      #Eliminamos las columnas innecesarias
Datos$Sexo <- NULL       #para el análisis de Rasch
Datos$X <- NULL
View(Datos)           #Echamos un vistazo a la base final
###################################################
###################################################
############## PARTE I 
############## Beta (IRT) vs Dificultad p (TTC)
###################################################
##############  Calcula para cada ítem el índice de dificultad (p) en el marco de la Teoría Clásica de los Tests (TCT).
P <- NULL
Items <- c(1:12)

for(a in 1:length(Items)){
P[a] <- mean(Datos[,a])  
}
Dif_TCT <- matrix(data=c(Items,P),ncol=2)      #Ordenamos el valor computado por cada item en una matriz
colnames(Dif_TCT) <- c("Item", "Dificultad (P)")                  #asignamos un nombre a cada columna
Dif_TCT         #Imprimimos la matriz resultante
###################################################
##############  Ajusta el modelo de Rasch a los datos para obtener estimaciones de los parámetros de dificultad (Beta) de los ítems.
library(mirt)
Rasch <- mirt(Datos, 1, itemtype = 'Rasch')
rep_coef<-data.frame(coef(Rasch, simplify=TRUE)$items)
b<- -1*(rep_coef$d/rep_coef$a1)
rep<-cbind(rep_coef,b)
rep <- subset(rep, select=5); round(rep,3)
###################################################
############## PREGUNTAS: 
#(a) Organiza los resultados de los analisis anteriores en una tabla que permite una comparacion directa entre los indices de dificultad 
# de la TCT y los parametros de dificultad en el modelo de Rasch.
Comparacion <- matrix(data=c(P,round(b, 3)),ncol=2)      #creamos una matriz con p en una columna y b en otra
colnames(Comparacion) <- c("p", "Beta")                  
Comparacion         #Imprimimos la matriz resultante

#(b) Explica como se interpretan los indices p de la TCT.
#    R: Es la proporcion de personas que respondieron correctamente. 
#       Se interpreta como la probabilidad de que el item sea contestado correctamente por la muestra
#       Valores cercanos a 1 indican que el ítem es más fácil, y cercanos a 0 que es más difícil.

#(c) Para el modelo de Rasch, interpreta:
#    i. la dimension latente que subyace a las respuestas;
#    R: la aceptabilidad de "explotar" animales para diversos usos, o la afinidad hacia actitudes opuestas al veganismo

#    ii. los parametros b de los items.
#    R: la cantidad o medida de la dimension latente que debe tener una persona para tener una probabilidad de .5 de responder afirmativamente al item.
#       Mientras mayor es el el valor de b, más se requiere del atributo/rasgo latente, es decir, más dificil es el ítem.

#(d) En terminos generales, ¿que relacion existe entre los indices p y los parámetros beta de ambos enfoques?
#    R: ambos indican que tan dificil es un ítem en función de la probabilidad de responderlo correctamente. Valores de p mas grandes se corresponden
#       con valores de b mas pequeños, y viceversa.



###################################################
###################################################
############## PARTE II 
############## Theta (IRT) vs X (TTC)
###################################################
############## Calcula para cada persona su puntuación (X) en el marco de la TCT (es decir, el núumero de veces que responde 
############## "Sí, es aceptable" en las 12 preguntas).
X <- NULL
Theta <- NULL
Personas <- nrow(Datos)
for(a in 1:Personas){
  X[a] <- sum(Datos[a,Items])  
}
Dif_TCT <- matrix(data=c(Items,P),ncol=2)      #Ordenamos el valor computado por cada item en una matriz
colnames(Dif_TCT) <- c("Item", "Dificultad (P)")                  #asignamos un nombre a cada columna
Dif_TCT         #Imprimimos la matriz resultante

a <- 0
table(X)
barplot(table(X), main="Frecuencia de Puntajes Totales (X)", col="darkgreen", 
        xlab="Puntajes", ylab="Frecuencia", ylim=c(0,180))
for(i in 1:length(sort(unique(X)))){
  text(0.7+a, table(X)[i]+15, paste(table(X)[i]), cex=0.8, f=2)
  a <- a+1.2
}
  

