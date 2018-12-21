#############################################################
# Tarea 1 : TCT vs TRI 
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Adriana/Desktop/Adrifelcha_PsicometriaYEvaluacion/Curso_Ram&Irwin/Tareas/Tarea1_IRTvsTCT")
#############################################################
Datos <- read.csv("Tarea1_Datos.csv")    #Datos a trabajar
Datos$Id <- NULL        #Eliminamos las columnas innecesarias
Datos$Sexo <- NULL      
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
P

Dif_TCT <- matrix(data=c(Items,P),ncol=2)          #Ordenamos el valor P calculado por item en una matriz
colnames(Dif_TCT) <- c("Item", "Dificultad (P)")   #Asignamos un nombre a cada columna
Dif_TCT         #Imprimimos la matriz resultante

###################################################
##############  Ajusta el modelo de Rasch a los datos para obtener estimaciones de los parámetros de dificultad (Beta) de los ítems.
library(mirt)    #Cargamos la Librería para Multivariate Item Response Theory
Rasch <- mirt(Datos, 1, itemtype = 'Rasch')        #Solicitamos el análisis y lo guardamos en una variable
rep_coef<-data.frame(coef(Rasch, simplify=TRUE)$items)    #Recogemos los coeficientes computados (omitimos el resumen completo)
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
#       Se interpreta como la probabilidad de que el item sea contestado correctamente por la muestra (sin importar su habilidad)
#       Valores cercanos a 1 indican que el ítem es más fácil, y cercanos a 0 que es más difícil.

#(c) Para el modelo de Rasch, interpreta:
#    i. la dimension latente que subyace a las respuestas;
#    R: la aceptabilidad de "explotar" animales para diversos usos, o bien, el grado de oposición hacia el veganismo

#    ii. los parametros b de los items.
#    R: la cantidad o medida de la dimension latente que debe tener una persona para tener una probabilidad de .5 de responder afirmativamente al item.
#       Mientras mayor es el el valor de b, más se requiere del atributo/rasgo latente, es decir, más dificil es observar una respuesta "Sí" al ítem.

#(d) En terminos generales, ¿que relacion existe entre los indices p y los parámetros beta de ambos enfoques?
#    R: Ambos señalan que tan dificil es un ítem en función de la probabilidad de responderlo correctamente. Valores de p mas grandes se corresponden
#       con valores de b mas pequeños, y viceversa.



###################################################
###################################################
############## PARTE II 
############## Theta (IRT) vs X (TTC)
###################################################
############## Calcula para cada persona su puntuación (X) en el marco de la TCT (es decir, el núumero de veces que responde 
############## "Sí, es aceptable" en las 12 preguntas).

X <- NULL
Personas <- nrow(Datos)
for(a in 1:Personas){
  X[a] <- sum(Datos[a,Items])  
}
X
table(X)

a <- 0
barplot(table(X), main="Frecuencia de Puntajes Totales (X)", col="darkgreen", 
        xlab="Puntajes", ylab="Frecuencia", ylim=c(0,180))
for(i in 1:length(sort(unique(X)))){
  text(0.7+a, table(X)[i]+15, paste(table(X)[i]), cex=0.8, f=2)
  a <- a+1.2
}

###################################################
##############    Obten estimaciones para los parametros (theta) de las personas en el modelo de Rasch.

theta <- fscores(Rasch, method='WLE')                      #fscores nos permite calcular theta para cada persona, a partir de los datos que ya
#habíamos estimado previamente con la función mirt y guardado en el objeto "Rasch"
#WLE es el método, hay varios, (código 07 - linea 46)
theta  #Imprimimos el resultado

#Ahora, responde las siguientes preguntas:
#(a) Representa las variables x y theta graficamente a traves de un diagrama de puntos. Describe, con base en esta grafica, la relacion entre
#    las variables x y theta.
plot(X,theta, pch=16, main = "Comparación gráfica entre T (TCT) y Theta (TRI)",
     xlab="T - TCT", ylab = "Theta - TRI")                                             

#    R: La gráfica luce como una función logit o de logaritmo de momios, que es la funcion que convierte valores de probabilidad en valores que van
#    de -infinito a +infinito (la función del modelo de Rasch). También se aprecia que existe una relación positiva entre x y theta 
#    (si bien no es estrictamente lineal). Un valor alto en theta (el rasgo latente) corresponde con un puntaje alto en la prueba.
#    El que sea una función implica que para cada valor en x solo hay un valor en y, es decir, hay un solo valor en theta que corresponde a obtener
#    cierto puntaje en la prueba.

#(b) La relacion descrita en el ejercicio anterior es consecuencia directa de uno de los supuestos del modelo de Rasch.
#    Cual supuesto? Justifica tu respuesta.

#    R: El supuesto 4: Conocer el número de ítems que acierta una persona es suficiente para calcular su theta.

#(c) ¿Los datos dan evidencia de una diferencia global entre hombres y mujeres respecto de su actitud hacia el veganismo? Justifica tu respuesta.

datos <- read.csv("Tarea1_Datos.csv")    #Datos a trabajar
sexo <- matrix(data=c(datos$Sexo,theta),nrow=length(datos[,1]),ncol=2) #creamos una matriz con el sexo de las personas y su theta
sexo
dif_sexo <- t.test(sexo[,2]~sexo[,1])                                  #realizamos una prueba t para ver si hay diferencia en funcion del sexo

dif_sexo                                                               #hay diferencia estadísticamente significativa entre las medias

#    R: Hay una diferencia global estadisticamente significativa en la actitud hacia el veganismo de hombres y mujeres ya que el grupo 1 tiene una
#    theta promedio mayor.


###################################################
###################################################
############## PARTE III
############## Evaluación de precisión
###################################################
############## 3. Para cuantificar la precision de las estimaciones de las x y theta de la pregunta anterior:

###################################################
############## a) Calcula el indice Alfa de Cronbach como estimacion de la confiabilidad del test.

library(psych)             #librería que ya contiene una función para calcular alfa de cronbach
alfa <- alpha(Datos)    #usamos la función en los datos (la base con sólo items)
alfa                                                                   

###################################################
############## b) Dentro del marco del modelo de Rasch, deriva la funcion de informacion del test. (Explica, paso por paso, como se hace.)

theta_r <- matrix(seq(-6,6,.01))     #Definimos un rango de theta sobre el que queremos conocer la funcion de información 
info <- testinfo(Rasch, theta_r)     #testinfo es la función para obtener la funcion de informacion

# En el caso del modelo de un parámetro (Rasch) y con respuestas dicotómicas, la función de información de un item es la probabilidad de responder
# correctamente multiplicada por la probabilidad de una respuesta incorrecta. A su vez, gracias al supuesto de que hay independencia local,
# las funciones de información de los ítems son aditivas, es decir, para conocer la función de información del test solo hay que sumar las de 
# todos los items. En general la formula de la funcion de información es: sumatoria ( [(theta_p)(1-theta_p)] )

plot(theta_r, info, type = 'l', main = 'Información del test',   #graficamos la funcion de información
     xlab=expression(paste("Habilidad (Rango ",theta, ")")),                    
     ylab=expression(paste("Información")))

#(c) ¿Como se interpreta la funcion de informacion del test?

#    R: El error estandar de medida (la desviación estandar de la muestra) es el recíproco de la información del test; a mayor información, menor error de medición.
#    La función de información permite conocer la precisión de la medida de theta cuando esta tiene distintos valores, en lugar de un solo índice.
#    La función tiene forma de campana, porque hay mayor información en los valores intermedios de theta, y menor información en los extremos
#    (las medidas son más impresisas cuando los valores de habilidad son muy altos o muy bajos), a menos que se tengan muchos ítems en estos
#    rangos de dificultad.

#(d) Explica, tanto para el Alfa de Cronbach como para la funcion de informacion, de que manera informa sobre la precision de la estimacion
#    (X o theta) obtenida de una persona en particular.

#    R: Alfa de cronbach informa de una confiabilidad de medida a nivel global (de todo el examen), por lo que es lo mismo para todas las personas;
#    asume que el examen es igual de confiable para medir la habilidad de las personas, independientemente de si su habilidad es baja, media, o alta.
#    En cambio la función de información supone que, en función de la estructura del examen, este es más preciso para evaluar ciertos niveles de
#    habilidad; un examen construído con muchos ítems fáciles es más preciso para evaluar a personas de habilidad baja, y a su vez, un examen con
#    muchos ítems difíciles será más confiable para evaluar a personas de habilidad alta.

#(e) Ademas del enfoque en el que se definen (TCT versus TRI), ¿cual consideras la diferencia principal entre el
#    Alfa de Cronbach y la funcion de informacion?

#    R: La función de información permite conocer el nivel de precisión de las mediciones a diferentes valores de theta, mientras que alfa de
#    cronbach supone que la precisión es uniforme a lo largo de todo el rango de puntajes.

#    Ocurre algo similar a la discrepancia entre los índices de dificultad P y beta: Dado que el Alfa está computado para todo el examen, con base en el 
#    desempeño de TODOS los sujetos (sin importar qué tan bien les fue en el examen), el alfa de Cronbach suele interpretarse como un índice de "replicabilidad"
#    ("¿qué tan probable sería que si sigo recuperando nuevas observaciones, encuentre una varianza similar entre los items?"). Sin embargo, como se mencionó anteriormente
#    este razonamiento es "tramposo" en tanto que no se consideran los distintos niveles de habilidad que poseen las personas en cada muestra posible. En este sentido,
#    la función de información intenta ser un indicador de precisión (¿qué tan precisas son mis estimaciones conforme me muevo en la escala de niveles de habilidad?).

####################################################################################################################################################

#4. Un colega psicometra opina que el analisis de los datos anteriores con base en el modelo de Rasch seria mas adecuado si se asignara el valor
#   de 1 a la respuesta \No es aceptable" (y el valor 0 a la respuesta \Sí es aceptable"), ya que una respuesta negativa es indicativa de una actitud
#   positiva hacia el veganismo.
#   Con base en un razonamiento logico (es decir, sin efectivamente llevar a cabo el analisis), explica como intercambiar los 0s y 1s en los datos
#   modificaria las estimaciones de los parametros (b y theta) en comparacion con las estimaciones obtenidas para estos parametros en las preguntas
#   1 y 2.

#   R: Previamente indicamos que la dimensión latente era la aceptabilidad de "explotar" animales para diversos usos, o la afinidad hacia actitudes
#   opuestas al veganismo. Si se invierten los valores, el rasgo latente pasaría a ser el rechazo a explotar animales, o la afinidad hacia 
#   actitudes veganas.
#   En el caso original, valores altos de theta corresponden con personas que no tienen una actitud positiva hacia el veganismo (estan dispuestos
#   a explotar animales), mientras que valores bajos si corresponden con el veganismo. En la versión con los 1s y 0s invertidos, theta corresponde
#   con una actitud positiva hacia el veganismo, es decir, personas con mayores valores de theta rechazan la explotación animal.
#   Lo mismo ocurre con beta, en la primera versión valores altos corresponden con ítems en los que veganos dificilmente responderan "Aceptable",
#   mientras que al invertir los 1s y 0s, valores altos de b corresponden con ítems en los que los veganos más probablemente responderán "Aceptable".