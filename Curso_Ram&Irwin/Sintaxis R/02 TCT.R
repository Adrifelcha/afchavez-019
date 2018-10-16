# Ejercicio para la teoría clásica de los tests

### Para este ejercicio, se simularán datos bajo los supuestos del 
### modelo de la teoría clásica de los tests para una población de personas.
### No es necesario que entiendas los detalles o la lógica de la simulación.  
### Solo piensa en una simulación como una creación de datos como si fueramos los dioses del universo.
###
### El modelo de la teoría clásico supone que la población tiene un número infinito de personas.
### Para la simulación, crearemos datos para un gran número de personas (100,000).  
### No es infinito, pero para fines de ilustración es como si fuera infinito.
###
### La "variable de interés" que vamos a simular es el grado de miopía en una población de personas 
### que necesitan lentes para corregir esta patología.
###
### Ahora, ejecuta las siguientes líneas.
### Se crean los datos simulados.
### Lo importante es que este procedimiento nos da un dataframe con 100,000 observaciones (personas) y para cada persona:
###   (a) su puntuación observada X: el grado de miopía que resulta de una medición realizada por un oftalmólogo;
###   (b) su puntuación verdadera T: su grado de miopía real;
###   (c) su puntuación error E: el error de medición (la diferencia entre el resultado obtenido en la medición y la miopía real).
###
### Recuerda que, en la práctica, solo se conoce X. Pero en la simulación, también tenemos acceso a T y E.


simular <- function(valor_esperado_t, varianza_t, varianza_e, n_personas){
  beta <- -valor_esperado_t/varianza_t
  alpha <- -valor_esperado_t*beta
  t <- round(-0.50 - rgamma(n = n_personas, shape = alpha, rate = beta), digits = 4)
  e <- rnorm(n = n_personas, mean = 0, sd = sqrt(varianza_e))
  x <- round(4*(t + e), digits = 0) / 4
  x[x > -0.25] <- -0.25
  e = x - t;
  as.data.frame(cbind(x, t, e))
  }
set.seed(70368)
datos.TCT <- simular(valor_esperado_t = -1, varianza_t = 0.8, varianza_e = 0.04, n_personas = 100000)

### Ejercicio
### Con el dataframe creado (datos.TCT) por favor reflexionen acerca de los siguientes tres puntos
### (1) Crear un histograma para cada variable apropiado que permita graficar la distribución de las tres variables (X, T y E);
### (2) verificar los supuestos del modelo de la TCT;
### (3) realizar una estimación lineal de T con base en X y derivar un intervalo de confianza.

library(plyr)
tab.X <-(count(datos.TCT$x)); tab.X

library(ggplot2)
# histograma puntuación observada (discreta)
Obs <- ggplot(tab.X, aes(x = x, y = freq)) +  
  labs(title="Histograma de frecuencias puntuación observada", x ="Dioptrías", y = "Frecuencia")
Obs + geom_bar(stat = "identity", colour="blue", width = 0.2) +
  scale_x_continuous(breaks = unique(tab.X$x)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# histograma puntuación verdadera (contínua)
ggplot(data=datos.TCT, aes(datos.TCT$t)) + geom_histogram(bins = 41, colour="blue") +
labs(title="Histograma de frecuencias puntuación verdadera", x ="Dioptrías", y = "Frecuencia")


# histograma puntuación verdadera (¿discreta o contínua?)
ggplot(data=datos.TCT, aes(datos.TCT$e)) + geom_histogram(bins = 41, colour="blue") +
labs(title="Histograma de frecuencias puntuación error", x ="Dioptrías", y = "Frecuencia")

# Descriptivos de las variables
#options(scipen=999)
#options(digits=3)

mean(datos.TCT$x) # media de la puntuación observada
mean(datos.TCT$t) # media de la puntuación verdadera
mean(datos.TCT$e) # media de la puntuación error

var.x<-var(datos.TCT$x) # desviación estándar de la puntuación observada
var.t<-var(datos.TCT$t) # desviación estándar de la puntuación verdadera 
var.e<-var(datos.TCT$e) # desviación estándar de la puntuación error

plot(datos.TCT$x, datos.TCT$t, main="Diagrama de dispersión", xlab="Puntuación observada (X)", 
     ylab="Puntuación verdadera (T)"); 
cor(datos.TCT$x, datos.TCT$t) # plot y correlación entre puntuación observada y puntuación verdadera
abline(-0.0795, 0.9469, col = "red", lwd = 2)
mtext(bquote( T == .(0.9469) * X - .(0.0795)), side=1, line=4) 
plot(datos.TCT$x, datos.TCT$e, main="Diagrama de dispersión", xlab="Puntuación observada (X)", 
     ylab="Puntuación error (E)"); cor(datos.TCT$x, datos.TCT$e) # plot y correlación entre puntuación observada y puntuación error

plot(datos.TCT$t, datos.TCT$e, main="Diagrama de dispersión", xlab="Puntuación verdadera (T)", 
     ylab="Puntuación error (E)"); cov(datos.TCT$t, datos.TCT$e) # plot y covarianza entre puntuación verdadera y puntuación error

# Regresión lineal 
lm(formula = datos.TCT$t ~ datos.TCT$x, data = datos.TCT)

# Verificación de los coeficientes de regresión con las fórmulas que vimos en clase

a <- (var(datos.TCT$t)/var(datos.TCT$x)); a
b <- (1-a)*mean(datos.TCT$x); b


# Ejercicio 1
# Si deseamos conocer la estimación puntual de una persona que tiene una puntuación observada igual a -3.5 dioptrías.
# ¿Cuál sería su puntuación verdadera, dado que ya se conoce los coeficientes de regresión (a, b)?

estima.diopt.ver <- a*(-3.5) + b; estima.diopt.ver
# [1] -3.39352

# Ejercicio 2
# Si ahora se desea dar una estimación del intervalo de confianza entorno a la puntuación verdadera.

sd<-sd(datos.TCT$e)
# Si p=0.95
IC.inf.95 <- estima.diopt.ver - 1.96 * sd; IC.inf.95
IC.sup.95 <- estima.diopt.ver + 1.96 * sd; IC.sup.95
Int.Conf.95 <- c(IC.inf.95, IC.sup.95); Int.Conf.95

# Si p=0.90
IC.inf.90 <- estima.diopt.ver - 1.645 * sd; IC.inf.90
IC.sup.90 <- estima.diopt.ver + 1.645 * sd; IC.sup.90
Int.Conf.90 <- c(IC.inf.90, IC.sup.90); Int.Conf.90

# Si p=0.99
IC.inf.99 <- estima.diopt.ver - 2.576 * sd; IC.inf.99
IC.sup.99 <- estima.diopt.ver + 2.576 * sd; IC.sup.99
Int.Conf.99 <- c(IC.inf.99, IC.sup.99); Int.Conf.99

Estima.ICs <- as.data.frame(rbind(Int.Conf.90, Int.Conf.95, Int.Conf.99))
names(Estima.ICs)[1]<-paste("Límite inferior")
names(Estima.ICs)[2]<-paste("Límite superior")
Estima.ICs

# Fin