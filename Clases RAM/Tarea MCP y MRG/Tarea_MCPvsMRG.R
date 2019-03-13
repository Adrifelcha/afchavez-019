##################################################
##################################################
# Clase de Modelos Politómicos en Psicometría
# impartida por el Dr. Ramsés V.L. y el Dr. Iwen 
##################################################
# TAREA 1:
# Contraste entre Modelo de Crédito Parcial y 
#                   el Modelo de Respuesta Graduada
###################################################
# Código por:
# Adriana Felisa Chávez De la Peña
#####################################################################################################################
#####################################################################################################################
# Instrucciones Generales:
# El archivo Tarea1 Datos.csv contiene datos de 1,500 sustentantes a un examen de matemáticas que consiste en 10 pre-
#guntas, donde cada pregunta vale hasta un máximo de 3 puntos. Los datos son las puntuaciones de cada sustentante en
#cada pregunta.

setwd("C:/Users/sandra/Desktop/afchavez-019/Clases RAM/Tarea MCP y MRG")   # Directorio de trabajo
library("mirt")

Datos <- read.csv("Tarea1_Datos.csv")     # CSV con los datos
View(Datos)

Respuestas <- Datos[,c(2:ncol(Datos))]    # Seleccionamos las columas que contienen las respuestas
Respuestas <- na.omit(Respuestas)
View(Respuestas)


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# 1: 1. Obtén estimaciones de los parámetros de los ítems para:
# -> El modelo de crédito parcial generalizado (Muraki, 1992) ajustado a estos datos;
MCP <- mirt(data=Respuestas, model="Respuestas = 1-10", itemtype="Rasch", SE=TRUE, verbose=FALSE)
MCP_coef <- coef(MCP, IRTpars=TRUE, simplify=TRUE)
MCP_item_par <- as.data.frame(MCP_coef$items)

plot(MCP, type = 'trace', which.items = c(1:10), 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))


# -> El modelo de respuesta graduada (Samejima, 1969) ajustado a estos mismos datos.
MRG <- mirt(data = Respuestas, model = "Respuestas = 1-10", itemtype = "graded", SE = TRUE)
MRG_params <- coef(MRG, IRTpars = TRUE, simplify = TRUE)
MRG_items <- as.data.frame(MRG_params$items)
MRG_items

plot(MRG, type = "trace", which.items = 1, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))
      




plot(MCP, type = 'trace', which.items = c(1), 
     main = "Hola")
#par(new=TRUE)
#par(fig = c(grconvertX(c(1, 3), from="user", to="ndc"),
#            grconvertY(c(50, 125), from="user", to="ndc")),
#    mar = c(4,6,1,1),
#    new = TRUE)
par(fig= c(plot(MRG, add=TRUE, type = "trace", which.items = 1)),
          mar = c(4,5,1,1),
          new=TRUE)



matplot(cbind(MRG,MCP), type = "trace", which.items = 1)




plot(MRG, type = "trace", which.items = 1, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))
plot(MCP, type = 'trace', which.items = 1, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))


#Utiliza para ambos casos el método de máxima verosimilitud marginal con el supuesto de que los parámetros de las
#personas se han extraído de una distribución normal con media 0 y varianza 1.


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
# 2: Interpreta los valores estimados de los parámetros asociados con las categorías/niveles de respuesta 
# (es decir, las Beta_ij) para:
# -> El modelo de crédito parcial generalizado;
# -> El modelo de respuesta graduada.
#####################################################################################################################
#####################################################################################################################





#####################################################################################################################
#####################################################################################################################
# Construye una gráfica para cada uno de los 10 ítems que, en la misma figura muestra las curvas características
# de las categorías del modelo de crédito parcial generalizado (con líneas solidas) y del modelo de respuesta graduada
# (con líneas punteadas), utilizando colores diferentes para las curvas características de las distintas categorías.
# Con base en estas figuras:
# ->Compara las curvas características de categoría en los dos modelos. ¿Qué tan diferentes/similares son? ¿En
# qué zonas para se encuentran más diferencias entre las curvas?
# -> ¿A qué conclusión general llegas?
#####################################################################################################################
#####################################################################################################################




#####################################################################################################################
#####################################################################################################################
# 4: Con base en las estimaciones para los parámetros de los ítems obtenidas en el primer punto, ¿qué opinas sobre 
# la habilidad matemática de los sustentantes? >Fue un examen relativamente fácil o difícil? Justifica tu respuesta.
#####################################################################################################################
#####################################################################################################################





#####################################################################################################################
#####################################################################################################################
# 5: Los datos, en realidad, son datos simulados. Es decir, no son datos observados de sustentantes reales, sino fueron
# construidos utilizando un algoritmo que opera bajo cierto modelo. Les comentamos que los datos fueron construidos
# bajo los supuestos de uno de los dos modelos ajustados en el primer ejercicio. ¿Cuál crees que fue el modelo que se
# utilizó para generar los datos, crédito parcial generalizado o respuesta graduada?
# Pista: Evalúa la bondad de ajuste de cada modelo. En general, si se utiliza un Modelo A para construir ciertos
# datos y posteriormente se ajusta este Modelo A a estos datos, entonces el Modelo A suele tener mejor ajuste que
# otro Modelo B.
#####################################################################################################################
#####################################################################################################################


