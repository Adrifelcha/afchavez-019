#############
#############
setwd("C:/Users/Adriana/Desktop/Adrifelcha_PsicometriaYEvaluacion/Lancaster/Reporte_01")

Datos <- read.csv("Respuestas.csv")
Datos$NUMERO <- NULL
View(Datos)

Items <- c(1:50)
Sujetos <- nrow(Datos)

Clave <- c("B", "C")
