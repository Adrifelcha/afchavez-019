rm(list=ls())
setwd("C:/Users/Alejandro/Desktop/afchavez19/Riesgo Mel/Repositorio Datos Proyecto Riesgo/Originales (letras) por sección/CSV")

Datos_grales <- read.csv("DGyDT-Sección I.csv")
NoA <- which(is.na(Datos_grales[,1])==TRUE)
Datos_grales <- Datos_grales[-(NoA),]

Sexo <- Datos_grales$DG1
Edad <- Datos_grales$DG2
