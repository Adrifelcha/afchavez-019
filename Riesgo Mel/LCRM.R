rm(list=ls())
setwd("C:/Users/sandra/Desktop/afchavez-019/Riesgo Mel/Repositorio Datos Proyecto Riesgo")
Datos <- read.csv("BaseCompletaNumbers.csv")
nrow(Datos)

#### Datos
DGrales <- Datos[,c(1:14)]
#### Escalas
DescTemp <- Datos[,c(15:41)]
RefCog <- Datos[,c(42:44)]
Impulsivity <- Datos[,c(166:225)]
ImpFreq <- Datos[,c(166:185)]
ImpTempt <- Datos[,c(186:205)]
ImpDam <- Datos[,c(206:225)]
Max <- Datos[,c(226:240)]
Grit <- Datos[,c(397:404)]
#### Conductas de Riesgo
Tabaco <- Datos[,c(45:105)]
Agretsuko <- Datos[,c(106:165)]
Alcohol <- Datos[,c(241:317)]
Sex <- Datos[,c(318:396)]


library(poLCA)