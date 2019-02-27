#############################################################
# Codigo Prueba Paquete CDM
#############################################################
rm(list=ls())                       #Limpiamos variables
library("CDM")
setwd("C:/Users/Alejandro/Desktop/afchavez19/Lancaster/Reporte_03  Calibración Nacional")

datos <- read.csv("Datos_Nacionales_sin22_Sandy.csv")
R_E1 <- datos[,c(c(1:8),c(26:33))]
R_E2 <- datos[,c(c(9:14),c(34:38))]
R_E3 <- datos[,c(c(15:25),c(39:49))]


qmat <- read.csv("MatrizQ_DrGuaner_sin22.csv")
Q_E1 <- qmat[1:16,1:12] 
Q_E2 <- qmat[17:27,1:10]
Q_E3 <- qmat[28:49,1:13]

