#############################################################
# Codigo Prueba Paquete CDM
#############################################################
rm(list=ls())                       #Limpiamos variables
library("CDM")
setwd("C:/Users/sandra/Desktop/afchavez-019/RIMEDIE")

Datos <- read.table("m_dat_q0.txt",header = T)
Datos$X <- NULL
write.csv(Datos,"Datos_Nacionales.csv")
datos <- read.csv("Datos_Nacionales.csv")

#View(Respuestas)
Qmat <- read.table("q_matrices_mdatq_modificada.txt")                #Matriz Q:  ItemxHabilidad con valores 0 y 1 que señalan cuáles de las 35 habilidades se requiern por item
Qmat$X <- NULL
write.csv(Qmat,"MatrizQ_DrGuaner.csv")
qmat <- read.csv("MatrizQ_DrGuaner.csv")
