#############################################################
# Codigo Prueba Paquete CDM
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("C:/Users/Alejandro/Desktop/afchavez19/RIMEDIE/Réplica Estimación 2015")

Datos <- read.table("m_dat_q0.txt",header = T)
Datos$X <- NULL
write.csv(Datos,"Datos_Nacionales_ELSEN_16500.csv")
datos <- read.csv("Datos_Nacionales_ELSEN_16500.csv")

#View(Respuestas)
Qmat <- read.table("q_matrices_mdatq_modificada.txt")                #Matriz Q:  ItemxHabilidad con valores 0 y 1 que señalan cuáles de las 35 habilidades se requiern por item
Qmat$X <- NULL
write.csv(Qmat,"MatrizQ_Dropbox.csv")
qmat <- read.csv("MatrizQ_Dropbox.csv")
