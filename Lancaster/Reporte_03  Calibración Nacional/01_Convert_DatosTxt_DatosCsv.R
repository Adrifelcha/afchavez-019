##########################################################################################
#Código basico generado por Guaner Rojas para ELSEN 06 matematica
#Funciona como inicio para probar otros modelos y metodologías
##########################################################################################
rm(list=ls())
library(foreign)
setwd("C:/Users/Alejandro/Desktop/afchavez19/Lancaster/Reporte_03  Calibración Nacional")
##########################################################################################
#leer aqui directamente
m.dat.q<-read.table("m_dat_q0.txt",header = T)
write.csv(m.dat.q, "Base_Nacional.csv" )


q.matrix<-read.table("q_matrices_mdatq_modificada.txt",header = T)
write.csv(q.matrix, "Matriz_Q_Guaner.csv" )