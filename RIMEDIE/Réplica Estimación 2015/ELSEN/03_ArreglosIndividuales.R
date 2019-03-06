rm(list=ls())                       #Limpiamos variables
library("CDM")
setwd("C:/Users/Alejandro/Desktop/afchavez19/Lancaster/Reporte_03  Calibración Nacional")


#Base de referencia
datos <- read.csv("Datos_Nacionales_sin22_Sandy.csv")
#Identificamos el lugar que ocupa cada uno de nuestros sujetos de interés en la matriz Nacional
Lancaster <- which(datos$ESCUELA=="Lancaster")
Quinto <- which(datos$NOM_ENT=="Quinto")
Sexto <- which(datos$NOM_ENT=="Sexto")
Grupo_5A <- which(datos$ID_ENT=="5A")
Grupo_5B <- which(datos$ID_ENT=="5B")
Grupo_6A <- which(datos$ID_ENT=="6A")
Grupo_6B <- which(datos$ID_ENT=="6B")

#pattern_e2 <- read.csv("E2_MI_postpattern_Fel.csv")
pattern_e2 <- read.csv("e2_minpostpattern_DINA-Guaner.csv")
pattern_e2 <- pattern_e2[Lancaster,6:15]
############ Estimaciones por SUJETO
#pattern_individual <- read.csv("E1_FEM_postpattern_Fel.csv")
pattern_e1 <- read.csv("e1_fempostpattern_DINA-Guaner.csv")
pattern_e1$post.attr8 <- NULL   #Descartamos el atributo 8 porque al eliminar el item 22, dejó de ser evaluado por el test
pattern_e1 <- pattern_e1[Lancaster,6:16]
############ Estimaciones por SUJETO
#pattern_individual <- read.csv("E3_SNPA_postpattern_Fel.csv")
pattern_e3 <- read.csv("e3_spapostpattern_DINA-Guaner.csv")
pattern_e3 <- pattern_e3[Lancaster,6:18]

Atributos_Lanc <- as.data.frame(c(pattern_e1,pattern_e2,pattern_e3),ncol=34)
write.csv(Atributos_Lanc,"Diagnostico Individual.csv")
