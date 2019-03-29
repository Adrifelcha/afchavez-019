rm(list=ls())
setwd("C:/Users/Alejandro/Desktop/afchavez19/Riesgo Mel/Repositorio Datos Proyecto Riesgo")
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




################# Demographics
library(plotrix)

Sexo <- DGrales$DG1
sex <- table(Sexo)
sex <- as.vector(table(Sexo))
sex/nrow(Datos)
pie(sex, col=c("gray55","gray80"), labels=c("  F = 623 (48.5%)", "M = 655 (51%)    "), cex=1.8)


Edad <- DGrales$DG2
age <- table(Edad)
age <- as.vector(age)
age/nrow(Datos)
pie(age, col=c("darkseagreen1","darkseagreen3", "darkseagreen4", "gray30"), labels=c("<16 (38.94%)","17-18 (47.11%)","19-20 (11.37%)",">21 (2.33%)"), cex=1.8)


Escuela <- DGrales$DG4
school <- table(Escuela)
school <- as.vector(school)
school/nrow(Datos)
pie(school, labels=c("Public (82.55%)", "Private (16.66%)", "Not studying (0.31%)"), col=c("gray50","gray36","red"), cex=1.6)


### Looking for duplicates
Duplicates <- NULL
Testing <- NULL
for(a in 1:nrow(Datos[,2:ncol(Datos)])){
  for(b in 1:nrow(Datos[,2:ncol(Datos)])){
  Duplicates[b] <- as.vector(table(Datos[a,2:ncol(Datos)]==Datos[b,2:ncol(Datos)]))[1]==0
  }
  Testing <- as.vector(table(Duplicates))[1]==0
  ifelse(Testing==TRUE, print(a), o<-2+3)
  }