setwd("C:/Users/sandra/Desktop/afchavez-019/JeanPiaget/Actividades")
Datos <- read.csv("Datos_JP.csv")
View(Datos)

Right <- c("d","c","b")

Datos$RC1 <- ifelse(Datos$RC1==Right[1], Datos$RC1<-1, Datos$RC1<-0)
Datos$RC2 <- ifelse(Datos$RC2==Right[2], Datos$RC2<-1, Datos$RC2<-0)
Datos$RC3 <- ifelse(Datos$RC3==Right[3], Datos$RC3<-1, Datos$RC3<-0)
View(Datos)