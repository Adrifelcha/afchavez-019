#############################################################
# Tarea 1 : TCT vs TRI 
#############################################################
rm(list=ls())                       #Limpiamos variables
setwd("D:/afchavez/Desktop/Adrifelcha_PsicometriaYEvaluacion/Curso_Ram&Irwin/Tareas/Tarea1_IRTvsTCT")
#############################################################
Datos <- read.csv("Tarea1_Datos.csv")    #Datos a trabajar
###################################################
###################################################
############## PARTE I 
############## Beta (IRT) vs Dificultad p (TTC)
###################################################
P <- NULL
Items <- c(1:12)

for(a in 1:length(Items)){
P[a] <- mean(Datos[,2+a])  
}

Datos$Id <- NULL
Datos$Sexo <- NULL
Datos$X <- NULL
write.csv(Datos, "JustData.csv", row.names = FALSE)
OnlyData <- read.csv("JustData.csv")

library(mirt)
Rasch <- mirt(OnlyData, 1, itemtype = 'Rasch')
rep_coef<-data.frame(coef(Rasch, simplify=TRUE)$items)
b<- -1*(rep_coef$d/rep_coef$a1)
rep<-cbind(rep_coef,b)
rep <- subset(rep, select=5); round(rep,3)


 Comparacion <- matrix(data=c(P,round(b, 3)),ncol=2)     
 colnames(Comparacion) <- c("p", "Beta")
 Comparacion



###################################################
###################################################
############## PARTE II 
############## Theta (IRT) vs X (TTC)
###################################################
X <- NULL
Theta <- NULL
Personas <- nrow(Datos)
for(a in 1:Personas){
  X[a] <- sum(Datos[a,2+Items])  
}
a <- 0
table(X)
barplot(table(X), main="Frecuencia de Puntajes Totales (X)", col="darkgreen", 
        xlab="Puntajes", ylab="Frecuencia", ylim=c(0,180))
for(i in 1:length(sort(unique(X)))){
  text(0.7+a, table(X)[i]+15, paste(table(X)[i]), cex=0.8, f=2)
  a <- a+1.2
}
  

