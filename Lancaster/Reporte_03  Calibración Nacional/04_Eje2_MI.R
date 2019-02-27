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

############# Estimaciones NACIONALES por habilidad  ################################
#####################################################################################
#skill_total <- read.csv("E2_MI_skilpatterns_Fel.csv")
skill_total <- read.csv("e2_skilpatt_min_DINA-Guaner.csv")
skill_nacional <- skill_total$skill.prob

############ Estimaciones por SUJETO
#pattern_individual <- read.csv("E2_MI_postpattern_Fel.csv")
pattern_individual <- read.csv("e2_minpostpattern_DINA-Guaner.csv")
pattern_individual <- pattern_individual[,6:15]

############ Estimación paramétrica del modelo
#parameter_estimation <- read.csv("E2_MI_itemparameters_Fel.csv")
parameter_estimation <- read.csv("e2_itempars_min_DINA-Guaner.csv")     #Just in case

#####################################################################################

skill_e2_Lancaster <- NULL
for(attribute in 1:10){
  skill_e2_Lancaster[attribute] <- mean(pattern_individual[Lancaster,attribute])
}


col_sk2 <- ifelse(skill_e2_Lancaster>0.5, "lightsalmon1", "lightsalmon4")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210")
valor_label <- 10.6
valor_dot <- 10.6
barplot(rev(skill_e2_Lancaster), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk2),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,10,1))){
  text(0.04,valor_label, paste(q2[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e2_Lancaster[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.19,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e2_Lancaster[i]+.06,valor_label, paste(round(skill_e2_Lancaster[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}







skill_e2_Sexto <- NULL
for(attribute in 1:10){
  skill_e2_Sexto[attribute] <- mean(pattern_individual[Sexto,attribute])
}

col_sk2 <- ifelse(skill_e2_Sexto>0.5, "lightsalmon1", "lightsalmon4")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210")
valor_label <- 10.6
valor_dot <- 10.6
barplot(rev(skill_e2_Sexto), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk2),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Sexto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,10,1))){
  text(0.04,valor_label, paste(q2[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e2_Sexto[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.19,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e2_Sexto[i]+.06,valor_label, paste(round(skill_e2_Sexto[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}





skill_e2_Quinto <- NULL
for(attribute in 1:10){
  skill_e2_Quinto[attribute] <- mean(pattern_individual[Quinto,attribute])
}

col_sk2 <- ifelse(skill_e2_Quinto>0.5, "lightsalmon1", "lightsalmon4")
q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210")
valor_label <- 10.6
valor_dot <- 10.6
barplot(rev(skill_e2_Quinto), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk2),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.4,0.4),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,1),lwd=2,lty=1, col="black"),lines(c(0.6,0.6),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,1),lwd=1,lty=3, col="black"),lines(c(0.8,0.8),c(0,1),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,1),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,1),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto año)", line=0.5, cex=1.1, f=2)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,10,1))){
  text(0.04,valor_label, paste(q2[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e2_Quinto[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.19,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e2_Quinto[i]+.06,valor_label, paste(round(skill_e2_Quinto[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}







skill_e2_g5A <- NULL
for(attribute in 1:10){
  skill_e2_g5A[attribute] <- mean(pattern_individual[Grupo_5A,attribute])
}

skill_e2_g5B <- NULL
for(attribute in 1:10){
  skill_e2_g5B[attribute] <- mean(pattern_individual[Grupo_5B,attribute])
}

skill_e2_g6A <- NULL
for(attribute in 1:10){
  skill_e2_g6A[attribute] <- mean(pattern_individual[Grupo_6A,attribute])
}

skill_e2_g6B <- NULL
for(attribute in 1:10){
  skill_e2_g6B[attribute] <- mean(pattern_individual[Grupo_6B,attribute])
}