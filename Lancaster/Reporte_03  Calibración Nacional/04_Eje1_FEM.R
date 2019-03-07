rm(list=ls())                       #Limpiamos variables
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
#skill_total <- read.csv("E1_FEM_skilpatterns_Fel.csv")
skill_total <- read.csv("e1_skilpatt_fem_DINA-Guaner.csv")
skill_nacional <- skill_total$skill.prob
skill_nacional <- skill_nacional[-8]     #Omitimos la información sobre el atributo 8, que ya no es evaluado al eliminar el item 22

############ Estimaciones por SUJETO
#pattern_individual <- read.csv("E1_FEM_postpattern_Fel.csv")
pattern_individual <- read.csv("e1_fempostpattern_DINA-Guaner.csv")
pattern_individual$post.attr8 <- NULL   #Descartamos el atributo 8 porque al eliminar el item 22, dejó de ser evaluado por el test
pattern_individual <- pattern_individual[,6:16]

############ Estimación paramétrica del modelo
#parameter_estimation <- read.csv("E1_FEM_itemparameters_Fel.csv")
parameter_estimation <- read.csv("e1_itempars_fem_DINA-Guaner.csv")     #Just in case

#####################################################################################

skill_e1_Lancaster <- NULL
for(attribute in 1:11){
  skill_e1_Lancaster[attribute] <- mean(pattern_individual[Lancaster,attribute])
}


#col_sk1 <- ifelse(skill_e1_Lancaster>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk1 <- ifelse(skill_e1_Lancaster<0.3, "brown4", ifelse(skill_e1_Lancaster<0.7,"gold3","darkseagreen3"))
q1 <- c("H101","H102","H103","H104","H105","H106","H107","H109","H110","H111","H112")
valor_label <- 11.7
valor_dot <- 11.7
barplot(rev(skill_e1_Lancaster), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1), ylim=c(0,13),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="gold4"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="darkseagreen4"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
text(0.15,12.55, "En construcción", f=2, cex=0.8, col="black")
text(0.45,12.55, "En desarrollo", f=2, cex=0.8, col="black")
text(0.85,12.55, "En consolidación", f=2, cex=0.8, col="black")
lines(c(0.05,0.09),c(12.5,12.5), col="brown4", lwd=4)
lines(c(.35,.39),c(12.5,12.5), col="gold3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,11,1))){
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e1_Lancaster[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.14,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e1_Lancaster[i]+.05,valor_label, paste(round(skill_e1_Lancaster[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}







skill_e1_Sexto <- NULL
for(attribute in 1:11){
  skill_e1_Sexto[attribute] <- mean(pattern_individual[Sexto,attribute])
}

#col_sk1 <- ifelse(skill_e1_Sexto>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk1 <- ifelse(skill_e1_Sexto<0.3, "brown4", ifelse(skill_e1_Sexto<0.7,"gold3","darkseagreen3"))
q1 <- c("H101","H102","H103","H104","H105","H106","H107","H109","H110","H111","H112")
valor_label <- 11.7
valor_dot <- 11.7
barplot(rev(skill_e1_Sexto), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1), ylim = c(0,13),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="gold3"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="darkseagreen3"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Sexto año)", line=0.5, cex=1.1, f=2)
text(0.15,12.55, "En construcción", f=2, cex=0.8, col="black")
text(0.45,12.55, "En desarrollo", f=2, cex=0.8, col="black")
text(0.85,12.55, "En consolidación", f=2, cex=0.8, col="black")
lines(c(0.05,0.09),c(12.5,12.5), col="brown4", lwd=4)
lines(c(.35,.39),c(12.5,12.5), col="gold3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,11,1))){
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e1_Sexto[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.14,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e1_Sexto[i]+.05,valor_label, paste(round(skill_e1_Sexto[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}





skill_e1_Quinto <- NULL
for(attribute in 1:11){
  skill_e1_Quinto[attribute] <- mean(pattern_individual[Quinto,attribute])
}

#col_sk1 <- ifelse(skill_e1_Quinto>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk1 <- ifelse(skill_e1_Quinto<0.3, "brown4", ifelse(skill_e1_Quinto<0.7,"gold3","darkseagreen3"))
q1 <- c("H101","H102","H103","H104","H105","H106","H107","H109","H110","H111","H112")
valor_label <- 11.7
valor_dot <- 11.7
barplot(rev(skill_e1_Quinto), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1), ylim=c(0,13),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="black"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="black"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto año)", line=0.5, cex=1.1, f=2)
text(0.15,12.55, "En construcción", f=2, cex=0.8, col="black")
text(0.45,12.55, "En desarrollo", f=2, cex=0.8, col="black")
text(0.85,12.55, "En consolidación", f=2, cex=0.8, col="black")
lines(c(0.05,0.09),c(12.5,12.5), col="brown4", lwd=4)
lines(c(.35,.39),c(12.5,12.5), col="gold3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,11,1))){
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e1_Quinto[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.14,valor_dot, paste("Media nacional: ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e1_Quinto[i]+.05,valor_label, paste(round(skill_e1_Quinto[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}








skill_e1_Quinto <- NULL
for(attribute in 1:11){
  skill_e1_Quinto[attribute] <- mean(pattern_individual[Quinto,attribute])
}

#col_sk1 <- ifelse(skill_e1_Quinto>0.5, "lightsteelblue1", "lightsteelblue3")
col_sk1 <- ifelse(skill_e1_Quinto<0.3, "brown4", ifelse(skill_e1_Quinto<0.7,"lightgoldenrod","palegreen3"))
q1 <- c("H101","H102","H103","H104","H105","H106","H107","H109","H110","H111","H112")
valor_label <- 11.7
valor_dot <- 11.7
barplot(rev(skill_e1_Quinto), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1), ylim=c(0,13),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="black"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="black"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 1: Espacio, Forma y Medida", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Estudiantes de Quinto en contraste con Sexto)", line=0.5, cex=1.1, f=2)
text(0.15,12.55, "En construcción", f=2, cex=0.8, col="black")
text(0.45,12.55, "En desarrollo", f=2, cex=0.8, col="black")
text(0.85,12.55, "En consolidación", f=2, cex=0.8, col="black")
lines(c(0.05,0.09),c(12.5,12.5), col="brown4", lwd=4)
lines(c(.35,.39),c(12.5,12.5), col="gold3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
for(i in 1:length(seq(1,11,1))){
  text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e1_Quinto[i]>skill_e1_Sexto[i], "darkgreen", "red")
  points(skill_e1_Sexto[i],valor_dot, pch=15, cex=2, lwd=4, col=col_comp)
  text(0.14,valor_dot, paste("Sexto año: ", round(skill_e1_Sexto[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e1_Quinto[i]+.05,valor_label, paste(round(skill_e1_Quinto[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}





skill_e1_g5A <- NULL
for(attribute in 1:11){
  skill_e1_g5A[attribute] <- mean(pattern_individual[Grupo_5A,attribute])
}

skill_e1_g5B <- NULL
for(attribute in 1:11){
  skill_e1_g5B[attribute] <- mean(pattern_individual[Grupo_5B,attribute])
}

skill_e1_g6A <- NULL
for(attribute in 1:11){
  skill_e1_g6A[attribute] <- mean(pattern_individual[Grupo_6A,attribute])
}

skill_e1_g6B <- NULL
for(attribute in 1:11){
  skill_e1_g6B[attribute] <- mean(pattern_individual[Grupo_6B,attribute])
}