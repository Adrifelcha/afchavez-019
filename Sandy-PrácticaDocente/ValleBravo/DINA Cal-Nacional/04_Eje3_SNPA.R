rm(list=ls())                       #Limpiamos variables
library("CDM")
setwd("C:/Users/Alejandro/Desktop/afchavez19/Sandy-PrácticaDocente/ValleBravo/DINA Cal-Nacional")


#Base de referencia
datos <- read.csv("Datos_Nacionales_Sandy.csv")
#Identificamos el lugar que ocupa cada uno de nuestros sujetos de interés en la matriz Nacional
ValleBravo <- which(datos$ESCUELA=="Valle")

############# Estimaciones NACIONALES por habilidad  ################################
#####################################################################################
#skill_total <- read.csv("E3_SNPA_skilpatterns_Fel.csv")
skill_total <- read.csv("e3_skilpatt_spa_DINA-Guaner.csv")
skill_nacional <- skill_total$skill.prob

############ Estimaciones por SUJETO
#pattern_individual <- read.csv("E3_SNPA_postpattern_Fel.csv")
pattern_individual <- read.csv("e3_spapostpattern_DINA-Guaner.csv")
pattern_individual <- pattern_individual[,6:18]

############ Estimación paramétrica del modelo
#parameter_estimation <- read.csv("E3_SNPA_itemparameters_Fel.csv")
parameter_estimation <- read.csv("e3_itempars_spa_DINA-Guaner.csv")     #Just in case

#####################################################################################

skill_e3 <- NULL
for(attribute in 1:13){
  skill_e3[attribute] <- mean(pattern_individual[ValleBravo,attribute])
}


#col_sk3 <- ifelse(skill_e3_Lancaster>0.5, "palegreen3", "palegreen4")
col_sk3 <- ifelse(skill_e3<0.3, "brown4", ifelse(skill_e3<0.7,"gold3","darkseagreen3"))
q3 <- c("H301","H302","H303","H304","H305","H306","H307","H308","H309","H310","H311","H312", "H313")
valor_label <- 13.9
valor_dot <- 13.9
barplot(rev(skill_e3), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk3), ylim=c(1,15),
        panel.first = 
          c(lines(c(0.1,0.1),c(-0.07,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(-0.07,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(-0.07,0.97),lwd=3,lty=1, col="gold3"),lines(c(0.4,0.4),c(-0.07,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(-0.07,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(-0.07,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(-0.07,0.97),lwd=3,lty=1, col="darkseagreen3"),lines(c(0.8,0.8),c(-0.07,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(-0.07,.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(-0.07,0.97),lwd=1,lty=3, col="black")))        
mtext(side=1, text = "Dominio de la habilidad", line=3.5, cex=1.3, f=2)
mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
mtext(side=3, text = "Eje 3: Sentido Numérico y Pensamiento Algebraico", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=1.1, f=2)
text(0.15,15, "En construcción", f=2, cex=0.8, col="black")
text(0.45,15, "En desarrollo", f=2, cex=0.8, col="black")
text(0.85,15, "En consolidación", f=2, cex=0.8, col="black")
lines(c(0.05,0.09),c(15,15), col="brown4", lwd=4)
lines(c(.35,.39),c(15,15), col="gold3", lwd=4)
lines(c(.75,.79),c(15,15), col="darkseagreen3", lwd=4)
axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=1, f=2)
for(i in 1:length(seq(1,13,1))){
  text(0.04,valor_label, paste(q3[i]), f=2, cex=1.2)
  col_comp <- ifelse(skill_e3[i]>skill_nacional[i], "darkblue", "darkred")
  points(skill_nacional[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
  text(0.19,valor_dot, paste("Media nacional (2018): ", round(skill_nacional[i],3)), f=2, cex=0.8, col=col_comp)
  text(skill_e3[i]+.06,valor_label, paste(round(skill_e3[i],3)), f=2, cex=1.2)
  valor_dot <- valor_dot - 1.1
  valor_label <- valor_label - 1.1}



