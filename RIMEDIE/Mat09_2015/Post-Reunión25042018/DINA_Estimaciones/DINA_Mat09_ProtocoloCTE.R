rm(list=ls())
setwd("C:/Users/Sandra/Desktop/afchavez-019/RIMEDIE/Mat09_2015/Post-Reunión25042018/DINA_Estimaciones")

##########################################################
# Cargamos la base con las estimaciones por CTE
CTE <- read.csv("CuadroEstimac_CTT_MAT09_ajustes.csv")
##### Seleccionamos un Estado
Estado <- "CHIHUAHUA"
#Nota: Según el Diagnóstico Nacional para PLANEA09 2015,
#Oaxaca, Guerrero y Tabasco fueron los más bajos y
#Distrito Federal, Puebla y Querétaro, los más altos
###########################################################
#Identificamos los promedios nacionales para referencias futuras
Nacional_E1 <- as.numeric(CTE[nrow(CTE),c(4:15)])
Nacional_E2 <- as.numeric(CTE[nrow(CTE),c(16:25)])
Nacional_E3 <- as.numeric(CTE[nrow(CTE),c(26:33)])

#Limpiamos la basey nos quedamos únicamente con los CTE que
#correspondan a nuestro estado de interés
si <- NULL
count <- 1
for(i in 1:nrow(CTE)){
  if(CTE$Entidad[i]==Estado){
    si[count] <- i
    count <- count+1
  }else{
    0+0
  }
}
CTE <- CTE[si,]

#Ubicamos la escuela con el mayor número de observaciones
MaxObservaciones <- which(CTE$Observaciones==max(CTE$Observaciones))

#En caso de que más de una escuela tenga el máximo número de 
# observaciones, revisamos su desempeño promedio para elegir una
for(i in 1:length(MaxObservaciones)){
  Promedio <- mean(as.numeric(CTE[MaxObservaciones[i],4:33]))
  print(MaxObservaciones[i])
  print(Promedio)
}

#Y nos quedamos con el elegido!
Chosen <- CTE[MaxObservaciones[2],]


###########################################################
# Ploteamos el desempeño de este CTE

pdf("DiagnósticoCognitivo_CTE.pdf", width=10, height=7)
plot(c(1,2),c(1,2), col="white", ann=F, axes=F, xlim=c(0,5), ylim=c(0,5))
text(2.5,3, paste("Diagnóstico cognitivo de habilidades matemáticas para un CTE"), f=2, cex=1.5, col="black")
text(2.5,2.3, paste("en el estado de Chihuahua"), f=2, cex=1.5, col="black")
text(2.5,0.8, paste("(con", max(CTE$Observaciones), "observaciones)"), f=2, cex=1.5, col="black")

Skill_E1 <- as.numeric(Chosen[,c(4:15)])
Skill_E2 <- as.numeric(Chosen[,c(16:25)])
Skill_E3 <- as.numeric(Chosen[,c(26:33)])
  
col_sk1 <- ifelse(Skill_E1<0.5, "brown4", ifelse(Skill_E1<0.75,"gold3","darkseagreen3"))
q1 <- c("H101","H102","H103","H104","H105","H106","H107","H108","H109","H110","H111","H112")
valor_label <- 12.8
valor_dot <- 12.8
barplot(rev(Skill_E1), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk1), ylim=c(0,14),
        panel.first = 
          c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="gold4"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="darkseagreen4"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
            lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
  mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
  mtext(side=3, text = "Eje 1: Sentido Numérico y Pensamiento Algebraico", line=1.5, cex=2, f=2)
  mtext(side=3, paste("Resultados del CTE con ID", CTE$ID.CTT[MaxObservaciones[2]]), line=0.5, cex=1.1, f=2)
  text(0.15,14, "En construcción", f=2, cex=0.8, col="black")
  text(0.45,14, "En desarrollo", f=2, cex=0.8, col="black")
  text(0.85,14, "En consolidación", f=2, cex=0.8, col="black")
  lines(c(0.05,0.08),c(14,14), col="brown4", lwd=4)
  lines(c(.35,.38),c(14,14), col="gold3", lwd=4)
  lines(c(.75,.78),c(14,14), col="darkseagreen3", lwd=4)
  axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
  for(i in 1:12){
    text(0.03,valor_label, paste(q1[i]), f=2, cex=1.2)
    col_comp <- ifelse(Skill_E1[i]>Nacional_E1[i], "darkblue", "darkred")
    points(Nacional_E1[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
    text(0.14,valor_dot, paste("Media nacional: ", round(Nacional_E1[i],3)), f=2, cex=0.8, col=col_comp)
    text(Skill_E1[i]+.05,valor_label, paste(round(Skill_E1[i],3)), f=2, cex=1.2)
    valor_dot <- valor_dot - 1.1
    valor_label <- valor_label - 1.1}
  
  
  col_sk2 <- ifelse(Skill_E2<0.3, "brown4", ifelse(Skill_E2<0.7,"gold3","darkseagreen3"))
  q2 <- c("H201","H202","H203","H204","H205","H206","H207","H208","H209","H210")
  valor_label <- 10.6
  valor_dot <- 10.6
  barplot(rev(Skill_E2), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk2), ylim=c(0,12),
          panel.first = 
            c(lines(c(0.1,0.1),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(0,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.3,0.3),c(0,0.97),lwd=3,lty=1, col="gold3"),lines(c(0.4,0.4),c(0,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.5,0.5),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(0,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.7,0.7),c(0,0.97),lwd=3,lty=1, col="darkseagreen3"),lines(c(0.8,0.8),c(0,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.9,0.9),c(0,0.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(0,0.97),lwd=1,lty=3, col="black")))        
  mtext(side=1, text = "Dominio de la habilidad", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
  mtext(side=3, text = "Eje 2: Manejo de la Información", line=1.5, cex=2, f=2)
  mtext(side=3, paste("Resultados del CTE con ID", CTE$ID.CTT[MaxObservaciones[2]]), line=0.5, cex=1.1, f=2)
  text(0.15,11.55, "En construcción", f=2, cex=0.8, col="black")
  text(0.45,11.55, "En desarrollo", f=2, cex=0.8, col="black")
  text(0.85,11.55, "En consolidación", f=2, cex=0.8, col="black")
  lines(c(0.05,0.09),c(11.5,11.5), col="brown4", lwd=4)
  lines(c(.35,.39),c(11.5,11.5), col="gold3", lwd=4)
  lines(c(.75,.79),c(11.5,11.5), col="darkseagreen3", lwd=4)
  axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=-0.5, f=2)
  for(i in 1:10){
    text(0.04,valor_label, paste(q2[i]), f=2, cex=1.2)
    col_comp <- ifelse(Skill_E2[i]>Nacional_E2[i], "darkblue", "darkred")
    points(Nacional_E2[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
    text(0.19,valor_dot, paste("Media nacional: ", round(Nacional_E2[i],3)), f=2, cex=0.8, col=col_comp)
    text(Skill_E2[i]+.06,valor_label, paste(round(Skill_E2[i],3)), f=2, cex=1.2)
    valor_dot <- valor_dot - 1.1
    valor_label <- valor_label - 1.1}

  col_sk3 <- ifelse(Skill_E3<0.3, "brown4", ifelse(Skill_E3<0.7,"gold3","darkseagreen3"))
  q3 <- c("H301","H302","H303","H304","H305","H306","H307","H308")
  valor_label <- 8.4
  valor_dot <- 8.4
  barplot(rev(Skill_E3), horiz=TRUE, ann=FALSE, axes=FALSE, xlim=c(0,1), space=0.1, col=rev(col_sk3), ylim=c(1,9.5),
          panel.first = 
            c(lines(c(0.1,0.1),c(-0.07,0.97),lwd=1,lty=3, col="black"),lines(c(0.2,0.2),c(-0.07,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.3,0.3),c(-0.07,0.97),lwd=3,lty=1, col="gold3"),lines(c(0.4,0.4),c(-0.07,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.5,0.5),c(-0.07,0.97),lwd=1,lty=3, col="black"),lines(c(0.6,0.6),c(-0.07,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.7,0.7),c(-0.07,0.97),lwd=3,lty=1, col="darkseagreen3"),lines(c(0.8,0.8),c(-0.07,0.97),lwd=1,lty=3, col="black"),
              lines(c(0.9,0.9),c(-0.07,.97),lwd=1,lty=3, col="black"),lines(c(1,1),c(-0.07,0.97),lwd=1,lty=3, col="black")))        
  mtext(side=1, text = "Dominio de la habilidad", line=3.5, cex=1.3, f=2)
  mtext(side=2, text = "Habilidad", line=1, cex=1.8, f=2)
  mtext(side=3, text = "Eje 3: Forma, Espacio y Medida", line=1.5, cex=2, f=2)
  mtext(side=3, paste("Resultados del CTE con ID", CTE$ID.CTT[MaxObservaciones[2]]), line=0.5, cex=1.1, f=2)
  text(0.15,9.2, "En construcción", f=2, cex=0.8, col="black")
  text(0.45,9.2, "En desarrollo", f=2, cex=0.8, col="black")
  text(0.85,9.2, "En consolidación", f=2, cex=0.8, col="black")
  lines(c(0.05,0.075),c(9.2,9.2), col="brown4", lwd=4)
  lines(c(.35,.375),c(9.2,9.2), col="gold3", lwd=4)
  lines(c(.75,.775),c(9.2,9.2), col="darkseagreen3", lwd=4)
  axis(1,at=seq(0,1,0.1),labels=seq(0,1,0.1), line=1, f=2)
  for(i in 1:8){
    text(0.04,valor_label, paste(q3[i]), f=2, cex=1.2)
    col_comp <- ifelse(Skill_E3[i]>Nacional_E3[i], "darkblue", "darkred")
    points(Nacional_E3[i],valor_dot, pch=3, cex=1.5, lwd=4, col=col_comp)
    text(0.19,valor_dot, paste("Media nacional: ", round(Nacional_E3[i],3)), f=2, cex=0.8, col=col_comp)
    text(Skill_E3[i]+.06,valor_label, paste(round(Skill_E3[i],3)), f=2, cex=1.2)
    valor_dot <- valor_dot - 1.1
    valor_label <- valor_label - 1.1}

dev.off()
