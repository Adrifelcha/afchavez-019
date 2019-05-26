rm(list=ls())
setwd("C:/Users/Sandra/Desktop/afchavez-019/RIMEDIE/Mat09_2015/Post-Reunión25042018/DINA_Estimaciones")

#################################################################
#### Buscamos los 5 estados con el peor y el mejor desempeño

#Abrimos la base con las estimaciones estatales
Estatales <- read.csv("CuadroEstimac_Nac-y-Estatal_MAT09_ajustes.csv")

#Identificamos el desempeño promedio de cada entidad
Av_performance <- NULL
for(i in 1:(nrow(Estatales)-1)){
Estimaciones <- as.numeric(Estatales[i,c(2:31)])
Av_performance[i] <- mean(Estimaciones)  
}

#Ordenamos los promedios
Orden <- sort(Av_performance)

#Seleccionamos el número de Casos más altos y más bajos
Casos <- 3

#Identificamos los N casos más altos y los N más bajos
a <- 0
Mayores <- NULL
Menores <- NULL
for(i in 1:Casos){
Mayores[i] <- Orden[(length(Av_performance)-a)]
a <- a+1
Menores[i] <- Orden[a]
}

#Rastreamos de qué estados se trata
Mejores <- NULL
Peores <- NULL
for(i in 1:Casos){
  a <- which(Av_performance==Mayores[i])
  b <- which(Av_performance==Menores[i])
Mejores[i] <- as.character(Estatales$Estado[a])
Peores[i] <- as.character(Estatales$Estado[b])
  }
Estados <- c(Mejores,Peores)

#Ubicamos estos estados dentro de la matriz de estimaciones totales
Select_M <- NULL
Select_P <- NULL
for(i in 1:Casos){
  Select_M[i] <- which(Estatales$Estado==Mejores[i])
  Select_P[i] <- which(Estatales$Estado==Peores[i])
}
Select <- c(Select_M,Select_P)

#Identificamos los promedios nacionales para referencias futuras
Nacional_E1 <- as.numeric(Estatales[nrow(Estatales),c(2:13)])
Nacional_E2 <- as.numeric(Estatales[nrow(Estatales),c(14:23)])
Nacional_E3 <- as.numeric(Estatales[nrow(Estatales),c(24:31)])



###########################################################
# Ploteamos el desempeño general de estos estados
pdf("DiagnósticoCognitivo_EstadosEnElExtremo.pdf", width=10, height=7)
plot(c(1,2),c(1,2), col="white", ann=F, axes=F, xlim=c(0,5), ylim=c(0,5))
text(2.5,3, paste("Diagnóstico cognitivo de habilidades matemáticas para los", Casos, "estados"), f=2, cex=1.5, col="black")
text(2.5,2.3, paste("con las mejores y peores estimaciones."), f=2, cex=1.5, col="black")

for(i in 1:length(Select)){
  Entidad <- Estados[i]
  Skill_E1 <- as.numeric(Estatales[Select[i],c(2:13)])
  Skill_E2 <- as.numeric(Estatales[Select[i],c(14:23)])
  Skill_E3 <- as.numeric(Estatales[Select[i],c(24:31)])
  
  plot(c(1,2),c(1,2), col="white", ann=F, axes=F, xlim=c(0,5), ylim=c(0,5))
  text(3.5,3, paste("Resultados por estado:", Entidad), f=2, cex=1, col="black")
  
  col_sk1 <- ifelse(Skill_E1<0.3, "brown4", ifelse(Skill_E1<0.7,"gold3","darkseagreen3"))
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
  mtext(side=3, paste("Resultados de", Entidad), line=0.5, cex=1.1, f=2)
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
  mtext(side=3, paste("Resultados de", Entidad), line=0.5, cex=1.1, f=2)
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
  mtext(side=3, paste("Resultados de", Entidad), line=0.5, cex=1.1, f=2)
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
}
dev.off()
