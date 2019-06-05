# Análisis de Datos 1.0      ########################################
# D E S C R I P T I V O S    ########################################
# Prueba PLANEA Matemáticas - 6to primaria     ######################
# 50 items liberados, 127 estudiantes          ######################
# Tesis doctoral de Sandra Conzuelo Serrato    ######################
# Apoyo técnico y análisis de datos: Adriana Felisa Chávez De la Peña
#####################################################################
#####################################################################

####################################################
#Cargamos librería de trabajo, las bases y los datos
setwd("C:/Users/Adriana/Desktop/afchavez-019/Sandy-PrácticaDocente/Netza")

Datos <- read.csv("Respuestas.csv")     #Archivo con las respuestas de los estudiantes
Datos$NO. <- NULL
Clave <- read.csv("Claves.csv")         #Archivo con las claves de respuestas

Items <- ncol(Datos)-2  
Sujetos <- nrow(Datos)
Correct <- Clave$RCorr_San

####################################################
#Evaluamos el desempeño de los estudiantes en la prueba aplicada
# 1. Traducimos la base de respuestas en una base dicotómica acierto-error

Dicot <- matrix(, nrow = Sujetos, ncol = Items+4)  #Creamos una matriz vacía

#Llenamos la matriz con 1's y 0's según el match entre las Respuestas registradas y la Clave Correcta
for(a in 1:Items){
  for(b in 1:Sujetos){
    Dicot[b,1] <- Datos$APELLIDO_NOMBRE[b]   #En la primera columna, anotamos el grupo de procedencia
    Dicot[b,2] <- 0    
    Dicot[b,3] <- 0    #Dejamos las siguientes tres columnas vacías para después llenarlas con los puntajes
    Dicot[b,4] <- 0         # (aún no calculados)
    #Evaluamos la correspondencia entre cada celda/respuesta y la respuesta correcta por item
    if(is.na(Datos[b,a+2])=="TRUE"){
      nada <- "Nada"
    }else{
    ifelse(grepl(Correct[a],Datos[b,a+2])==TRUE,     
           Dicot[b,a+4] <- 1, Dicot[b,a+4] <-0)
  }}}



# Creamos un arreglo con 50 etiquetas, una por cada item, para los Headers de nuestra nueva matriz
Label_Items <- NULL
for(a in 1:Items){
  ifelse(a<=25, 
         Label_Items[a] <- paste("Item ",a, "-FA"),
         Label_Items[a] <- paste("Item ",(a-25), "-FB"))
}

colnames(Dicot) <- c("Nombre", "Forma A", "Forma B", "Total", Label_Items) #Asignamos los Headers

# 2. Estimamos la puntuación obtenida por cada estudiante
Score_FA <- NULL    #Creamos tres arreglos vacíos a llenar por cada sujeto
Score_FB <- NULL
Score_Total <- NULL
for(a in 1:Sujetos){    #Por cada sujeto...
  Score_FA[a] <- sum(Dicot[a,c(5:29)], na.rm=TRUE)    #Puntaje de la Forma A
  Score_FB[a] <- sum(Dicot[a,c(30:54)], na.rm=TRUE)   #Puntaje de la Forma B
  Score_Total[a] <- sum(c(Score_FA[a] + Score_FB[a]), na.rm=TRUE)   #Puntaje Total
}

Dicot[,2] <- Score_FA   #Sobre escribimos las columnas antes vacías con las puntuaciones calculadas
Dicot[,3] <- Score_FB
Dicot[,4] <- Score_Total

#Copiamos la matriz resultante a un archivo csv:
write.csv(Dicot,"MatrizAciertos.csv",row.names = FALSE) #Creamos un csv con nuestra matriz


######################################################################
# Ploteamos las puntuaciones obtenidas por todos los estudiantes (sin importar grupo)
layout(matrix(1:1,ncol=1))

valor_label <- NULL
hist(Score_FA, breaks=seq(-.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col="lightgoldenrod1", 
     ylim=c(0,8))
lines(c(1,1),c(0,8),col='black', lty=4,lwd=2) 
text(7,7.5, paste("Respondieron la Forma A:"), f=2)
text(7,7.1, paste(length(which(is.na(Dicot[,10])==FALSE)), " estudiantes en total"), f=2)
text(0.5,4, paste("0 estudiantes no asistieron"), srt=90, f=2)
text(7,6.5, paste("Promedio: 11.60"), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Forma A", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.5)
axis(1,at=c(0.5:25.5),labels=c(0:25), line=-0.5, f=2)
for(i in 1:length(seq(0,25,1))){
  valor_label[i] <- length(which(Score_FA==i))
  if(valor_label[i]==0){
    nada <- "nada"
  }else{
    text(i+0.5, valor_label[i]+0.35, paste(valor_label[i]), cex=1.2, f=2)
  }}


valor_label <- NULL
hist(Score_FB,ann=F, breaks=seq(-0.1,25,1), axes=F, xlim=c(0,25), col="lightgoldenrod2", 
     ylim=c(0,8))
lines(c(0.9,0.9),c(0,8),col='black', lty=4,lwd=2) 
text(7,7.5, paste("Respondieron la Forma B:"), f=2)
text(7,7.1, paste(length(which(is.na(Dicot[,46])==FALSE)), " estudiantes en total"), f=2)
text(0.3,4, paste(length(which(is.na(Dicot[,46])==TRUE)), " estudiante no asistió"), srt=90, f=2)
text(7,6.5, paste("Promedio: 5.92"), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Forma B", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.2)
axis(1,at=seq(0.4,50.4,1),labels=c(0:50), line=-0.5, f=2)
for(i in 1:length(seq(0,25,1))){
  valor_label[i] <- length(which(Score_FB==i))
  if(valor_label[i]==0){
    nada <- "nada"
  }else{
    text(i+0.4, valor_label[i]+0.35, paste(valor_label[i]), cex=1.2, f=2)
  }}


Second_FB <- Score_FB+26
valor_label <- NULL
hist(c(Score_FA,Second_FB),ann=F, breaks=seq(-0.1,50,1), axes=F, xlim=c(0,50), col="lightgoldenrod4", ylim=c(0,8),
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
text(45.5, 15, "Forma 2", f=2, col="lightgoldenrod4", cex=2)
text(5.5, 15, "Forma 1", f=2, col="lightgoldenrod3", cex=2)
lines(c(25.9,25.9),c(0,8),col='black', lty=1,lwd=3) 
lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
text(10.5,7.7, paste("Respondieron la Forma A:"), f=2)
text(10,7.3, paste(length(which(is.na(Dicot[,10])==FALSE)), " estudiantes en total"), f=2)
text(10,5.5, paste("Promedio: 11.6"), f=2)
text(40,7.7, paste("Respondieron la Forma B:"), f=2)
text(40,7.3, paste(length(which(is.na(Dicot[,46])==FALSE)), " estudiantes en total"), f=2)
text(40,5.5, paste("Promedio: 5.92"), f=2)
mtext(side=1, text = "Puntuaciones", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Formas A y B", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.2)
axis(1,at=seq(0.4,51.4,1),labels=c(c(0:25),c(0:25)), line=-0.5, f=2)









Completo <- 0
Total_Completo <- NULL
for(a in 1:Sujetos){
  if(is.na(Dicot[a,10])==FALSE&is.na(Dicot[a,50])==FALSE){
    Completo <- Completo + 1
    Total_Completo[Completo] <- Score_Total[a]
  }else{
    nada <- "nada"  
  }}

mark <- 0
valor_label <- NULL
hist(Total_Completo, breaks=seq(0,50,by=5), main="Puntajes totales", ann=F, axes=F, xlim=c(0,50), 
     col="goldenrod", ylim=c(0,12))
text(40,30, paste("Respondieron todo el examen:"), f=2)
text(40,29, paste(length(Total_Completo), " estudiantes en total"), f=2)
text(8,25, paste("Promedio: ", round(mean(Total_Completo),2)), f=2)
text(8,20.5, paste("Máxima: ", round(max(Total_Completo),2)), f=2)
text(8,22, paste("Mínima: ", round(min(Total_Completo),2)), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuación TOTAL", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,10,1),labels=seq(0,10,1),las=1, line=-1.5)
axis(1,at=seq(2.5,50,5),labels=c("0-5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50"), line=-0.5, f=2)
for(i in 1:length(seq(2.5,50,5))){
  valor_label[i] <- length(which(Total_Completo>=(-4+(5 * i))&Total_Completo<((5*i)+1)))
  text(2.5+mark, valor_label[i]+0.8, paste(valor_label[i]), cex=1.2, f=2)
  mark <- mark+5
}

mark <- 0.5
valor_label <- NULL
hist(Total_Completo, breaks=seq(0,50,by=1), main="Puntajes totales", ann=F, axes=F, 
     xlim=c(0,50), col="goldenrod", ylim=c(0,4))
text(40,9, paste("Respondieron todo el examen:"), f=2)
text(40,8.5, paste(length(Total_Completo), " estudiantes en total"), f=2)
text(8,8, paste("Promedio: ", round(mean(Total_Completo),2)), f=2)
text(8,7.2, paste("Máxima: ", round(max(Total_Completo),2)), f=2)
text(8,6.8, paste("Mínima: ", round(min(Total_Completo),2)), f=2)
mtext(side=1, text = "Puntuación total", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuación TOTAL", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,4,1),labels=seq(0,4,1),las=1, line=-1.5)
axis(1,at=c(0.5:50.5),labels=c(1:51), line=.5, f=2)
for(i in 1:length(seq(0,50,1))){
  valor_label[i] <- length(which(Total_Completo==i))
  if(valor_label[i]==0){
    nada <- "nada"
  }else{
    text(i-0.5, valor_label[i]+0.3, paste(valor_label[i]), cex=1.2, f=2)
  }}

#Aciertos por item
Rights <- NULL
for(i in 1:Items){
  Rights[i] <- sum(Dicot[,i+4], na.rm = TRUE)
}

valor_label <- 1.2
barplot(Rights, col="gray96", ann=F, axes=F, ylim=c(0,25))
axis(2,at=seq(0,25,5),labels=seq(0,25,5),las=1, line=-1.5)
text(32.5, 24, "Forma 2", f=2, col="lightgoldenrod4")
text(27.5, 24, "Forma 1", f=2, col="lightgoldenrod3")
lines(c(30.1,30.1),c(0,25),col='black', lty=1,lwd=1.3) 
lines(c(30.1,60),c(25,25),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(25,25),lty=1,lwd=4, col="lightgoldenrod1") 
mtext(side=3, text = "Tomando en cuenta la muestra total", line=0.5, cex=0.8, f=2)
mtext(side=3, text = "Número de estudiantes que acertaron cada item", line=1.5, cex=2, f=2)
mtext(side=1, text = "Item ID", line=2.5, cex=1.2, f=2)
axis(1,at=seq(.7,59.7,1.2),labels=seq(1,50,1), line=0, f=2,las=2)
for(i in 1:length(Rights)){
  if(i==1){
    text(0.7, Rights[i]+0.5, paste(Rights[i]), cex=1.1, f=2)
  }else{
    text(0.7+valor_label, Rights[i]+0.5, paste(Rights[i]), cex=1.1, f=2)
    valor_label <- valor_label+1.2
  }}


PRights <- NULL
for(i in 1:Items){
  PRights[i] <- ((Rights[i])/Sujetos)*100
}

valor_label <- 1.2
barplot(PRights, col="gray96", ann=F, axes=F, ylim=c(0,100))
axis(2,at=seq(0,100,10),labels=paste(seq(0,100,10),"%"),las=1, line=-1.5)
text(32.5, 98, "Forma 2", f=2, col="lightgoldenrod4")
text(27.5, 98, "Forma 1", f=2, col="lightgoldenrod3")
lines(c(30.1,30.1),c(0,100),col='black', lty=1,lwd=1.3) 
lines(c(30.1,60),c(100,100),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(100,100),lty=1,lwd=4, col="lightgoldenrod1") 
mtext(side=3, text = "Tomando en cuenta la MUESTRA TOTAL", line=0.5, cex=0.8, f=2)
mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=1.5, cex=2, f=2)
mtext(side=1, text = "Item ID", line=2.5, cex=1.2, f=2)
axis(1,at=seq(.7,59.7,1.2),labels=seq(1,50,1), line=0, f=2,las=2)
for(i in 1:length(PRights)){
  if(i==1){
    text(0.7, PRights[i]+6.5, paste(round(PRights[i],2),"%"), cex=1.1, f=2, srt=90)
  }else{
    text(0.7+valor_label, PRights[i]+6.5, paste(round(PRights[i],2),"%"), cex=1.1, f=2, srt=90)
    valor_label <- valor_label+1.2
  }}


#Indices de dificultad GLOBAL
P <- NULL
for(i in 1:Items){
  P[i] <- mean(Dicot[,i+4],na.rm = TRUE)
}


hist(P,breaks = seq(0,1,0.05), col="firebrick2", xlab="", ann=F, axes=F,ylim = c(0,11))
axis(2,at=seq(0,11,1),labels=seq(0,11,1),las=1, line=-1.5)
mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=0.5, cex=0.8, f=2)
mtext(side=3, text = "Dificultad de los ítems", line=1.5, cex=2, f=2)
mtext(side=2, text = "Frecuencia (No. de items)", line=0.5, cex=1.2, f=2)
axis(1,at=seq(0.025,1,0.05),labels=c("0 a 5%","6 a 10%","11 a 15%","16 a 20%","21 a 25%","26 a 30%","31 a 35%","36 a 40%","41 a 45%","46 a 50%","51 a 55%","56 a 60%","61 a 65%","66 a 70%","71 a 75%","76 a 80%","81 a 85%","86 a 90%","91 a 95%","96 a 100%"), line=0.1, f=2,las=2)

length(which(P<.4))
Dificiles <- c(which(P<.4))
Dificiles
