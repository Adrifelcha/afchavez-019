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
setwd("C:/Users/Alejandro/Desktop/afchavez19/Lancaster/Reporte_01")

Datos <- read.csv("Respuestas.csv")     #Archivo con las respuestas de los estudiantes
Datos$NUMERO <- NULL
Clave <- read.csv("Claves.csv")         #Archivo con las claves de respuestas

Items <- c(1:50)                        #Especificamos las variables
Sujetos <- nrow(Datos)
Correct <- Clave$RCorr_San

####################################################
#Evaluamos el desempeño de los estudiantes en la prueba aplicada
# 1. Traducimos la base de respuestas en una base dicotómica acierto-error

Dicot <- matrix(, nrow = Sujetos, ncol = length(Items)+7)  #Creamos una matriz vacía

#Llenamos la matriz con 1's y 0's según el match entre las Respuestas registradas y la Clave Correcta
for(a in 1:length(Items)){
  for(b in 1:Sujetos){
    Dicot[b,1] <- Datos$APELLIDO_NOMBRE[b]   #En la primera columna, anotamos el grupo de procedencia
    Dicot[b,2] <- 0    
    Dicot[b,3] <- 0    #Dejamos las siguientes tres columnas vacías para después llenarlas con los puntajes
    Dicot[b,4] <- 0         # (aún no calculados)
    Dicot[b,5] <- Datos$SEXO[b]
    Dicot[b,6] <- Datos$GRADO[b]
    Dicot[b,7] <- Datos$GRUPO[b]
    #Evaluamos la correspondencia entre cada celda/respuesta y la respuesta correcta por item
    if(is.na(Datos[b,a+2])=="TRUE"){
      nada <- "Nada"
    }else{
    ifelse(grepl(Correct[a],Datos[b,a+2])==TRUE,     
           Dicot[b,a+7] <- 1, Dicot[b,a+7] <-0)
  }}}



# Creamos un arreglo con 50 etiquetas, una por cada item, para los Headers de nuestra nueva matriz
Label_Items <- NULL
for(a in 1:length(Items)){
  ifelse(a<=25, 
         Label_Items[a] <- paste("Item ",a, "-FA"),
         Label_Items[a] <- paste("Item ",a, "-FB"))
}

colnames(Dicot) <- c("Nombre", "Forma A", "Forma B", "Total", "Sexo", "Grado", "Grupo", Label_Items) #Asignamos los Headers

# 2. Estimamos la puntuación obtenida por cada estudiante
Score_FA <- NULL    #Creamos tres arreglos vacíos a llenar por cada sujeto
Score_FB <- NULL
Score_Total <- NULL
for(a in 1:Sujetos){    #Por cada sujeto...
  Score_FA[a] <- sum(Dicot[a,c(8:32)], na.rm=TRUE)    #Puntaje de la Forma A
  Score_FB[a] <- sum(Dicot[a,c(33:57)], na.rm=TRUE)   #Puntaje de la Forma B
  Score_Total[a] <- sum(c(Score_FA[a] + Score_FB[a]), na.rm=TRUE)   #Puntaje Total
}

Dicot[,2] <- Score_FA   #Sobre escribimos las columnas antes vacías con las puntuaciones calculadas
Dicot[,3] <- Score_FB
Dicot[,4] <- Score_Total
#View(Dicot)  #Imprimimos la matriz resultante
#Y la copiamos a un archivo csv:
write.csv(Dicot,"MatrizAciertos.csv",row.names = FALSE) #Creamos un csv con nuestra matriz

######################################################################
# Ploteamos las puntuaciones obtenidas por todos los estudiantes (sin importar grupo)
layout(matrix(1:1,ncol=1))

valor_label <- NULL
hist(Score_FA, breaks=seq(-.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col="lightgoldenrod1", 
     ylim=c(0,16))
lines(c(1,1),c(0,16),col='black', lty=4,lwd=2) 
text(7,15.5, paste("Respondieron la Forma A:"), f=2)
text(7,15, paste(length(which(is.na(Dicot[,10])==FALSE)), " estudiantes en total"), f=2)
text(0.5,8, paste(length(which(is.na(Dicot[,10])==TRUE)), " estudiantes no asistieron"), srt=90, f=2)
text(7,12, paste("Promedio: 16.4"), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Forma A", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,16,2),labels=seq(0,16,2),las=1, line=-1.5)
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
     ylim=c(0,16))
lines(c(0.9,0.9),c(0,16),col='black', lty=4,lwd=2) 
text(16,15.5, paste("Respondieron la Forma B:"), f=2)
text(16,15, paste(length(which(is.na(Dicot[,45])==FALSE)), " estudiantes en total"), f=2)
text(0.3,10, paste(length(which(is.na(Dicot[,45])==TRUE)), " estudiantes no asistieron"), srt=90, f=2)
text(16,12, paste("Promedio: 10.18"), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Forma B", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,16,2),labels=seq(0,16,2),las=1, line=-1.2)
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
hist(c(Score_FA,Second_FB),ann=F, breaks=seq(-0.1,50,1), axes=F, xlim=c(0,50), col="lightgoldenrod4", ylim=c(0,16),
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
lines(c(0.9,0.9),c(0,16),col='black', lty=4,lwd=2) 
text(45.5, 15, "Forma 2", f=2, col="lightgoldenrod4", cex=2)
text(5.5, 15, "Forma 1", f=2, col="lightgoldenrod3", cex=2)
lines(c(25.9,25.9),c(0,16),col='black', lty=1,lwd=3) 
lines(c(26.9,26.9),c(0,16),col='black', lty=4,lwd=2) 
text(26.3,10, paste(length(which(is.na(Dicot[,45])==TRUE)), " estudiantes no asistieron"), srt=90, f=2)
lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
text(6.5,11.5, paste("Respondieron la Forma A:"), f=2)
text(6,11, paste(length(which(is.na(Dicot[,10])==FALSE)), " estudiantes en total"), f=2)
text(6,10, paste("Promedio: 16.14"), f=2)
text(46,11.5, paste("Respondieron la Forma B:"), f=2)
text(46,11, paste(length(which(is.na(Dicot[,45])==FALSE)), " estudiantes en total"), f=2)
text(0.3,10, paste(length(which(is.na(Dicot[,45])==TRUE)), " estudiantes no asistieron"), srt=90, f=2)
text(46,10, paste("Promedio: 10.18"), f=2)
mtext(side=1, text = "Puntuaciones", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuaciones - Formas A y B", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Todos los estudiantes)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,16,2),labels=seq(0,16,2),las=1, line=-1.2)
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
hist(Total_Completo, breaks=seq(0,50,by=5), main="Puntajes totales", ann=F, axes=F, xlim=c(0,50), col="goldenrod")
text(40,30, paste("Respondieron todo el examen:"), f=2)
text(40,29, paste(length(Total_Completo), " estudiantes en total"), f=2)
text(8,25, paste("Promedio: ", round(mean(Total_Completo),2)), f=2)
text(8,20.5, paste("Máxima: ", round(max(Total_Completo),2)), f=2)
text(8,22, paste("Mínima: ", round(min(Total_Completo),2)), f=2)
mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuación TOTAL", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,30,5),labels=seq(0,30,5),las=1, line=-1.5)
axis(1,at=seq(2.5,50,5),labels=c("0-5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50"), line=-0.5, f=2)
for(i in 1:length(seq(2.5,50,5))){
  valor_label[i] <- length(which(Total_Completo>=(-4+(5 * i))&Total_Completo<((5*i)+1)))
  text(2.5+mark, valor_label[i]+0.8, paste(valor_label[i]), cex=1.2, f=2)
  mark <- mark+5
}

mark <- 0.5
valor_label <- NULL
hist(Total_Completo, breaks=seq(0,50,by=1), main="Puntajes totales", ann=F, axes=F, xlim=c(0,50), col="goldenrod")
text(40,9, paste("Respondieron todo el examen:"), f=2)
text(40,8.5, paste(length(Total_Completo), " estudiantes en total"), f=2)
text(8,8, paste("Promedio: ", round(mean(Total_Completo),2)), f=2)
text(8,7.2, paste("Máxima: ", round(max(Total_Completo),2)), f=2)
text(8,6.8, paste("Mínima: ", round(min(Total_Completo),2)), f=2)
mtext(side=1, text = "Puntuación total", line=2.8, cex=1.3, f=2)
mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
mtext(side=3, text = "Puntuación TOTAL", line=1.5, cex=2, f=2)
mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=0.8, f=2)
axis(2,at=seq(0,10,1),labels=seq(0,10,1),las=1, line=-1.5)
axis(1,at=c(0.5:50.5),labels=c(1:51), line=-0.5, f=2)
for(i in 1:length(seq(0,50,1))){
  valor_label[i] <- length(which(Total_Completo==i))
  if(valor_label[i]==0){
    nada <- "nada"
  }else{
  text(i-0.5, valor_label[i]+0.3, paste(valor_label[i]), cex=1.2, f=2)
  }}

#Aciertos por item
Rights <- NULL
for(i in 1:length(Items)){
  Rights[i] <- sum(Dicot[,i+7], na.rm = TRUE)
}

valor_label <- 1.2
barplot(Rights, col="gray96", ann=F, axes=F, ylim=c(0,120))
axis(2,at=seq(0,120,6),labels=seq(0,120,6),las=1, line=-1.5)
text(32.5, 115, "Forma 2", f=2, col="lightgoldenrod4")
text(27.5, 115, "Forma 1", f=2, col="lightgoldenrod3")
lines(c(30.1,30.1),c(0,120),col='black', lty=1,lwd=1.3) 
lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
mtext(side=3, text = "Tomando en cuenta la muestra total", line=0.5, cex=0.8, f=2)
mtext(side=3, text = "Número de estudiantes que acertaron cada item", line=1.5, cex=2, f=2)
mtext(side=1, text = "Item ID", line=2.5, cex=1.2, f=2)
axis(1,at=seq(.7,59.7,1.2),labels=seq(1,50,1), line=0, f=2,las=2)
for(i in 1:length(Rights)){
  if(i==1){
    text(0.7, Rights[i]+0, paste(Rights[i]), cex=1.1, f=2)
  }else{
    text(0.7+valor_label, Rights[i]+5, paste(Rights[i]), cex=1.1, f=2)
    valor_label <- valor_label+1.2
  }}


PRights <- NULL
for(i in 1:length(Items)){
  PRights[i] <- ((Rights[i])/Sujetos)*100
}

valor_label <- 1.2
barplot(PRights, col="gray96", ann=F, axes=F, ylim=c(0,100))
axis(2,at=seq(0,100,10),labels=paste(seq(0,100,10),"%"),las=1, line=-1.5)
text(32.5, 115, "Forma 2", f=2, col="lightgoldenrod4")
text(27.5, 115, "Forma 1", f=2, col="lightgoldenrod3")
lines(c(30.1,30.1),c(0,120),col='black', lty=1,lwd=1.3) 
lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
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
for(i in 1:length(Items)){
  P[i] <- mean(Dicot[,i+7],na.rm = TRUE)
}


hist(P,breaks = seq(0,1,0.05), col="firebrick2", xlab="", ann=F, axes=F,
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.142,0.142),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.284,.284),lwd=2,lty=3, col="black"),lines(c(0,1),c(.426,.426),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.57,.57),lwd=2,lty=3, col="black"),lines(c(0,1),c(.712,.712),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.858,.858),lwd=2,lty=3, col="black")))
axis(2,at=seq(0,7,1),labels=seq(0,7,1),las=1, line=-1.5)
mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=0.5, cex=0.8, f=2)
mtext(side=3, text = "Dificultad de los ítems", line=1.5, cex=2, f=2)
mtext(side=2, text = "Frecuencia (No. de items)", line=0.5, cex=1.2, f=2)
axis(1,at=seq(0.025,1,0.05),labels=c("0 a 5%","6 a 10%","11 a 15%","16 a 20%","21 a 25%","26 a 30%","31 a 35%","36 a 40%","41 a 45%","46 a 50%","51 a 55%","56 a 60%","61 a 65%","66 a 70%","71 a 75%","76 a 80%","81 a 85%","86 a 90%","91 a 95%","96 a 100%"), line=-0.5, f=2,las=2)

length(which(P<.4))
Dificiles <- c(which(P<.4))
Dificiles

######################################################################
######################################################################
# Se segrega el análisis por GRADO

colores <- c("cadetblue2", "dodgerblue3")
col_dif <- c("cadetblue3", "dodgerblue4")
mean_A <- c(16.09836, 16.69355)
mean_B <- c(8.951613, 11.22623)
mean_T <- c(25.96667, 28.2)
w <- 0
for(g in sort(unique(Datos$GRADO))){
  w <- w+1
  valor_label <- NULL
  hist(Score_FA[Datos$GRADO==g], breaks=seq(-.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,11), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.09,0.09),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.18,.18),lwd=2,lty=3, col="black"),lines(c(0,1),c(.27,.27),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.36,.36),lwd=2,lty=3, col="black"),lines(c(0,1),c(.45,.45),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.545,.545),lwd=2,lty=3, col="black"),lines(c(0,1),c(.635,.635),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.725,.725),lwd=2,lty=3, col="black"),lines(c(0,1),c(.82,.82),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.91,.91),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,11),col='black', lty=4,lwd=2) 
  text(9,10.3, paste("Respondieron la Forma A:"), f=2, cex=1.2)
  text(9,9.8, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,10])==FALSE)), " estudiantes de",g, "año" ), f=2)
  text(0.5,6, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,10])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(4,7.4, paste("Promedio: ",round(mean_A[w],2)), f=2)
  text(4,6.3, paste("Máxima: ",round(max(Score_FA[Datos$GRADO==g]),2)), f=2)
  text(4,5.9, paste("Mínima: ",round(min(Score_FA[Datos$GRADO==g]),2)), f=2)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación - Forma A"), line=1.5, cex=2, f=2)
  mtext(side=3, text = paste("(",g,"año)"), line=0.2, cex=1.2, f=2)
  axis(2,at=seq(0,11,1),labels=seq(0,11,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  hist(Score_FB[Datos$GRADO==g], breaks=seq(-0.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,11), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.09,0.09),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.18,.18),lwd=2,lty=3, col="black"),lines(c(0,1),c(.27,.27),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.36,.36),lwd=2,lty=3, col="black"),lines(c(0,1),c(.45,.45),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.545,.545),lwd=2,lty=3, col="black"),lines(c(0,1),c(.635,.635),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.725,.725),lwd=2,lty=3, col="black"),lines(c(0,1),c(.82,.82),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.91,.91),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,11),col='black', lty=4,lwd=2) 
  text(11,10.3, paste("Respondieron la Forma B:"), f=2, cex=1.2)
  text(11,9.8, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,40])==FALSE)), " estudiantes de ",g ), f=2)
  text(0.5,6, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,40])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(19,7.3, paste("Promedio: ",round(mean_B[w],2)), f=2)
  text(19,6.3, paste("Máxima: ",round(max(Score_FB[Datos$GRADO==g]),2)), f=2)
  text(19,5.9, paste("Mínima: ",round(min(Score_FB[Datos$GRADO==g]),2)), f=2)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación - Forma B"), line=1.5, cex=2, f=2)
  mtext(side=3, text = paste("(",g,"año)"), line=0.2, cex=1.2, f=2)
  axis(2,at=seq(0,11,1),labels=seq(0,11,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  Second_FB <- Score_FB+26
  valor_label <- NULL
  hist(c(Score_FA[Datos$GRADO==g],Second_FB[Datos$GRADO==g]),ann=F, breaks=seq(-0.1,50,1), axes=F, xlim=c(0,50), col="lightgoldenrod4", ylim=c(0,11),
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.0909,0.0909),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.1818,.1818),lwd=2,lty=3, col="black"),lines(c(0,1),c(.2727,.2727),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.3636,.3636),lwd=2,lty=3, col="black"),lines(c(0,1),c(.4545,.4545),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5454,.5454),lwd=2,lty=3, col="black"),lines(c(0,1),c(.6363,.6363),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.7272,.7272),lwd=2,lty=3, col="black"),lines(c(0,1),c(.8181,.8181),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.909,.909),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,16),col='black', lty=4,lwd=2) 
  text(45.5, 15, "Forma 2", f=2, col="lightgoldenrod4", cex=2)
  text(5.5, 15, "Forma 1", f=2, col="lightgoldenrod3", cex=2)
  lines(c(25.9,25.9),c(0,16),col='black', lty=1,lwd=3) 
  lines(c(26.9,26.9),c(0,16),col='black', lty=4,lwd=2) 
  text(26.3,5, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,40])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
  lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
  text(6.5,8.8, paste("Respondieron la Forma A:"), f=2)
  text(6,8.3, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,15])==FALSE)), " estudiantes de ",g ), f=2)
  text(6,6.8, paste("Promedio: ",round(mean_B[w],2)), f=2)
  text(46,8.8, paste("Respondieron la Forma B:"), f=2)
  text(46,8.3, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,40])==FALSE)), " estudiantes de ",g ), f=2)
  text(0.3,5, paste(length(which(is.na(Dicot[(Datos$GRADO==g)==TRUE,15])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(46,6.8, paste("Promedio: ",round(mean_B[w],2)), f=2)
  mtext(side=1, text = "Puntuaciones", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = "Puntuaciones - Formas A y B", line=1.5, cex=2, f=2)
  mtext(side=3, text = paste("(",g,"año)"), line=0.2, cex=1.2, f=2)
  axis(2,at=seq(0,11,1),labels=seq(0,11,1),las=1, line=-1.2)
  axis(1,at=seq(0.4,51.4,1),labels=c(c(0:25),c(0:25)), line=-0.5, f=2)
  
  
  Completo <- 0
  Total_Completo <- NULL
  Falta_FA <- is.na(Dicot[(Datos$GRADO==g)==TRUE,10])
  Falta_FB <- is.na(Dicot[(Datos$GRADO==g)==TRUE,40])
  FA <- Score_FA[Datos$GRADO==g] 
  FB <- Score_FB[Datos$GRADO==g]
  for(a in 1:length(Sujetos[Datos$GRADO==g])){
    if(Falta_FA[a]==TRUE){
      nada <- "nada"}else{
        if(Falta_FB[a]==FALSE){
          Completo <- Completo + 1
          Total_Completo[Completo] <- FA[a]+FB[a]
        }else{
          nada <- "nada"  
        }}}
  
  hist(Total_Completo, breaks=seq(0,50,by=1), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,6), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.165,0.165),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.33,.33),lwd=2,lty=3, col="black"),lines(c(0,1),c(.498,.498),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.665,.665),lwd=2,lty=3, col="black"),lines(c(0,1),c(.83,.83),lwd=2,lty=3, col="black")))
  text(40,5.8, paste(Completo, "de", length(FA),"estudiantes"), f=2,cex=1.3)
  text(40,5.5, paste("respondieron todo el examen"), f=2, cex=1.3)
  text(9,4.5, paste("Promedio: ",round(mean_T[w],2)), f=2,cex=1.1)
  text(9,3.8, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(9,3.5, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuaciones Totales en",g, "año"), line=1.5, cex=2, f=2)
  mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=, f=2)
  axis(2,at=seq(0,6,1),labels=seq(0,6,1),las=1, line=-1.1)
  axis(1,at=c(0.5:50.5),labels=c(1:51), line=-0.5, f=2)
  
  hist(Total_Completo, breaks=seq(0,50,by=5), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,15), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.065,0.065),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.132,.132),lwd=2,lty=3, col="black"),lines(c(0,1),c(.195,.195),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.262,.262),lwd=2,lty=3, col="black"),lines(c(0,1),c(.33,.33),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.4,.4),lwd=2,lty=3, col="black"),lines(c(0,1),c(.465,.465),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.532,.532),lwd=2,lty=3, col="black"),lines(c(0,1),c(.595,.595),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.662,.662),lwd=2,lty=3, col="black"),lines(c(0,1),c(.73,.73),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.865,.865),lwd=2,lty=3, col="black"),lines(c(0,1),c(.932,.932),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.8,.8),lwd=2,lty=3, col="black")))
  text(40,14.4, paste(Completo, "de", length(FA),"estudiantes"), f=2,cex=1.3)
  text(40,13.4, paste("respondieron todo el examen"), f=2, cex=1.3)
  text(9,10.3, paste("Promedio: ",round(mean_T[w],2)), f=2,cex=1.1)
  text(9,8.4, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(9,7.7, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación TOTAL -",g, "año"), line=1.5, cex=2, f=2)
  mtext(side=3, text = "(Contando sólo los estudiantes que hicieron ambas formas)", line=0.5, cex=, f=2)
  axis(2,at=seq(0,15,1),labels=seq(0,15,1),las=1, line=-1.1)
  axis(1,at=seq(2.5,50,5),labels=c("0-5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50"), line=-0.5, f=2)
  
  
  P <- NULL
  for(i in 1:length(Items)){
    P[i] <- mean(Dicot[(Datos$GRADO==g)==TRUE,i+7],na.rm = TRUE)
  }
  
  hist(P,breaks = seq(-0.001,1,0.05), col=col_dif[w], main="Indices de dificultad generales registrados",
       xlab="", ann=F, axes=F, ylim=c(0,7),
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.14,0.14),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.285,.285),lwd=2,lty=3, col="black"),lines(c(0,1),c(.43,.43),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.71,.71),lwd=2,lty=3, col="black"),lines(c(0,1),c(.57,.57),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.855,.855),lwd=2,lty=3, col="black")))
  axis(2,at=seq(0,7,1),labels=seq(0,7,1),las=1, line=-1.5)
  mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=0.5, cex=1.2, f=2)
  mtext(side=3, text = paste("Dificultad de los ítems para", g,"año"), line=1.5, cex=2, f=2)
  mtext(side=2, text = "Frecuencia (No. de items)", line=0.5, cex=1.2, f=2)
  axis(1,at=seq(0.025,1,0.05),labels=c("0 a 5%","6 a 10%","11 a 15%","16 a 20%","21 a 25%","26 a 30%","31 a 35%","36 a 40%","41 a 45%","46 a 50%","51 a 55%","56 a 60%","61 a 65%","66 a 70%","71 a 75%","76 a 80%","81 a 85%","86 a 90%","91 a 95%","96 a 100%"), line=-0.5, f=2,las=2)
  
  print(g)
  print(which(P<.3))
  print(P[40])
  print(P[48])
  print(P[27])
  print(P[31])
  
  #Aciertos por item
  Unos <- NULL
  for(i in 1:length(Items)){
    Unos[i] <- sum(Dicot[(Datos$GRADO==g)==TRUE,i+7], na.rm = TRUE)
  }
  
  valor_label_u <- 1.2
  valor_label_r <- 1.2
  barplot(Unos, col="gray96", ann=F, axes=F, ylim=c(0,120))
  axis(2,at=seq(0,120,6),labels=seq(0,120,6),las=1, line=-1.5)
  text(32.5, 115, "Forma 2", f=2, col="lightgoldenrod4")
  text(27.5, 115, "Forma 1", f=2, col="lightgoldenrod3")
  lines(c(30.1,30.1),c(0,120),col='black', lty=1,lwd=1.3) 
  lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
  lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
  mtext(side=3, text = paste("Tomando en cuenta a los estudiantes de", g, "y comparándolos con la muestra total"), line=0.5, cex=0.8, f=2)
  mtext(side=3, text = "Número de estudiantes que acertaron cada item", line=1.5, cex=2, f=2)
  mtext(side=1, text = "Item ID", line=2.5, cex=1.2, f=2)
  axis(1,at=seq(.7,59.7,1.2),labels=seq(1,50,1), line=0, f=2,las=2)
  for(i in 1:length(Unos)){
    if(i==1){
      text(0.7, Unos[i]+0, paste(Unos[i]), cex=1.1, f=2)
    }else{
      text(0.7+valor_label_u, Unos[i]+5, paste(Unos[i]), cex=1.1, f=2)
      valor_label_u <- valor_label_u+1.2
    }}
  for(i in 1:length(Rights)){
    if(i==1){
      text(0.7, Rights[i]+0, paste(Rights[i]), cex=0.7, f=2, col="gray60")
      lines(c(0.7,0.7),c(0,Rights[i]), col="gray87", lwd=2,lty=4)
    }else{
      text(0.7+valor_label_r, Rights[i]+5, paste(Rights[i]), cex=0.7, f=2, col="gray60")
      lines(c(0.7+valor_label_r,0.7+valor_label_r),c(0,Rights[i]), col="gray87", lwd=2,lty=4)
      valor_label_r <- valor_label_r+1.2}}
  
  PUnos <- NULL
  for(i in 1:length(Items)){
    PUnos[i] <- (((Unos[i])/length(Score_FA[Datos$GRADO==g]))*100)
  }
  
  valor_label <- 1.2
  barplot(PUnos, col="gray96", ann=F, axes=F, ylim=c(0,100))
  axis(2,at=seq(0,100,10),labels=paste(seq(0,100,10),"%"),las=1, line=-1.5)
  text(32.5, 115, "Forma 2", f=2, col="lightgoldenrod4")
  text(27.5, 115, "Forma 1", f=2, col="lightgoldenrod3")
  lines(c(30.1,30.1),c(0,120),col='black', lty=1,lwd=1.3) 
  lines(c(30.1,60),c(120,120),lty=1,lwd=4, col="lightgoldenrod2") 
  lines(c(30.1,0),c(120,120),lty=1,lwd=4, col="lightgoldenrod1") 
  mtext(side=3, text = paste("Tomando en cuenta a los estudiantes de",g), line=0.5, cex=0.8, f=2)
  mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=1.5, cex=2, f=2)
  mtext(side=1, text = "Item ID", line=2.5, cex=1.2, f=2)
  axis(1,at=seq(.7,59.7,1.2),labels=seq(1,50,1), line=0, f=2,las=2)
  for(i in 1:length(PRights)){
    if(i==1){
      text(0.7, PUnos[i]+6.5, paste(round(PUnos[i],2),"%"), cex=1.1, f=2, srt=90)
    }else{
      text(0.7+valor_label, PUnos[i]+6.5, paste(round(PUnos[i],2),"%"), cex=1.1, f=2, srt=90)
      valor_label <- valor_label+1.2
    }}
  
  
  
}


######################################################################
######################################################################
# Se segrega el análisis por Grupo

colores <- c("darkseagreen1", "darkseagreen3", "lightcyan2", "lightcyan3")
col_dif <- c("cyan4", "cyan4", "darkcyan", "darkcyan")
mean_A <- c(16.51613, 15.66667, 16.03125, 17.4)
mean_B <- c(8.806452,9.096774,10.1875,12.7931)
mean_T <- c(25.6, 24.53333, 26.21875, 30.46429)
w <- 0
for(g in sort(unique(Datos$GRUPO))){
  w <- w+1
  valor_label <- NULL
  hist(Score_FA[Datos$GRUPO==g], breaks=seq(-.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,8), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
  lines(c(1,1),c(0,8),col='black', lty=4,lwd=2) 
  text(9,7.7, paste("Respondieron la Forma A:"), f=2, cex=1.2)
  text(9,7.3, paste(length(which(is.na(Dicot[(Datos$GRUPO==g)==TRUE,10])==FALSE)), " estudiantes de ",g ), f=2, cex=1.2)
  text(0.5,4, paste(length(which(is.na(Dicot[(Datos$GRUPO==g)==TRUE,10])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(9,6.2, paste("Promedio: ",round(mean_A[w],2)), f=2, cex=1.1)
  text(9,5.5, paste("Máxima: ",round(max(Score_FA[Datos$GRUPO==g]),2)), f=2, cex=1.1)
  text(9,5.2, paste("Mínima: ",round(min(Score_FA[Datos$GRUPO==g]),2)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste(g, ": Puntuaciones en la Forma A"), line=1, cex=2, f=2)
  axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  hist(Score_FB[Datos$GRUPO==g], breaks=seq(-0.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,8), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,8),col='black', lty=4,lwd=2) 
  text(9,7.7, paste("Respondieron la Forma B:"), f=2, cex=1.2)
  text(9,7.3, paste(length(which(is.na(Dicot[(Datos$GRUPO==g)==TRUE,50])==FALSE)), " estudiantes de ",g ), f=2, cex=1.2)
  text(0.5,4, paste(length(which(is.na(Dicot[(Datos$GRUPO==g)==TRUE,50])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(20,6.2, paste("Promedio: ",round(mean_B[w],2)), f=2, cex=1.1)
  text(20,5.5, paste("Máxima: ",round(max(Score_FB[Datos$GRUPO==g]),2)), f=2, cex=1.1)
  text(20,5.2, paste("Mínima: ",round(min(Score_FB[Datos$GRUPO==g]),2)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste(g, ": Puntuaciones en la Forma B"), line=1, cex=2, f=2)
  axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  Completo <- 0
  Total_Completo <- NULL
  Falta_FA <- is.na(Dicot[(Datos$GRUPO==g)==TRUE,10])
  Falta_FB <- is.na(Dicot[(Datos$GRUPO==g)==TRUE,40])
  FA <- Score_FA[Datos$GRUPO==g] 
  FB <- Score_FB[Datos$GRUPO==g]
  for(a in 1:length(Sujetos[Datos$GRUPO==g])){
    if(Falta_FA[a]==TRUE){
      nada <- "nada"}else{
      if(Falta_FB[a]==FALSE){
      Completo <- Completo + 1
      Total_Completo[Completo] <- FA[a]+FB[a]
    }else{
      nada <- "nada"  
    }}}
  
  hist(Total_Completo, breaks=seq(0,50,by=1), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,5), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.2,0.2),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.4,.4),lwd=2,lty=3, col="black"),lines(c(0,1),c(.6,.6),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.8,.8),lwd=2,lty=3, col="black")))
  text(38,4.8, paste(Completo, "de", length(FA),"estudiantes"), f=2, cex=1.2)
  text(38,4.6, paste("respondieron todo el examen"), f=2, cex=1.2)
  text(9,4.2, paste("Promedio: ",round(mean_T[w],2)), f=2, cex=1.1)
  text(9,3.75, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(9,3.5, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación TOTAL -",g), line=1, cex=2, f=2)
  axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.1)
  axis(1,at=c(0.5:50.5),labels=c(1:51), line=-0.5, f=2)
  
  hist(Total_Completo, breaks=seq(0,50,by=5), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,10), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.2,0.2),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.4,.4),lwd=2,lty=3, col="black"),lines(c(0,1),c(.6,.6),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.8,.8),lwd=2,lty=3, col="black")))
  text(43,9, paste(Completo, "de", length(FA),"estudiantes"), f=2, cex=1.2)
  text(43,8.5, paste("respondieron todo el examen"), f=2, cex=1.2)
  text(9,5.8, paste("Promedio: ",round(mean_T[w],2)), f=2, cex=1.1)
  text(9,5, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(9,4.6, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación TOTAL -",g), line=1, cex=2, f=2)
  axis(2,at=seq(0,10,1),labels=seq(0,10,1),las=1, line=-1.1)
  axis(1,at=seq(2.5,50,5),labels=c("0-5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50"), line=-0.5, f=2)
  
  
  P <- NULL
  for(i in 1:length(Items)){
    P[i] <- mean(Dicot[(Datos$GRUPO==g)==TRUE,i+7],na.rm = TRUE)
  }
  
  print(g)
  #print(P)
  print(which(P<.3))
  print(P[40])
  print(P[48])
  print(P[27])
  print(P[31])
  
  Dific <- matrix(data=c(Dificiles,P[Dificiles]),ncol=2)          #Ordenamos el valor P calculado por item en una matriz
  colnames(Dific) <- c("Item", "Dificultad (P)")   #Asignamos un nombre a cada columna
  print(Dific)
  
}


######################################################################
######################################################################
# Se segrega el análisis por Sexo
colores <- c("dodgerblue2","white","deeppink2")
mean_A <- c(17.0339,0,16.01695)
mean_B <- c(11.3,0,9.067797)
mean_T <- c(28.25424,0,25.17544)
w <- 0
for(g in sort(unique(Datos$SEXO))){
  w <- w+1
  valor_label <- NULL
  hist(Score_FA[Datos$SEXO==g], breaks=seq(-.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,8), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,8),col='black', lty=4,lwd=2) 
  text(9,7.5, paste("Respondieron la Forma A:"), f=2, cex=1.2)
  text(9,7.2, paste(length(which(is.na(Dicot[(Datos$SEXO==g)==TRUE,10])==FALSE)), " estudiantes"), f=2, cex=1.2)
  text(0.5,5.5, paste(length(which(is.na(Dicot[(Datos$SEXO==g)==TRUE,10])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(6,5.2, paste("Promedio: ",round(mean_A[w],2)), f=2, cex=1.1)
  text(6,4.5, paste("Máxima: ",round(max(Score_FA[Datos$SEXO==g]),2)), f=2, cex=1.1)
  text(6,4.2, paste("Mínima: ",round(min(Score_FA[Datos$SEXO==g]),2)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuaciones en Forma A"), line=2, cex=2, f=2)
  mtext(side=3, text = paste("(Sexo:",g, ")"), line=0.5, cex=1.2, f=2)
  axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  hist(Score_FB[Datos$SEXO==g], breaks=seq(-0.1,25,by=1), ann=F, axes=F, xlim=c(0,25), col= colores[w], 
       ylim=c(0,8), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.125,0.125),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black")))
  lines(c(0.9,0.9),c(0,8),col='black', lty=4,lwd=2) 
  text(19,7.5, paste("Respondieron la Forma B:"), f=2, cex=1.2)
  text(19,7.2, paste(length(which(is.na(Dicot[(Datos$SEXO==g)==TRUE,50])==FALSE)), " estudiantes"), f=2, cex=1.2)
  text(0.5,5.5, paste(length(which(is.na(Dicot[(Datos$SEXO==g)==TRUE,50])==TRUE)), " estudiante(s) no asistieron"), srt=90, f=2)
  text(21,5.2, paste("Promedio: ",round(mean_B[w],2)), f=2, cex=1.1)
  text(21,4.5, paste("Máxima: ",round(max(Score_FB[Datos$SEXO==g]),2)), f=2, cex=1.1)
  text(21,4.2, paste("Mínima: ",round(min(Score_FB[Datos$SEXO==g]),2)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuaciones en Forma B"), line=2, cex=2, f=2)
  mtext(side=3, text = paste("(Sexo:",g, ")"), line=0.5, cex=1.2, f=2)
  axis(2,at=seq(0,8,1),labels=seq(0,8,1),las=1, line=-1.1)
  axis(1,at=c(0.4:50.4),labels=c(0:50), line=-0.5, f=2)
  
  Completo <- 0
  Total_Completo <- NULL
  Falta_FA <- is.na(Dicot[(Datos$SEXO==g)==TRUE,10])
  Falta_FB <- is.na(Dicot[(Datos$SEXO==g)==TRUE,40])
  FA <- Score_FA[Datos$SEXO==g] 
  FB <- Score_FB[Datos$SEXO==g]
  for(a in 1:length(Sujetos[Datos$SEXO==g])){
    if(Falta_FA[a]==TRUE){
      nada <- "nada"}else{
        if(Falta_FB[a]==FALSE){
          Completo <- Completo + 1
          Total_Completo[Completo] <- FA[a]+FB[a]
        }else{
          nada <- "nada"  
        }}}
  
  hist(Total_Completo, breaks=seq(0,50,by=1), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,7), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.142,0.142),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.284,.284),lwd=2,lty=3, col="black"),lines(c(0,1),c(.426,.426),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.57,.57),lwd=2,lty=3, col="black"),lines(c(0,1),c(.712,.712),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.858,.858),lwd=2,lty=3, col="black")))
  text(43,6.7, paste(Completo, "de", length(FA),"estudiantes"), f=2, cex=1.2)
  text(43,6.3, paste("respondieron el examen completo"), f=2, cex=1.2)
  text(8,5.2, paste("Promedio: ",round(mean_T[w],2)), f=2, cex=1.1)
  text(8,4.2, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(8,3.9, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación TOTAL"), line=1.5, cex=2, f=2)
  mtext(side=3, text = paste("(Sexo:",g, ")"), line=0.5, cex=1.2, f=2)
  axis(2,at=seq(0,7,1),labels=seq(0,7,1),las=1, line=-1.1)
  axis(1,at=c(0.5:50.5),labels=c(1:51), line=-0.5, f=2)
  
  hist(Total_Completo, breaks=seq(0,50,by=5), ann=F, axes=F, xlim=c(0,50), col= colores[w], 
       ylim=c(0,16), 
       panel.first = 
         c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.06,0.06),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.125,.125),lwd=2,lty=3, col="black"),lines(c(0,1),c(.185,.185),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.31,.31),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.372,.372),lwd=2,lty=3, col="black"),lines(c(0,1),c(.435,.435),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.56,.56),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.622,.622),lwd=2,lty=3, col="black"),lines(c(0,1),c(.685,.685),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.81,.81),lwd=2,lty=3, col="black"),
           lines(c(0,1),c(.872,.872),lwd=2,lty=3, col="black"),lines(c(0,1),c(.935,.935),lwd=2,lty=3, col="black")))
  text(41,14.5, paste(Completo, "de", length(FA),"estudiantes"), f=2, cex=1.2)
  text(41,13.5, paste("respondieron el examen completo"), f=2, cex=1.2)
  text(6,12.4, paste("Promedio: ",round(mean_T[w],2)), f=2, cex=1.1)
  text(6,10.4, paste("Máxima: ",max(Total_Completo)), f=2, cex=1.1)
  text(6,9.5, paste("Mínima: ",min(Total_Completo)), f=2, cex=1.1)
  mtext(side=1, text = "Puntuación", line=2.8, cex=1.3, f=2)
  mtext(side=2, text = "Frecuencia", line=1, cex=1.3, f=2)
  mtext(side=3, text = paste("Puntuación TOTAL"), line=1.5, cex=2, f=2)
  mtext(side=3, text = paste("(Sexo:",g, ")"), line=0.5, cex=1.2, f=2)
  axis(2,at=seq(0,16,1),labels=seq(0,16,1),las=1, line=-1.1)
  axis(1,at=seq(2.5,50,5),labels=c("0-5","5-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50"), line=-0.5, f=2)
  
  
  P <- NULL
  for(i in 1:length(Items)){
    P[i] <- mean(Dicot[(Datos$SEXO==g)==TRUE,i+7],na.rm = TRUE)
  }
  
  print(g)
  #print(P)
  print(which(P<.3))
}
