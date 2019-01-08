##########################################################
# Codigo base para el análisis de datos obtenidos en el experimento presentado por Manu!
# con los parámetros y estadísticos expuestos por Stainslaw (1999) y Gescheider (2013)
##########################################################
#Referencia 1: Stainslaw & Todorov (1999), Calculation of signal detection theory measures.
#Referencia 2: Gescheider, George A. (2013). "Psychophysics: The Fundamentals". 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Codigo hecho por: Adriana F. Chávez De la Peña
#Contacto: adrifelcha@gmail.com
##########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


####################################
# # # # # # # #  Parte I: Cargamos los datos
####################################

setwd("C:/Users/Alejandro/Desktop/Felisa/Proyectos/ManuZamora_Parametrico") # Directorio de trabajo
rm(list=ls())  #Reseteamos la consola
archive <-'Manuel.csv'  #Señalamos el archivo que contiene los datos a analizar
datos <- read.csv(archive)             #Extraemos los datos del archivo


########################################
# # # # # # # #  Parte II
# # # # # # # #  Especificamos variables
########################################
Sujeto <- datos$Sujeto         #Del archivo 'datos', identificamos los datos en la columna 'Sujeto' como variable Sujeto
Hits <- datos$Corto_Corto       #Del archivo 'datos', identificamos los datos en la columna 'Grupo' como variable Condición
FAlarmas <- datos$Corto_Largo  #etc, etc, etc
Ensayos<-datos$Ensayos


FA_rate <- NULL       #Creamos cuatro arreglos vacíos para los parámetros que vamos a llenar
Hit_rate <- NULL      #con ciclos "if" (ver más adelante)
A <- NULL
B <- NULL

FA_rate <- FAlarmas/Ensayos  #Computamos Tasas de Falsas Alarmas y Hits
Hit_rate <- Hits/Ensayos

for(i in 1:length(Sujeto)){          #Corrección para los casos en que No hayan Falsas Alarmas
  if(FAlarmas[i]==0){
    FA_rate[i]<-.01
  } else {
    FA_rate[i]<-FAlarmas[i]/Ensayos[i]}
  
  if(Hits[i]==Ensayos[i]){           #Corrección para los casos en que Siempre haya Hits
    Hit_rate[i]<-0.99
    #Hit_rate[i]<-(Hits[i]-1)/Signal[i]
  } else {
    Hit_rate[i]<-Hits[i]/Ensayos[i]}}
  
  
############################################
# # # # # # # #  Parte III
# # # # # # # #  Computamos los parámetros
############################################
  
for(i in 1:length(Sujeto)){
  #Dprima computada segun la formula presentada por Gesheider, (2013) y Stainslaw (1999)
  d<- qnorm(Hit_rate,0,1)-qnorm(FA_rate,0,1)
  #D' = Distancia en unidades de desviación estándar (puntajes Z) entre las distribuciones de Ruido y Señal
  #D' forma parte del análisis paramétrico e implica que se acepta que hay distribuciones normales equivariantes involucradas
  
  #A-Dprima computada segun la formula de Stainslaw, (1999)
  Ad<-pnorm(d/(sqrt(2)))
  #A_D'= Area bajo la curva ROC computada aceptando los supuestos de normalidad y equivarianza en las distribuciones R y S
  
  #A' de acuerdo con la formula presentada por Stainslaw, (1999) de manera diferencial para los casos donde H<FA y H>FA
  if(FA_rate[i] > Hit_rate[i]){
    A[i] <- 0.5- ( ((FA_rate[i]-Hit_rate[i])*(1+FA_rate[i]-Hit_rate[i])) / ((4*FA_rate[i])*(1-Hit_rate[i])) )
  } else {
    A[i] <- 0.5+ ( ((Hit_rate[i]-FA_rate[i])*(1+Hit_rate[i]-FA_rate[i])) / ((4*Hit_rate[i])*(1-FA_rate[i])) )}
  #A' = Medida NO paramétrica de sensibilidad, definida como el area bajo la curva ROC trazada SIN asumir normalidad o
  #equivarianza en las distribuciones de Ruido y Señal
  
  #Criterio (K) de acuerdo con la fórmula de Gescheider, (2013)
  k<-qnorm(1-FA_rate,0,1)               
  #K = La localización, en unidades de desviaciones estándar, del criterio de elección sobre el eje de decisión
  #que determina la emisión de juicios a favor, o en contra, de la detección de la Señal
  
  #Beta (de acuerdo a la fórmula de Gescheider, 2013)
  beta<-dnorm(k,d,1)/dnorm(k,0,1)             
  #Beta = Medida paramétrica de sesgo que calcula el radio de la densidad de probabilidad en el criterio
  
  #Sesgo C calculado de acuerdo con la formula de Gescheider (2013)
  c<-k-(d/2)                                
  #C = Medida paramétrica de sesgo que computa la distancia entre la localización del criteiro y el punto en que se
  #intercectan las distribuciones S y R (cuando el sesgo es nulo)
  
  
  #B" de acuerdo con el metodo de Grier (expuesto en Stainslaw. 1999), para los casos en que H>FA o H<FA
  if(FA_rate[i] > Hit_rate[i]){
    B[i] <- (((FA_rate[i]*(1-FA_rate[i]))-(Hit_rate[i]*(1-Hit_rate[i])))/((FA_rate[i]*(1-FA_rate[i]))+(Hit_rate[i]*(1-Hit_rate[i]))))
  } else {
    B[i] <- (((Hit_rate[i]*(1-Hit_rate[i]))-(FA_rate[i]*(1-FA_rate[i])))/((Hit_rate[i]*(1-Hit_rate[i]))+(FA_rate[i]*(1-FA_rate[i]))))}
  #B" = Medida NO paramétrica que computa la distancia entre la localización del criterio en el espacio ROC y 
  # la diagonal negativa, que define la localización esperada en un criterio sin sesgo alguno.
  
  
  # # # # # # # # Redondeamos los valores computados a 3 dígitos
  
  A_prima <- round(A,3)
  B_biprima <- round(B,3)
  Beta<-round(beta,3)
  D_prima <- round(d,2)
  Sesgo_C<-round(c,3)
  A_dprima <- round(Ad,3)
  Hit_rate <- round(Hit_rate,3)
  FA_rate <- round(FA_rate,3)
}

















###########################################################################################################
###########################################################################################################
# # # # # # # #  Parte IV
# # # # # # # #  Imprimimos los valores computados
##########################################################################################################

############################
#OPCION 1:  TODOS LOS DATOS 
#Imprimimos TODOS los parámetros computados, distinguiendo con Variables Dummies 1) El sujeto a presentar;
#2) La magnitud probada #3) El tipo de sesión (LB o M) y 4) El día
#valores<- data.frame(cbind(Sujeto, Magnitudes, Sesiones, Dia, D_prima, A_dprima, A_prima, Beta, Sesgo_C, B_biprima))   #Acomodamos los valores en un arreglo
valores<- data.frame(cbind(Sujeto,Hits, Hit_rate, FAlarmas, FA_rate, D_prima, A_dprima, A_prima, Beta, Sesgo_C, B_biprima))   #Acomodamos los valores en un arreglo
colnames(valores) <- c("Sujeto","Hits", "Tasa Hits", "F.A.", "Tasa F.A.","D'","A_d'","A'","Beta","C","B''")
options(max.print=10000000)
print(valores)



layout(matrix(1:6,ncol=3, byrow=TRUE))
for(i in 1:length(Sujeto)){
barplot(c(Hits[i],FAlarmas[i]), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,15), axes = FALSE, col =c("darkgreen", "darkred"))
axis(1,at=c(0.72,1.9),labels=c("Tasa Hits", "Tasa F.A"), font=2)
axis(2,at=c(0, 5, 10, 15),labels=c("0", "5", "10", "15"),las=1)
text(0.72,(Hits[i]/2),paste(Hits[i]),cex=1.2,col='white',f=2)
text(1.9,(FAlarmas[i]/2),paste(FAlarmas[i]),cex=1.2,col='white',f=2)
mtext(paste("Participante No.",Sujeto[i]),1,cex=1.3, line=3, f=2)
}

eje_x <- c(0.7,1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5, 12.7, 13.9, 15.1)

layout(matrix(1:2,ncol=1, byrow=TRUE))
barplot(A_prima, main = "", xlab = "", ylab = "", font.lab=2, ylim=c(0,1), axes=FALSE, col="skyblue")
axis(1,at=eje_x,labels=c("1", "2", "3", "4", "5", "6", "7","8","9", "10", "11", "12", "13"), font=2, line=5)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
for(i in 1:length(eje_x)){
  text(eje_x[i],0.5,paste(A_prima[i]),cex=0.9,col='white',f=2)}
mtext(paste("A'"),3,cex=1.3, line=2, f=2)
mtext(paste("Sujeto"),1,cex=1.2, line=3, f=2)

barplot(B_biprima, main = "", xlab = "", ylab = "", font.lab=2, ylim=c(-1,1), axes=FALSE, col="darkgreen")
#axis(1,at=c(0.7,1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5, 12.7, 13.9, 15.1),labels=c("1", "2", "3", "4", "5", "6", "7","8","9", "10", "11", "12", "13"), font=2)
for(i in 1:length(eje_x)){
  text(eje_x[i],-.9,paste(B_biprima[i]),cex=0.9,col='black',f=2)}
axis(2,at=c(0, 0.25, 0.5, 0.75, 1, -0.25, -0.5, -0.75, -1),labels=c("0", "0.25", "0.5", "0.75", "1", "-0.25", "-0.5", "-0.75", "-1"),las=1)
mtext(paste("B''"),1,cex=1.3, line=3, f=2)






########################################
# OPCION 2: Promedio de los parámetros (Contiene error!!!)
promedios <- data.frame(cbind(round(mean(Hits),3), round(mean(Hit_rate),3), round(mean(FAlarmas),3), round(mean(FA_rate),3), round(mean(D_prima),3), round(mean(A_dprima),3), round(mean(A_prima),3), round(mean(Beta),3), round(mean(Sesgo_C),3), round(mean(B_biprima),3)))
colnames(promedios) <- c("Hits", "Tasa Hits", "F.A.", "Tasa F.A.", "D'", "A_d'", "A'", "Beta", "C", "B''")
print(promedios)

########################################
# OPCION 3: Desempeño del Grupo (SUPER SUJETO)
# En vez de promediar los parámetros, estimados a partir de tasas, (lo cual mete muchísimo ruido al estimado final)
# se computa una "super tasa" que contempla el desempeño de todos los participantes

Super_Hits <- sum(Hits)
Super_FA <- sum(FAlarmas)
Super_Ensayos <- 15*length(Sujeto)

S_Hit_rate <- Super_Hits/Super_Ensayos
S_FA_rate <- Super_FA/Super_Ensayos

S_d<- qnorm(S_Hit_rate,0,1)-qnorm(S_FA_rate,0,1)
S_Ad<-pnorm(S_d/(sqrt(2)))

if(S_FA_rate > S_Hit_rate){
  S_A <- 0.5- ( ((S_FA_rate-S_Hit_rate)*(1+S_FA_rate-S_Hit_rate)) / ((4*S_FA_rate)*(1-S_Hit_rate)) )
} else {
  S_A <- 0.5+ ( ((S_Hit_rate-S_FA_rate)*(1+S_Hit_rate-S_FA_rate)) / ((4*S_Hit_rate)*(1-S_FA_rate)) )}

S_k<-qnorm(1-S_FA_rate,0,1)               
S_beta<-dnorm(S_k,S_d,1)/dnorm(S_k,0,1)             
S_c<-S_k-(S_d/2)                                

if(S_FA_rate > S_Hit_rate){
  S_B <- (((S_FA_rate*(1-S_FA_rate))-(S_Hit_rate*(1-S_Hit_rate)))/((S_FA_rate*(1-S_FA_rate))+(S_Hit_rate*(1-S_Hit_rate))))
} else {
  S_B <- (((S_Hit_rate*(1-S_Hit_rate))-(S_FA_rate*(1-S_FA_rate)))/((S_Hit_rate*(1-S_Hit_rate))+(S_FA_rate*(1-S_FA_rate))))}

S_A_prima <- round(S_A,3)
S_B_biprima <- round(S_B,3)
S_Beta<-round(S_beta,3)
S_D_prima <- round(S_d,2)
S_Sesgo_C<-round(S_c,3)
S_A_dprima <- round(S_Ad,3)
S_Hit_rate <- round(S_Hit_rate,3)
S_FA_rate <- round(S_FA_rate,3)

super<- data.frame(cbind(Super_Hits, S_Hit_rate, Super_FA, S_FA_rate, S_D_prima, S_A_dprima, S_A_prima, S_Beta, S_Sesgo_C, S_B_biprima))   #Acomodamos los valores en un arreglo
colnames(super) <- c("Hits", "Tasa Hits", "F.A.", "Tasa F.A.","D'","A_d'","A'","Beta","C","B''")
print(super)


layout(matrix(1:4,ncol=2, byrow=FALSE))
barplot(c(mean(Hit_rate),mean(FA_rate)), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("darkgreen", "darkred"))
axis(1,at=c(0.72,1.9),labels=c("Tasa Hits", "Tasa F.A"), font=2)
axis(2,at=c(0, .25, .5, .75, 1),labels=c("0", ".25", ".5", ".75", "1"),las=1)
text(0.72,(mean(Hits)/2),paste(Hits[i]),cex=1.2,col='white',f=2)
text(1.9,(mean(FAlarmas)/2),paste(FAlarmas[i]),cex=1.2,col='white',f=2)
mtext(paste("Promedio Bruto"),3,cex=1.3, line=1.5, f=2)

barplot(c(mean(A_prima),mean(B_biprima)), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkgreen", "darkred"))
axis(1,at=c(0.72,1.9),labels=c("Tasa Hits", "Tasa F.A"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1, -0.25, -0.5, -0.75, -1),labels=c("0", "0.25", "0.5", "0.75", "1", "-0.25", "-0.5", "-0.75", "-1"),las=1)
text(0.72,(Hits[i]/2),paste(Hits[i]),cex=1.2,col='white',f=2)
text(1.9,(FAlarmas[i]/2),paste(FAlarmas[i]),cex=1.2,col='white',f=2)
mtext(paste("Participante No.",Sujeto[i]),1,cex=1.3, line=3, f=2)

barplot(c(S_Hit_rate,S_FA_rate), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("darkgreen", "darkred"))
axis(1,at=c(0.72,1.9),labels=c("Tasa Hits", "Tasa F.A"), font=2)
axis(2,at=c(0, .25, .5, .75, 1),labels=c("0", ".25", ".5", ".75", "1"),las=1)
text(0.72,(Hits[i]/2),paste(Hits[i]),cex=1.2,col='white',f=2)
text(1.9,(FAlarmas[i]/2),paste(FAlarmas[i]),cex=1.2,col='white',f=2)
mtext(paste("Super Participante"),3,cex=1.3, line=1.5, f=2)

barplot(c(S_A_prima,S_B_biprima), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkgreen", "darkred"))
axis(1,at=c(0.72,1.9),labels=c("Tasa Hits", "Tasa F.A"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1, -0.25, -0.5, -0.75, -1),labels=c("0", "0.25", "0.5", "0.75", "1", "-0.25", "-0.5", "-0.75", "-1"),las=1)
text(0.72,(Hits[i]/2),paste(Hits[i]),cex=1.2,col='white',f=2)
text(1.9,(FAlarmas[i]/2),paste(FAlarmas[i]),cex=1.2,col='white',f=2)
mtext(paste("Participante No.",Sujeto[i]),1,cex=1.3, line=3, f=2)


##########

layout(matrix(1:4,ncol=2, byrow=FALSE))
barplot(c(mean(Hit_rate),S_Hit_rate), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("darkgreen", "forestgreen"))
axis(1,at=c(0.72,1.9),labels=c("Bruto", "Super Sujeto"), font=2)
axis(2,at=c(0, .25, .5, .75, 1),labels=c("0", ".25", ".5", ".75", "1"),las=1)
text(0.72,(mean(Hit_rate)/2),paste(round(mean(Hit_rate),3)),cex=1.2,col='white',f=2)
text(1.9,(S_Hit_rate/2),paste(S_Hit_rate),cex=1.2,col='white',f=2)
mtext(paste("Tasa de Hits"),3,cex=1.3, line=1.5, f=2)

barplot(c(mean(A_prima),S_A_prima), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("darkblue", "skyblue"))
axis(1,at=c(0.72,1.9),labels=c("Bruto", "Super Sujeto"), font=2)
axis(2,at=c(0, .25, .5, .75, 1),labels=c("0", ".25", ".5", ".75", "1"),las=1)
text(0.72,(mean(A_prima)/2),paste(round(mean(A_prima),3)),cex=1.2,col='white',f=2)
text(1.9,(S_A_prima/2),paste(S_A_prima),cex=1.2,col='white',f=2)
mtext(paste("A' - Sensibilidad"),3,cex=1.3, line=1.5, f=2)

barplot(c(mean(FA_rate),S_FA_rate), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("firebrick1", "firebrick4"))
axis(1,at=c(0.72,1.9),labels=c("Bruto", "Super Sujeto"), font=2)
axis(2,at=c(0, .25, .5, .75, 1),labels=c("0", ".25", ".5", ".75", "1"),las=1)
text(0.72,(mean(FA_rate)+.1),paste(round(mean(FA_rate),3)),cex=1.2,col='black',f=2)
text(1.9,(S_FA_rate+.1),paste(S_FA_rate),cex=1.2,col='black',f=2)
mtext(paste("Tasa F.A."),3,cex=1.3, line=1.5, f=2)

barplot(c(mean(B_biprima),S_B_biprima), main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkolivegreen4", "darkolivegreen2"))
axis(1,at=c(0.72,1.9),labels=c("Bruto", "Super Sujeto"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1, -0.25, -0.5, -0.75, -1),labels=c("0", "0.25", "0.5", "0.75", "1", "-0.25", "-0.5", "-0.75", "-1"),las=1)
text(0.72,(mean(B_biprima)+.15),paste(round(mean(B_biprima),3)),cex=1.2,col='black',f=2)
text(1.9,(S_B_biprima+.15),paste(S_B_biprima),cex=1.2,col='black',f=2)
mtext(paste("B'' - Sesgo"),3,cex=1.3, line=1.5, f=2)