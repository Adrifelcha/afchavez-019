#Codigo Base para Analisis Parametrico y Graficacion (SDT)
#Autor: Adriana F. Chávez De la Peña
#adrifelcha@gmail.com
##########################################################
setwd("D:/afchavez/Desktop/Adrifelcha_Lab25/Proyectos/Kari")
dir()
rm(list=ls())

datos <- "BaseDatos_Kari.csv"   
data <- read.csv(datos)

Hits_Neutro <- NULL
FA_Neutro <- NULL
Hits_Emo <- NULL
FA_Emo <- NULL


for(i in 1:length(data$Participante)){
  a <- 0
  if(data$Condicion[i] == 1){
    
    Hits_Neutro[i] <- data$Hits[i]
    FA_Neutro[i] <- data$FA[i] 
  }else{
    Hits_Emo[i-length(Hits_Neutro)] <- data$Hits[i]
    FA_Emo[i-length(Hits_Neutro)] <- data$FA[i]
  }
}


Total_S <- 50
Total_R <- 50


Fr_N <- NULL
Fr_E <- NULL
HR_N <- NULL
HR_E <- NULL

for(i in 1:length(FA_Neutro)){
  if(FA_Neutro[i]==0){
    Fr_N[i]<-.01
    #FA_rate[i]<-(FA[i]+1)/Noise[i]
  } else {
    Fr_N[i]<-FA_Neutro[i]/Total_R}
  
  if(Hits_Neutro[i]==Total_S){
    HR_N[i]<-0.99
    #Hit_rate[i]<-(Hits[i]-1)/Signal[i]
  } else {
    HR_N[i]<-Hits_Neutro[i]/Total_S}}

for(i in 1:length(FA_Emo)){
  if(FA_Emo[i]==0){
    Fr_E[i]<-.01
    #FA_rate[i]<-(FA[i]+1)/Noise[i]
  } else {
    Fr_E[i]<-FA_Emo[i]/Total_R}
  
  if(Hits_Emo[i]==Total_S){
    HR_E[i]<-0.99
    #Hit_rate[i]<-(Hits[i]-1)/Signal[i]
  } else {
    HR_E[i]<-Hits_Neutro[i]/Total_S}}



#####Estimación Paramétrica

## Grupo: Lectura Neutra
k_N<-qnorm(1-Fr_N,0,1)   #Calculamos la localizacion del Criterio
d_N<-qnorm(HR_N,0,1)-qnorm(Fr_N,0,1)     #Calculamos d'
c_N<-k_N-(d_N/2)                                  #Calculamos el Sesgo c
beta_N<-dnorm(k_N,d_N,1)/dnorm(k_N,0,1)             #Calculamos el Sesgo Beta

## Grupo: Lectura Emotiva
k_E<-qnorm(1-Fr_E,0,1)   #Calculamos la localizacion del Criterio
d_E<-qnorm(HR_E,0,1)-qnorm(Fr_E,0,1)     #Calculamos d'
c_E<-k_E-(d_E/2)                                  #Calculamos el Sesgo c
beta_E<-dnorm(k_E,d_E,1)/dnorm(k_E,0,1)             #Calculamos el Sesgo Beta



d_N<-round(d_N,5)   #Redondeamos el valor de d' a 5 decimales para facilitar su representacion
k_N<-round(k_N,5)   #Redondeamos el valor del criterio a 5 decimales para facilitar su representacion
c_N<-round(c_N,5)   #Redondeamos el valor del Sesgo c a 5 decimales para facilitar su representacion
beta_N<-round(beta_N,5) #Redondeamos el valor del Sesgo Beta a 5 decimales para facilitar su representacion


d_E<-round(d_E,5)    #Redondeamos el valor de d' a 5 decimales para facilitar su representacion
k_E<-round(k_E,5)    #Redondeamos el valor del criterio a 5 decimales para facilitar su representacion
c_E<-round(c_E,5)     #Redondeamos el valor del Sesgo c a 5 decimales para facilitar su representacion
beta_E<-round(beta_E,5) #Redondeamos el valor del Sesgo Beta a 5 decimales para facilitar su representacion


########################################
#  P L O T E A M O S           #########
########################################
soporte <- seq(-6,6,.05)        #Especificamos el Soporte de nuestras distribuciones

for(a in 1:length(d_N)){
d_ruido_N <- dnorm(soporte,0,1)   #Definimos nuestra distribucion de Ruido de acuerdo a la teoria (Media=0 y DV=1)
d_senal_N <- dnorm(soporte,d_N[a],1)   #Definimos la distribucion de Se??al, con media en d'

d_ruido_E <- dnorm(soporte,0,1)   #Definimos nuestra distribucion de Ruido de acuerdo a la teoria (Media=0 y DV=1)
d_senal_E <- dnorm(soporte,d_E[a],1)   #Definimos la distribucion de Se??al, con media en d'

layout(matrix(c(1:2), ncol=1))

plot(soporte,d_ruido_N,type='l')             #Dibujamos la distribucion de ruido
lines(soporte,d_senal_N,type='l',col='blue') #Dibujamos la distribucion de Se??al
abline(v=k_N[a],col='red')                      #Dibujamos el criterio
abline(v=d_N[a]/2,col='blue',lty=2)             #Se??alamos la localizacion Optima (sin sesgo) del criterio

plot(soporte,d_ruido_E,type='l')             #Dibujamos la distribucion de ruido
lines(soporte,d_senal_E,type='l',col='blue') #Dibujamos la distribucion de Se??al
abline(v=k_E[a],col='red')                      #Dibujamos el criterio
abline(v=d_E[a]/2,col='blue',lty=2)             #Se??alamos la localizacion Optima (sin sesgo) del criterio

text(-4,0.39,paste("K = ",k_N[a]))  #Imprimimos la localizacion del criterio 
text(-5,0.34,paste("C = ",c_N[a]))  #Imprimimos el valor del Sesgo C
text(-4,0.29,paste("D' = ",d_N[a])) #Imprimimos el valor de la distancia entre las medias (D')
text(-5,0.24,paste("Hit Rate = ",HR_N[a]))            #Especificamos la Tasa de Hits
text(-4,0.19,paste("False alarm Rate = ",Fr_N[a]))   #Especificamos la Tasa de Falsas Alarmas
}

############ Curvas ROC

# Una vez que hemos computado el valor de la d' que separa las distribuciones de ruido y señal
# podemos trazar una curva ROC, que nos describa todas las combinaciones posibles de tasa de
# Hits y Falsas Alarmas, dada todas las posibles combinaciones del criterio. 

#Para trazar la Curva ROC, necesitamos crear un Ciclo For:


hits <- c()   #Creamos un arreglo vacío, que vamos a ir llenando con el Ciclo For, para las tasas de Hits
falarms <- c()  #Creamos un arreglo vacío para las tasas de Falsas Alarmas
bias_c <- seq(-10,10,0.1) 
d_null <- 0  #Como referencia, vamos a incluir una curva ROC para una d' de 0. El sistema a evaluar será juzgado como 'más preciso' conforme su ROC se aleje de ésta función de identidad.
hits_na <- c()     #Creamos un arreglo vacío para los hits en  d' 0
falarms_na <- c()  # Creamos un arreglo vacío para las falsas alarmas en d' 0


for (i in 1:length(bias_c)){                     #Creamos un For donde, para cada posible valor del sesgo C (que relaciona directamente d' con el criterio)
  hits[i] <- pnorm((-d/2)-bias_c[i])             #se compute la proporción de la distribución de señal que cae sobre el criterio
  falarms[i] <- pnorm((d/2)-bias_c[i])           # y la proporción de la distribución de ruido.
  hits_na[i] <- pnorm((d_null/2)-bias_c[i])      #Para referencia, realizamos el mismo cómputo para la d' de 0
  falarms_na[i] <- pnorm((-d_null/2)-bias_c[i])
}

plot(fa_rate,h_rate, pch=16, col='deepskyblue4', xlim=c(0,1), ylim=c(0,1), xlab='F.A. Rate', ylab='Hit Rate')     #Ploteamos las tasas de hits y falsas alarmas obervadas como un punto en el espacio
lines(hits,falarms,lwd=2,col='deepskyblue2')        #Dibujamos la curva ROC correspondiente a la d' de nuestro sistema evaluado
lines(hits_na,falarms_na,lwd=1,col='black', lty=2)  #Dibujamos la función de identidad, que corresponde a una d' de 0 (Donde las distribuciones de ruido y señal se empalman por completo)
lines(c(0.38, 0.48),c(0.2,0.2), lwd=2, lty=1, col="deepskyblue3")      
points(0.43,0.1, lty=3, pch=16, col='deepskyblue4')
text(0.5, 0.2, labels="All possible trade-offs between Hit & F.A. rates given the d'", offset=0, cex = 0.7, pos=4)
text(0.5, 0.1, labels="Observed Hit & F.A. rates given the used criterion", offset=0, cex = 0.7, pos=4)
text(0.85, 0.83, labels="d' = 0", offset=0, cex = 0.8, pos=4)
text(fa_rate-0.13, h_rate+0.02, paste("d' =", d), offset=0, cex = 0.8, pos=4)
title('ROC')



