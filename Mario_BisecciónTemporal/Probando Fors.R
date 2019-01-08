

#Cargamos los datos
setwd("C:/Users/Alejandro/Desktop/Felisa/Proyectos/Mario_BisecciónTemporal") # Directorio de trabajo
rm(list=ls())  #Reseteamos la consola
dir()          #Imprimimos los archivos del directorio     
archive <-'Datos_Prueba.csv'  # Archivo que contiene los datos a analizar
datos <- read.csv(archive)       # Extraemos los datos del archivo

#Especificamos las variables
Sujeto <- datos$Sujeto
Condicion <- datos$Grupo
TipoSesion <- datos$Condicion
Magnitudes <- c('1v4', '2v8', '3v12', '5v2')
Dia<-datos$Día
Hits <- datos$Corto_enCorto
FA <- datos$Corto_enLargo
Signal <- datos$EnsayosCortos #Deberia ser Hits+Misses
Noise <- datos$EnsayosLargos

Magnitudes <- unique(datos$Grupo)

FA_rate<-FA/Noise
Hit_rate<-Hits/Signal




A<-Hits+FA
B<-Hits-FA

Z <- NULL
X <- NULL
W <- NULL

for(nce in sort(unique(Condicion))){
  Z <- append(Z, sum(Hits[Condicion==nce]))
  X <- (FA[Condicion==nce]*2)
  W <- Z/100
  output <- data.frame(cbind(Z,X,W))
  
  W <- round(W,1)
  
  print(nce)
  print(c('Suma:', W[length(W)]))
  print(c('Hits:', Z[length(Z)]))
  print(c('FA:', X))
}


for(x in sort(unique(datos$Sujeto))){
  print('===================================================')
  print(c('Sujeto:', x))
  print('===================================================')
  for(a in sort(unique(Condicion))){
    print(c('========> Magnitud:', a))
    for(b in sort(unique(TipoSesion))){
      Sesion <- b
      Hits_TS <- sum(Hits[Condicion==a & TipoSesion==b])
      Signal_TS <- sum(Signal[Condicion==a & TipoSesion==b])
      RateTS <- Hits_TS/Signal_TS
      RateTS <- round(RateTS,2)
      valoresTS<- data.frame(cbind(Hits_TS, RateTS, Sesion))   #Acomodamos los valores en un arreglo
      print(valoresTS)
    }}}


Ana <- c(7,10,5,6)
Pepe <- c(10,7,8,2)

R <- NULL
Cas <- NULL

for(i in 1:length(Ana)){
if(Ana[i] <Pepe[i]){
  R[i]<- 'Ana es menor que Pepe'
  Cas[i] <- 2*3 }
  else {
  R[i]<- 'Ana es mayor que Pepe'
  Cas[i]<- 3*3}
  Promedio <- Cas*3
  PAPA <-  Promedio-Cas}

valores <- data.frame(cbind(R, Cas, Promedio, PAPA))
print(valores)  

layout(matrix(1:2,ncol=1, byrow=TRUE))
for(u in 1:length(Cas)){
barplot(c(Cas[u], Promedio[u]))}


vctor <- c(1,2,3,4,5)
valor <- c(9,10,2,3)
Poo <- NULL 

for(i in 1:length(valor)){
  if(valor[i] %in% vctor == TRUE){
    Poo[i]<- 'Holiii' 
}else {
    Poo[i]<- 'Ana es mayor que Pepe'}}

is.element('b', v)
'b' %in% v
## both return TRUE

is.element('f', v)
'f' %in% v

Uno <- c(2,3)
Dos <- c(4,5)

Tres <- c(Uno,Dos)
Tres_B <- c(rbind(Uno, Dos))



Grupo_1<- c(1,3,6,8,10)
Grupo_2 <- c(2,4,5,7,9)
Sujetos <- c(1,2,3,4,5,6,7,8,9,10)

x <- sum(Sujetos)
Lola_ <- NULL

Lola_L<- sum(Sujetos %in% Grupo_1)  
Lola_C<- sum(Sujetos %in% Grupo_1== FALSE)



print(Lola_L)
print(Lola_C)





################################