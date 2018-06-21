setwd("D:/afchavez/Desktop/Adrifelcha_Work/1-AdaptacionEtapa1/DATOS")
dir()
rm(list=ls())
library("foreign")  
###############
datos <- "unidad_de_calificacion_l.dbf"
data <- read.dbf(datos, as.is = TRUE)
##############


###############
###############
# Segmentamos los datos por figura

n_per_figure <- NULL   #Total de datos contenidos en cada Figura
for(figura in sort(unique(data$ID_EXAM9))){
  n_per_figure[figura] <- sum(data$ID_EXAM9==figura)
}
n_per_figure

#Establecemos qué datos pertenecen a cada figura
figuras <- sort(unique(data$ID_EXAM9))

ATP <- c(1:sum(data$ID_EXAM9=='EDIRPDATP0218'))
Dir <- c((tail(ATP,n=1)+1) : (sum(data$ID_EXAM9=='EDIRPDDIR0218')+tail(ATP,n=1)))
Doc0218 <- c((tail(Dir,n=1)+1) : (sum(data$ID_EXAM9=='EDIRPDDOC0218')+tail(Dir,n=1)))
Doc0318 <- c((tail(Doc0218,n=1)+1) : (sum(data$ID_EXAM9=='EDIRPDDOC0318')+tail(Doc0218,n=1)))
SAC <- c((tail(Doc0318,n=1)+1) : (sum(data$ID_EXAM9=='EDIRPDSAC0218')+tail(Doc0318,n=1)))
SGE <- c((tail(SAC,n=1)+1) : (sum(data$ID_EXAM9=='EDIRPDSGE0218')+tail(SAC,n=1)))


################
################
### Número de Respuestas por Cadena
r_9<- NULL
r_10<- NULL

for(i in 1:length(data$CURP)){
  r_9[i] <- nchar(data$RESP_9[i])
  r_10[i] <- nchar(data$RESP_10[i])
}


sort(unique(r_9[ATP]))
sort(unique(r_9[Dir]))
sort(unique(r_9[Doc0218]))
sort(unique(r_9[Doc0318]))
sort(unique(r_9[SAC]))
sort(unique(r_9[SGE]))

sort(unique(r_10[ATP]))
sort(unique(r_10[Dir]))
sort(unique(r_10[Doc0218]))
sort(unique(r_10[Doc0318]))
sort(unique(r_10[SAC]))
sort(unique(r_10[SGE]))