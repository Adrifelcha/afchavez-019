setwd("D:/afchavez/Desktop/Adrifelcha_Work/1-AdaptacionEtapa1/DATOS")
dir()
rm(list=ls())
library("foreign")  
###############
datos <- "unidad_de_calificacion_l.dbf"
data <- read.dbf(datos, as.is = TRUE)
##############

CURP <- data$CURP
RESP_9 <- data$RESP_9
RESP_10 <- data$RESP_10
ID_EXAM9 <- data$ID_EXAM9
ID_EXAM10 <- data$ID_EXAM10
PRESENTO9 <- data$PRESENTO9
PRESENTO10 <- data$PRESENTO10

###############
###############
# Segmentamos los datos por figura

#Total de datos contenidos en cada Figura
n_per_figure <- NULL   
for(figura in sort(unique(ID_EXAM9))){
  n_per_figure[figura] <- sum(ID_EXAM9==figura)
}
n_per_figure

#Establecemos qué datos pertenecen a cada figura
figuras <- unique(ID_EXAM9)

ATP <- c(1:sum(ID_EXAM9=='EDIRPDATP0218'))
Dir <- c((tail(ATP,n=1)+1) : (sum(ID_EXAM9=='EDIRPDDIR0218')+tail(ATP,n=1)))
SGE <- c((tail(Dir,n=1)+1) : (sum(ID_EXAM9=='EDIRPDSGE0218')+tail(Dir,n=1)))
Doc0218 <- c((tail(SGE,n=1)+1) : (sum(ID_EXAM9=='EDIRPDDOC0218')+tail(SGE,n=1)))
Doc0318 <- c((tail(Doc0218,n=1)+1) : (sum(ID_EXAM9=='EDIRPDDOC0318')+tail(Doc0218,n=1)))
SAC <- c((tail(Doc0318,n=1)+1) : (sum(ID_EXAM9=='EDIRPDSAC0218')+tail(Doc0318,n=1)))



################
################
### Número de Respuestas por Cadena
r_9<- NULL
r_10<- NULL

for(i in 1:length(CURP)){
  r_9[i] <- nchar(RESP_9[i])
  r_10[i] <- nchar(RESP_10[i])
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

