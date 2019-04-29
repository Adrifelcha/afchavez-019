#############################################################
# Estimacion de algunos de los índices más importantes de la
# TCT y Análisis Facoriales Exploratoris y Confirmatorios para 
# la base de datos empleada para la estimación PLANEA 09 
#############################################################
rm(list=ls())
library(psych)
setwd("C:/Users/Alejandro/Desktop/afchavez19/RIMEDIE/Mat09_2015/Post-Reunión25042018")
Respuestas <- read.csv("PLANEA.09.2015_IDENT_ITEMS.csv")
Respuestas[,1:12] <- NULL


Eje1 <- c(c(1:10),c(26:35))           #Items en el Eje 1: SNPA
Eje2 <- c(c(19:25),43,c(45:50))       #Items en el Eje 2: MI
Eje3 <- c(c(11:18),c(36:42),44)       #Items en el Eje 3: FEM
R_E1 <- Respuestas[,Eje1]
R_E2 <- Respuestas[,Eje2]
R_E3 <- Respuestas[,Eje3]

#Eliminamos los casos con datos faltantes
Datos <- Respuestas[complete.cases(Respuestas),]
D_E1 <- R_E1[complete.cases(R_E1),]
D_E2 <- R_E2[complete.cases(R_E2),]
D_E3 <- R_E3[complete.cases(R_E3),]


#############################################
#    ALFA DE CRONBACH      ##################
#############################################
#Calculamos las alphas
Cronbach_general <- alpha(Datos); Cronbach_general    
# 0.82
Cronbach_Eje1 <- alpha(D_E1); Cronbach_Eje1        
# 0.67
Cronbach_Eje2 <- alpha(D_E2); Cronbach_Eje2
# 0.59
Cronbach_Eje3 <- alpha(D_E3); Cronbach_Eje3
# 0.54

##########################################################
##########################################################
##########################################################
# Análisis Factorial 
#############################################
Eje_1_1 <- factanal(D_E1, factors = 1, method = "mle") ; Eje_1_1 #SMB09
Eje_1_2 <- factanal(D_E1, factors = 2, method = "mle") ; Eje_1_2

Eje_2_1 <- factanal(D_E2, factors = 1, method = "mle") ; Eje_2_1 #SMB22 y 24
Eje_2_2 <- factanal(D_E2, factors = 2, method = "mle") ; Eje_2_2

Eje_3_1 <- factanal(D_E3, factors = 1, method = "mle") ; Eje_3_1 #SMB17 y 15
Eje_3_2 <- factanal(D_E3, factors = 2, method = "mle") ; Eje_3_2


#############################################################
# Ajustamos y quitamos los reactivos que cargaron bajo para 
#############################################################
Respuestas$SMB09 <- NULL
Respuestas$SMB22 <- NULL
Respuestas$SMB24 <- NULL
Respuestas$SMB17 <- NULL
Respuestas$SMB15 <- NULL
write.csv(Respuestas, "PLANEA.09.2015_IDENT_ITEMS_ajustado.csv")



Eje1 <- c(c(1:10),c(26:34))           #Items en el Eje 1: SNPA
Eje2 <- c(c(19:25),40,c(42:45))       #Items en el Eje 2: MI
Eje3 <- c(c(11:18),c(35:39),41)       #Items en el Eje 3: FEM
R_E1 <- Respuestas[,Eje1]
R_E2 <- Respuestas[,Eje2]
R_E3 <- Respuestas[,Eje3]

#Eliminamos los casos con datos faltantes
Datos <- Respuestas[complete.cases(Respuestas),]
D_E1 <- R_E1[complete.cases(R_E1),]
D_E2 <- R_E2[complete.cases(R_E2),]
D_E3 <- R_E3[complete.cases(R_E3),]


#############################################
#    ALFA DE CRONBACH      ##################
#############################################
#Calculamos las alphas
Cronbach_general <- alpha(Datos); Cronbach_general    
# 0.82
Cronbach_Eje1 <- alpha(D_E1); Cronbach_Eje1        
# 0.67
Cronbach_Eje2 <- alpha(D_E2); Cronbach_Eje2
# 0.6
Cronbach_Eje3 <- alpha(D_E3); Cronbach_Eje3
# 0.55

#############################################
# Análisis Factorial 
#############################################
Eje_1_Aj <- factanal(D_E1, factors = 1, method = "mle") ; Eje_1_Aj #SMB09
Eje_2_Aj <- factanal(D_E2, factors = 1, method = "mle") ; Eje_2_Aj #SMB22 y 24
Eje_3_Aj <- factanal(D_E3, factors = 1, method = "mle") ; Eje_3_Aj #SMB17 y 15

