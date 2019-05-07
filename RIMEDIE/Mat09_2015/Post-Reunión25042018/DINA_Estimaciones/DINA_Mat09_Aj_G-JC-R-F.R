#############################################################
# Estimacion del modelo DINA para la aplicación de la prueba
# PLANEA ELCE 2015 con las especificaciones del modelo 
# empleadas por el Dr. Guaner Rojas para la prueba PLANEA 06
#
#Se incluye también una versión más "rigida" del modelo
#############################################################
rm(list=ls())                       #
library("CDM")
setwd("C:/Users/Alejandro/Desktop/afchavez19/RIMEDIE/Mat09_2015/Post-Reunión25042018/DINA_Estimaciones")


Respuestas <- read.csv("PLANEA.09.2015_IDENT_ITEMS_ajustado.csv")
Respuestas$ESCUELA <- NULL
Respuestas$ID_ENT <- NULL 
Respuestas$NOM_ENT <- NULL
Respuestas$ID_SERV <- NULL    #Se eliminan las columnas innecesarias
Respuestas$SERV <- NULL
Respuestas$MARGINC <- NULL
Respuestas$TAM_LOC_SEC <- NULL
Respuestas$SEXO <- NULL
Respuestas$EDAD_AC <- NULL
Respuestas$MES_NAC <- NULL
Respuestas$EDAD <- NULL
Respuestas$ï..ALUMNO <- NULL

Qmat <- read.csv("QMatriz_MAT09_ajustada.csv")       

#Se utiliza la Matriz Q para identificar los reactivos que corresponden a cada Eje
# para realizar la estimación independiente de cada Eje

Eje1 <- NULL
Eje2 <- NULL
Eje3 <- NULL
e1 <- 0
e2 <- 0
e3 <- 0

for(i in 1:nrow(Qmat)){              
  if(Qmat$Eje[i]=="3 - FEM"){
    e3 <- e3 + 1
    Eje3[e3] <- Qmat$Item[i]
  }else{
    if(Qmat$Eje[i]=="2 - MI"){
      e2 <- e2+1
      Eje2[e2] <- Qmat$Item[i]
    }else{
      e1 <- e1+1
      Eje1[e1] <- Qmat$Item[i]
    }}}
Qmat$Item <- NULL
Qmat$Cod_Ram <- NULL         #Se cancelan las columnas innecesarias de la MatrizQ
Qmat$Eje <- NULL
Q_E1 <- Qmat[Eje1,1:12]
Q_E2 <- Qmat[Eje2,13:22]
Q_E3 <- Qmat[Eje3,23:30]
R_E1 <- Respuestas[,Eje1]
R_E2 <- Respuestas[,Eje2]
R_E3 <- Respuestas[,Eje3]
#View(Qmat)


###########################################################
# DINA

#EJE 1
d1 <- din(R_E1, q.matr = Q_E1, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

write.csv(d1$item, "E1SNPA_itempars_lax.csv")
write.csv(d1$skill.patt, "E1SNPA_skilpatt_lax.csv")
write.csv(data.frame(d1$pattern), "E1SNPA_fempostpattern_lax.csv",row.names = F)


#EJE 2
d2 <- din(R_E2, q.matr = Q_E2, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

write.csv(d2$item, "E2MI_itempars_lax.csv")
write.csv(d2$skill.patt, "E2MI_skilpatt_lax.csv")
write.csv(data.frame(d2$pattern), "E2MI_minpostpattern_lax.csv", row.names = F)



#EJE 3
d3 <- din(R_E3, q.matr = Q_E3, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

write.csv(d3$item, "E3FEM_itempars_lax.csv")
write.csv(d3$skill.patt, "E3FEM_skilpatt_lax.csv")
write.csv(data.frame(d3$pattern), "E3FEM_spapostpattern_lax.csv",row.names = F)

##########################