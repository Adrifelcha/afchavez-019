#############################################################
# Codigo Para analizar los datos obtenidos por SCS
# calibrando con la base nacional del ELSEN 2015
# Codigo base elaborado por el Dr. Guaner Rojas Rojas
# modificado por Adriana Felisa Chávez
#
#El item PMB02 se omite del análisis porque no fue bien aplicado
#############################################################
rm(list=ls())                       #Limpiamos variables
library("CDM")
setwd("C:/Users/Alejandro/Desktop/afchavez19/Lancaster/Reporte_03  Calibración Nacional")


#Cargamos la base de ELSEN + 127 datos de Sandy SIN CONTAR el reactivo 22 (PMB02)
datos <- read.csv("Datos_Nacionales_sin22_Sandy.csv")
R_E1 <- datos[,c(c(1:8),c(26:33))]
R_E2 <- datos[,c(c(9:14),c(34:38))]
R_E3 <- datos[,c(c(15:25),c(39:49))]

#Segmentamos la Matriz Q subida por el Dr. Guaner en tres sub-matrices Q
qmat <- read.csv("MatrizQ_DrGuaner_sin22.csv")
Q_E1 <- qmat[1:16,1:12] 
Q_E2 <- qmat[17:27,1:10]
Q_E3 <- qmat[28:49,1:13]


#############################################
# Se corren los modelos
# Los objetos DINA con mayúsculas corresponden al código de Fel
# Los objetos d1, d2 y d3 corresponden al codigo del Dr. Guaner
# Por cada estimación se guardan:
# Las estimaciones paramétricas (itemparameters)
# El nivel de habilidad estimado para toda la muestra (skilpatterns)
# Las estimaciones individuales (postpattern)

DINA_1 <- din(R_E1, Q_E1, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E1)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E1)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

d1 <- din(R_E1, q.matr = Q_E1, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)


write.csv(DINA_1$item, "E1_FEM_itemparameters_Fel.csv")
write.csv(DINA_1$skill.patt, "E1_FEM_skilpatterns_Fel.csv")
write.csv(data.frame(DINA_1$pattern), "E1_FEM_postpattern_Fel.csv",row.names = F)

write.csv(d1$item, "e1_itempars_fem_DINA-Guaner.csv")
write.csv(d1$skill.patt, "e1_skilpatt_fem_DINA-Guaner.csv")
write.csv(data.frame(d1$pattern), "e1_fempostpattern_DINA-Guaner.csv",row.names = F)



DINA_2 <- din(R_E2, Q_E2, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E2)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E2)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

d2 <- din(R_E2, q.matr = Q_E2, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

write.csv(DINA_2$item, "E2_MI_itemparameters_Fel.csv")
write.csv(DINA_2$skill.patt, "E2_MI_skilpatterns_Fel.csv")
write.csv(data.frame(DINA_2$pattern), "E2_MI_postpattern_Fel.csv",row.names = F)

write.csv(d2$item, "e2_itempars_min_DINA-Guaner.csv")
write.csv(d2$skill.patt, "e2_skilpatt_min_DINA-Guaner.csv")
write.csv(data.frame(d2$pattern), "e2_minpostpattern_DINA-Guaner.csv", row.names = F)






DINA_3 <- din(R_E3, Q_E3, skillclasses=NULL,
              conv.crit=0.001, dev.crit=10^(-2), maxit=500,
              constraint.guess=NULL, constraint.slip=NULL,
              guess.init= rep(0.25, ncol(R_E3)), slip.init=guess.init,
              guess.equal=FALSE, slip.equal=FALSE, zeroprob.skillclasses=NULL,
              weights=rep(1, nrow(R_E3)), rule="DINA",
              wgt.overrelax=0, wgtest.overrelax=FALSE, param.history=FALSE,
              seed=125, progress=TRUE, guess.min=0, slip.min=0, guess.max=1, slip.max=1)

d3 <- din(R_E3, q.matr = Q_E3, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

write.csv(DINA_3$item, "E3_SNPA_itemparameters_Fel.csv")
write.csv(DINA_3$skill.patt, "E3_SNPA_skilpatterns_Fel.csv")
write.csv(data.frame(DINA_3$pattern), "E3_SNPA_postpattern_Fel.csv",row.names = F)

write.csv(d3$item, "e3_itempars_spa_DINA-Guaner.csv")
write.csv(d3$skill.patt, "e3_skilpatt_spa_DINA-Guaner.csv")
write.csv(data.frame(d3$pattern), "e3_spapostpattern_DINA-Guaner.csv",row.names = F)





