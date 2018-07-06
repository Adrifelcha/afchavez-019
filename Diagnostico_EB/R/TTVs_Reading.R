setwd("D:/afchavez/Desktop/Adrifelcha_Work/Julio_2018/Diagnostico_EB/R/TTVs")
dir()
rm(list=ls())

######
Dir_Pree <- "DIR-PREE (versión 1).csv"
Dir_Pree <- read.csv(Dir_Pree)
######
Dir_Prim <- "DIR-PRI (versión 1).csv"
Dir_Prim <- read.csv(Dir_Prim)
######
Dir_Sec <- "DIR-SEC (versión 1).csv"
Dir_Sec <- read.csv(Dir_Sec)
######
Dir_Esp <- "DIR-ESPE (Versión 1).csv"
Dir_Esp <- read.csv(Dir_Esp)
######
Sub_Pre <- "SUB-PRE.csv"
Sub_Pre <- read.csv(Sub_Pre)
######
Sub_Prim <- "SUB-PRIM.csv"
Sub_Prim <- read.csv(Sub_Prim) 
######
Sub_Sec <- "SUB-SEC (versión 1).csv"
Sub_Sec <- read.csv(Sub_Sec) 
######
Coord <- "COOR-SEC.csv"
Coord <- read.csv(Coord)
######
Sup_EF <- "SUPER-EDUFIS (versión 1).csv"
Sup_EF <- read.csv(Sup_EF)
######
Sup_Pre <- "SUPER-PRE (versión 1).csv"
Sup_Pre <- read.csv(Sup_Pre)
######
Sup_Prim <-"SUP-PRI (Versión 1).csv"
Sup_Prim <- read.csv(Sup_Prim)
######
Sup_Sec <-"SUPER-SEC (version_1).csv"
Sup_Sec <- read.csv(Sup_Sec)
######
Sup_Esp <- "SUPER-ESPE (versión 1).csv"
Sup_Esp <- read.csv(Sup_Esp)
######
Sup_Ad <- "SUPER-ADUL.csv"
Sup_Ad <- read.csv(Sup_Ad)
######
Jefe_Pre <- "JEFE-PRE.csv"
Jefe_Pre <- read.csv(Jefe_Pre)
######
Jefe_Prim <- "JEFE-PRIM (Versión 1).csv"
Jefe_Prim <- read.csv(Jefe_Prim)
######
Jefe_Tele <- "JEFE-TELE (Versión 1).csv"
Jefe_Tele <- read.csv(Jefe_Tele)
  
JefeS_Art <- "ENSE-SEC-ART.csv"
JefeS_Art <- read.csv(JefeS_Art)
####
JefeS_His <- "ENSE-SEC-HIS.csv"
JefeS_His <- read.csv(JefeS_His)
####
JefeS_Ing <- "ENSE-SEC-ING.csv"
JefeS_Ing <- read.csv(JefeS_Ing)
####
JefeS_Esp <- "ENSE-SEC-ESP.csv"
JefeS_Esp <- read.csv(JefeS_Esp)
####
JefeS_FCE <- "ENSE-SEC-FCE.csv"
JefeS_FCE <- read.csv(JefeS_FCE)
####
JefeS_Fis <- "ENSE-SEC-FIS.csv"
JefeS_Fis <- read.csv(JefeS_Fis)
####
JefeS_Geo <- "ENSE-SEC-GEO.csv"
JefeS_Geo <- read.csv(JefeS_Geo)
####
JefeS_Mate <- "ENSE-SEC-MAT.csv"
JefeS_Mate <- read.csv(JefeS_Mate)
####
JefeS_Quim <- "ENSE-SEC-QUI.csv"
JefeS_Quim <- read.csv(JefeS_Quim)
####
JefeS_Bio <- "ENSE-SEC-BIO.csv" 
JefeS_Bio <- read.csv(JefeS_Bio)
####
JefeS_Tec <- "ENSE-SEC-TEC.csv"
JefeS_Tec <- read.csv(JefeS_Tec)
####
ATP_LEN_Pre <- "ATP-LEN-PRE (version_1).csv"
ATP_LEN_Pre <- read.csv(ATP_LEN_Pre)
####
ATP_LEN_Prim <- "ATP-LEN-PRI (versión 1).csv"
ATP_LEN_Prim <- read.csv(ATP_LEN_Prim)
####
ATP_LEN_Sec <- "ATP-LEN-SEC (versión 1).csv"
ATP_LEN_Sec <- read.csv(ATP_LEN_Sec)
####
ATP_PEN_Pre <- "ATP_PENS_PRE.csv"
ATP_PEN_Pre <- read.csv(ATP_PEN_Pre)
####
ATP_PEN_Prim <- "ATP-PEN-PRI (versión 1).csv"
ATP_PEN_Prim <- read.csv(ATP_PEN_Prim)
####
ATP_PEN_Sec <- "ATP_PENS_SEC.csv"
ATP_PEN_Sec <- read.csv(ATP_PEN_Sec)
####
ATP_Esp <- "ATP_ESPECIAL.csv"
ATP_Esp <- read.csv(ATP_Esp)
####
ATP_EF <- "ATP-EDUFIS (versión 1).csv"
ATP_EF <- read.csv(ATP_EF)
####
Dir <- "COMUN-DIR.csv"
Dir <- read.csv(Dir)
####
Sup <-"COMÚN-SUPER.csv"
Sup <- read.csv(Sup)
####
ATP <- "COMUN-ATP.csv"
ATP <- read.csv(ATP)