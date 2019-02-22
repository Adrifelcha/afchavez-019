##########################################################################################
#Código basico generado por Guaner Rojas para ELSEN 06 matematica
#Funciona como inicio para probar otros modelos y metodologías
##########################################################################################
rm(list=ls())
library(foreign)
setwd("C:/Users/sandra/Desktop/afchavez-019/RIMEDIE/Codigos_Guaner/Replica Fel")
##########################################################################################
#leer aqui directamente
m.dat.q<-read.table("m_dat_q0.txt",header = T)
#m.dat.q<- data.frame(m.dat.q[sample(nrow(m.dat.q), 3000),])

FEM<-c(1:8,26:34); length(FEM) #Primaria
MIN<-c(9:14,35:39); length(MIN) #Primaria
SPA<-c(15:25,40:50); length(SPA) #Primaria

qes <- read.table("q_matrices_mdatq_modificada.txt")

q1<-qes[1:17,1:12]; dim(q1)         #Primaria
q2<-qes[18:28,1:10]; dim(q2)        #Primaria
q3<-qes[29:50,]; dim(q3)            #Primaria

##########################################################################################
#install.packages("CDM")
library("CDM")
# DINA Model
d1 <- din(m.dat.q[,FEM], q.matr = q1, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

summary(d1)
write.table(d1$item, "itempars_fem.txt", sep="\t")
write.table(d1$skill.patt, "skilpatt_fem.txt", sep="\t")
write.table(data.frame(d1$pattern), "fempostpattern.txt", sep="\t",row.names = F)

write.csv(d1$item, "itempars_fem.csv")
write.csv(d1$skill.patt, "skilpatt_fem.csv")
write.csv(data.frame(d1$pattern), "fempostpattern.csv",row.names = F)
#########################################################################################

d2 <- din(m.dat.q[,MIN], q.matr = q2, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

summary(d2)
write.table(d2$item, "itempars_min.txt", sep="\t")
write.table(d2$skill.patt, "skilpatt_min.txt", sep="\t")
write.table(data.frame(d2$pattern), "minpostpattern.txt", sep="\t",row.names = F)
write.csv(d2$item, "itempars_min.csv")
write.csv(d2$skill.patt, "skilpatt_min.csv")
write.csv(data.frame(d2$pattern), "minpostpattern.csv", row.names = F)

#########################################################################################
d3 <- din(m.dat.q[,SPA], q.matr = q3, rule = "DINA",
          conv.crit = 0.01, maxit = 500, progress = TRUE)

summary(d3)
write.table(d3$item, "itempars_spa.txt", sep="\t")
write.table(d3$skill.patt, "skilpatt_spa.txt", sep="\t")
write.table(data.frame(d3$pattern), "spapostpattern.txt", sep="\t",row.names = F)
write.csv(d3$item, "itempars_spa.csv")
write.csv(d3$skill.patt, "skilpatt_spa.csv")
write.csv(data.frame(d3$pattern), "spapostpattern.csv",row.names = F)

##########################################################################################

Datos <- read.csv("fempostpattern.csv")
Aguascalientes <- which(m.dat.q$ID_ENT=="Aguascalientes")
mean(Datos$post.attr1[Aguascalientes])

for(i in 1){
  d1 <- din(m.dat.q[1:612,FEM], q.matr = q1, rule = "DINA",
            conv.crit = 0.01, maxit = 500, progress = TRUE)
  
  summary(d1)
  write.table(d1$item, "itempars_fem_ag.txt", sep="\t")
  write.table(d1$skill.patt, "skilpatt_fem_ag.txt", sep="\t")
  write.table(data.frame(d1$pattern), "fempostpattern_ag.txt", sep="\t",row.names = F)
  
  write.csv(d1$item, "itempars_fem_ag.csv")
  write.csv(d1$skill.patt, "skilpatt_fem_ag.csv")
  write.csv(data.frame(d1$pattern), "fempostpattern_ag.csv",row.names = F)
  ###########################################################################
}