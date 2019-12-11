# Análisis de Datos 1.0      ########################################
# D E S C R I P T I V O S    ########################################
# Prueba PLANEA Matemáticas - 6to primaria     ######################
# 50 items liberados, 127 estudiantes          ######################
# Tesis doctoral de Sandra Conzuelo Serrato    ######################
# Apoyo técnico y análisis de datos: Adriana Felisa Chávez De la Peña
#####################################################################
#####################################################################

####################################################
#Cargamos librería de trabajo, las bases y los datos
setwd("C:/Users/asus/Desktop/afchavez-019/Sandy-PrácticaDocente/ValleBravoComparación")
Datos <- read.csv("MatrizAciertos_Full.csv")     #Archivo con las respuestas de los estudiantes
Primaria <- read.csv("MatrizAciertos_Primaria.csv")
Secundaria <- read.csv("MatrizAciertos_Secundaria.csv")

#Indices de dificultad GLOBAL
Porc_Prim <- NULL
Porc_Secu <- NULL
Items <- 50
Items_A <- 25
Items_B <- 25
for(i in 1:Items){
  Porc_Prim[i] <- mean(Primaria[,i+4],na.rm = TRUE)
  Porc_Secu[i] <- mean(Secundaria[,i+4],na.rm = TRUE)
}

Secu_A <- Porc_Secu[1:25]
Prim_A <- Porc_Prim[1:25]
Secu_B <- Porc_Secu[26:50]
Prim_B <- Porc_Prim[26:50]
Forma_A <- c(rbind(Prim_A, Secu_A))
Forma_B <-c(rbind(Prim_B, Secu_B))
Init <- 0
End <- 0.039
colores <- c("deepskyblue3", "darkorange3", "deepskyblue3", "darkorange3")
back <- c(rep(c("gray59", "gray80"),12),"gray57")
barplot(Forma_A, col=colores, xlab="", ann=F, axes=F,ylim = c(0,1.1),
        panel.first = 
          for(i in 1:25){
            polygon(c(Init,End,End,Init),c(0,0,0.91,0.91), col=back[i])
            Init <- Init + 0.04
            End <- End + 0.04}
        )
axis(2,at=seq(0,1,.1),labels=paste(seq(0,100,10),"%"),las=1, line=-1.8)
mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=1.2, cex=2, f=2)
mtext(side=3, text = "Forma A", line=-0.5, cex=1.5, f=2)
mtext(side=2, text = "Porcentaje de aciertos", line=1.8, cex=1.6, f=2)
text(17,1.04, "Primaria", f=2, cex=0.8, col="black")
text(38,1.04, "Secundaria", f=2, cex=0.8, col="black")
lines(c(10,15),c(1.03,1.03), col="deepskyblue3", lwd=4)
lines(c(30,35),c(1.03,1.03), col="darkorange3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(1.3,59,2.4),labels=paste("Ítem", seq(1,25,1)), line=0.1, f=2,las=2)


Init <- 0
End <- 0.039
colores <- c("deepskyblue3", "darkorange3", "deepskyblue3", "darkorange3")
barplot(Forma_B, col=colores, xlab="", ann=F, axes=F,ylim = c(0,1.1),
        panel.first = 
          for(i in 1:25){
            polygon(c(Init,End,End,Init),c(0,0,0.91,0.91), col=back[i])
            Init <- Init + 0.04
            End <- End + 0.04})
axis(2,at=seq(0,1,.1),labels=paste(seq(0,100,10),"%"),las=1, line=-1.8)
mtext(side=3, text = "Porcentaje de estudiantes que acertaron cada item", line=1.2, cex=2, f=2)
mtext(side=3, text = "Forma B", line=-0.5, cex=1.5, f=2)
mtext(side=2, text = "Porcentaje de aciertos", line=1.8, cex=1.6, f=2)
text(17,1.04, "Primaria", f=2, cex=0.8, col="black")
text(38,1.04, "Secundaria", f=2, cex=0.8, col="black")
lines(c(10,15),c(1.03,1.03), col="deepskyblue3", lwd=4)
lines(c(30,35),c(1.03,1.03), col="darkorange3", lwd=4)
lines(c(.75,.79),c(12.5,12.5), col="darkseagreen3", lwd=4)
axis(1,at=seq(1.3,59,2.4),labels=paste("Ítem", seq(1,25,1)), line=0.1, f=2,las=2)



