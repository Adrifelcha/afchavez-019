setwd("C:/Users/Adriana/Desktop/afchavez-019/JeanPiaget/Actividades")
Datos <- read.csv("Datos_JP.csv")
View(Datos)

Right <- c("d","c","b")

Datos$RC1 <- ifelse(Datos$RC1==Right[1], Datos$RC1<-1, Datos$RC1<-0)
Datos$RC2 <- ifelse(Datos$RC2==Right[2], Datos$RC2<-1, Datos$RC2<-0)
Datos$RC3 <- ifelse(Datos$RC3==Right[3], Datos$RC3<-1, Datos$RC3<-0)
View(Datos)

layout(matrix(1:3,ncol=3))
barplot(as.vector(table(Datos$RC1)), ylim=c(0,70), col=c("orange4","orange2"), 
        main="1) El bat y la pelota")
text(0.7,table(Datos$RC1)[1]+5,paste(table(Datos$RC1)[1]), cex=1.5, f=2)
text(1.9,table(Datos$RC1)[2]+5,paste(table(Datos$RC1)[2]), cex=1.5, f=2)
axis(1,c(0.7,1.9), c("Error","Acierto"))
barplot(as.vector(table(Datos$RC2)), ylim=c(0,70), col=c("palegreen4","palegreen2"),
        main="2) Máquinas")
text(0.7,table(Datos$RC2)[1]+5,paste(table(Datos$RC2)[1]), cex=1.5, f=2)
text(1.9,table(Datos$RC2)[2]+5,paste(table(Datos$RC2)[2]), cex=1.5, f=2)
axis(1,c(0.7,1.9), c("Error","Acierto"))
barplot(as.vector(table(Datos$RC3)), ylim=c(0,70), col=c("mediumpurple4","mediumpurple1"),
        main="3) El sendero")
text(0.7,table(Datos$RC3)[1]+3,paste(table(Datos$RC3)[1]), cex=1.5, f=2)
text(1.9,table(Datos$RC3)[2]+3,paste(table(Datos$RC3)[2]), cex=1.5, f=2)
axis(1,c(0.7,1.9), c("Error","Acierto"))



Total <- Datos$RC1 + Datos$RC2 + Datos$RC3

layout(matrix(1:1,ncol=1))
barplot(as.vector(table(Total)), main="Aciertos Finales", ylim=c(0,60),
        col=c("dodgerblue3", "darkorange2", "darkorchid3", "chartreuse4"))
text(0.7,table(Total)[1]+5,paste(table(Total)[1]), cex=1.5, f=2)
text(1.9,table(Total)[2]+5,paste(table(Total)[2]), cex=1.5, f=2)
text(3.1,table(Total)[3]+5,paste(table(Total)[3]), cex=1.5, f=2)
text(4.3,table(Total)[4]+5,paste(table(Total)[4]), cex=1.5, f=2)
axis(1,c(0.7,1.9,3.1,4.3), c(0,1,2,3))

Total_4A <- Total[Datos$Grupo=="410"]
Total_4B <- Total[Datos$Grupo=="420"]
Total_5 <- Total[Datos$Grupo=="510"]
Total_6 <- Total[Datos$Grupo=="610"]

Tab_Total_4A <- as.vector(table(Total_4A))
Tab_Total_4B <- as.vector(table(Total_4B))
Tab_Total_5 <- as.vector(table(Total_5))
Tab_Total_6 <- as.vector(table(Total_6))

TOTAL_porGrupo <- c(Tab_Total_4A[1], Tab_Total_4B[1], Tab_Total_5[1], Tab_Total_6[1],
                    Tab_Total_4A[2], Tab_Total_4B[2], Tab_Total_5[2], Tab_Total_6[2],
                    Tab_Total_4A[3], Tab_Total_4B[3], Tab_Total_5[3], Tab_Total_6[3],
                    0, Tab_Total_4B[4], 0, Tab_Total_6[4])

espacios <- as.vector(seq(0.7,18.7,1.2))
barplot(TOTAL_porGrupo, main="Aciertos Totales por Grupo", ylim=c(0,20),
        col=c(rep("dodgerblue3",4),rep("darkorange2",4),rep("darkorchid3",4),rep("chartreuse4",4)),
        axes=F, cex.main=2)
axis(1,c(2.5,7.3,12.1,17), c(0:3), cex=2, f=2)
#axis(1,seq(0.7,18.7,1.2), c(rep(c("4A","4B","5","6"),4)), cex=2, f=2)
text(12.3,16,"Comparación 4*A - 4*B - 5to - 6to", cex=1.2, f=2)
axis(2,seq(0,20,1),seq(0,20,1))
for(a in 1:length(TOTAL_porGrupo)){
  text(espacios[a],TOTAL_porGrupo[a]+1,paste(TOTAL_porGrupo[a]), cex=1.5, f=2)
}
  

col_dots <- ifelse(Datos$Grupo=="410", col_dots<- "darkmagenta", 
                   (ifelse(Datos$Grupo=="420",col_dots <- "chartreuse3",
                           (ifelse(Datos$Grupo=="510", col_dots <- "chocolate3",
                                   (ifelse(Datos$Grupo=="610",col_dots <- "darkblue",a<-0.2)))))))
plot(Datos$Numero.Elegido,Total, pch=16, ann=F, axes=F, main="NùmeroElegido_x_TotalAciertos",
     col=col_dots, cex=2)
legend(80,3, c("4A","4B","5to","6to"), lty=1, col=c("darkmagenta","chartreuse3","chocolate3","darkblue"), lwd=2)
axis(2,c(0:3), c(0:3))
axis(1,c(0:100), c(0:100))

plot(Datos$Edad, Datos$Numero.Elegido,pch=16, cex=2,
     col=col_dots)

plot(Datos$Edad, Total,pch=16, cex=2,
     col=col_dots)


#Ganador4A
Objetivo4A <- mean(Datos$Numero.Elegido[Datos$Grupo=="410"])*.5; Objetivo4A
Objetivo4B <- mean(Datos$Numero.Elegido[Datos$Grupo=="420"])*.5; Objetivo4B
Objetivo6 <- mean(Datos$Numero.Elegido[Datos$Grupo=="610"])*.5; Objetivo6

