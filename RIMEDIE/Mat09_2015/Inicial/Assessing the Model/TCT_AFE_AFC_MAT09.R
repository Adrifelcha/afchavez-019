#############################################################
# Estimacion de algunos de los índices más importantes de la
# TCT y Análisis Facoriales Exploratoris y Confirmatorios para 
# la base de datos empleada para la estimación PLANEA 09 
#############################################################
rm(list=ls())
library(psych)
setwd("C:/Users/Alejandro/Desktop/afchavez19/RIMEDIE/Mat09_2015/Inicial")
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
################################################################
################################################################
########################   I N D I C E S    D E     L A     TCT


##################################################################
#Distribución de puntajes totales alcanzados
Total <- NULL
for(i in 1:nrow(Datos)){
  Total[i] <- sum(Datos[i,])
}
min(Total)
max(Total)
hist(Total, breaks = seq(0,50,1), axes=F, main="Distribución de Puntaje Total en la muestra", col="goldenrod3",
     xlab="Puntaje total",ylab="Frecuencia (No. de sustentantes)", cex.lab=1.3, ylim=c(0,1000),
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.05,0.05),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.1,.1),lwd=2,lty=3, col="black"),lines(c(0,1),c(.15,.15),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.2,.2),lwd=2,lty=3, col="black"),lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.3,.3),lwd=2,lty=3, col="black"),lines(c(0,1),c(.35,.35),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.4,.4),lwd=2,lty=3, col="black"),lines(c(0,1),c(.45,.45),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.55,.55),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.6,.6),lwd=2,lty=3, col="black"),lines(c(0,1),c(.65,.65),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.7,.7),lwd=2,lty=3, col="black"),lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.8,.8),lwd=2,lty=3, col="black"),lines(c(0,1),c(.85,.85),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.9,.9),lwd=2,lty=3, col="black"),lines(c(0,1),c(.95,.95),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(1,1),lwd=2,lty=3, col="black")))
axis(1, seq(0.5,49.5,1), seq(1,50.1))
axis(2, seq(0,1000,50), seq(0,1000,50))


#####################################################################
### Indices de dificultad
P <- NULL
for(i in 1:ncol(Datos)){
  P[i] <- mean(Datos[,i])
}

min(P)
max(P)
hist(P, breaks = seq(0.0,1,.05), axes=F, main="Distribución de Índices de Dificultad clásicos", col="indianred3",
     xlab="Índice de dificultad",ylab="Frecuencia (No. de items)", cex.lab=1.3, 
     panel.first = 
       c(lines(c(0,1),c(1,1),lwd=2,lty=3, col="black"),lines(c(0,1),c(0.0625,0.0625),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.125,.125),lwd=2,lty=3, col="black"),lines(c(0,1),c(.1875,.1875),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.25,.25),lwd=2,lty=3, col="black"),lines(c(0,1),c(.3125,.3125),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.375,.375),lwd=2,lty=3, col="black"),lines(c(0,1),c(.4375,.4375),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.5,.5),lwd=2,lty=3, col="black"),lines(c(0,1),c(.5625,.5625),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.625,.625),lwd=2,lty=3, col="black"),lines(c(0,1),c(.6875,.6875),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.75,.75),lwd=2,lty=3, col="black"),lines(c(0,1),c(.8125,.8125),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(.875,.875),lwd=2,lty=3, col="black"),lines(c(0,1),c(.9375,.9375),lwd=2,lty=3, col="black"),
         lines(c(0,1),c(1,1),lwd=2,lty=3, col="black")))
axis(1, seq(0,1,0.05), seq(0,1,0.05))
axis(2, seq(0,16,1), seq(0,16,1))


#############################################
##
#    Correlaciones      #####################
##
#############################################
cor_dat <- cor(Datos)
lowerCor(Datos)
corPlot(Datos)   #Note all correlations are *almost* positive

#Ordenamos las correlaciones en orden ascendente y descendiente
ascendiente <- sort(lowerCor(Datos))
descendiente <- sort(lowerCor(Datos), decreasing=TRUE)

ascendiente[c(1,3,5,7,9)]   #Cinco correlaciones más BAJAS
#-.003, -.002,   0.0003,    0.002,   0.004
descendiente[c(51,53,55,57,59)]    #Cinco correlaiones más ALTAS
#.251,    .246     .236     .229     .223

library(corrplot)
round(cor_dat,2)
corrplot(cor_dat, type = "upper", order = "hclust",    #This is just a cool plot, which utility gets lost due to the 
         tl.col = "black", tl.srt = 45)                            #very low levels of Correlation reflected in our data





#############################################
##
#    ALFA DE CRONBACH      ##################
##
#############################################
#Calculamos las alphas
Cronbach_general <- alpha(Datos); Cronbach_general
write.csv(Cronbach_general$total,"Cronbach_Gral.csv")
# 0.82
Cronbach_Eje1 <- alpha(D_E1); Cronbach_Eje1
write.csv(Cronbach_Eje1$total,"Cronbach_Eje1.csv")
# 0.67
Cronbach_Eje2 <- alpha(D_E2); Cronbach_Eje2
write.csv(Cronbach_Eje2$total,"Cronbach_Eje2.csv")
# 0.59
Cronbach_Eje3 <- alpha(D_E3); Cronbach_Eje3
write.csv(Cronbach_Eje3$total,"Cronbach_Eje3.csv")
# 0.54

#library(psy)             # Aquí hay código con otro paquete, pero el resultado es el mismo
#cronbach(Datos)          #      y el output es menos informativo
#cronbach(D_E1)
#cronbach(D_E2)
#cronbach(D_E3)


##########################################################
##########################################################
##########################################################
# Primera revisión de la composición de los datos
#
# Análisis de Componentes Principales (ACP)
# Análisis Paralelo
# VSS
##
#############################################
ACP <- prcomp(Datos, cor=TRUE)
summary(ACP)
plot(ACP,type="lines", main="Principal Component Analysis - Screeplot")   #Line Screeplot
screeplot(ACP)   #Bar screeplot
#According to the Screeplot, 1 component seems to be good enough

AP <- fa.parallel(Datos); AP
#Suggest that number of factors = 18 with 5 components


VSS <- vss(Datos);VSS
#Velicer MAP criterion achieves a minimun with 1 factor
#BIC achieves a minimum with 4 factors
#BIC-Sample Size adjusted is reduced with 7 factors


library(nFactors)
ev <- eigen(cor(Datos)) # get eigenvalues
ap <- parallel(subject=nrow(Datos),var=ncol(Datos),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


#############################################
##
# Análisis Factorial 
##
#############################################
explore_1 <- factanal(Datos, factors = 1, method = "mle") 
explore_2 <- factanal(Datos, factors = 2, method = "mle") 
explore_3 <- factanal(Datos, factors = 3, method = "mle") 
explore_3b <- factanal(Datos, factors = 3, rotation= "varimax") 
explore_4 <- factanal(Datos, factors = 4, method = "mle") 
explore_5 <- factanal(Datos, factors = 5, method = "mle")
explore_6 <- factanal(Datos, factors = 6, method = "mle")
explore_7 <- factanal(Datos, factors = 7, method = "mle")
explore_8 <- factanal(Datos, factors = 8, method = "mle")
explore_9 <- factanal(Datos, factors = 9, method = "mle")
explore_10 <- factanal(Datos, factors = 10, method = "mle")
explore_11 <- factanal(Datos, factors = 11, method = "mle")


