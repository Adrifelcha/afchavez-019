# Estimación de parámetros (discreta)

# Patrón de respuestas (0,1,1,0)
# B1 = 2.0 B2 = 0.0 B3 = 1.0 B4 = 0.5

icc.prob <- function(theta) {
  P1 <- 1 / (1 + exp(theta - 2.0))
  P2 <- (exp(theta - 0.0)) / (1 + exp(theta - 0.0))
  P3 <- (exp(theta - 1.0)) / (1 + exp(theta - 1.0))
  P4 <- 1 / (1 + exp(theta - 0.5))
  as.vector(return(c(P1,P2,P3,P4)))
}


round(icc.prob(-3), digits = 3)

# Si colocamos un conjunto de theta's
thetas <- seq(-3, 3, .1)

round(icc.prob(thetas), digits = 3)

# ordenar los datos 
probabs.4items <- as.data.frame(round(icc.prob(thetas), digits = 3)); probabs.4items # segmentar por cada 61 filas

proba.item1 <- as.data.frame(probabs.4items[c(1:61),]); proba.item1
proba.item2 <- as.data.frame(probabs.4items[c(62:122),]); proba.item2
proba.item3 <- as.data.frame(probabs.4items[c(123:183),]); proba.item3
proba.item4 <- as.data.frame(probabs.4items[c(184:244),]); proba.item4

tabla.proba.4items <- cbind(thetas, proba.item1, proba.item2, proba.item3, proba.item4); tabla.proba.4items
verosimilitud <- as.data.frame(tabla.proba.4items$`probabs.4items[c(1:61), ]` * 
                                                    tabla.proba.4items$`probabs.4items[c(62:122), ]` * 
                    tabla.proba.4items$`probabs.4items[c(123:183), ]` * 
                      tabla.proba.4items$`probabs.4items[c(184:244), ]`); verosimilitud

Log.verosimilitud <- log(verosimilitud)


tabla.proba.4items <- cbind(tabla.proba.4items, verosimilitud, Log.verosimilitud); tabla.proba.4items

colnames(tabla.proba.4items) <- c("Theta", "P(Item 1)", "P(Item 2)", "P(Item 3)", "P(Item 4)", "Verosimilitud", "Log (V)")

max(tabla.proba.4items$Verosimilitud) # 39 [1] 0.1017179
max(tabla.proba.4items$`Log (V)`) # 39 [1] -2.285552

tabla.proba.4items


plot(tabla.proba.4items$Theta, tabla.proba.4items$Verosimilitud, 
     main="Función de verosimilitud (discreta)",
     xlab=expression(paste("Habilidad, ",theta)), 
     ylab=expression(paste("Verosimilitud"))) 
abline(v = 0.861); abline(h = (max(tabla.proba.4items$Verosimilitud)))

plot(tabla.proba.4items$Theta, tabla.proba.4items$`Log (V)`, 
     main="Función de verosimilitud (discreta)",
     xlab=expression(paste("Habilidad, ",theta)), 
     ylab=expression(paste("Logaritmo de la verosimilitud"))) 
abline(v = 0.861); abline(h = (max(tabla.proba.4items$`Log (V)`)))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Estimación de parámetros (contínua) con datos reales
# Calibracion con datos reales
# Cargar datos 

# The item response data come from a mathematics test for Year 7 students. 
# There are 15 questions. Some are multiple-choice and some are constructed-response. 
# 876 students took the test. The data file has already been scored (data consist of 0 and 1). 
# The test paper and the data file can be downloaded through the following links:
  
#  http://www.edmeasurementsurveys.com/TAM/Tutorials/data/NumeracyD1.doc


 # data <- read.csv("D1_scored.csv")# datos dicotómicos
 
raw_resp <- read.csv("D:/afchavez/Desktop/Adrifelcha_PsicometriaYEvaluacion/Curso_Ram&Irwin/Sintaxis R/D1_resp.csv") # respuestas originales

key <- c(1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1)

data <- as.data.frame(sapply( seq(1,length(key)), FUN = function(ii){ 1*(raw_resp[,ii] == key[ii]) } ))
colnames(data) <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", 
                                  "Item 6", "Item 7", "Item 8", "Item 9", "Item 10",
                                  "Item 11", "Item 12", "Item 13", "Item 14", "Item 15")

# Rasch Model
library(mirt)

Rasch <- mirt(data, 1, itemtype = 'Rasch')
coef<-coef(Rasch)
coeficientes<-coef(Rasch, simplify=TRUE); coeficientes
rep<-data.frame(coeficientes$items); rep
b<- -1*(rep$d/rep$a1)
rep<-cbind(rep,b)
names(rep)[names(rep) == 'b'] <- 'Dificultad'
options(scipen=999)
rep <- subset(rep, select=5); round(rep,3)

#         Dificultad
# Item 1       0.067
# Item 2       0.259
# Item 3      -2.056
# Item 4       0.159
# Item 5      -0.536
# Item 6       0.528
# Item 7       0.453
# Item 8       0.239
# Item 9      -0.011
# Item 10     -0.650
# Item 11     -2.268
# Item 12      0.192
# Item 13      0.120
# Item 14     -1.099
# Item 15     -0.792


# vector de parámetros de sustentantes (thetas)

Thetas.Rasch <- fscores(Rasch, method='WLE')
hist(Thetas.Rasch)

# Estimation parameters alternatives (options)
# "EAP" for the expected a-posteriori (default)
# 
# "MAP" for the maximum a-posteriori (i.e, Bayes modal)
# 
# "ML" for maximum likelihood
# 
# "WLE" for weighted likelihood estimation
# 
# "EAPsum" for the expected a-posteriori for each sum score
# 
# "plausible" for a single plausible value imputation for each case. This is equivalent to setting plausible.draws = 1
# 
# "classify" for the posteriori classification probabilities (only applicable when the input model was of class MixtureClass)


# CML 


# Wright map
library(TAM)

# fit Rasch model
mod1 <- TAM::tam.mml(resp=data)
wle1 <- TAM::tam.wle( mod1 )
hist(wle1$theta)

# Wright map
IRT.WrightMap( mod1 )
# some customized plots
IRT.WrightMap( mod1, show.thr.lab=FALSE, label.items=c(1:40), label.items.rows=3)
IRT.WrightMap( wle1, show.thr.lab=FALSE, label.items=c(1:40), label.items.rows=3)

IRT.WrightMap( mod1,  show.thr.sym=FALSE, thr.lab.text=paste0("I",1:ncol(dat)),
               label.items="", label.items.ticks=FALSE)

#--- direct specification with wrightMap function
theta <- TAM::tam.wle(mod1)$theta
thr <- TAM::tam.threshold(mod1)

# default wrightMap plots
WrightMap::wrightMap( theta, thr, label.items.srt=90)
WrightMap::wrightMap( theta, t(thr), label.items=c("items") )

# stack all items below each other
thr.lab.text <- matrix( "", 1, ncol(dat) )
thr.lab.text[1,] <- colnames(dat)
WrightMap::wrightMap( theta, t(thr), label.items=c("items"),
                      thr.lab.text=thr.lab.text, show.thr.sym=FALSE )



# CCR's
plot(Rasch)

empirical_plot(data) # dividir por grupos empírico

empirical_plot(data, c(1, 2, 3, 4)) # misma que la anterior 
empirical_plot(data, c(5, 6, 7, 8))
empirical_plot(data, c(9, 10, 11, 12))
empirical_plot(data, c(13, 14, 15))

# Item plot CCI
itemplot(Rasch, 1)
itemplot(Rasch, 2)
itemplot(Rasch, 3)
itemplot(Rasch, 4)
itemplot(Rasch, 5)
itemplot(Rasch, 6)
itemplot(Rasch, 7)
itemplot(Rasch, 8)
itemplot(Rasch, 9)
itemplot(Rasch, 10)
itemplot(Rasch, 11)
itemplot(Rasch, 12)
itemplot(Rasch, 13)
itemplot(Rasch, 14)
itemplot(Rasch, 15)

# Information curves (11, 14, 9, 1, 4, 6)

extr.11 <- extract.item(Rasch, 11)
extr.14 <- extract.item(Rasch, 14)
extr.9 <- extract.item(Rasch, 9)
extr.1 <- extract.item(Rasch, 1)
extr.4 <- extract.item(Rasch, 4)
extr.6 <- extract.item(Rasch, 6)

Theta <- matrix(seq(-6,6, by = .01))

info.11 <- iteminfo(extr.11, Theta)
info.14 <- iteminfo(extr.14, Theta)
info.9 <- iteminfo(extr.9, Theta)
info.1 <- iteminfo(extr.1, Theta)
info.4 <- iteminfo(extr.4, Theta)
info.6 <- iteminfo(extr.6, Theta)


# calcular manualmente ppt 

tabla.info.items <- as.data.frame(cbind(Theta,info.11, info.14, info.9,
                                        info.1, info.4, info.6)); tabla.info.items

colnames(tabla.info.items) <- c("Theta", "Info.Item.11", "Info.Item.14", "Info.Item.9", 
                                "Info.Item.1", "Info.Item.4", "Info.Item.6")

max(tabla.info.items$Info.Item.11) # 654 0.53 .2499998
max(tabla.info.items$Info.Item.14) # 491 -1.1 .25
max(tabla.info.items$Info.Item.9)  # 600 -0.01 .2499999
max(tabla.info.items$Info.Item.1)  # 608 0.07 .2499995
max(tabla.info.items$Info.Item.4)  # 617 0.16 .25
max(tabla.info.items$Info.Item.6)  # 654 0.53 .2499998

#do something with the info?
plot(Theta, info.2, type = 'l', main = 'Información del ítem',
     xlab=expression(paste("Habilidad, ",theta)), 
     ylab=expression(paste("Información")))

# multiple plots
with(tabla.info.items, plot(Theta, Info.Item.11, type = 'l', main = 'Información de los ítems',
                            xlab=expression(paste("Habilidad, ", theta)), 
                            ylab=expression(paste("Información"))))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.14, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.9, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
# par(new=TRUE)
# with(tabla.info.items, plot(Theta,Info.Item.1, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
# par(new=TRUE)
# with(tabla.info.items, plot(Theta,Info.Item.4, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
# par(new=TRUE)
# with(tabla.info.items, plot(Theta,Info.Item.6, type = 'l', xaxt='n', yaxt='n', ann=FALSE))

dev.off()

# Test Information

Theta <- matrix(seq(-6,6,.01))
tinfo <- testinfo(Rasch, Theta)
plot(Theta, tinfo, type = 'l', main = 'Información del test',
     xlab=expression(paste("Habilidad, ",theta)), 
     ylab=expression(paste("Información")))

tabla.info.test <- as.data.frame(cbind(Theta,tinfo)); tabla.info.test
max(tabla.info.test$tinfo) # 583 -0.18 3.272781


tabla.info.items_and_test <- as.data.frame(cbind(Theta,tinfo, info.11, info.14, info.9,
                                        info.1, info.4, info.6)); tabla.info.items_and_test

colnames(tabla.info.items_and_test) <- c("Theta", "Test.Info", "Info.Item.11", "Info.Item.14", "Info.Item.9", 
                                "Info.Item.1", "Info.Item.4", "Info.Item.6")


# Information plot items + test

with(tabla.info.items_and_test, plot(Theta, Test.Info, type = 'l', main = 'Información de los ítems y el test', ylim=c(0,3.3), lwd=2,
                            xlab=expression(paste("Habilidad, ", theta)), 
                            ylab=expression(paste("Información"))))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.11, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.14, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.9, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))

dev.off()

# bondad de ajuste

# End
