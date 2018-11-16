# Estimación de parámetros (contínua) con datos reales
# Calibracion con datos reales
# Cargar datos 

# The item response data come from a mathematics test for Year 7 students. 
# There are 15 questions. Some are multiple-choice and some are constructed-response. 
# 876 students took the test. The data file has already been scored (data consist of 0 and 1). 
# The test paper and the data file can be downloaded through the following links:

#  http://www.edmeasurementsurveys.com/TAM/Tutorials/data/NumeracyD1.doc


# data <- read.csv("D1_scored.csv")# datos dicotómicos

raw_resp <- read.csv("D1_resp.csv") # respuestas originales

key <- c(1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1)

data <- as.data.frame(sapply( seq(1,length(key)), FUN = function(ii){ 1*(raw_resp[,ii] == key[ii]) } ))
colnames(data) <- c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", 
                    "Item 6", "Item 7", "Item 8", "Item 9", "Item 10",
                    "Item 11", "Item 12", "Item 13", "Item 14", "Item 15")

# Rasch Model
# Joint ML
library(sirt)
fit.JML<- rasch.jml(data, method="MLE")
summary(fit.JML)
hist(fit.JML$person$theta)

# Conditional ML 
library(eRm)
fit.CML.eRM <- RM(data, sum0 = TRUE); fit.CML.eRM
person <- person.parameter(fit.CML.eRM); person$theta.table
hist(person$theta.table$`Person Parameter`)


# Marginal ML
library(TAM)
fit.MML <- TAM::tam.mml(resp=data, constraint = TRUE)
wle1 <- TAM::tam.wle( mod1 ) # alternativa

# Wright map
IRT.WrightMap( fit.MML )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ejemplo completo
# MIRT
library(mirt)

Rasch <- mirt(data, 1, itemtype = 'Rasch')
coeficientes<-data.frame(coef(Rasch, simplify=TRUE, IRTpars = T))
dificultad <- subset(coeficientes, select=2); round(dificultad,4)


# vector de parámetros de sustentantes (thetas)
Thetas.Rasch <- fscores(Rasch, method='ML') 
hist(Thetas.Rasch)

Thetas.Rasch <- fscores(Rasch, method='WLE') # Corrección de sesgo en ML y permite dar cuenta de patrones de respuesta 00000...0; 11111...1
hist(Thetas.Rasch)

# Estimaciones Bayesianas
# En MAP la función de probabilidad conjunta se multiplica por una curva adicional 
# que representa una distribución de población supuesta. 
# MAP calcula el modo de esta distribución posterior como la estimación final del rasgo latente.
Thetas.Rasch <- fscores(Rasch, method='MAP')
hist(Thetas.Rasch)

# Una variante del enfoque MAP es el expected a posteriori, o EAP, 
# que utiliza la media en lugar del modo de la distribución posterior.
Thetas.Rasch <- fscores(Rasch, method='EAP')
hist(Thetas.Rasch)


# Item plot CCI
plot(Rasch, type = "itemscore")
itemplot(Rasch, 1); itemplot(Rasch, 2)
itemplot(Rasch, 3); itemplot(Rasch, 4)
itemplot(Rasch, 5); itemplot(Rasch, 6)
itemplot(Rasch, 7); itemplot(Rasch, 8)
itemplot(Rasch, 9); itemplot(Rasch, 10)
itemplot(Rasch, 11); itemplot(Rasch, 12)
itemplot(Rasch, 13); itemplot(Rasch, 14)
itemplot(Rasch, 15)

# Information curves 

extr.1 <- extract.item(Rasch, 1); extr.2 <- extract.item(Rasch, 2)
extr.3 <- extract.item(Rasch, 3); extr.4 <- extract.item(Rasch, 4)
extr.5 <- extract.item(Rasch, 5); extr.6 <- extract.item(Rasch, 6)
extr.7 <- extract.item(Rasch, 7); extr.8 <- extract.item(Rasch, 8)
extr.9 <- extract.item(Rasch, 9); extr.10 <- extract.item(Rasch, 10)
extr.11 <- extract.item(Rasch, 11); extr.12 <- extract.item(Rasch, 12)
extr.13 <- extract.item(Rasch, 13); extr.14 <- extract.item(Rasch, 14)
extr.15 <- extract.item(Rasch, 15)

# comprobar la info manualmente
info.item1.manual <- dificultad[1,]
info.item1.manual * (1-info.item1.manual) # [1] 0.06273908
info.1 <- iteminfo(extr.1, Theta); max(info.1) # 608 0.07 0.2499995

# Crear la variable latente
Theta <- matrix(seq(-6,6, by = .01))

info.1 <- iteminfo(extr.1, Theta); info.2 <- iteminfo(extr.2, Theta)
info.3 <- iteminfo(extr.3, Theta); info.4 <- iteminfo(extr.4, Theta)
info.5 <- iteminfo(extr.5, Theta); info.6 <- iteminfo(extr.6, Theta)
info.7 <- iteminfo(extr.7, Theta); info.8 <- iteminfo(extr.8, Theta)
info.9 <- iteminfo(extr.9, Theta); info.10 <- iteminfo(extr.10, Theta)
info.11 <- iteminfo(extr.11, Theta); info.12 <- iteminfo(extr.12, Theta)
info.13 <- iteminfo(extr.13, Theta); info.14 <- iteminfo(extr.14, Theta)
info.15 <- iteminfo(extr.15, Theta)



tabla.info.items <- as.data.frame(cbind(Theta,info.1, info.2, info.3, info.4, info.5,
                                        info.6, info.7, info.8, info.8, info.10,
                                        info.11, info.12, info.13, info.14, info.15)); tabla.info.items

colnames(tabla.info.items) <- c("Theta", "Info.Item.1", "Info.Item.2", "Info.Item.3", "Info.Item.4", "Info.Item.5", 
                                "Info.Item.6", "Info.Item.7", "Info.Item.8", "Info.Item.9", "Info.Item.10",
                                "Info.Item.11", "Info.Item.12", "Info.Item.13", "Info.Item.14", "Info.Item.15")

# some examples!
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

# some multiple plots
with(tabla.info.items, plot(Theta, Info.Item.11, type = 'l', main = 'Información de los ítems',
                            xlab=expression(paste("Habilidad, ", theta)), 
                            ylab=expression(paste("Información"))))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.14, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.9, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.1, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.4, type = 'l', xaxt='n', yaxt='n', ann=FALSE))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.6, type = 'l', xaxt='n', yaxt='n', ann=FALSE))

dev.off()

# Test Information

Theta <- matrix(seq(-6,6,.01))
tinfo <- testinfo(Rasch, Theta)
plot(Theta, tinfo, type = 'l', main = 'Información del test',
     xlab=expression(paste("Habilidad, ",theta)), 
     ylab=expression(paste("Información")))

tabla.info.test <- as.data.frame(cbind(Theta,tinfo)); tabla.info.test
max(tabla.info.test$tinfo) # 583 -0.18 3.272781

# Standard Error of estimation
std.error.estima <- 1/(tinfo^0.5)

tabla.info.items_and_test <- as.data.frame(cbind(Theta,tinfo, info.1, info.2, info.3, info.4, info.5,
                                                 info.6, info.7, info.8, info.9, info.10,
                                                 info.11, info.12, info.13, info.14, info.15, std.error.estima)); tabla.info.items_and_test

colnames(tabla.info.items_and_test) <- c("Theta", "Test.Info", "Info.Item.1", "Info.Item.2", "Info.Item.3", "Info.Item.4", "Info.Item.5", 
                                         "Info.Item.6", "Info.Item.7", "Info.Item.8", "Info.Item.9", "Info.Item.10",
                                         "Info.Item.11", "Info.Item.12", "Info.Item.13", "Info.Item.14", "Info.Item.15", "SEE")


# Information plot items + test

with(tabla.info.items_and_test, plot(Theta, Test.Info, type = 'l', main = 'Información de los ítems y el test', ylim=c(0,3.3), lwd=2,
                                     xlab=expression(paste("Habilidad, ", theta)), 
                                     ylab=expression(paste("Información"))))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.1, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.2, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.3, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.4, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.5, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.6, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.7, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.8, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.9, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.10, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.11, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.12, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.13, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.14, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))
par(new=TRUE)
with(tabla.info.items, plot(Theta,Info.Item.15, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3)))

dev.off()


# calculo manual información del test
tabla.info.items2 <- tabla.info.items[,-1]
info.test.manual <-rowSums(tabla.info.items2); info.test.manual
plot(Theta, info.test.manual, type = 'l', main = 'Información del test (manualmente)', ylim=c(0,3.3), lwd=2,
     xlab=expression(paste("Habilidad, ", theta)), 
     ylab=expression(paste("Información")))

dev.off()

# Verificación de las informaciones
plot(tinfo, info.test.manual,  main = 'Información del test (verificación)', ylim=c(0,3.3), xlim=c(0,3.3), lwd=1,
     xlab=expression(paste("Información del test (función)")), 
     ylab=expression(paste("Información del test (manual)")))

dev.off()

# Information test + standard error of estimation

with(tabla.info.items_and_test, plot(Theta, Test.Info, type = 'l', main = 'Información del test y error estándar de estimación', 
                                     ylim=c(0,3.3), lwd=2, col="blue",
                                     xlab=expression(paste("Habilidad, ", theta)), 
                                     ylab=expression(paste("Información"))))
par(new=TRUE)
with(tabla.info.items_and_test, plot(Theta,SEE, type = 'l', xaxt='n', yaxt='n', ann=FALSE, ylim=c(0,3.3), lwd=2,
                                     col="red")); mtext("Error estándar de estimación", side=4, col="red")

dev.off()

# alternative
plot(Rasch, type = "infoSE")


plot(Rasch, type = "trace")

# bondad de ajuste
# goodness of fit of the GRM model
M2(Rasch)

# GOF items
# shut down (sirt, eRm, TAM)
detlist<-c('sirt','eRm','TAM')
lapply(detlist, function(k) detach( paste('package:', k, sep='', collapse=''), unload=TRUE, char=TRUE))

# Run item fit 
item.fit <- itemfit(Rasch); item.fit


# expected response curves + CCI
library(TAM)
mod <- TAM::tam.mml(data)

# poor fit
plot(mod, items=4, export=FALSE)
plot(mod, items=15, export=FALSE)

# good fit
plot(mod, items=5, export=FALSE)
plot(mod, items=6, export=FALSE)


# End