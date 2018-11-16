# MCAR MAR MNAR

library(mirt)
library(TAM)
library(psych)

data.CD.original <- read.csv("CompleteData.csv")
data.CD <- data.CD.original[,-c(1:3)]; head(data.CD,10)
# data.CD.a <- data.CD[2:1001,4:43]
# data.CD.b <- data.CD[1002:2001,4:43]
# data.CD.c <- data.CD[2002:3001,4:43]

data.MCAR <- read.csv("ObsData_1.csv")
data.MCAR <- data.MCAR[,-c(1:2)]; head(data.MCAR,10)
# data.MCAR.a <- data.MCAR[2:1001,3:42]
# data.MCAR.b <- data.MCAR[1002:2001,3:42]
# data.MCAR.c <- data.MCAR[2002:3001,3:42]

data.MAR <- read.csv("ObsData_2.csv")
data.MAR <- data.MAR[,-c(1:2,43)]; head(data.MAR,10)
# data.MAR.a <- data.MAR[2:1001,3:42]
# data.MAR.b <- data.MAR[1002:2001,3:42]
# data.MAR.c <- data.MAR[2002:3001,3:42]

data.NMAR <- read.csv("ObsData_3.csv")
data.NMAR <- data.NMAR[,-c(1:2)]; head(data.NMAR,10)
# data.NMAR.a <- data.NMAR[2:1001,3:42]
# data.NMAR.b <- data.NMAR[1002:2001,3:42]
# data.NMAR.c <- data.NMAR[2002:3001,3:42]


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Complete data
# TAM
# estimate Rasch model
mod.CD <- TAM::tam.mml( resp=data.CD)
par.items.CD <-mod.CD$item$xsi.item; par.items.CD
write.csv(par.items.CD, "par.items.CD.csv")


# compute person parameters
wmod.CD <- TAM::tam.wle( mod.CD ); head(wmod.CD, 10)
hist(wmod.CD$theta)
write.csv(wmod.CD, "ability.CD.csv")

#mirt
Rasch.CD <- mirt(data.CD, 1, itemtype = 'Rasch')
coeficientes<-data.frame(coef(Rasch.CD, simplify=TRUE, IRTpars = T))
dificultad <- subset(coeficientes, select=2); round(dificultad,4)


# vector de parámetros de sustentantes (thetas)
Thetas.Rasch.CD <- fscores(Rasch.CD, method='WLE') 
hist(Thetas.Rasch.CD)
describe(Thetas.Rasch.CD)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MCAR
# TAM
# estimate Rasch model
mod.MCAR <- TAM::tam.mml( resp=data.MCAR)
par.items.MCAR <-mod.MCAR$item$xsi.item; par.items.MCAR
write.csv(par.items.MCAR, "par.items.MCAR.csv")

# compute person parameters
wmod.MCAR <- TAM::tam.wle( mod.MCAR )
hist(wmod.MCAR$theta)
write.csv(wmod.MCAR, "ability.MCAR.csv")


#mirt
Rasch.MCAR <- mirt(data.MCAR, 1, itemtype = 'Rasch')
coeficientes<-data.frame(coef(Rasch.MCAR, simplify=TRUE, IRTpars = T))
dificultad <- subset(coeficientes, select=2); round(dificultad,4)


# vector de parámetros de sustentantes (thetas)
Thetas.Rasch.MCAR <- fscores(Rasch.MCAR, method='WLE') 
hist(Thetas.Rasch.MCAR)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# MAR
# TAM
# estimate Rasch model
mod.MAR <- TAM::tam.mml( resp=data.MAR)
par.items.MAR <-mod.MAR$item$xsi.item; par.items.MAR
write.csv(par.items.MAR, "par.items.MAR.csv")

# compute person parameters
wmod.MAR <- TAM::tam.wle( mod.MAR )
hist(wmod.MAR$theta)
write.csv(wmod.MAR, "ability.MAR.csv")


#mirt
Rasch.MAR <- mirt(data.MAR, 1, itemtype = 'Rasch')
coeficientes<-data.frame(coef(Rasch.MAR, simplify=TRUE, IRTpars = T))
dificultad <- subset(coeficientes, select=2); round(dificultad,4)


# vector de parámetros de sustentantes (thetas)
Thetas.Rasch.CD.c <- fscores(Rasch.CD.c, method='ML') 
hist(Thetas.Rasch.CD.c)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# NMAR
# TAM
# estimate Rasch model
mod.NMAR <- TAM::tam.mml( resp=data.NMAR)
par.items.NMAR <-mod.NMAR$item$xsi.item; par.items.NMAR
write.csv(par.items.NMAR, "par.items.NMAR.csv")

# compute person parameters
wmod.NMAR <- TAM::tam.wle( mod.NMAR )
hist(wmod.NMAR$theta)
write.csv(wmod.NMAR, "ability.NMAR.csv")



#mirt
Rasch.NMAR <- mirt(data.NMAR, 1, itemtype = 'Rasch')
coeficientes<-data.frame(coef(Rasch.NMAR, simplify=TRUE, IRTpars = T))
dificultad <- subset(coeficientes, select=2); round(dificultad,4)

# vector de parámetros de sustentantes (thetas)
Thetas.Rasch.CD.c <- fscores(Rasch.CD.c, method='ML') 
hist(Thetas.Rasch.CD.c)


# dos tablas una de items y una de habilidad
items<-read.csv("tabla.par.items.csv")
persons<-read.csv("tabla.par.perso.csv")

# items
describe(items)

par(mfrow=c(2,3))

hist(items$True_Beta)
hist(items$Complete_data)
hist(items$MCAR)
hist(items$MAR)
hist(items$MNAR)

# persons

describe(persons)

par(mfrow=c(2,3))

hist(persons$TrueTheta)
hist(persons$Complete_data)
hist(persons$MCAR)
hist(persons$MAR)
hist(persons$NMAR)
