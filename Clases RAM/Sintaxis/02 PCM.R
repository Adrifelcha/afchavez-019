# Modelo de cr?dito parcial
setwd("C:/Users/sandra/Desktop/afchavez-019/Clases RAM/Sintaxis")
library(mirt)
library(psych)

# Datos
TIMSS_scored <- read.csv("Scored_TIMSS.csv")
head(TIMSS_scored)

# Descriptivos
describe(TIMSS_scored)
PCM <- TIMSS_scored[,c(4,5,8,9)]
PCM_omit <-na.omit(PCM)


# Modelo
model.pcm <- 'PCM_omit = 1-4' 
results.pcm <- mirt(data=PCM_omit, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)


# Par?metros de los ?tems
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)

# Plot de items
plot(results.pcm, type = 'trace', which.items = c(2, 4), 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))

# Funci?n de informaci?n del item
plot(results.pcm, type = 'infotrace', which.items = c(2, 4), 
     main = "", par.settings = simpleTheme(lwd=2))

# Funci?n de informaci?n del test
plot(results.pcm, type = 'info', theta_lim = c(-4,4), lwd=2)     

# Error est?ndar de medici?n
plot(results.pcm, type = 'SE', theta_lim = c(-4,4), lwd=2)    

# Par?metros de las personas
mod <- mirt(PCM_omit, 1, itemtype="Rasch")
thetas_PCM <- fscores(mod, method = "WLE")
head(thetas_PCM)
hist(thetas_PCM)
describe(thetas_PCM)

# Ajuste de los items
itemfit(mod)
itemfit(mod, 'X2') # just X2
itemfit(mod, 'X2', method = 'ML') # X2 with maximum-likelihood estimates for traits
itemfit(mod, c('S_X2', 'X2')) #both S_X2 and X2
itemfit(mod, group.bins=15, empirical.plot = 1, method = 'ML') #empirical item plot with 15 points
itemfit(mod, group.bins=15, empirical.plot = 3, method = 'ML')

# Ajuste del modelo
M2(mod)
