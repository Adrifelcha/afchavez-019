# Seleccionar a aquellos sustentantes que tienen una puntuación significativamente mayor de 30/50

# Calcular la puntuación observada
# leer datos 

# Calcular un intervalo de confianza y tomar en cuenta el error de estimación

datos<- read.csv("Responses.csv")
head(datos,10)

responses <- datos[,-1]; responses

library(psych)

punt.observada <- as.data.frame(rowSums(responses)); punt.observada$`rowSums(responses)`
hist(punt.observada$`rowSums(responses)`)

library(ltm)
alpha <- cronbach.alpha(responses)
cronbach_alpha <- alpha$alpha; cronbach_alpha

desv_est_X <- sd(punt.observada$`rowSums(responses)`); desv_est_X

# error estándar de estimación

err_est_est <- (desv_est_X*(sqrt(1-cronbach_alpha))); err_est_est

# Limites
lim_inf <- punt.observada$`rowSums(responses)` - (1.96*err_est_est); lim_inf
lim_sup <- punt.observada$`rowSums(responses)` + (1.96*err_est_est); lim_sup

limites_95 <- cbind(lim_inf, lim_sup, punt.observada); head(limites_95, 10)

# mayores a 30
mayor_30 <- as.data.frame(limites_95[which(limites_95$lim_inf > 30),]); head(mayor_30, 10)
describe(mayor_30$lim_inf)

# Con Spearman-Brown, cuántos ítems se tienen que agregar para llegar a una determinada confiabilidad.

# TRI
library(mirt)

# Modelo TRI
mod_3PL <- mirt(responses, 1, '3PL')
params_3PL <- coef(mod_3PL, IRTpars = TRUE, simplify = TRUE); params_3PL

# CCI
itemplot(mod_3PL, 18)
par(new=TRUE)
itemplot(mod_3PL, 1)
par(new=TRUE)
itemplot(mod_3PL, 25)

beta_3PL <- as.data.frame(params_3PL$items[,"b"]); beta_3PL
beta_ord <- as.data.frame(sort(beta_3PL$`params_3PL$items[, "b"]`)); beta_ord                          
beta_30 <- beta_ord[30,]; beta_30

# Estimación thetas
thetas_3PL <- fscores(mod_3PL, method = "ML"); head(thetas_3PL, 10)
thetas_3PL <- fscores(mod_3PL, method = "WLE"); head(thetas_3PL, 10)
hist(thetas_3PL)
describe(thetas_3PL)

# Precisión de la estimación
# Función de información 
Theta <- matrix(seq(-6,6,.01))
tinfo <- testinfo(mod_3PL, Theta)
plot(Theta, tinfo, type = 'l')

# Standard Error of Measurement
Theta <- matrix(seq(-6,6,.01))
SEM <- (1 / (sqrt(tinfo)))
plot(Theta, SEM, type = 'l')

# Calcular la información para cada persona en la muestra
# Encontrar el error estándar 
# Calcular los intervalos de confianza
# Seleccionar a los sustentantes que estén por encima del item 30
