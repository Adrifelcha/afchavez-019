# GRM (Samejima, 1969),  RSM (Muraki, 1992), NRM (Bock, 1972)

# install.packages("devtools")
# library(devtools)
# install_github("cddesja/hemp") # Si no se tiene instalado "hemp" hacerlo desde su github.

library(hemp)
library(mirt)
library(psych)
library(car) # recodificar
library(plyr) # contar


# Datos
data(rse)
describe(rse)
hist(rse$Q3)

# count(rse$Q3)
# count(rse$Q5)
# count(rse$Q8)
# count(rse$Q9)
# count(rse$Q10)
# 
# # Recodificar items 3, 5, 8, 9 and 10
# rse$Q3 <- recode(rse$Q3, "0=3; 1=2; 2=1; 3=0")
# rse$Q5 <- recode(rse$Q5, "0=3; 1=2; 2=1; 3=0")
# rse$Q8 <- recode(rse$Q8, "0=3; 1=2; 2=1; 3=0")
# rse$Q9 <- recode(rse$Q9, "0=3; 1=2; 2=1; 3=0")
# rse$Q10 <- recode(rse$Q10, "0=3; 1=2; 2=1; 3=0")
# 
# # Verificar recodificación
# count(rse$Q3)
# count(rse$Q5)
# count(rse$Q8)
# count(rse$Q9)
# count(rse$Q10)

# Contexto
?rse

# items
items <- rse[,c(1:10)]
# items <- rse[,c(1,2,4,6,7)]
# items <- rse[,c(3,5,8,9,10)]

# Modelo de Respuesta Graduada (Samejima, 1969)
grm_mod <- "items = 1-10"
grm_fit <- mirt(data = items, model = grm_mod, itemtype = "graded", SE = TRUE)
grm_params <- coef(grm_fit, IRTpars = TRUE, simplify = TRUE)
grm_items <- as.data.frame(grm_params$items); grm_items

# Plots Modelo de Respuesta Graduada
plot(grm_fit, type = "trace", which.items = 1, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# función de información
plot(grm_fit, type = "info", theta_lim = c(-6, 6))

# item info
iteminfo()
extr.2 <- extract.item(grm_fit, 2)
Theta <- matrix(seq(-4,4, by = .1))
info.2 <- iteminfo(extr.2, Theta)
plot(Theta, info.2, type = 'l', main = 'Item information')

# Error estándar de estimación
plot(grm_fit, type = "SE", theta_lim = c(-3, 3))

# item and model fit
itemfit(grm_fit)
M2(grm_fit)

itemfit(rsm_fit)
M2(rsm_fit)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Modelo Rating Scale (Muraki, 1990)
rsm_mod <- "items = 1-10"
rsm_fit_IRT <- mirt(data = items, model = rsm_mod, itemtype = "grsmIRT", SE = TRUE)
rsm_fit <- mirt(data = items, model = rsm_mod, itemtype = "grsm", SE = TRUE)

rsm_params_IRT <- coef(rsm_fit_IRT, simplify = TRUE); rsm_params_IRT
rsm_params <- coef(rsm_fit, simplify = TRUE); rsm_params

rsm_items_IRT <- as.data.frame(rsm_params_IRT$items)
rsm_items <- as.data.frame(rsm_params$items)

# Reparametrización IRT
rsm_items_IRT$b1 <- -(rsm_items_IRT$b1)
rsm_items_IRT$b2 <- -(rsm_items_IRT$b2)
rsm_items_IRT$b3 <- -(rsm_items_IRT$b3)
rsm_items_IRT

# Reparametrización Deltas
rsm_items$d1 <- -(rsm_items$d1)
rsm_items$d2 <- -(rsm_items$d2)
rsm_items$d3 <- -(rsm_items$d3)
rsm_items

# Plots Modelo Rating Scale
plot(rsm_fit, type = "trace", which.items = 8, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# función de información
plot(rsm_fit, type = "info", theta_lim = c(-6, 6))

# Error estándar de estimación
plot(rsm_fit, type = "SE", theta_lim = c(-6, 6))

# +++++++++++++++++++++++++++++++++++++++++

#Modelo de Respuesta nominal de Bock 

model.nrm <- 'items = 1-10' 
results.nrm <- mirt(data=items, model=model.nrm, itemtype="nominal", SE=TRUE, verbose=FALSE)
coef.nrm <- coef(results.nrm, IRTpars=TRUE, simplify=TRUE)
items.nrm <- as.data.frame(coef.nrm$items)
print(items.nrm)

# Plots Modelo Nominal
plot(results.nrm, type = "trace", which.items = 1, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# función de información
plot(results.nrm, type = "info", theta_lim = c(-6, 6))

# Error estándar de estimación
plot(results.nrm, type = "SE", theta_lim = c(-6, 6))

# Fin
