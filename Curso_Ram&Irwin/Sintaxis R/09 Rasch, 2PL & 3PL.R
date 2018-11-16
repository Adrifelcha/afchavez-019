# Leer los datos
SAPA <- read.csv("SAPA.csv")
SAPA_items <- SAPA[,-1]
SAPA_items[is.na(SAPA_items)] <- 0

# Calcular la suma de las puntuaciones de los ítems por persona para obtener su puntuación observada en el test
SAPA$SumScore <- rowSums(SAPA_items)
SAPA.itemmean <- apply(SAPA_items, 2, mean)

# CML para estimar los parámetros de los ítems en el modelo de Rasch
library(eRm)
fit.items.CML1 <- RM(SAPA_items, sum0=FALSE);
fit.items.CML2 <- RM(SAPA_items, sum0=TRUE);
mean(fit.items.CML2$betapar)

# Extraer las betas
beta.CML1 <- fit.items.CML1$betapar
beta.CML2 <- fit.items.CML2$betapar

# Cambiar el signo de las betas
beta.CML1 <- -beta.CML1
beta.CML2 <- -beta.CML2

# Generar un diagrama de puntos para comparar las betas de las dos estimaciones
plot(x = beta.CML1, y = beta.CML2,
     main = 'ComparaciÃ³n de las betas obtenidas por CML1 y CML2',
     xlab = 'Beta CML1', ylab = 'Beta CML2')

# Generar un diagrama de puntos para comparar las betas y los índices de dificultad
plot(x = SAPA.itemmean, y = beta.CML1,
     main = 'ComparaciÃ³n de las betas y los índices p',
     xlab = 'p', ylab = 'Beta CML2')

# Estimar los parÃ¡metros de las personas utilizando las estimaciones de los ítems en al paso previo
fit.persons.CML1 <- person.parameter(fit.items.CML1);
fit.persons.CML2 <- person.parameter(fit.items.CML2);

# Generar un histograma de las estimaciones de las thetas
theta.CML1 <- fit.persons.CML1$theta.table$`Person Parameter`
hist(theta.CML1,
     main = 'Histograma de las thetas obtenidas con las betas de CML1',
     xlab = 'Theta CML1', ylab = 'Frec.')

# MML para estimar los parámetros de los ítems en el modelo de Rasch
library(mirt)
fit.MML.mirt <- mirt(data = SAPA_items, model = 'F = 1 - 16', itemtype = 'Rasch')
params.MML.mirt <- coef(fit.MML.mirt, IRTpars = TRUE, simplify = TRUE)
beta.MML.mirt <- params.MML.mirt$items[,"b"]

library(TAM)
fit.items.MML.tam <- tam.mml(resp = SAPA_items, irtmodel = "1PL")
beta.MML.tam <- fit.items.MML.tam$xsi$xsi

# Cambiar el signo de las betas en las distintas estimaciones
beta.MML.mirt <- -beta.MML.mirt
beta.MML.tam <- -beta.MML.tam

# Estimar los parámetros de los Ãítems en el modelo 2PL
fit.2PL <- mirt(data = SAPA_items, model = 'F = 1 - 16', itemtype = '2PL')
params.2PL.mirt <- coef(fit.2PL, IRTpars = TRUE, simplify = TRUE)
itemparams.2PL.mirt <- params.2PL.mirt$items

### Graficar las curvas características de los ítems en el 2PL --- Por completar
plot(fit.2PL, type = "itemscore")
itemplot(fit.2PL, 1)
par(new=T)
itemplot(fit.2PL, 2)
###

# Estimar los parámetros de los ítems en el modelo 2PL
fit.3PL <- mirt(data = SAPA_items, model = 'F = 1 - 16', itemtype = '3PL')
params.3PL.mirt <- coef(fit.3PL, IRTpars = TRUE, simplify = TRUE)
itemparams.3PL.mirt <- params.3PL.mirt$items

# Generar la función de informaciÃ³n de los ítems y el test (bajo el modelo de Rasch)
library(eRm)
info.items <- item_info(fit.items.CML1, theta = seq(-5, 5, 0.01))
info.test <- test_info(fit.items.CML1, theta = seq(-5, 5, 0.01))
plotINFO(fit.items.CML1, type = "item")

# Calcular la función de información de los ítems y el test manualmente
rasch_item_info <- function(beta, theta) {
  prob_acertar <- exp(theta - beta) / (1 + exp(theta - beta))
  info <- prob_acertar * (1 - prob_acertar)
  as.vector(return(info))
}
theta <- seq(-5, 5, 0.01)
myinfo <- data.frame(theta)
myinfo$item1 <- rasch_item_info(beta.CML1[1], theta)
myinfo$item2 <- rasch_item_info(-beta.CML1[2], theta)
myinfo$item3 <- rasch_item_info(beta.CML1[3], theta)
myinfo$item4 <- rasch_item_info(beta.CML1[4], theta)
myinfo$item5 <- rasch_item_info(beta.CML1[5], theta)
myinfo$item6 <- rasch_item_info(beta.CML1[6], theta)
myinfo$item7 <- rasch_item_info(beta.CML1[7], theta)
myinfo$item8 <- rasch_item_info(beta.CML1[8], theta)
myinfo$item9 <- rasch_item_info(beta.CML1[9], theta)
myinfo$item10 <- rasch_item_info(beta.CML1[10], theta)
myinfo$item11 <- rasch_item_info(beta.CML1[11], theta)
myinfo$item12 <- rasch_item_info(beta.CML1[12], theta)
myinfo$item13 <- rasch_item_info(beta.CML1[13], theta)
myinfo$item14 <- rasch_item_info(beta.CML1[14], theta)
myinfo$item15 <- rasch_item_info(beta.CML1[15], theta)
myinfo$item16 <- rasch_item_info(beta.CML1[16], theta)
myinfo$test <- rowSums(myinfo[,2:17])
