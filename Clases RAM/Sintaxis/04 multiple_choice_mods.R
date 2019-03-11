# NRM & 3PL
library(mirt)
library(psych)
library(plyr)
library(sjPlot)
library(CTT)
library(ShinyItemAnalysis)

# Datos crudos
raw_resp <- read.csv("raw_resp_complete.csv")

# Calificar con plantilla 
RC_prim_mat <- read.csv("RC_prim_mat.csv")
key <- c(2,3,4,3,2,3,2,1,3,1,3,4,1,4,4,1,4,2,4,2,3,3,1,1,2)
scored <- as.data.frame(sapply(seq(1,length(key)), FUN = function(ii){ 1*(raw_resp[,ii] == key[ii]) } ))

# Item Analysis 
iteman.sjPlot <- sjt.itemanalysis(scored); iteman.sjPlot
iteman.CTT <- itemAnalysis(scored, itemReport=TRUE); iteman.CTT$itemReport

# Conteo
fn <- function(x){
  a = count(x)
  print(a)
}

conteo <- as.data.frame(apply(raw_resp, 2, fn)) # 2 implica columnas, 1 implica filas
conteo <- conteo[,-seq(3, 49, by=2)]; colnames(conteo)[1] <- "Opción"; conteo


# Aplica el método gráfico para cada item 
DDplot(scored)
plotDistractorAnalysis(raw_resp, key, num.groups = 5, item = 17, multiple.answers = F)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3PL model
mod.3PL <- mirt(scored, 1, itemtype = '3PL')
coef(mod.3PL, IRTpars = TRUE, simplify = TRUE)

# Plots Modelo de Respuesta Graduada
plot(mod.3PL, type = "trace", which.items = 1, par.settings = simpleTheme(lty = 1:4, lwd = 2),
     auto.key = list(points = FALSE, lines = TRUE, columns = 4))

# función de información
plot(mod.3PL, type = "info", theta_lim = c(-6, 6))

# item info
iteminfo()
extr.2 <- extract.item(mod.3PL, 2)
Theta <- matrix(seq(-4,4, by = .1))
info.2 <- iteminfo(extr.2, Theta)
plot(Theta, info.2, type = 'l', main = 'Item information')

# Error estándar de estimación
plot(mod.3PL, type = "SE", theta_lim = c(-3, 3))

# item and model fit
itemfit(mod.3PL)
M2(mod.3PL)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# NRM model

model.nrm <- 'raw_resp = 1-25' 
results.nrm <- mirt(data=raw_resp, model=model.nrm, itemtype="nominal", SE=TRUE, verbose=FALSE)
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


# item and model fit
itemfit(results.nrm)
M2(results.nrm)
