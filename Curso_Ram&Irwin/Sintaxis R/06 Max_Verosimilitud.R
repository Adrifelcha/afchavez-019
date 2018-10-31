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

