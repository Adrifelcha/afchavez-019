# Item Analysis

https://cran.r-project.org/view=Psychometrics # link con los pirncipales paquetes en R sobre Psicometría 

library(mirt) # key2binary(fulldata, key, score_missing = FALSE)
library(ShinyItemAnalysis)
library(CTT)
library(itemanalysis)
library(psychometric) 
library(sjPlot)
library(Scale)
library(epmr) # Albano 2017 with devtools bcs it's a github
library(psych) # alpha function

# raw.r: correlation between the item and the total score from the scale (i.e., item-total correlations)
# r.drop: item-total correlation without that item itself (i.e., item-rest correlation or corrected item-total correlation);
# r.cor: item-total correlation corrected for item overlap and scale reliability

# Referencias
# Handbook of Educational Measurement and Psychometrics Using R
# Introduction to Educational and Psychological Measurement Using R

# Cargar los datos
# segmentar datos de la base para jalarlos, seleccionar la fila 1; eliminar la fila 1
data <- read.csv("Clase_05-06 - Ejemplos análisis ítems.csv", stringsAsFactors=FALSE); head(data,10)
datos <- data[-1,-1]; head(datos, 10)
key <- data[1,-1]; key
clave <- as.character(as.vector(key[1,])); clave

# calificar la base con la clave de respuestas, if datos = clave then 1, else 0
library(mirt) # Este paquete ofrece una función para calificar la base "key2binary"
datos.dicotom <- as.data.frame(key2binary(datos, clave)); head(datos.dicotom, 10)
write.csv(datos.dicotom, "base_calificada.csv") # guardan la base calificada de manera dicotómica en un archivo .csv

# Computa la dificultad de los items
library(sjPlot)
iteman.sjPlot <- sjt.itemanalysis(datos.dicotom); iteman.sjPlot

library(CTT)
iteman.CTT <- itemAnalysis(datos.dicotom, itemReport=TRUE); iteman.CTT$itemReport 
# considerar como se estima la biserial en este paquete

# Computa los diversos índices de discriminación
# Computar D33 con "ShinyItemAnalysis"
library(ShinyItemAnalysis)
D33 <- as.data.frame(gDiscrim(datos.dicotom, k = 3, l = 1, u = 3)); D33 

library(psychometric)
Discrim <- as.data.frame(discrim(datos.dicotom)); Discrim

# comparar entre las dos estimaciones de discriminación
Compara.Discrimina <- cbind(D33, Discrim); Compara.Discrimina # cambiar nombre a las columnas

# Aplica el método gráfico para cada item 

DDplot(datos.dicotom)

plotDistractorAnalysis(datos, clave, num.groups = 5, item = 8, multiple.answers = F)

library(plyr)
count(datos$Ítem.8)

library(psych)
iteman.psych <- alpha(datos.dicotom); iteman.psych

# FIN

# Tarea 
# Generar la función/script/sintaxis para calcular los índices D27%, D50% o D10%
https://www.rasch.org/rmt/rmt121r.htm # referencia Tristán (1998)

