library(eRm)
library(TAM)

### Comparar estimaciones CML y MML
## Initializaciones
nrep <- 500     # El número de veces que se corre el procedimiento para una comparación entre CML y MML
npersons <- 1000
# 1a.  Crear el vector de betas verdaderas de los 7 ítems
truebeta <- seq(from = -3, to = +3, length.out = 7)

# Especifica de qué distribución teórica se extraen aleatoriamente las thetas.  Elige una de las siguientes dos instrucciones
#GetRandomThetas <- function() {rnorm(n = npersons, mean = 0, sd = 1)}
GetRandomThetas <- function() {rgamma(n = npersons, shape = 2)}

# Crear objetos vacias a los cuales se añadirán el error de estimación de todas las NRep réplicas
allbias.cml <- NULL
allbias.mml <- NULL

## Realizar un gran número de veces la comparación entre CML y MML
for (rep in 1:nrep){
  # 1b.  Crear el vector de thetas verdaderas de las 1,000 personas
  truetheta <- GetRandomThetas()

  # 1c. Simular datos bajo el modelo de Rasch con base en las thetas y betas verdaderas
  raschdata <- sim.rasch(persons = truetheta, items = truebeta)

  # 2. Estimar los parámetros del modelo de Rasch a partir de los datos simulados utilizando CML y MML, respectivamente
  fit.cml.raschdata <- RM(X = raschdata, sum0 = TRUE)
  fit.mml.raschdata <- tam.mml(resp = raschdata, verbose = FALSE)
  
  # 3. Sacar las betas del objeto de ajuste.  Nótese que la función RM da los parámetros de facilidad, por lo que hay que cambiar el signo de las betas por CML.
  estbeta.cml.raschdata <- -fit.cml.raschdata[["betapar"]]
  estbeta.mml.raschdata <- fit.mml.raschdata[["xsi"]][["xsi"]]
  
  # 4. Poner las betas estimadas en la misma escala que las betas verdaderas, es decir, la media de las betas estimadas se fija en 0.
  # Nótese que, por la opción sum0 = TRUE en la función RM arriba, el procedimiento resuelve la indeterminación precisamente fijando la media de las betas estimadas en 0.
  # Entonces, para CML la media de las betas ya está en 0.
  # Para MML, se resta la media de cada beta, así la media de las betas estará en 0.
  estbeta.mml.raschdata <- estbeta.mml.raschdata - mean(estbeta.mml.raschdata)
  
  # 5. Crear el vector con la diferencia absoluta entre las estimaciones CML y el valor verdadero de las betas, para cada ítem
  absbias.cml <- abs(truebeta - estbeta.cml.raschdata)
  absbias.mml <- abs(truebeta - estbeta.mml.raschdata)
  
  # 6. Añadir este vector con los sesgos de esta replica a la matriz de sesgos (finalmente tendrá 500 filas por las réplicas y 7 columnas por los ítems)
  allbias.cml <- rbind(allbias.cml, absbias.cml)
  allbias.mml <- rbind(allbias.mml, absbias.mml)
}

## Calcular y mostrar el sesgo promedio para cada ítem, tanto para CML como para MML.
colMeans(allbias.cml)
colMeans(allbias.mml)