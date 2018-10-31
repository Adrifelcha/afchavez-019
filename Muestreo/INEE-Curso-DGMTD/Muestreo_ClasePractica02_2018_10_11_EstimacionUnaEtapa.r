#################################################################
#################################################################
#################################################################
###
### Curso de Muestreo - INEE
###
### Clase Practica 02 (Estimacion 1)
###
### Emilio Lopez Escobar (http://www.info-Emilio.NET)
### emilio@numerika.mx
### Ciudad de México. Octubre 11, 2018
###
#################################################################
#################################################################
#################################################################



# Primero, revisamos el directorio de trabajo actual en R.
getwd() # Del vocablo en Ingles "get working directory"



# Si es necesario definimos el directorio donde trabajaremos. Es decir, en donde estaran los datos que utilizaremos.
setwd("C:/DATOS/USBF_EMILIO/WORK/Lecturing/2017_12_ITAM_Diplomado_EstadisticaAplicada_Muestreo/08_R")



# A continuacion, leemos el conjunto de datos llamado MU284.csv. Este esta en formato .csv (Comma Separated Values)
Marco  <- read.csv(file = "MU284.csv")      #Nombre del archivo. Debe estár en el directorio de trabajo.



# Otra forma de hacerlo es utilizando los botones de R-studio... Hagamoslo...



# Ahora, echemos directamente un vistazo a los datos cargados...



# Podemos ver que ya aparece un dataframe en nuestra memoria, vemos sus caracteristicas.



# Vamos a ver las variables y les voy explicando que es cada una...



# Ahora vamos a instalar dos paquetes.... el paquete "sampling" y el paquete "samplingVarEst"



# Los paquetes se instalan utilizando clicks con R-studio o con el comando
?install.packages    # Cuando no sepa que hacer utilizo el signo de interrogaciÃ³n para abrir la ayuda relativa a ese comando....



# Si no tengo ni idea del comando, entonces utilizo doble ??



# Tercera opcion de ayuda, es utilizar Google, ponga una R antes...



# Ok, instalemos utilizando clicks con el R-Studio o con la siguiente linea de comando
#install.packages("samplingVarEst")



# Una vez instalados, ahora los cargamos...



#Recordar aquí que podemos tener muchos paquetes instalados y no necesariamente cargados (activos) en memoria...
library(sampling)
require(samplingVarEst)



# Ahora vamos a dar una revisada rapida al paquete sampling (paquete especializado en seleccion de muestras)
# En particular los comandos que utilizaremos en esta sesion:
#srswor1
#inclusionprobabilities
#UPmaxentropy
#UPbrewer



# Tambien utilizaremos el paquete samplingVarEst (paquete especializado en estimacion de varianza)
# En particular utilizaremos los comandos:
#Pk.PropNorm.U
#Est.Total.NHT
#VE.HT.Total.NHT
#VE.SYG.Total.NHT



# Una vez que ya sabemos que hace cada comando, supongamos que me interesa estimar Theta...

# Theta: Total de la variable P85

# n: De acuerdo con cierto nivel de confianza y de error absoluto... para este ejercicio utilizaremos 50

# De modo que:
n                  <- 50

# Y tenemos que N es:
N                  <- nrow(Marco)


# Supongamos que vamos a utilizar un diseno SI (muestreo aleatorio simple - sin reemplazo)
# Entonces, como no es necesario que calculemos las Pk antes de extraer la muestra, nos vamos directo a la extraccion...
# Recuerden, cada renglon en mi marco muestral es un municipio sueco....
# Revisamos de nuevo como se ejecuta el comando de seleccion de muestras aleatorias simples (sin reemplazo)
?srswor1



####
#### Nota: Estas lineas de comando que siguen a continuación NO SON EFICIENTES, son mas bien didacticas. Traten de mejorarlas o comentarlas para ustedes mismos.
####



# Extraemos las muestras... saquemos 4, piensen en 4 companias que hacen lo mismito...
s.SI1.U            <- srswor1(n,N) # Compania "El buen número"
s.SI2.U            <- srswor1(n,N) # Compania "Salgo en la TV"
s.SI3.U            <- srswor1(n,N) # Compania "Salgo en el periódico"
s.SI4.U            <- srswor1(n,N) # Compania "Macondo"



# Suponemos que se levantan los datos....



# Entonces, tenemos la variable de interes pero para cada muestra
VecY.s.SI1         <- Marco$P85[s.SI1.U==1]
VecY.s.SI2         <- Marco$P85[s.SI2.U==1]
VecY.s.SI3         <- Marco$P85[s.SI3.U==1]
VecY.s.SI4         <- Marco$P85[s.SI4.U==1]



# Tratandose de muestreo aleatorio simple tenemos que se tienen los mismos valores en las probabilidades de inclusion para todos los individuos.



# Tambien en este caso, como cada empresa tiene el mismo tamaño de muestra, todas tendran las mismas probabilidades de inclusion.
VecPk.s            <- rep(n/N, times=n)



# Si tienen duda de como se usa el comando rep, teclear ?rep



# Entonces, si estimamos puntualmente utilizando Narain(1951);Horvitz-Thompson (1952)
EstTheta1          <- Est.Total.NHT(VecY.s.SI1, VecPk.s)
EstTheta2          <- Est.Total.NHT(VecY.s.SI2, VecPk.s)
EstTheta3          <- Est.Total.NHT(VecY.s.SI3, VecPk.s)
EstTheta4          <- Est.Total.NHT(VecY.s.SI4, VecPk.s)



# Veamos las estimaciones
EstTheta1
EstTheta2
EstTheta3
EstTheta4



# A cual le creen?



# Vamos a calcular el coeficiente de variación estimado de cada medición. Pero para ello primero necesitamos calcular la varianza



# Veamos cómo se utilizan los dos comandos
?VE.HT.Total.NHT
?VE.SYG.Total.NHT



# Me hace falta la matriz de probabilidades de inclusión conjuntas MatPkl.s



# La podemos calcular de la siguiente forma:



# Camino largo...
MatPkl.s           <- matrix(n*(n-1)/(N*(N-1)), ncol=n, nrow=n)
diag(MatPkl.s)     <- n/N
MatPkl.s[1:5,1:5]



# Camino corto gracias a nuestro amigo Hajek que en 1964 publicó...   Ojo: esto es solo para muestreos de alta entropia.
MatPkl.s           <- Pkl.Hajek.s(VecPk.s)
MatPkl.s[1:5,1:5]



# Entonces, calculemos las varianzas....
EstVarEstTheta1    <- VE.HT.Total.NHT(VecY.s.SI1, VecPk.s, MatPkl.s)
EstVarEstTheta2    <- VE.HT.Total.NHT(VecY.s.SI2, VecPk.s, MatPkl.s)
EstVarEstTheta3    <- VE.HT.Total.NHT(VecY.s.SI3, VecPk.s, MatPkl.s)
EstVarEstTheta4    <- VE.HT.Total.NHT(VecY.s.SI4, VecPk.s, MatPkl.s)



# Y entonces los errores estandar son...
StdErrEstTheta1    <- sqrt(EstVarEstTheta1)
StdErrEstTheta2    <- sqrt(EstVarEstTheta2)
StdErrEstTheta3    <- sqrt(EstVarEstTheta3)
StdErrEstTheta4    <- sqrt(EstVarEstTheta4)



# Y ahora calculemos nuestro error absoluto o precision al 95% de confianza
alpha              <- 0.05
AbsErrEstTheta1    <- StdErrEstTheta1*qnorm(1-alpha/2)
AbsErrEstTheta2    <- StdErrEstTheta2*qnorm(1-alpha/2)
AbsErrEstTheta3    <- StdErrEstTheta3*qnorm(1-alpha/2)
AbsErrEstTheta4    <- StdErrEstTheta4*qnorm(1-alpha/2)



# Entonces nuestros 4 intervalos de confianza a un nivel de 95% son:
LimInfICEstTheta1  <- EstTheta1 - AbsErrEstTheta1
LimInfICEstTheta2  <- EstTheta2 - AbsErrEstTheta2
LimInfICEstTheta3  <- EstTheta3 - AbsErrEstTheta3
LimInfICEstTheta4  <- EstTheta4 - AbsErrEstTheta4
LimSupICEstTheta1  <- EstTheta1 + AbsErrEstTheta1
LimSupICEstTheta2  <- EstTheta2 + AbsErrEstTheta2
LimSupICEstTheta3  <- EstTheta3 + AbsErrEstTheta3
LimSupICEstTheta4  <- EstTheta4 + AbsErrEstTheta4



# Ahora calculemos el coeficiente de variacion estimado de cada una de las 4 estimaciones...
CVEEstTheta1       <- StdErrEstTheta1/EstTheta1
CVEEstTheta2       <- StdErrEstTheta2/EstTheta2
CVEEstTheta3       <- StdErrEstTheta3/EstTheta3
CVEEstTheta4       <- StdErrEstTheta4/EstTheta4



# Entonces si queremos tener un output bonito: (cbind pega vectores columna)
OUTPUT1            <- c(EstTheta1, EstTheta2, EstTheta3, EstTheta4)
OUTPUT1            <- cbind(EstTheta = OUTPUT1, StdErr = c(StdErrEstTheta1, StdErrEstTheta2, StdErrEstTheta3, StdErrEstTheta4))
OUTPUT1            <- cbind(OUTPUT1, LInfCI95 = c(LimInfICEstTheta1, LimInfICEstTheta2, LimInfICEstTheta3, LimInfICEstTheta4))
OUTPUT1            <- cbind(OUTPUT1, LSupCI95 = c(LimSupICEstTheta1, LimSupICEstTheta2, LimSupICEstTheta3, LimSupICEstTheta4))
OUTPUT1            <- cbind(OUTPUT1, CVE = c(CVEEstTheta1, CVEEstTheta2, CVEEstTheta3, CVEEstTheta4))
OUTPUT1



# Y si ahora muestreamos con probabilidades desiguales...



# Con que sera mejor muestrear, con probabilidades proporcionales a la variable P75 o con probabilides iguales...?



# Hagamoslo...



# Primero construimos nuestras probabilidades de inclusion proporcional a P75
?Pk.PropNorm.U
VecPk.U            <- Pk.PropNorm.U(n, Marco$P75)



# Ahora necesitamos las probabilidades de inclusion de segundo orden, esto seria con el comando
MatPkl.U           <- Pkl.Hajek.U(VecPk.U)   #Ojo: No espantarse, puede salir error si excedemos las dimensiones permitidas...



# Que alternativas tenemos?



# Por que antes cuando utilizmos SI primero extrajimos las muestras y luego construimos las Pik's?



# Claramente porque no importa que muestra cae, podemos reconstruir las probabilides de inclusion sin problema...



# Para el caso de probabilidades desiguales podemos hacer lo mismo con el comando Pkl.Hajek.s en lugar de utilizar Pkl.Hajek.U



# Este comando Pkl.Hajek.s estima las probas de inclusion de segundo orden a partir de las probas de inclusion de primer orden pero de los individuos que tenemos en la muestra extraida...



# Es decir, manejaremos matrices de n por n, que son mucho mas manejables...



# Entonces vamos a extraer primero las muestras... y luego estimamos las Pikls



# Extraemos las muestras... vamos a utilizar el muestreo de Brewer que es de alta entropia Grafstrom (2010, p. 97) da evidencia de que es de alta entropía aunque no está demostrado. Berger (2011) plantea que basta que el muestreo sea de alta entropía (aunque no maxima) para poder utilizar los resultados de Hajek (1964)
s.Br1.U            <- UPbrewer(VecPk.U)
s.Br2.U            <- UPbrewer(VecPk.U)
s.Br3.U            <- UPbrewer(VecPk.U)
s.Br4.U            <- UPbrewer(VecPk.U)



# Notar que si hubieramos utilizado UPmaxentropy hubiera sido muy lento... pueden hacer la prueba en su computadora... Notar tambien que el paquete sampling esta programado en R, seria bueno tener una version mas veloz (programado en C)



# Y entonces ahora si, estimamos nuestras probabilidades de inclusion de 2do orden a partir de datos muestrales.... (ver la expresion en el manual del paquete samplingVarEst).



# Primero creo un vector con las probabilidades de inclusion de primer orden con solo los datos muestrales (tengo que hacer uno para cada muestra que saque)
VecPk.s1           <- VecPk.U[s.Br1.U==1]
VecPk.s2           <- VecPk.U[s.Br2.U==1]
VecPk.s3           <- VecPk.U[s.Br3.U==1]
VecPk.s4           <- VecPk.U[s.Br4.U==1]



# Como tenemos la matrix N por N de Pkl's tenemos que quedarnos con los renglones y las columnas de esta:
MatPkl.s1          <- MatPkl.U[s.Br1.U==1,s.Br1.U==1]
MatPkl.s2          <- MatPkl.U[s.Br2.U==1,s.Br2.U==1]
MatPkl.s3          <- MatPkl.U[s.Br3.U==1,s.Br3.U==1]
MatPkl.s4          <- MatPkl.U[s.Br4.U==1,s.Br4.U==1]
MatPkl.s1[1:5,1:5]



# Si no tuviera la matrix poblacional de Pkl's entonces creo las matrices de probas de 2do orden estimadas utilizando la version muestral del Hajek(1964)
MatPkl.s1          <- Pkl.Hajek.s(VecPk.s1)
MatPkl.s2          <- Pkl.Hajek.s(VecPk.s2)
MatPkl.s3          <- Pkl.Hajek.s(VecPk.s3)
MatPkl.s4          <- Pkl.Hajek.s(VecPk.s4)
MatPkl.s1[1:5,1:5]



# Ahora creo mis datos muestrales, i.e. solo tendre datos observados de la variable Y para aquellos que cayeron en muestra...
VecY.s.Br1         <- Marco$P85[s.Br1.U==1]
VecY.s.Br2         <- Marco$P85[s.Br2.U==1]
VecY.s.Br3         <- Marco$P85[s.Br3.U==1]
VecY.s.Br4         <- Marco$P85[s.Br4.U==1]



# Entonces, si estimamos puntualmente utilizando Narain(1951);Horvitz-Thompson (1952)
EstTheta1          <- Est.Total.NHT(VecY.s.Br1, VecPk.s1)
EstTheta2          <- Est.Total.NHT(VecY.s.Br2, VecPk.s2)
EstTheta3          <- Est.Total.NHT(VecY.s.Br3, VecPk.s3)
EstTheta4          <- Est.Total.NHT(VecY.s.Br4, VecPk.s4)



# Entonces, calculemos las varianzas....
#
#
#
#
# Pero aqui, como usamos probabilidades desiguales tiene más sentido utilizar SYG en lugar de NHT para la estimación de varianza, no tanto por el asunto de obtener valores negativos, sino porque es mejor estimador (quizas estas diferencias aquí no sean tan importantes porque tenemos una población grande y un tamano de muestra grande... estos argumentos son mas utiles cuando se está operando en poblaciones moderadas y tamanos de muestra más pequeños, e.g. en algunos estratos en particular)
# Ademas, recuerden que el estimador de varianza de SYG es mas rapido porque hace la mitad de calculos
#
#
#
EstVarEstTheta1    <- VE.SYG.Total.NHT(VecY.s.Br1, VecPk.s1, MatPkl.s1)
EstVarEstTheta2    <- VE.SYG.Total.NHT(VecY.s.Br2, VecPk.s2, MatPkl.s2)
EstVarEstTheta3    <- VE.SYG.Total.NHT(VecY.s.Br3, VecPk.s3, MatPkl.s3)
EstVarEstTheta4    <- VE.SYG.Total.NHT(VecY.s.Br4, VecPk.s4, MatPkl.s4)



# Y entonces los errores estandar son...
StdErrEstTheta1    <- sqrt(EstVarEstTheta1)
StdErrEstTheta2    <- sqrt(EstVarEstTheta2)
StdErrEstTheta3    <- sqrt(EstVarEstTheta3)
StdErrEstTheta4    <- sqrt(EstVarEstTheta4)



# Y ahora calculemos nuestro error absoluto o precision al 95% de confianza
alpha              <- 0.05
AbsErrEstTheta1    <- StdErrEstTheta1*qnorm(1-alpha/2)
AbsErrEstTheta2    <- StdErrEstTheta2*qnorm(1-alpha/2)
AbsErrEstTheta3    <- StdErrEstTheta3*qnorm(1-alpha/2)
AbsErrEstTheta4    <- StdErrEstTheta4*qnorm(1-alpha/2)



# Entonces nuestros 4 intervalos de confianza a un nivel de 95% son:
LimInfICEstTheta1  <- EstTheta1 - AbsErrEstTheta1
LimInfICEstTheta2  <- EstTheta2 - AbsErrEstTheta2
LimInfICEstTheta3  <- EstTheta3 - AbsErrEstTheta3
LimInfICEstTheta4  <- EstTheta4 - AbsErrEstTheta4
LimSupICEstTheta1  <- EstTheta1 + AbsErrEstTheta1
LimSupICEstTheta2  <- EstTheta2 + AbsErrEstTheta2
LimSupICEstTheta3  <- EstTheta3 + AbsErrEstTheta3
LimSupICEstTheta4  <- EstTheta4 + AbsErrEstTheta4



# Ahora calculemos el coeficiente de variación estimado de cada una de las 4 estimaciones...
CVEEstTheta1       <- StdErrEstTheta1/EstTheta1
CVEEstTheta2       <- StdErrEstTheta2/EstTheta2
CVEEstTheta3       <- StdErrEstTheta3/EstTheta3
CVEEstTheta4       <- StdErrEstTheta4/EstTheta4



# Y como ahora estamos muestreando diferente del SI entonces sí tiene sentido calcular el deff (estimado)
VecPk.s            <- rep(n/N, times=n) # Ya lo teniamos en memoria pero para recordar que es... Notar que es igual en todas las estimaciones porque es para un diseno SI y todas las muestras tienen el mismo tamaño.
deffEstTheta1      <- EstVarEstTheta1/VE.SYG.Total.NHT(VecY.s.Br1, VecPk.s, Pkl.Hajek.s(VecPk.s))
deffEstTheta2      <- EstVarEstTheta2/VE.SYG.Total.NHT(VecY.s.Br2, VecPk.s, Pkl.Hajek.s(VecPk.s))
deffEstTheta3      <- EstVarEstTheta3/VE.SYG.Total.NHT(VecY.s.Br3, VecPk.s, Pkl.Hajek.s(VecPk.s))
deffEstTheta4      <- EstVarEstTheta4/VE.SYG.Total.NHT(VecY.s.Br4, VecPk.s, Pkl.Hajek.s(VecPk.s))



# Entonces si queremos tener un output bonito:
OUTPUT2            <- c(EstTheta1, EstTheta2, EstTheta3, EstTheta4)
OUTPUT2            <- cbind(EstTheta = OUTPUT2, StdErr = c(StdErrEstTheta1, StdErrEstTheta2, StdErrEstTheta3, StdErrEstTheta4))
OUTPUT2            <- cbind(OUTPUT2, LInfCI95 = c(LimInfICEstTheta1, LimInfICEstTheta2, LimInfICEstTheta3, LimInfICEstTheta4))
OUTPUT2            <- cbind(OUTPUT2, LSupCI95 = c(LimSupICEstTheta1, LimSupICEstTheta2, LimSupICEstTheta3, LimSupICEstTheta4))
OUTPUT2            <- cbind(OUTPUT2, CVE = c(CVEEstTheta1, CVEEstTheta2, CVEEstTheta3, CVEEstTheta4))
OUTPUT2            <- cbind(OUTPUT2, deff = c(deffEstTheta1, deffEstTheta2, deffEstTheta3, deffEstTheta4))
OUTPUT2



# Como son los FE de cada diseno?
summary(1/VecPk.s)
plot(sort(1/VecPk.s))
summary(1/VecPk.s1)
plot(sort(1/VecPk.s1))



# Notar que si el tamano de muestra es generoso, incluso con muestreo SI se obtienen buenas estimaciones. Pero si tenemos poquita muestra, entonces el SI tendera a fallar y sera mucho mejor utilizar pesos desiguales.



# Entonces, utilizando probabilidades desiguales esperamos que la distribucion muestral de nuestro estimador está más concentrada alrededor del verdadero valor, es decir, que utilizando probabilidades desiguales voy a obtener con mucho menor frecuencia relativa estimaciones que disten mucho del verdadero valor...



# Nomás por puro ocio, revisemos qué tan lejos estamos del verdadero valor en cada caso
OUTPUT1
OUTPUT2
# El verdadero valor es:
Theta              <- sum(Marco$P85)
Theta



# Esperamos que vamos a tener mejores estimaciones utilizando un diseno diferente al muestreo SI por como se comporta la variable de interes



# Para que esto de utilizar probas desiguales funcione, tenemos que la variable de interes tiene que estar correlacionada con la variable que estamos utilizando para calcular las Pks



# Chequemos que tal esta la correlacion
cor(Marco$P75,Marco$P85)



# Usar las muestras con probas desiguales pero estimar con SI (vicio del mercado - exagerado)
EstTheta1          <- Est.Total.NHT(VecY.s.Br1, VecPk.s)
EstTheta2          <- Est.Total.NHT(VecY.s.Br2, VecPk.s)
EstTheta3          <- Est.Total.NHT(VecY.s.Br3, VecPk.s)
EstTheta4          <- Est.Total.NHT(VecY.s.Br4, VecPk.s)
EstTheta1
EstTheta2
EstTheta3
EstTheta4
