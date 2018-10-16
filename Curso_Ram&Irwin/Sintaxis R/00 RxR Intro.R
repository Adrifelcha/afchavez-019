# Una introducción a RxR
# 1.0 Instalación

# La página web principal donde se aloja R es

#     http://www.r-project.org

# De donde podrán descargar el programa base

#     https://cran.rstudio.com/bin/windows/base/
#     https://cran.rstudio.com/bin/macosx/
  
# Para simplificarles la vida ¡les sugiero! RStudio 
  
#     https://www.rstudio.com/




# Función para instalar paquetes y cargarlos
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Paquetes con los cuales usar la función
paquetes <- c("MASS", "foreign", "readxl", "car", "plyr", "Rcmdr", "dplyr", 
                "tidyverse", "psych", "TAM", "ltm", "psychometric", "foreign", 
              "plotly", "ggthemes", "markdown", "rmarkdown", "shiny", "learnr", 
              "QuantPsyc", "datasauRus", "caret", "mirt")

# Uso de la función
ipak(paquetes)


# librerías instalación 
install.packages(c("MASS", "foreign", "readxl", "car", "plyr", "Rcmdr", "dplyr", 
                   dependencies=TRUE, repos='http://cran.rstudio.com/'))

library(MASS)
library(foreign)
library(readxl)
library(readxl)
library(car)
library(plyr)
#library(Rcmdr)
library(dplyr)


# Para salir
  
q()

# limpiar consola
ctrl + l
# limpiar consola (alternativa)
cat("\014") 

# limpiar workspace
rm(list=ls())


# limpiar plots
if(!is.null(dev.list())) dev.off()


# 2.0 Los básicos en R
#   2.1 Aritmética escalar

+ for addition
- for subtraction
* for multiplication
/ for division
O for exponentiation (i.e., raising to a power)
%/% for integer division
%% for remainder from integer division

1 + 2 - 3 * 4 / 5 ^ 6
#[1] 2.999232

(1 + 2 - 3) * 4 / 5 ^ 6
#[1] 0

1 + 2 - (3 * 4 / 5) ^ 6
#[1] -188.103

1 + (2 - 3) * 4 / 5 ^ 6
#[1] 0.999744

1 + (2 - 3) * (4 / 5) ^ 6
#[1] 0.737856

6 + 5 - 4 * 3 / 2 ^ 1 # La respuesta es... [Enter]
#[1] 5

options(digits=22)
1 / 3
#[1] 0.3333333333333333148296

pi
#[1] 3.141592653589793115998

options(digits=7)

# Más funciones

abs #absolute value
exp #exponential (e to a power)
log #logarithm
log10 #logarithm of base 10
sqrt #square root
floor #largest integer, less than or equal to
ceiling #smallest integer, greater than or equal to
trunc #truncation to the nearest integer
factorial #factorial

# Operadores lógicos

> #greater than
< #less than
>= #greater than or equal to
<= #less than or equal to
== #equality
!= #non-equality
& #elementwise and
| #elementwise or
&& #control and
|| #control or
! #unary not

# Funciones trigonométricas

cos #cosine
sin #sine
tan #tangent
acos #arc cosine
asin #arc sine
atan #arc tangent
cosh #hyperbolic cosine
sinh #hyperbolic sine
tanh #hyperbolic tangent
acosh #arc hyperbolic cosine
asinh #arc hyperbolic sine
atanh #arc hyperbolic tangent

sin(pi / 6)
#[1] 0.5

pi / 6
#[1] 0.5235988

sin(0.5235988)
#[1] 0.5

sin((30 / 180) * pi)
#[1] 0.5

#sin(pi / 6

# + 
# + )
#[1] 0.5

a <- sin(pi / 6)
b <- cos(pi / 6)
c <- sqrt(a ^ 2 + b ^ 2)
c

#[1] 1

# Subconjuntos de datos

x <- c(1, 2, 3, 3, 4, 5, 16, 17, 18, 19, 20)

x[1]
#[1] 1

x[2 : 4]
#[1] 2 3 3

x[-3] #Quita el tercer valor en los datos
#[1] 1 2 2

x[x < 3]
#[1] 1 2

x[x > 2]
#[1] 3

x <- x[-3]; x # Quita el décimo valor, es una manera alternativa a la línea 141 del código
#[1] 1 2 2

x <- x[-1]; x
#[1] 1 2 2

x <- c(1, 2, 3, 2)
x < 3
#[1] TRUE TRUE FALSE TRUE

labels <- c("red", "white", "blue", "white"); labels
labels # si lo colocan abajo o después del ; se ejecuta la función
#[1] "red" "white" "blue" "white"

names(x) <- labels; x
#red white blue white
#1 2 3 2

names(x) <- NULL; x
[1] 1 2 3 2

# Vectores

numeric #a vector of zeros with the length of the argument
character #a vector of blank characters of argument length
logical #a vector of FALSE of argument length
seq #argument 1 to argument 2 with the increment of argument 3
1 : 4 #numbers equivalent to seq(1, 4, 1)
rep #replicate argument1 as many times as argument 2
as.numeric #conversion to numeric
as.character #conversion to string-type
as.logical #conversion to logical
factor #creating factor from vector

# Ejemplos

x <- 1 : 4; x
#[1] 1 2 3 4

x <- seq(1, 4, 1); x
#[1] 1 2 3 4

x <- seq(1, 2, 0.2); x
#[1] 1.0 1.2 1.4 1.6 1.8 2.0

x <- rep(1, 4); x
#[1] 1 1 1 1


x <- c(rep(1,4), rep(2,2)); x
#[1] 1 1 1 1 2 2

# Matrices y funciones de matrices

X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), nrow=4); X
#    [,1] [,2]
#[1,]  1    1
#[2,]  1    2
#[3,]  1    3
#[4,]  1    2

X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), nrow=2); X
#      [,1] [,2] [,3] [,4]
#[1,]    1    1    1    3
#[2,]    1    1    2    2

X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), ncol=2); X
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), nrow=4, ncol=2); X
X <- matrix(c(1, 1, 1, 2, 1, 3, 1, 2), nrow=4, byrow=T); X
X <- matrix(c(1, 1, 1, 2, 1, 3, 1, 2), ncol=2, byrow=T); X
X <- matrix(c(1,1,1,2,1,3,1,2), nrow=4, ncol=2, byrow=T); X


# Elementos de una matriz (Filas y Columnas)
X[2,2]
#[1] 2

X[,2]
#[1] 1 2 3 2

X[2,]
#[1] 1 2

X[1 : 2,]
#    [,1] [,2]
#[1,]  1    1
#[2,]  1    2

u <- c(1, 1, 1, 1)
x <- c(1, 2, 3, 2)
X <- cbind(u, x); X # Pegamos los vectores
#u x
#[1,] 1 1
#[2,] 1 2
#[3,] 1 3
#[4,] 1 2

X <- cbind(x, u); X # Pegamos los vectores con otro orden
#x u
#[1,] 1 1
#[2,] 2 1
#[3,] 3 1
#[4,] 2 1

X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), ncol=2,
            dimnames=list(c(),c("uU","xX"))); X

# Alternativamente se pueden ocupar las siguientes funciones para nombrar filas y columnas
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2), ncol=2)
colnames(X) <- c("u", "x")
rownames(X) <- c("1st", "2nd", "3rd", "4th")
X

# Otro ejemplo
r1 <- c(1, 1); r2 <- c(1, 2); r3 <- c(1, 3); r4 <- c(1, 2)
X <- rbind(r1, r2, r3, r4); X
#[,1] [,2]
#r1 1 1
#r2 1 2
#r3 1 3
#r4 1 2

rownames(X) <- c() # Lo regresamos al default
X

# Método alternativo para construir matrices con la función array, arreglo
X <- array(c(1, 1, 1, 1, 1, 2, 3, 2), dim=c(4,2)); X
#[,1] [,2]
#[1,] 1 1
#[2,] 1 2
#[3,] 1 3
#[4,] 1 2

# Le preguntamos a R varias cosillas
dim(X) # dimensión = matriz de 4 filas por 2 columnas
#[1] 4 2

nrow(X) # tamaño o número de filas
#[1] 4

ncol(X) # tamaño o número de columnas
[1] 2

  
# 4.0 Data Frames o arreglos bidimensionales

x <- c(1, 2, 3, 2)
y <- c(1, 3, 2, 2)
X <- data.frame(x, y); X
#  x y
#1 1 1
#2 2 3
#3 3 2
#4 2 2

X[,1] # Extrayendo variables Método A (elimina la que no se declara)
X$x   # Extrayendo variables Método B (elimina la que se declara) de X deshecha x

names(X)
#[1] "x" "y"
 
# Instrucción contraria, en donde se anexa una  nueva variable (z a X) 
X$z <- c("a", "b", "c", "d"); X
#  x y z
#1 1 1 a
#2 2 3 b
#3 3 2 c
#4 2 2 d

# Si removemos una variable por partes

X <- X[,-3]; X #(aquí se eliminó la tercera columna)
X <- X[,-1]; X #(aquí se eliminó la primera columna) ojo de volver a crear la matriz original con X = xyz

X <- X[,1 : 2]; X #(aquí se eliminó la tercera columna)

X <- edit(data.frame()) # Abre un diálogo para que con una GUI se edite el arreglo
save(X, file="X.RData"); X
load("X.RData"); X

#D:/rvazquez/Google Drive/INEE/1. Curso UNAM Psicometría aplicada/IRT/R scripts/X.RData #Ruta donde se guardó en el WD por default

write.table(X, file="X.txt", sep=" ") # El conjunto de datos que se guardaron en el WD

# Working Dir
getwd() # Para obtenerlo

setwd("C:\\") #Para fijarlo método A
setwd("C:/") #Para fijarlo método B

attach(X); X # Para cargar datos ubicados en el WD

detach(X) # Para desechar los datos de la sesión actual

ls() # Lista de los objetos actuales

x <- c(1, 2, 3, 2)
ls()
#[1] "x"

rm(x) # Remueve este objeto de la lista actual
ls() # se desecho x minúscula

rm(list=ls()) # Limpiar todo el área de trabajo




###### 8. Figuras/Gráficas

x <- c(1,2,2,3)
hist(x)
hist(x, seq(.5,3.5,1))
hist(x, seq(.5,3.5,1), xlim=c(0,4))
hist(x, seq(0,5,0.5), xlim=c(-1.0, 5), ylim=c(-2,4.5))

#### Funciones más comunes para crear gráficas/figuras en R

plot #generic function with many objects
boxplot #box and whisker plots
matplot #plots of two or more vectors of an equal length
qqnorm #quantile-quantile plots
qqline #draw a line in qqnorm
qqplot #distribution comparison plots
pairs #plots of a matrix or data frame
coplot #conditioning plots for 3 or more variables
dotchart #construct a dotchart
image #plot of three variables
coutour #plot of three variables
persp #plot of three variables
stem #stem and leaf plot

# Para variables categóricas

barplot #bar graph
pie #pie chart
dotchart #frequency summary with dots


########## HEEEEEEELP!
help(plot)
?plot
example(boxplot) # Ayuda muuuuy gráfica en plots
example(coplot)
demo(graphics)

x <- rnorm(100) # algunos plots se utilizan con comandos de modelamiento estadístico
y <- rnorm(100)
par(mfrow = c(2,2))
plot(lm(y ~ x))

win.graph() # ejecuta una ventana para una nueva gráfica/figura

# Ejemplos de comandos que listan devices

dev.list()
dev.cur()
dev.set()
dev.off()
graphics.off()


# extensiones 
postscript #PostScript or Encapsulated PostScript (7KB)
pdf #Portable Document Format (5KB)
win.metafile #Windows Metafile (15KB)
png #Portable network Graphics (3KB)
jpeg #Joint Photographic Experts Group (10KB)
bmp #Bitmap (227KB)
tiff #Tagged Image File Format (676KB)


#Guardamos en un formato específico  
x <- c(1, 2, 3, 2); y <- c(1, 3, 2, 2)
postscript("xyplot.eps")
plot(x, y)
dev.off() # esta línea elimina su plot, OJO!


# Ejemplo real 
# Cuidar la creaión del metaarchivo que colapsa los plots 

postscript("BIRTFigure1p1.eps", width=3.5, height=2.5, pointsize=7)
par(lab=c(7,3,3))
theta <- seq(-3, 3, .1)
b <- 0
a <- 1
P <- 1 / (1 + exp(-a * (theta - b)))
#x11(width = 10, height = 6)      # Este comando se incorpora para abrir ventana fuera de rstudio y genera el plot. 
plot(theta, P, type="l", xlim=c(-3,3), ylim=c(0,1),
       xlab=expression(paste("Habilidad, ",theta)),
     ylab=expression(paste(
       "Probabilidad de responder correctamente, P(",theta,")")))
dev.off() #Emplear esta linea tantas veces hasta obtener error


##### LaTeX input lines

\begin{figure}[ht]
\centering
\includegraphics[scale=0.3,angle=-90]{BIRTFigure1p1.eps}
\caption{A typical item characteristic curve.}
\label{BIRTFigure1p1}
\end{figure}


###### 9. VAlores perdidos
# In R, not available (i.e., NA) is used as a missing value

x <- c(1, NA, 3, 2); x
#[1] 1 NA 3 2

is.na(x)
#[1] FALSE TRUE FALSE FALSE

sum(is.na(x))
#[1] 1

sum(!is.na(x))
#[1] 3

newx <- x[!is.na(x)]; newx
#[1] 1 3 2

x[2] <- sum(newx)/sum(!is.na(x)); x
#[1] 1 2 2 3

# NaN significa "Not a Number"
# Muy distinto a Inf "infinito"

x1 <- 0/0; x1
x2 <- Inf; x2
x3 <- Inf - Inf; x3
x <- c(x1, x2, x3, 2); x
#[1] NaN NaN NaN 2

is.na(x)
#[1] TRUE TRUE TRUE FALSE

# Paquetes en R

.libPaths()
library()


install.packages("MASS", dependencies = T)
library(MASS)

# Cargar y guardar datos
# .csv format

PLANEA.csv <- read.csv("PLANEA12_2017_AlumnosTEST.csv")

# .sav format
library(foreign)

file.choose()

PLANEA.sav <- read.spss("D:\\rvazquez\\Dropbox\\Curso UNAM FP -Modelos Psicométricos\\2019-1\\R\\Planea12_2017_AlumnosTEST.sav"
                        , to.data.frame=TRUE)

# .xlsx format
library(readxl)

# xls files
PLANEA.xls <- read_excel("PLANEA12_2017_AlumnosTEST.xls")
# PLANEA.xls.DF <- data.frame(PLANEA.xls)

# xlsx files
PLANEA.xlsx <- read_excel("PLANEA12_2017_AlumnosTEST.xlsx")
# PLANEA.xlsx.DF <- data.frame(PLANEA.xlsx)


# Recode
library(car)
library(plyr)

PLANEA12_2017 <- read.csv("PLANEA12_2017_AlumnosTEST.csv")

MA_13 <- PLANEA12_2017$MA_13; MA_13
MA_15 <- PLANEA12_2017$MA_15; MA_15
MA_17 <- PLANEA12_2017$MA_17; MA_17
MA_18 <- PLANEA12_2017$MA_18; MA_18

count(MA_13); count(MA_15); count(MA_17); count(MA_18)

PLANEA12_2017$MA_13[PLANEA12_2017$MA_13==94] <-NA
PLANEA12_2017$MA_15[PLANEA12_2017$MA_15==94] <-NA
PLANEA12_2017$MA_17[PLANEA12_2017$MA_17==94] <-NA
PLANEA12_2017$MA_18[PLANEA12_2017$MA_18==94] <-NA

PLANEA12_2017$MA_13[PLANEA12_2017$MA_13==99] <-NA
PLANEA12_2017$MA_15[PLANEA12_2017$MA_15==99] <-NA
PLANEA12_2017$MA_17[PLANEA12_2017$MA_17==99] <-NA
PLANEA12_2017$MA_18[PLANEA12_2017$MA_18==99] <-NA

MA_13 <- PLANEA12_2017$MA_13; MA_13
MA_15 <- PLANEA12_2017$MA_15; MA_15
MA_17 <- PLANEA12_2017$MA_17; MA_17
MA_18 <- PLANEA12_2017$MA_18; MA_18

count(MA_13); count(MA_15); count(MA_17); count(MA_18)

# RECODE or REVERSE 
originales <- cbind(MA_13, MA_15); originales

# Recode SAB_57, SAB_59 & SAB_63

MA_13 <- recode(MA_13, "1=3; 3=1")
MA_15 <- recode(MA_15, "1=3; 3=1")

recoded <- cbind(MA_13, MA_15); recoded

both <- cbind(originales,recoded); 
head(both, 100) # primeros 25
tail(both, 25) # ultimos 25

# Rcmdr



# Sumar y agregar k a filas o columnas

originales.df <- data.frame(originales); head(originales.df, 15)

originales_mas_k <- originales + 1; head(originales_mas_k, 15)

originales_menos_k <- originales_mas_k - 1; head(originales_menos_k, 15)

nuevos <- originales.df$MA_13 + originales.df$MA_15; head(nuevos, 15)


originales.df<-cbind(originales.df,nuevos); head(originales.df)


library(dplyr)

originales.df.sin.nuevos <- select(originales.df, -nuevos); head(originales.df.sin.nuevos, 15)

# ordenar datos (sort)

sort(originales.df$MA_13,  decreasing = FALSE)
order(originales.df$MA_13) # ascendiente


sort(originales.df$MA_13, decreasing = TRUE)
order(-originales.df$MA_13) # descendiente


reordenados<- order(originales.df$MA_13, -originales.df$MA_15); reordenados

write.csv(reordenados, "reordenados.csv")



# merge by rows


PLANEA_A <- read.csv("PLANEA12_2017_AlumnosTEST.csv")

PLANEA_B <- read.csv("PLANEA12_2017_AlumnosTESTb.csv")



PLANEA_C <- rbind(PLANEA_A, PLANEA_B) # same number of columns in both data.frame's


# split & merge data 4 rows or cases
# seleccionar casos que cumplan la condición 1 en una variable


AGUASCALIENTES <- subset(PLANEA_A, ENT == "AGUASCALIENTES")

ID_ENT_1 <- subset(PLANEA_A, ID_ENT == 1)



# con varias condiciones


AGUASCALIENTES_Estrato <- subset(PLANEA_A, ENT == "AGUASCALIENTES" & ESTRATO == 1208)

AGUASc <- PLANEA_A[(PLANEA_A$ENT == "AGUASCALIENTES") & (PLANEA_A$ESTRATO == 1208), ]

ID_ENT_1_estrato <- subset(PLANEA_A, ID_ENT == 1 & ESTRATO == 1208)

alum.499.to.1449 <- subset(PLANEA_A, ALUMNO > 499 & ALUMNO < 1449)


# Conteo por condiciones y elementos


entidades <- as.factor(PLANEA_A$ENT); entidades

levels(entidades)

count(entidades)

summary(entidades)

nombres.ent <- cbind(levels(entidades)); nombres.ent

tabla_ent <- data.frame(summary(entidades)); tabla_ent

tabla.final.entidades <- cbind(nombres.ent, tabla_ent$summary.entidades.); tabla.final.entidades


tabla.final.entidades.DF <- data.frame(tabla.final.entidades); tabla.final.entidades.DF


names(tabla.final.entidades.DF)[names(tabla.final.entidades.DF) == 'X1'] <- 'Entidad'
names(tabla.final.entidades.DF)[names(tabla.final.entidades.DF) == 'X2'] <- 'Frecuencia'

tabla.final.entidades.DF



### por id_ent

id.ent <- as.factor(PLANEA_A$ID_ENT); id.ent

levels(id.ent)

tabla_id.ent <- data.frame(summary(estratos)); tabla_id.ent


# Ejercicio estadística descriptiva univariada y bivariada 
# Generador de datos

# Script Cortisol
# Experimento 1
# Grupo control

diab = rnorm(n=5000, mean=0, sd=1)
diab<-sort(diab, decreasing = FALSE);diab
hist(diab)
summary(diab)


m<-mean(diab); m
std<-sd(diab); std
hist(diab, density=20, breaks=20, prob=TRUE, 
     xlab="Variable X", ylab="Densidad", xlim=c(-4, 4), ylim=c(0, 0.5), 
     main="Curva normal sobre histograma")
curve(dnorm(x, mean=0, sd=1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


shapiro.test(diab)
# From the output, the p-value > 0.05 implying that the distribution of the data 
# are not significantly different from normal distribution. 
# In other words, we can assume the normality.

# Ejercicios Bivariados 

data<-data(cars)
summary(cars)
head(cars,10)

# scatterplot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  
plot(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

# Boxplot & outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

# Density plot - Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


# Bivariate stats
cov(cars$speed, cars$dist)
cor(cars$speed, cars$dist)
correlacion <- (cov(cars$speed, cars$dist) /((sd(cars$speed))*sd(cars$dist))); correlacion

# linear regression
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
par(new=T) # Plot over the same graph
# dev.off()  # Shut down graphical devices
abline(-17.549, 3.932)
R2 <- (cor(cars$speed, cars$dist)^2); R2


# Integrate a constant to data and check bivariate stats

speed.k <- cars$speed + 5; speed.k
dist.k <- cars$dist - 9; dist.k

cars2 <- cbind(cars, speed.k, dist.k); head(cars2, 10)

describe(cars2) # Require Hmisc or psych

# another options to compute descriptive stats "library(pastecs)"
stat.desc(cars2) # complete output 


# Integrate a linear transformation to data and check bivariate stats

speed.lt <- ((cars$speed * 1.2) + 2.6); speed.lt
dist.lt <- ((cars$dist*0.6) - 3.2); dist.lt

cars3 <- cbind(cars2, speed.lt, dist.lt); head(cars3, 10)

describe(cars3) # Require library(psych)

# another options to compute descriptive stats "library(pastecs)"
stat.desc(cars3) # complete output 

hist(cars3$speed.lt)


# review of plots
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(cars3$speed, cars3$dist)
plot(cars3$speed.lt, cars3$dist.lt)

# review of cors and covars

cor(cars3$speed, cars3$dist)
cor(cars3$speed.lt, cars3$dist.lt)

cov(cars3$speed, cars3$dist)
cov(cars3$speed.lt, cars3$dist.lt)

# demo

demo()
demo(package = .packages(all.available = TRUE)) # demos of your installed packages

demo(lm.glm, package = "stats", ask = TRUE) # to interact

demo(lm.glm, package = "stats", ask = FALSE) # automated


# https://cran.r-project.org/web/packages/irtDemo/irtDemo.pdf
library(irtDemo)
irtDemo("dich")
irtDemo("eapmap")
irtDemo("mirt")

#### END.,
