# Sintaxis para distribuciones acumuladas, boxplots etc...

library(car)
library(plyr)
library(psych)

# cargar la base
#PISA_2003 <- read.csv("PISA_2003_MEX_WLE&PVs.csv")
PISA_2003 <- read.csv("PISA_2003_MEX_SUB.csv")

# Build object 

Well.With.Stud <- PISA_2003$ST26Q01
count(Well.With.Stud)

#WLE_Math <- PISA_2003$ï..wlemath
#describe(PISA_2003$ï..wlemath)

# Recode missing values (9997, 9998, 9999)

PISA_2003$ST26Q01[PISA_2003$ST26Q01 == 7] <- NA
PISA_2003$ST26Q01[PISA_2003$ST26Q01 == 8] <- NA
PISA_2003$ST26Q01[PISA_2003$ST26Q01 == 9] <- NA


# Build RECODED object 

Well.With.Stud.recoded <- PISA_2003$ST26Q01
count(Well.With.Stud.recoded)


Well.With.Stud.recoded.na_rm <- as.numeric(na.omit(Well.With.Stud.recoded)) # Remove NA's

#WLE_Math.recoded <- PISA_2003$ï..wlemath; WLE_Math.recoded
#WLE_Math.recoded.na_rm <- as.numeric(na.omit(WLE_Math.recoded)) # Remove NA's


describe(Well.With.Stud.recoded.na_rm)
count(Well.With.Stud.recoded.na_rm)
hist(Well.With.Stud.recoded, breaks=seq(0,4,l=5), xlab="Categorías", ylab="Frecuencia",
     main="¿Cuán bien me llevo con otros estudiantes?", col="orange", ylim = c(0, 20000))



# describe(WLE_Math.recoded.na_rm)
# hist(WLE_Math.recoded.na_rm, xlab="WLE en Matemáticas", ylab="Frecuencia", xlim=c(100, 700), ylim=c(0, 8000), 
#      main="PISA 2003 MEX", col="green", breaks=20)
# 
# # Add a Normal Curve
# x <- WLE_Math.recoded.na_rm 
# h<-hist(WLE_Math.recoded.na_rm, breaks=20, col="green", ylab="Frecuencia", xlab="WLE en Matemáticass",  
#         main="PISA 2003 MEX", xlim=c(100, 700), ylim=c(0, 8000)) 
# xfit<-seq(min(x),max(x),length=400) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2)
# 
# 
# boxplot(WLE_Math.recoded.na_rm, ylab="Puntajes", xlab="WLE in Mathematics",  
#         main="PISA 2003 MEX")


# Subsample of the variable

Well.With.Stud.recoded.na_rm.sub <- sample(Well.With.Stud.recoded.na_rm, 289) # Random subsample of the variable (1%)

describe(Well.With.Stud.recoded.na_rm.sub)
count(Well.With.Stud.recoded.na_rm.sub)

hist(Well.With.Stud.recoded.na_rm.sub, breaks=seq(0,4,l=5), xlab="Categorías", ylab="Frecuencia",
     main="¿Cuán bien me llevo con otros estudiantes?", col="green", ylim = c(0, 200))



# WLE_Math.recoded.na_rm.sub <- sample(WLE_Math.recoded.na_rm, 299) # Random subsample of the variable (1%)
# 
# describe(WLE_Math.recoded.na_rm.sub)
# 
# x <- WLE_Math.recoded.na_rm.sub 
# h<-hist(WLE_Math.recoded.na_rm.sub, breaks=20, col="green", ylab="Frecuencia", xlab="WLE en Matemáticass",  
#         main="PISA 2003 MEX", xlim=c(100, 700), ylim=c(0, 40)) 
# xfit<-seq(min(x),max(x),length=400) 
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2)
# 
# 
# boxplot(WLE_Math.recoded.na_rm.sub, ylab="Puntajes", xlab="WLE in Mathematics (sub)",  
#         main="PISA 2003 MEX")


# Empirical Cumulative Distribution Function

ECDF <- ecdf(Well.With.Stud.recoded.na_rm.sub)
plot(ECDF, verticals = TRUE, do.points = FALSE)
plot(ECDF, verticals = TRUE, do.points = TRUE)

# Quantile
summary(Well.With.Stud.recoded.na_rm.sub)
quantile(Well.With.Stud.recoded.na_rm.sub, c(.25, .50, .75))
quantile(Well.With.Stud.recoded.na_rm.sub, c(.05, .25, .50, .75, .95))
quantile(Well.With.Stud.recoded.na_rm.sub, c(.10, .20, .30, .40, 
                                   .50, .60, .70, .80, .90))
quantile(Well.With.Stud.recoded.na_rm.sub, c(.13, .42, .86))

# Boxplot of MPG by Car Cylinders 
# ?mtcars
# boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
#         xlab="Number of Cylinders", ylab="Miles Per Gallon")
# describe(mtcars)


# +++++++++++++++ Theoretical distributions
#++++++++ Normal distribution
# no necesitamos datos 

# Generates random numbers  from normal distribution
random.normal.numbers <- rnorm(1000, 0, 1) # Generates 1000 numbers from a normal with mean 0 and sd = 1
plot(random.normal.numbers)

m<-mean(random.normal.numbers); m
std<-sd(random.normal.numbers); std
hist(random.normal.numbers, density=20, breaks=20, prob=TRUE, 
     xlab="Variable X", ylab="Densidad", xlim=c(-3, 3), ylim=c(0, 0.5), 
     main="Curva normal sobre histograma")
curve(dnorm(x, mean=0, sd=1), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


# Probability Density Function (PDF)
dnorm(0, 0, .5) # Gives the density (height of the PDF) of the normal with mean=0 and sd=.5. 

# Cumulative Distribution Function (CDF)
pnorm(1.96, 0, 1) #Gives the area under the standard normal curve to the left of 1.96, i.e. ~0.975

# Quantile Function - inverse of pnorm
qnorm(0.975, 0, 1) #Gives the value at which the CDF of the standard normal is .975, i.e. ~1.96

# Exercises

set.seed(666)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1)

par(mfrow=c(1,3), mar=c(3,4,4,2))

plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)

plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)

hist(randomdeviates, main="Random draws from Normal Distribution", cex.axis=.8, xlim=c(-4,4))

boxplot(randomdeviates, main="Boxplot example", xlab="Variable", ylab="Score")


# Ejercicios en clase

# 1. Con la variable ¿Desde hace cuanto tiempo uso computadora? (IC03Q01) extraigan una muestra aleatoria de 416 observaciones
# 2. Crear un resumen con estadísticos descriptivos de la misma
# 3. Crear un histograma de frecuencias
# 4. Crear la función de distribución acumulada para dicha variable 
# 5. Validar los valores para los cuartiles 1, 2 y 3.

ic03 <- PISA_2003$IC03Q01
sc3.sub <- subset(PISA_2003, ï..state == 005 )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Tarea para entregar 
# 1. Con la variable ¿Cuán frecuente descargo música? (IC05Q10) extraigan una muestra 
# que contenga a los estados de Chiapas y Distrito federal
# 2. Crear un resumen con estadísticos descriptivos de la misma
# 3. Crear un histograma de frecuencias 
# 4. Crear la función de distribución acumulada para dicha variable 
# 5. Validar los valores para los cuartiles 1, 2 y 3.
# 6. Contrastar los resultados por estados y argumentar los resultados
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
