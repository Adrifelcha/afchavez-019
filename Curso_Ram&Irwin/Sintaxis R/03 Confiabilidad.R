# TCT y confiabilidad 

# Funciones para estimar la confiabilidad de un test alargado 

# Un test inicial consiste en 25 ítems y tiene una confiabilidad rho= 0.80
# Si se añaden 15 ítems al test inicial.

# Entonces, el test alargado tendrá 40 items
# por lo tanto el factor de alargamiento sería:

# m = el número de ítems en el test inicial
# q = ítems paralelos

n <- function(m, q) { 
  n <- ((m+q)/m)
  numeric(return(n))
}

n(25, 15)
# n(m=25, q=15)
# n(q=15, m=25)

# Aplicando la Fórmula de Spearman-Brown (YEAR)
# su confiabilidad estimada:

Spearman_Brown <-function(n, conf.original) {
  conf.alargada <- ((n*conf.original)/(1+(n-1)*conf.original))
  numeric(return(conf.alargada))  
}

Spearman_Brown(1.6, 0.8)
# Spearman_Brown(n=2, conf.original=0.8)
# Spearman_Brown(conf.original=0.8, n=2)

# Pero recuerden que, si se cumplen los supuestos que subyacen la fórmula de Spearman-Brown, puede
# utilizarse para responder las siguientes preguntas:

# ¿Cuántos números de ítems paralelos se requieren añadir al test para obtener una confiabilidad deseada de conf.alargada _ _ _?
# Consideremos que si tenemos un test inicial, que consiste en 25 ítems y tiene una confiabilidad conf.original = 0.65
# Se desea tener una confiabilidad rho. alargada = 0.80

# Entonces, el factor de alargamiento será:

n.alarga <- function(conf.alargada, conf.original) {
  factor.alarga <- (conf.alargada*(1-conf.original)/(conf.original*(1-conf.alargada)))
  numeric(return(factor.alarga))  
}

n.alarga(0.80, 0.65)
# n.alarga(conf.alargada=0.80, conf.original=0.65)
# n.alarga(conf.original=0.65, conf.alargada=0.80)


# El test alargado debe tener n.alarga * 25 = 53:85 items.
n.alargado <- n.alarga(0.80, 0.65)
test.alargado <- n.alargado * 25; ceiling(test.alargado)

# Para conocer el número de ítems a agregar 
items.añadir <- ceiling(test.alargado) - 25; items.añadir # redondear hacia arriba 

# --------------------------------------------------------------------------------
# Ejercicio 
# n.acortado <- n.alarga(0.95, 0.85)
# test.acortado <- 100 / n.acortado; ceiling(test.acortado)
# Para conocer el número de ítems a agregar 
# items.eliminar <- 100 - ceiling(test.acortado); items.eliminar # redondear hacia arriba 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ejercicio en clase
# PLANEA09_2017
# Hábitos hacia el estudio	SAB_66 - SAB_77 (12 items)

# SAB_66 ¿Con qué frecuencia haces o te pasa lo siguiente? Hago mi mayor esfuerzo para hacer mi tarea
# SAB_67 ... Olvido hacer la tarea
# SAB_68 ... Pongo atención cuando hago la tarea
# SAB_69 ... Ordeno el lugar donde haré mi tarea antes de empezar a hacerla
# SAB_70 ... Me falta tiempo para terminar mi tarea
# SAB_71 ... Evito cosas que me distraen cuando hago mi tarea (por ejemplo: apago la televisión o dejo de jugar)
# SAB_72 ... Me desespero cuando una tarea me es difícil
# SAB_73 ... Me distraigo fácilmente cuando estoy haciendo la tarea
# SAB_74 ... Cuando me cuesta resolver la tarea, sigo trabajando hasta que logro terminarla
# SAB_75 ... Tengo un horario para hacer mi tarea
# SAB_76 ... Busco un lugar tranquilo para hacer mi tarea
# SAB_77 ... Me desagrada hacer mi tarea

# 1=Nunca, 2=Pocas veces, 3=Muchas veces, 4=Siempre

# Cargar datos
PLANEA09_2017 <- read.csv("Planea09_2017_AlumnosTEST.csv"); PLANEA09_2017

# 2. Crear objetos
library(plyr) 
library(car) 

SAB_66 <- PLANEA09_2017$SAB_66; SAB_66
SAB_67 <- PLANEA09_2017$SAB_67; SAB_67
SAB_68 <- PLANEA09_2017$SAB_68; SAB_68
SAB_69 <- PLANEA09_2017$SAB_69; SAB_69
SAB_70 <- PLANEA09_2017$SAB_70; SAB_70
SAB_71 <- PLANEA09_2017$SAB_71; SAB_71
SAB_72 <- PLANEA09_2017$SAB_72; SAB_72
SAB_73 <- PLANEA09_2017$SAB_73; SAB_73
SAB_74 <- PLANEA09_2017$SAB_74; SAB_74
SAB_75 <- PLANEA09_2017$SAB_75; SAB_75
SAB_76 <- PLANEA09_2017$SAB_76; SAB_76
SAB_77 <- PLANEA09_2017$SAB_77; SAB_77

original <- cbind(SAB_67, SAB_70, SAB_72, SAB_73, SAB_77); head(original,10)

# Recodifican para la misma dirección 

PLANEA09_2017$SAB_67 <- recode(PLANEA09_2017$SAB_67, "1=4; 2=3; 3=2; 4=1")
PLANEA09_2017$SAB_70 <- recode(PLANEA09_2017$SAB_70, "1=4; 2=3; 3=2; 4=1")
PLANEA09_2017$SAB_72 <- recode(PLANEA09_2017$SAB_72, "1=4; 2=3; 3=2; 4=1")
PLANEA09_2017$SAB_73 <- recode(PLANEA09_2017$SAB_73, "1=4; 2=3; 3=2; 4=1")
PLANEA09_2017$SAB_77 <- recode(PLANEA09_2017$SAB_77, "1=4; 2=3; 3=2; 4=1")

SAB_67 <- PLANEA09_2017$SAB_67; SAB_67
SAB_70 <- PLANEA09_2017$SAB_70; SAB_70
SAB_72 <- PLANEA09_2017$SAB_72; SAB_72
SAB_73 <- PLANEA09_2017$SAB_73; SAB_73
SAB_77 <- PLANEA09_2017$SAB_77; SAB_77


recoded <- cbind(SAB_67, SAB_70, SAB_72, SAB_73, SAB_77); head(recoded,10)
both <- cbind(original, recoded); head(both,10)

head(PLANEA09_2017$SAB_67,10)
head(PLANEA09_2017$SAB_70,10)
head(PLANEA09_2017$SAB_72,10)
head(PLANEA09_2017$SAB_73,10)
head(PLANEA09_2017$SAB_77,10)

count(SAB_66); count(SAB_67); count(SAB_68); count(SAB_69); 
count(SAB_70); count(SAB_71); count(SAB_72); count(SAB_73); 
count(SAB_74); count(SAB_75); count(SAB_76); count(SAB_77)

PLANEA09_2017$SAB_66[PLANEA09_2017$SAB_66==94] <-NA
PLANEA09_2017$SAB_67[PLANEA09_2017$SAB_67==94] <-NA
PLANEA09_2017$SAB_68[PLANEA09_2017$SAB_68==94] <-NA
PLANEA09_2017$SAB_69[PLANEA09_2017$SAB_69==94] <-NA
PLANEA09_2017$SAB_70[PLANEA09_2017$SAB_70==94] <-NA
PLANEA09_2017$SAB_71[PLANEA09_2017$SAB_71==94] <-NA
PLANEA09_2017$SAB_72[PLANEA09_2017$SAB_72==94] <-NA
PLANEA09_2017$SAB_73[PLANEA09_2017$SAB_73==94] <-NA
PLANEA09_2017$SAB_74[PLANEA09_2017$SAB_74==94] <-NA
PLANEA09_2017$SAB_75[PLANEA09_2017$SAB_75==94] <-NA
PLANEA09_2017$SAB_76[PLANEA09_2017$SAB_76==94] <-NA
PLANEA09_2017$SAB_77[PLANEA09_2017$SAB_77==94] <-NA


PLANEA09_2017$SAB_66[PLANEA09_2017$SAB_66==96] <-NA
PLANEA09_2017$SAB_67[PLANEA09_2017$SAB_67==96] <-NA
PLANEA09_2017$SAB_68[PLANEA09_2017$SAB_68==96] <-NA
PLANEA09_2017$SAB_69[PLANEA09_2017$SAB_69==96] <-NA
PLANEA09_2017$SAB_70[PLANEA09_2017$SAB_70==96] <-NA
PLANEA09_2017$SAB_71[PLANEA09_2017$SAB_71==96] <-NA
PLANEA09_2017$SAB_72[PLANEA09_2017$SAB_72==96] <-NA
PLANEA09_2017$SAB_73[PLANEA09_2017$SAB_73==96] <-NA
PLANEA09_2017$SAB_74[PLANEA09_2017$SAB_74==96] <-NA
PLANEA09_2017$SAB_75[PLANEA09_2017$SAB_75==96] <-NA
PLANEA09_2017$SAB_76[PLANEA09_2017$SAB_76==96] <-NA
PLANEA09_2017$SAB_77[PLANEA09_2017$SAB_77==96] <-NA

PLANEA09_2017$SAB_66[PLANEA09_2017$SAB_66==98] <-NA
PLANEA09_2017$SAB_67[PLANEA09_2017$SAB_67==98] <-NA
PLANEA09_2017$SAB_68[PLANEA09_2017$SAB_68==98] <-NA
PLANEA09_2017$SAB_69[PLANEA09_2017$SAB_69==98] <-NA
PLANEA09_2017$SAB_70[PLANEA09_2017$SAB_70==98] <-NA
PLANEA09_2017$SAB_71[PLANEA09_2017$SAB_71==98] <-NA
PLANEA09_2017$SAB_72[PLANEA09_2017$SAB_72==98] <-NA
PLANEA09_2017$SAB_73[PLANEA09_2017$SAB_73==98] <-NA
PLANEA09_2017$SAB_74[PLANEA09_2017$SAB_74==98] <-NA
PLANEA09_2017$SAB_75[PLANEA09_2017$SAB_75==98] <-NA
PLANEA09_2017$SAB_76[PLANEA09_2017$SAB_76==98] <-NA
PLANEA09_2017$SAB_77[PLANEA09_2017$SAB_77==98] <-NA


PLANEA09_2017$SAB_66[PLANEA09_2017$SAB_66==99] <-NA
PLANEA09_2017$SAB_67[PLANEA09_2017$SAB_67==99] <-NA
PLANEA09_2017$SAB_68[PLANEA09_2017$SAB_68==99] <-NA
PLANEA09_2017$SAB_69[PLANEA09_2017$SAB_69==99] <-NA
PLANEA09_2017$SAB_70[PLANEA09_2017$SAB_70==99] <-NA
PLANEA09_2017$SAB_71[PLANEA09_2017$SAB_71==99] <-NA
PLANEA09_2017$SAB_72[PLANEA09_2017$SAB_72==99] <-NA
PLANEA09_2017$SAB_73[PLANEA09_2017$SAB_73==99] <-NA
PLANEA09_2017$SAB_74[PLANEA09_2017$SAB_74==99] <-NA
PLANEA09_2017$SAB_75[PLANEA09_2017$SAB_75==99] <-NA
PLANEA09_2017$SAB_76[PLANEA09_2017$SAB_76==99] <-NA
PLANEA09_2017$SAB_77[PLANEA09_2017$SAB_77==99] <-NA


SAB_66 <- PLANEA09_2017$SAB_66; SAB_66
SAB_67 <- PLANEA09_2017$SAB_67; SAB_67
SAB_68 <- PLANEA09_2017$SAB_68; SAB_68
SAB_69 <- PLANEA09_2017$SAB_69; SAB_69
SAB_70 <- PLANEA09_2017$SAB_70; SAB_70
SAB_71 <- PLANEA09_2017$SAB_71; SAB_71
SAB_72 <- PLANEA09_2017$SAB_72; SAB_72
SAB_73 <- PLANEA09_2017$SAB_73; SAB_73
SAB_74 <- PLANEA09_2017$SAB_74; SAB_74
SAB_75 <- PLANEA09_2017$SAB_75; SAB_75
SAB_76 <- PLANEA09_2017$SAB_76; SAB_76
SAB_77 <- PLANEA09_2017$SAB_77; SAB_77


count(SAB_66); count(SAB_67); count(SAB_68); count(SAB_69); 
count(SAB_70); count(SAB_71); count(SAB_72); count(SAB_73); 
count(SAB_74); count(SAB_75); count(SAB_76); count(SAB_77)

# Escala Hábitos hacia el estudio
HE <- PLANEA09_2017[c('SAB_66', 'SAB_67', 'SAB_68', 'SAB_69', 
                      'SAB_70', 'SAB_71', 'SAB_72', 'SAB_73', 
                      'SAB_74', 'SAB_75', 'SAB_76', 'SAB_77')]; HE


# eliminar datos perdidos
HE.na_omit <- na.omit(HE)

library(psych)
describe(HE.na_omit)
alpha(HE.na_omit) 
# 0.73


# ¿Cuántos números de ítems paralelos se requieren añadir al test para obtener una confiabilidad deseada de conf.alargada _ _ _?
# Consideremos que si tenemos un test inicial, que consiste en 25 ítems y tiene una confiabilidad conf.original = 0.65
# Se desea tener una confiabilidad rho. alargada = 0.80

# Entonces, el factor de alargamiento será:

n.alarga <- function(conf.alargada, conf.original) {
  factor.alarga <- (conf.alargada*(1-conf.original)/(conf.original*(1-conf.alargada)))
  numeric(return(factor.alarga))  
}

n.alarga(0.80, 0.73)
# n.alarga(conf.alargada=0.80, conf.original=0.65)
# n.alarga(conf.original=0.65, conf.alargada=0.80)


# El test alargado debe tener n.alarga * 25 = 53:85 items.
n.alargado <- n.alarga(0.80, 0.73)
test.alargado <- n.alargado * 12; round(test.alargado,0)

# Para conocer el número de ítems a agregar 
items.añadir <- round(test.alargado,0) - 12; items.añadir # redondear hacia arriba 


# test re-test
# plantearles el problema de que podemos hacer con los datos de la clase
# 30 items formas paralelas, 

# cargar datos calificados de manera dicot??mica

library(plyr)
library(car)
library(psych)


data <- read.csv("Grit_Test-Retest_Data.csv")

#recodificar variables
# 1,4,6,9,10,12 <- 5 me siento totalmente identificado
# 2,3,5,7,8,11  <- 1 me siento totalmente identificado ***


Item2a <- data$Item2a
Item2b <- data$Item2b
Item3a <- data$Item3a
Item3b <- data$Item3b
Item5a <- data$Item5a
Item5b <- data$Item5b
Item7a <- data$Item7a
Item7b <- data$Item7b
Item8a <- data$Item8a
Item8b <- data$Item8b
Item11a <- data$Item11a
Item11b <- data$Item11b


count(Item2a); count(Item2b); 
count(Item3a); count(Item3b); 
count(Item5a); count(Item5b); 
count(Item7a); count(Item7b); 
count(Item8a); count(Item8b); 
count(Item11a); count(Item11b); 


# Recodificar variables
data$Item2a<-recode(data$Item2a,"1=5; 2=4; 4=2; 5=1")
data$Item2b<-recode(data$Item2b,"1=5; 2=4; 4=2; 5=1")
data$Item3a<-recode(data$Item3a,"1=5; 2=4; 4=2; 5=1")
data$Item3b<-recode(data$Item3b,"1=5; 2=4; 4=2; 5=1")
data$Item5a<-recode(data$Item5a,"1=5; 2=4; 4=2; 5=1")
data$Item5b<-recode(data$Item5b,"1=5; 2=4; 4=2; 5=1")
data$Item7a<-recode(data$Item7a,"1=5; 2=4; 4=2; 5=1")
data$Item7b<-recode(data$Item7b,"1=5; 2=4; 4=2; 5=1")
data$Item8a<-recode(data$Item8a,"1=5; 2=4; 4=2; 5=1")
data$Item8b<-recode(data$Item8b,"1=5; 2=4; 4=2; 5=1")
data$Item11a<-recode(data$Item11a,"1=5; 2=4; 4=2; 5=1")
data$Item11b<-recode(data$Item11b,"1=5; 2=4; 4=2; 5=1")


# sumar variables
test <- subset(data, select=c(2:13))
retest <- subset(data, select=c(14:25))

sum_test<-rowSums(test); sum_retest<-rowSums(retest);
sums<- data.frame(sum_test,sum_retest)
names(sums)[names(sums) == 'sum_test'] <- 'Test'
names(sums)[names(sums) == 'sum_retest'] <- 'Retest'; sums

cor.test(sum_test,sum_retest)
cov(sum_test,sum_retest)/(sd(sum_test)*sd(sum_retest)) # alternativa formas paralelas

plot(sum_test,sum_retest, main="Correlación test-retest", 
     xlab = "Puntuación Test", ylab = "Puntuación Retest", pch=19) 
abline(lm(Test ~ Retest, data=sums), col="red", lw=2)

alpha(test) # 0.81
alpha(retest) # 0.86


# Consistencia interna dos mitades
half_1 <- subset(data, select=seq(2,12,2)); sums_half1<-rowSums(half_1)
half_2 <- subset(data, select=seq(3,13,2)); sums_half2<-rowSums(half_2)
sums12 <- as.data.frame(cbind(sums_half1, sums_half2))
plot(sums_half1,sums_half2, main="Correlación 2 mitades", 
     xlab = "Puntuación mitad 1", ylab = "Puntuación mitad 2", pch=19)
abline(lm(sums_half2 ~ sums_half1, data=sums12), col="red", lw=2)

cor(sums_half1,sums_half2) # confiabilidad de la mitad del test
corr_2mitades <- (2*cor(sums_half1,sums_half2))/(1+cor(sums_half1,sums_half2)); corr_2mitades # Compensa por el factor de alargamiento

# FIN