setwd("C:/Users/Adriana/Desktop/Adrifelcha_PsicometriaYEvaluacion/Lancaster/Reporte_01")
Scores <- read.csv("MatrizAciertos.csv")

Sexo_A <- NULL
Grado_A <- NULL
Grupo_A <- NULL
FA <- NULL
contador <- 0
for(i in 1:nrow(Scores)){
  if(Scores$Forma.A[i]==0){
    nada <- "nada"
  }else{
    contador <- contador+1
    Sexo_A[contador] <- Scores$Sexo[i] 
    Grado_A[contador] <- Scores$Grado[i]
    Grupo_A[contador] <- Scores$Grupo[i]
    FA[contador] <- Scores$Forma.A[i] 
  }
}

Sexo_B <- NULL
Grado_B <- NULL
Grupo_B <- NULL
FB <- NULL
contador <- 0
for(i in 1:nrow(Scores)){
  if(Scores$Forma.B[i]==0){
    nada <- "nada"
  }else{
    contador <- contador+1
    Sexo_B[contador] <- Scores$Sexo[i] 
    Grado_B[contador] <- Scores$Grado[i]
    Grupo_B[contador] <- Scores$Grupo[i]
    FB[contador] <- Scores$Forma.B[i] 
  }
}


Completo <- 0
Sexo_T <- NULL
Grado_T <- NULL
Grupo_T <- NULL
Total_Completo <- NULL
for(a in 1:nrow(Scores)){
  if(is.na(Scores[a,10])==FALSE&is.na(Scores[a,50])==FALSE){
    Completo <- Completo + 1
    Total_Completo[Completo] <- Scores$Total[a]
    Sexo_T[Completo] <- Scores$Sexo[a]
    Grado_T[Completo] <- Scores$Grado[a]
    Grupo_T[Completo] <- Scores$Grupo[a]
  }else{
    nada <- "nada"  
  }}

##################################################################
###### 1 Evaluando Diferencias por Grado
###### PRUEBAS T: ¿Es sexto significativamente mejor que quinto?
##################################################################
# 1. Comparando Forma A
A_Q <- FA[Grado_A=="1"] #Quinto
A_S <- FA[Grado_A=="2"] #Sexto
#Ordenamos los datos
c_QyS<- data.frame(cbind(A_Q, A_S))
Grado_FA <- stack(c_QyS)
#Corremos la T
t.test(values~ind,data=Grado_FA,alternative = c("two.sided", "less", "greater"))
#No hay diferencias (p debe ser menor a .05)

# 2. Comparando Forma B
B_Q <- FB[Grado_B=="1"] #Quinto
B_S <- FB[Grado_B=="2"] #Sexto
#Ordenamos los datos
cB_QyS<- data.frame(cbind(B_Q, B_S))
Grado_FB <- stack(cB_QyS)
#Corremos la T
t.test(values~ind,data=Grado_FB,alternative = c("two.sided", "less", "greater"))
#Sexto es significativamente mejor que Quinto

# 3. Comparando puntuaciones
T_Q <- Total_Completo[Grado_T=="1"]
T_S <- Total_Completo[Grado_T=="2"]
#Ordenamos los datos
cT_QyS<- data.frame(cbind(T_Q, T_S))
Grado_T <- stack(cT_QyS)
#Corremos la T
t.test(values~ind,data=Grado_T,alternative = c("two.sided", "less", "greater"))
#Sexto es significativamente mejor que Quinto




##################################################################
###### 2 Evaluando Diferencias por Grupo
###### PRUEBAS T: ¿Es sexto significativamente mejor que quinto?
##################################################################
# 1. Comparando Forma A
QA <- FA[Grupo_A=="1"] #Quinto
QB <- FA[Grupo_A=="2"] #Sexto
SA <- FA[Grupo_A=="3"] #Quinto
SB <- FA[Grupo_A=="4"] #Sexto
#Ordenamos los datos
Quintos<- data.frame(cbind(QA, QB))
Sextos<- data.frame(cbind(SA, SB))
Quinto_FA <- stack(Quintos)
Sexto_FA <- stack(Sextos)
#Corremos la T para comparar 5A y 5B
t.test(values~ind,data=Quinto_FA,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas
#Corremos la T para comparar 6A y 6B
t.test(values~ind,data=Sexto_FA,alternative = c("two.sided", "less", "greater"))
#No hay diferencias (p debe ser menor a .05)

QA <- FB[Grupo_B=="1"] #Quinto
QB <- FB[Grupo_B=="2"] #Sexto
SA <- FB[Grupo_B=="3"] #Quinto
SB <- FB[Grupo_B=="4"] #Sexto
#Ordenamos los datos
Quintos<- data.frame(cbind(QA, QB))
Sextos<- data.frame(cbind(SA, SB))
Quinto_FB <- stack(Quintos)
Sexto_FB <- stack(Sextos)
#Corremos la T para comparar 5A y 5B
t.test(values~ind,data=Quinto_FB,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas
#Corremos la T para comparar 6A y 6B
t.test(values~ind,data=Sexto_FB,alternative = c("two.sided", "less", "greater"))
#Sí hay diferencias significativas

# 3. Comparando puntuaciones
QA <- Total_Completo[Grupo_T=="1"] #Quinto A
QB <- Total_Completo[Grupo_T=="2"] #Quinto B
SA <- Total_Completo[Grupo_T=="3"] #Sexto A
SB <- Total_Completo[Grupo_T=="4"] #Sexto B
#Ordenamos los datos
Quintos<- data.frame(cbind(QA, QB))
Sextos<- data.frame(cbind(SA, SB))
Quinto_T <- stack(Quintos)
Sexto_T <- stack(Sextos)
#Corremos la T para comparar 5A y 5B
t.test(values~ind,data=Quinto_T,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas
#Corremos la T para comparar 6A y 6B
t.test(values~ind,data=Sexto_T,alternative = c("two.sided", "less", "greater"))
#Sí hay diferencias significativas


##################################################################
###### 3 Evaluando Diferencias por Sexo
###### PRUEBAS T: ¿Es sexto significativamente mejor que quinto?
##################################################################
# 1. Comparando Forma A
H_A <- FA[Sexo_A=="1"] #Hombre
M_A <- FA[Sexo_A=="3"] #Mujer
#Ordenamos los datos
FA<- data.frame(cbind(H_A,M_A))
FA_sex <- stack(FA)
#Corremos la T
t.test(values~ind,data=FA_sex,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas

# 2. Comparando Forma B
H_B <- FB[Sexo_B=="1"] #Hombre
M_B <- FB[Sexo_B=="3"] #Mujer
#Ordenamos los datos
FB<- data.frame(cbind(H_B,M_B))
FB_sex <- stack(FB)
#Corremos la T
t.test(values~ind,data=FB_sex,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas

# 3. Comparando TOTAL
H_T <- Total_Completo[Sexo_T=="1"] #Hombre
M_T <- Total_Completo[Sexo_T=="3"] #Mujer
#Ordenamos los datos
FT<- data.frame(cbind(H_T,M_T))
FT_sex <- stack(FT)
#Corremos la T
t.test(values~ind,data=FT_sex,alternative = c("two.sided", "less", "greater"))
#NO hay diferencias significativas