####################################
# Ploteo de Datos
####################################
setwd("D:/afchavez/Desktop/Adrifelcha_Lab25/Proyectos/Uri Tesis/Data")
rm(list=ls())
dir()

Uri <- "data_full_prop.csv"
Uri <- read.csv(Uri)

Participante <- Uri$participante
Condicion <- Uri$condicion
Escala <- Uri$escala
Secuencia <- Uri$secuencia
p_largo <- Uri$largo_p
Estimulo <- Uri$duracion
#########################################
#########################################

Sujetos <- unique(Participante)
Secuencias <- unique(Secuencia)
Estimulos <- unique(Estimulo)
Condiciones <- unique(Condicion)
Escalas <- unique(Escala)


a <- NULL
b <- NULL
for(c in 1:length(unique(Participante))){
  a[c] <- mean(p_largo[Participante==Sujetos[c]])
  b[c] <- mean(p_largo[Participante==unique(Participante)[c]])
}
print(a)
print(b)
###############################




Pap <- matrix(data = NA, ncol = 3, nrow=length(unique(Estimulo)), byrow = TRUE)

for(a in 1:length(unique(Participante))){
  for(b in 1:length(unique(Condicion))){
    for(c in 1:length(unique(Escala))){
      for(d in 1:length(unique(Estimulo))){
        for(e in 1:3){
          Pap[d,e] <- p_largo[Secuencia==Secuencias[e] & Estimulo==Estimulos[d] & Escala==Escalas[c] & Condicion==Condiciones[b] & Participante==Sujetos[a]]
        } } } } }

print(Pap)
