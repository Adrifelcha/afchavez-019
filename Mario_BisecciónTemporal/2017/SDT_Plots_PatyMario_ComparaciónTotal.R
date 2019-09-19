##########################################################
# Codigo base para el análisis de mario obtenidos en el experimento presentado por Pérez et al (2017)
# con los parámetros y estadísticos expuestos por Stainslaw (1999) y Gescheider (2013)
##########################################################
#Referencia 1: Stainslaw & Todorov (1999), Calculation of signal detection theory measures.
#Referencia 2: Gescheider, George A. (2013). "Psychophysics: The Fundamentals". 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#Codigo hecho por: Adriana F. Chávez De la Peña
#Contacto: adrifelcha@gmail.com
##########################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


####################################
# # # # # # # #  Parte I
# # # # # # # #  Cargamos los mario
####################################
rm(list=ls())  #Reseteamos la consola
setwd("C:/Users/Adriana/Desktop/Felisa/Proyectos/Mario_BisecciónTemporal") # Directorio de trabajo
dir()  #Comprobamos los archivos contenidos en el directorio

#Datos de Mario
archive_1 <-'Datos_Mario_.csv'
mario <- read.csv(archive_1) 

#Datos de Paty
Paty_Duracion <- 'Extrema' #Opciones: 'Intermedia' u 'Extrema'
if(Paty_Duracion=='Intermedia'){
archive_2 <-'Datos_Paty_Int.csv'} else{
archive_2 <- 'Datos_Paty_Ext.csv'}
paty <- read.csv(archive_2)


########################################
# # # # # # # #  Especificamos variables
########################################
Mario_LargoEsSignal <- c(3, 5, 20, 33)  #Especificamos cuáles Sujetos estuvieron en el grupo donde LARGO es Señal
Mario_Intermedias <- c('1v4', '2v8')
Paty_Signal <- 'Largo'  #OPCIONES: 'Corto' ó 'Largo' (con todo y comillas)


if(Paty_Duracion=='Intermedia'){
  if(Paty_Signal=='Corto'){
    Sujeto_M <- mario$Sujeto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]         #Del archivo 'datos', identificamos los datos en la columna 'Sujeto' como variable Sujeto
    Condicion_M <- mario$Grupo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]       #Del archivo 'datos', identificamos los datos en la columna 'Grupo' como variable Condición
    TipoSesion_M <- mario$Condicion[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]  #etc, etc, etc
    Dia_M <- mario$Día[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Hits_M <- mario$Corto_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    FA_M <- mario$Corto_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    CRej_M <- mario$Largo_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Miss_M <- mario$Largo_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Noise_M <- mario$EnsayosLargos[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Signal_M <- mario$EnsayosCortos[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == TRUE]

    Sujeto_P <- paty$sujeto[paty$grupo %in% Mario_Intermedias == TRUE]         #Del archivo 'datos', identificamos los datos en la columna 'Sujeto' como variable Sujeto
    Condicion_P <- paty$grupo[paty$grupo %in% Mario_Intermedias == TRUE]       #Del archivo 'datos', identificamos los datos en la columna 'Grupo' como variable Condición
    TipoSesion_P <- paty$condición[paty$grupo %in% Mario_Intermedias == TRUE]  #etc, etc, etc
    Dia_P <- paty$dia[paty$grupo %in% Mario_Intermedias == TRUE]
    Hits_P <- paty$hit[paty$grupo %in% Mario_Intermedias == TRUE]
    FA_P <- paty$fa[paty$grupo %in% Mario_Intermedias == TRUE]
    CRej_P <- paty$rechazo[paty$grupo %in% Mario_Intermedias == TRUE]
    Miss_P <- paty$miss[paty$grupo %in% Mario_Intermedias == TRUE]
    Noise_P <- paty$ensayos_largo[paty$grupo %in% Mario_Intermedias == TRUE]
    Signal_P <- paty$ensayos_cortos[paty$grupo %in% Mario_Intermedias == TRUE]
  } else{
    Sujeto_M <- mario$Sujeto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]         
    Condicion_M <- mario$Grupo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]       
    TipoSesion_M <- mario$Condicion[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]  
    Dia_M <- mario$Día[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Hits_M <- mario$Corto_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    FA_M <- mario$Corto_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    CRej_M <- mario$Largo_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Miss_M <- mario$Largo_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Noise_M <- mario$EnsayosLargos[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]
    Signal_M <- mario$EnsayosCortos[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == TRUE]

    Sujeto_P <- paty$sujeto[paty$grupo %in% Mario_Intermedias == TRUE]         
    Condicion_P <- paty$grupo[paty$grupo %in% Mario_Intermedias == TRUE]       
    TipoSesion_P <- paty$condición[paty$grupo %in% Mario_Intermedias == TRUE]  
    Dia_P <- paty$dia[paty$grupo %in% Mario_Intermedias == TRUE]
    Hits_P <- paty$rechazo[paty$grupo %in% Mario_Intermedias == TRUE]
    FA_P <- paty$miss[paty$grupo %in% Mario_Intermedias == TRUE]
    CRej_P <- paty$hit[paty$grupo %in% Mario_Intermedias == TRUE]
    Miss_P <- paty$fa[paty$grupo %in% Mario_Intermedias == TRUE]
    Noise_P <- paty$ensayos_cortos[paty$grupo %in% Mario_Intermedias == TRUE]
    Signal_P <- paty$ensayos_largo[paty$grupo %in% Mario_Intermedias == TRUE]}
}else{
  if(Paty_Signal=='Corto'){
    Sujeto_M <- mario$Sujeto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]         
    Condicion_M <- mario$Grupo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]       
    TipoSesion_M <- mario$Condicion[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]  
    Dia_M <- mario$Día[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Hits_M <- mario$Corto_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    FA_M <- mario$Corto_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    CRej_M <- mario$Largo_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Miss_M <- mario$Largo_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Noise_M <- mario$EnsayosLargos[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Signal_M <- mario$EnsayosCortos[mario$Sujeto %in% Mario_LargoEsSignal == FALSE & mario$Grupo %in% Mario_Intermedias == FALSE]
    
    Sujeto_P <- paty$sujeto[paty$grupo %in% Mario_Intermedias == FALSE]         
    Condicion_P <- paty$grupo[paty$grupo %in% Mario_Intermedias == FALSE]       
    TipoSesion_P <- paty$condición[paty$grupo %in% Mario_Intermedias == FALSE]  
    Dia_P <- paty$dia[paty$grupo %in% Mario_Intermedias == FALSE]
    Hits_P <- paty$hit[paty$grupo %in% Mario_Intermedias == FALSE]
    FA_P <- paty$fa[paty$grupo %in% Mario_Intermedias == FALSE]
    CRej_P <- paty$rechazo[paty$grupo %in% Mario_Intermedias == FALSE]
    Miss_P <- paty$miss[paty$grupo %in% Mario_Intermedias == FALSE]
    Noise_P <- paty$ensayos_largo[paty$grupo %in% Mario_Intermedias == FALSE]
    Signal_P <- paty$ensayos_cortos[paty$grupo %in% Mario_Intermedias == FALSE]
 } else{
    Sujeto_M <- mario$Sujeto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]         
    Condicion_M <- mario$Grupo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]       
    TipoSesion_M <- mario$Condicion[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]  
    Dia_M <- mario$Día[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Hits_M <- mario$Corto_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    FA_M <- mario$Corto_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    CRej_M <- mario$Largo_enLargo[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Miss_M <- mario$Largo_enCorto[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Noise_M <- mario$EnsayosLargos[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    Signal_M <- mario$EnsayosCortos[mario$Sujeto %in% Mario_LargoEsSignal == TRUE & mario$Grupo %in% Mario_Intermedias == FALSE]
    
    Sujeto_P <- paty$sujeto[paty$grupo %in% Mario_Intermedias == FALSE]         #Del archivo 'datos', identificamos los datos en la columna 'Sujeto' como variable Sujeto
    Condicion_P <- paty$grupo[paty$grupo %in% Mario_Intermedias == FALSE]       #Del archivo 'datos', identificamos los datos en la columna 'Grupo' como variable Condición
    TipoSesion_P <- paty$condición[paty$grupo %in% Mario_Intermedias == FALSE]  #etc, etc, etc
    Dia_P <- paty$dia[paty$grupo %in% Mario_Intermedias == FALSE]
    Hits_P <- paty$rechazo[paty$grupo %in% Mario_Intermedias == FALSE]
    FA_P <- paty$miss[paty$grupo %in% Mario_Intermedias == FALSE]
    CRej_P <- paty$hit[paty$grupo %in% Mario_Intermedias == FALSE]
    Miss_P <- paty$fa[paty$grupo %in% Mario_Intermedias == FALSE]
    Noise_P <- paty$ensayos_cortos[paty$grupo %in% Mario_Intermedias == FALSE]
    Signal_P <- paty$ensayos_largo[paty$grupo %in% Mario_Intermedias == FALSE]}}

TotalSujetos_M <- length(unique(Sujeto_M))
TotalSujetos_P <- length(unique(Sujeto_P))

Sesiones <- c(rep('LB',10), rep('M',10))







###################################           Grafica de Barras comparando A' y B'' 
###################################           en Linea Base vs Magnitud, en cada condición
###################################           PROMEDIANDO TODOS LOS SUJETOS

layout(matrix(1:4,ncol=2, byrow=TRUE))

Par_A_LBM <- NULL
Par_A_M <- NULL
Par_B_LBM <- NULL
Par_B_M <- NULL

Par_A_LBP <- NULL
Par_A_Pre <- NULL
Par_B_LBP <- NULL
Par_B_Pre <- NULL


#Ciclo para Mario
for(z in 1){
  print("Experimento con Magnitudes")
for(a in sort(unique(Condicion_M))){
  print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion_M))){
    M_Sesion <- b
    M_Hits <- sum(Hits_M[Condicion_M==a&TipoSesion_M==b])
    M_FA <- sum(FA_M[Condicion_M==a&TipoSesion_M==b]) 
    M_Signal <- sum(Signal_M[Condicion_M==a&TipoSesion_M==b])
    M_Noise <- sum(Noise_M[Condicion_M==a&TipoSesion_M==b])
    M_Hrate <- M_Hits/M_Signal
    M_FArate <- M_FA/M_Noise
    if(M_FArate > M_Hrate){
      M_A <- 0.5- ( ((M_FArate-M_Hrate)*(1+M_FArate-M_Hrate)) / ((4*M_FArate)*(1-M_Hrate)) )
    } else {
      M_A <- 0.5+ ( ((M_Hrate-M_FArate)*(1+M_Hrate-M_FArate)) / ((4*M_Hrate)*(1-M_FArate)) )}
    if(M_FArate > M_Hrate){
      M_B <- (((M_FArate*(1-M_FArate))-(M_Hrate*(1-M_Hrate)))/((M_FArate*(1-M_FArate))+(M_Hrate*(1-M_Hrate))))
    } else {
      M_B <- (((M_Hrate*(1-M_Hrate))-(M_FArate*(1-M_FArate)))/((M_Hrate*(1-M_Hrate))+(M_FArate*(1-M_FArate))))}
    
    M_A <- round(M_A,3)
    M_B <- round(M_B,3)
    M_Hrate <- round(M_Hrate,3)
    M_FArate <- round(M_FArate,3)
    
    valores_M<- data.frame(cbind(b, M_Hrate, M_FArate, M_A, M_B))   #Acomodamos los valores en un arreglo
    colnames(valores_M) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valores_M)
    
    if(b=='LB'){
      Par_A_LBM[a] <- M_A
      Par_B_LBM[a] <- M_B
    } else {
      Par_A_M[a] <- M_A
      Par_B_M[a] <- M_B
    }}}}

#Ciclo para PAty
for(z in 1){
  print("Experimento con Magnitudes")
  for(a in sort(unique(Condicion_M))){
    print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion_P))){
    P_Sesion <- b
    P_Hits <- sum(Hits_P[Condicion_P==a&TipoSesion_P==b])
    P_FA <- sum(FA_P[Condicion_P==a&TipoSesion_P==b]) 
    P_Signal <- sum(Signal_P[Condicion_P==a&TipoSesion_P==b])
    P_Noise <- sum(Noise_P[Condicion_P==a&TipoSesion_P==b])
    P_Hrate <- P_Hits/P_Signal
    P_FArate <- P_FA/P_Noise
    if(P_FArate > P_Hrate){
      P_A <- 0.5- ( ((P_FArate-P_Hrate)*(1+P_FArate-P_Hrate)) / ((4*P_FArate)*(1-P_Hrate)) )
    } else {
      P_A <- 0.5+ ( ((P_Hrate-P_FArate)*(1+P_Hrate-P_FArate)) / ((4*P_Hrate)*(1-P_FArate)) )}
    if(P_FArate > P_Hrate){
      P_B <- (((P_FArate*(1-P_FArate))-(P_Hrate*(1-P_Hrate)))/((P_FArate*(1-P_FArate))+(P_Hrate*(1-P_Hrate))))
    } else {
      P_B <- (((P_Hrate*(1-P_Hrate))-(P_FArate*(1-P_FArate)))/((P_Hrate*(1-P_Hrate))+(P_FArate*(1-P_FArate))))}
    
    P_A <- round(P_A,3)
    P_B <- round(P_B,3)
    P_Hrate <- round(P_Hrate,3)
    P_FArate <- round(P_FArate,3)
    
    valores_P<- data.frame(cbind(b, P_Hrate, P_FArate, P_A, P_B))   #Acomodamos los valores en un arreglo
    colnames(valores_P) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valores_P)
    
    if(b=='LB'){
      Par_A_LBP[a] <- P_A
      Par_B_LBP[a] <- P_B
    } else {
      Par_A_Pre[a] <- P_A
      Par_B_Pre[a] <- P_B
    }}}}


for(d in c(1:2)){
  Blank <- c(0.00001,0.00001)
  Parejas_A <- c(cbind(Par_A_LBP[d], Par_A_Pre[d],Blank[d], Par_A_LBM[d], Par_A_M[d]))
  Parejas_B <- c(cbind(Par_B_LBP[d], Par_B_Pre[d],Blank[d], Par_B_LBM[d], Par_B_M[d]))

barplot(Parejas_A, main = "", ylim = c(0,1.05), axes=FALSE, col =c("cadetblue2", "deepskyblue4", "white", "cadetblue2", "dodgerblue4"))
axis(1,at=c(1.3,5),labels=c("Motivation", "Bias"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
text(0.72,(Parejas_A[1]-.1),paste(Parejas_A[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,(Parejas_A[2]-.1),paste(Parejas_A[2]),cex=.9,col='white',f=2, srt=90)
text(4.26,(Parejas_A[4]-.1),paste(Parejas_A[4]),cex=.9,col='black',f=2, srt=90)
text(5.5,(Parejas_A[5]-.1),paste(Parejas_A[5]),cex=.9,col='white',f=2, srt=90)
lines(c(2.5, 3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
lines(c(0.5, 1),c(1,1), lwd=3, lty=1, col="cadetblue2")
lines(c(4.5, 5),c(1,1), lwd=3, lty=1, col="dodgerblue4")
text(3.1, 1, labels="Pre-fed", offset=0, cex = 0.9, pos=4)
text(1.1, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(5.1, 1, labels="Magnitud", offset=0, cex = 0.9, pos=4)
mtext("A'",1,cex=2, line=1, f=2)
if(Paty_Signal=='Corto'){
  title("All Subjects", sub="Short is signal", outer = TRUE, line = -2)
} else {
  title("All Subjects", sub="Long is signal", outer = TRUE, line = -2)}


barplot(Parejas_B, main = "", border=NA, xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4", "white", "cadetblue2", "dodgerblue4"))
axis(1,at=c(1.3,5),labels=c("Motivation", "Bias"), font=2)
axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
text(0.72,0.5,paste(Parejas_B[1]),cex=.9,col='black',f=2)
text(1.9,0.5,paste(Parejas_B[2]),cex=.9,col='black',f=2)
text(4.26,0.5,paste(Parejas_B[4]),cex=.9,col='black',f=2)
text(5.5,0.5,paste(Parejas_B[5]),cex=.9,col='black',f=2)
lines(c(2.5, 3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
lines(c(0.5, 1),c(1,1), lwd=3, lty=1, col="cadetblue2")
lines(c(4.5, 5),c(1,1), lwd=3, lty=1, col="dodgerblue4")
text(3.1, 1, labels="Pre-fed", offset=0, cex = 0.9, pos=4)
text(1.1, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(5.1, 1, labels="Magnitud", offset=0, cex = 0.9, pos=4)
mtext("B'",1,cex=2, line=1, f=2)
if(Paty_Duracion=='Intermedia'){
  if(d==1){
    mtext("1v4",4,cex=2.5, line=1, f=2)
  } else {
    mtext("2v8",4,cex=2.5, line=1, f=2)}
} else {
  if(d==1){
    mtext("0.5v2",4,cex=2.5, line=1, f=2)
  } else {
    mtext("3v12",4,cex=2.5, line=1, f=2)}}} 
