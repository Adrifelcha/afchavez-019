##########################################################
# Codigo base para el análisis de datos obtenidos en el experimento presentado por Cruz, Pérez et al (2017)
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
# # # # # # # #  Parte I: Cargamos los datos
####################################
setwd("C:/Users/Adriana/Desktop/Felisa/Proyectos/Mario_BisecciónTemporal") # Directorio de trabajo
rm(list=ls())  #Reseteamos el espacio de trabajo
dir()          #Verificamos el contenido del directorio

Signal_Stimulus <- 'Corto'  #Opciones: 'Corto' o 'Largo'
Signal_Is <- c(rep(Signal_Stimulus, length(Sujeto)))
Paty_Duracion <- 'Extremas' #Opcion: 'Intermedias' o 'Extremas'

if(Paty_Duracion=='Intermedias'){
  archive <-'Datos_Paty_Int.csv'} else{
  archive <- 'Datos_Paty_Ext.csv'}

paty <- read.csv(archive)     #Extraemos los datos del archivo
########################################
# # # # # # # #  Parte II; Especificamos variables
########################################
Sujeto <- paty$sujeto         #Del archivo 'paty', identificamos los datos en la columna 'Sujeto' como variable Sujeto
Condicion <- paty$grupo       #Del archivo 'paty', identificamos los datos en la columna 'Grupo' como variable Condición
TipoSesion <- paty$condición  #etc, etc, etc
Dia<- paty$dia
Hits <- paty$hit
FA <- paty$fa
CRej <- paty$rechazo
Miss <- paty$miss
Noise <- paty$ensayos_largo
Signal <- paty$ensayos_cortos


########################################
# # # # # # # #  Parte III: Gráficos
########################################

  
# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 1
# Por cada Sujeto, se obtienen 4 gráficos donde se comparan
# A' y B'' en Linea Base vs Magnitud, en cada condición
# (un gráfico por cada par de duraciones)
#######################################    
layout(matrix(1:4,ncol=2, byrow=TRUE))
  
for(x in sort(unique(paty$sujeto))){
  print('===================================================')
  print(c('Sujeto:', x))
  print('===================================================')
  for(a in sort(unique(Condicion))){
    print(c('========> Magnitud:', a))
    Sesion <- c('LB','Pre')
    Hits_TS <- Hits[Condicion==a&Sujeto==x]
    FA_TS <- FA[Condicion==a&Sujeto==x] 
    Hits_LB <- sum(Hits_TS[1:10])
    FA_LB <- sum(FA_TS[1:10])
    Hits_M <- sum(Hits_TS[11:20])
    FA_M <- sum(FA_TS[11:20])
    Signal_TS <- Signal[Condicion==a&Sujeto==x]
    Noise_TS <- Noise[Condicion==a&Sujeto==x]
    N_LB <- sum(Noise_TS[1:10])
    S_LB <- sum(Signal_TS[1:10])
    N_M <- sum(Noise_TS[11:20])
    S_M <- sum(Signal_TS[11:20])
    H_rateLB <- Hits_LB/S_LB
    FA_rateLB <- FA_LB/N_LB
    H_rateM <- Hits_M/S_M
    FA_rateM <- FA_M/N_M
    if(FA_rateLB > H_rateLB){
      A_LB <- 0.5- ( ((FA_rateLB-H_rateLB)*(1+FA_rateLB-H_rateLB)) / ((4*FA_rateLB)*(1-H_rateLB)) )
    } else {
      A_LB <- 0.5+ ( ((H_rateLB-FA_rateLB)*(1+H_rateLB-FA_rateLB)) / ((4*H_rateLB)*(1-FA_rateLB)) )}
    if(FA_rateM > H_rateM){
      A_M <- 0.5- ( ((FA_rateM-H_rateM)*(1+FA_rateM-H_rateM)) / ((4*FA_rateM)*(1-H_rateM)) )
    } else {
      A_M <- 0.5+ ( ((H_rateM-FA_rateM)*(1+H_rateM-FA_rateM)) / ((4*H_rateM)*(1-FA_rateM)) )}
    if(FA_rateLB > H_rateLB){
      B_LB <- (((FA_rateLB*(1-FA_rateLB))-(H_rateLB*(1-H_rateLB)))/((FA_rateLB*(1-FA_rateLB))+(H_rateLB*(1-H_rateLB))))
    } else {
      B_LB <- (((H_rateLB*(1-H_rateLB))-(FA_rateLB*(1-FA_rateLB)))/((H_rateLB*(1-H_rateLB))+(FA_rateLB*(1-FA_rateLB))))}
    if(FA_rateM > H_rateM){
      B_M <- (((FA_rateM*(1-FA_rateM))-(H_rateM*(1-H_rateM)))/((FA_rateM*(1-FA_rateM))+(H_rateM*(1-H_rateM))))
    } else {
      B_M <- (((H_rateM*(1-H_rateM))-(FA_rateM*(1-FA_rateM)))/((H_rateM*(1-H_rateM))+(FA_rateM*(1-FA_rateM))))}
      
    A_TS <- c(round(A_LB,3), round(A_M,3))
    B_TS <- c(round(B_LB,3), round(B_M,3))
    H_rateTS <- c(round(H_rateLB,3), round(H_rateM,3))
    FA_rateTS <- c(round(FA_rateLB,3), round(FA_rateM,3))
      
    valoresTS<- data.frame(cbind(Sesion[unique(TipoSesion)], H_rateTS, FA_rateTS, A_TS, B_TS))   #Acomodamos los valores en un arreglo
    colnames(valoresTS) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valoresTS)
      
    barplot(A_TS, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
    axis(1,at=c(0.72,1.9),labels=c("Baseline", "Pre-fed"), font=2)
    axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
    text(0.72,(A_TS[1]/2),paste(A_TS[1]),cex=1.2,col='black',f=2)
    text(1.9,(A_TS[2]/2),paste(A_TS[2]),cex=1.2,col='white',f=2)
    mtext("A'",1,cex=1.3, line=3, f=2)
      
    barplot(B_TS, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
    axis(1,at=c(0.72,1.9),labels=c("Baseline", "Pre-fed"), font=2)
    axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
    text(0.72,0.6,paste(B_TS[1]),cex=1.2,col='black',f=2)
    text(1.9,0.6,paste(B_TS[2]),cex=1.2,col='black',f=2)
    mtext("B''",1,cex=1.5, line=3, f=2)
    mtext(a,4,cex=1.3, line=1, f=2)
    title(paste("Subject No.",x), outer = TRUE, line = -2)
    }}
  
# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 2
# Por cada Sujeto, se obtienen 2 gráficos donde se comparan
# A' y B'' en Linea Base vs Magnitud, en cada condición
# (todas las condiciones se presentan en un mismo gráfico)
#######################################
layout(matrix(1:2,ncol=2, byrow=TRUE))
  
Par_A_LB <- NULL
Par_A_M <- NULL
Par_B_LB <- NULL
Par_B_M <- NULL
  
for(x in sort(unique(paty$sujeto))){
  print('===================================================')
  print(c('Sujeto:', x))
  print('===================================================')
  for(a in sort(unique(Condicion))){
    print(c('========> Magnitud:', a))
    Sesion <- c('LB','Pre')
    Hits_TS <- Hits[Condicion==a&Sujeto==x]
    FA_TS <- FA[Condicion==a&Sujeto==x] 
    Hits_LB <- sum(Hits_TS[1:10])
    FA_LB <- sum(FA_TS[1:10])
    Hits_M <- sum(Hits_TS[11:20])
    FA_M <- sum(FA_TS[11:20])
    Signal_TS <- Signal[Condicion==a&Sujeto==x]
    Noise_TS <- Noise[Condicion==a&Sujeto==x]
    N_LB <- sum(Noise_TS[1:10])
    S_LB <- sum(Signal_TS[1:10])
    N_M <- sum(Noise_TS[11:20])
    S_M <- sum(Signal_TS[11:20])
    H_rateLB <- Hits_LB/S_LB
    FA_rateLB <- FA_LB/N_LB
    H_rateM <- Hits_M/S_M
    FA_rateM <- FA_M/N_M
    if(FA_rateLB > H_rateLB){
      A_LB <- 0.5- ( ((FA_rateLB-H_rateLB)*(1+FA_rateLB-H_rateLB)) / ((4*FA_rateLB)*(1-H_rateLB)) )
    } else {
      A_LB <- 0.5+ ( ((H_rateLB-FA_rateLB)*(1+H_rateLB-FA_rateLB)) / ((4*H_rateLB)*(1-FA_rateLB)) )}
    if(FA_rateM > H_rateM){
      A_M <- 0.5- ( ((FA_rateM-H_rateM)*(1+FA_rateM-H_rateM)) / ((4*FA_rateM)*(1-H_rateM)) )
    } else {
      A_M <- 0.5+ ( ((H_rateM-FA_rateM)*(1+H_rateM-FA_rateM)) / ((4*H_rateM)*(1-FA_rateM)) )}
    if(FA_rateLB > H_rateLB){
      B_LB <- (((FA_rateLB*(1-FA_rateLB))-(H_rateLB*(1-H_rateLB)))/((FA_rateLB*(1-FA_rateLB))+(H_rateLB*(1-H_rateLB))))
    } else {
      B_LB <- (((H_rateLB*(1-H_rateLB))-(FA_rateLB*(1-FA_rateLB)))/((H_rateLB*(1-H_rateLB))+(FA_rateLB*(1-FA_rateLB))))}
    if(FA_rateM > H_rateM){
      B_M <- (((FA_rateM*(1-FA_rateM))-(H_rateM*(1-H_rateM)))/((FA_rateM*(1-FA_rateM))+(H_rateM*(1-H_rateM))))
    } else {
      B_M <- (((H_rateM*(1-H_rateM))-(FA_rateM*(1-FA_rateM)))/((H_rateM*(1-H_rateM))+(FA_rateM*(1-FA_rateM))))}
      
    A_TS <- c(round(A_LB,3), round(A_M,3))
    B_TS <- c(round(B_LB,3), round(B_M,3))
    H_rateTS <- c(round(H_rateLB,3), round(H_rateM,3))
    FA_rateTS <- c(round(FA_rateLB,3), round(FA_rateM,3))
      
    valoresTS<- data.frame(cbind(Sesion[unique(TipoSesion)], H_rateTS, FA_rateTS, A_TS, B_TS))   #Acomodamos los valores en un arreglo
    colnames(valoresTS) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valoresTS)
      
    Par_A_LB[a] <- A_TS[1]
    Par_A_M[a] <- A_TS[2]
    Par_B_LB[a] <- B_TS[1]
    Par_B_M[a] <- B_TS[2]
    }
    
    Parejas_A <- c(rbind(Par_A_LB, Par_A_M))
    Parejas_B <- c(rbind(Par_B_LB, Par_B_M))
    
    barplot(Parejas_A, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
    if(Paty_Duracion=='Intermedias'){
    axis(1,at=c(1.3,3.7),labels=c("1vs4", "2vs8"), font=2)} else{
    axis(1,at=c(1.3,3.7),labels=c("0.5vs2", "3vs12"), font=2)}
    axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
    text(0.72,(Parejas_A[1]-.07),paste(Parejas_A[1]),cex=.9,col='black',f=2, srt=90)
    text(1.9,(Parejas_A[2]-.07),paste(Parejas_A[2]),cex=.9,col='white',f=2, srt=90)
    text(3.08,(Parejas_A[3]-.07),paste(Parejas_A[3]),cex=.9,col='black',f=2, srt=90)
    text(4.26,(Parejas_A[4]-.07),paste(Parejas_A[4]),cex=.9,col='white',f=2, srt=90)
    lines(c(3.4, 4.1),c(1,1), lwd=3, lty=1, col="deepskyblue4")
    lines(c(0.5, 1.2),c(1,1), lwd=3, lty=1, col="cadetblue2")
    text(1.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
    text(4.4, 1, labels="Pre-Fed", offset=0, cex = 0.9, pos=4)
    mtext("A'",3,cex=3.5, line=1, f=2)
    
    barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
    if(Paty_Duracion=='Intermedias'){
    axis(1,at=c(1.3,3.7),labels=c("1vs4", "2vs8"), font=2)} else{
    axis(1,at=c(1.3,3.7),labels=c("0.5vs2", "3vs12"), font=2)}
    axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
    text(0.72,0.5,paste(Parejas_B[1]),cex=.9,col='black',f=2, srt=90)
    text(1.9,0.5,paste(Parejas_B[2]),cex=.9,col='black',f=2, srt=90)
    text(3.08,0.5,paste(Parejas_B[3]),cex=.9,col='black',f=2, srt=90)
    text(4.26,0.5,paste(Parejas_B[4]),cex=.9,col='black',f=2, srt=90)
    lines(c(3.4, 4.1),c(1,1), lwd=3, lty=1, col="deepskyblue4")
    lines(c(0.5, 1.2),c(1,1), lwd=3, lty=1, col="cadetblue2")
    text(1.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
    text(4.4, 1, labels="Pre-Fed", offset=0, cex = 0.9, pos=4)
    mtext("B'",3,cex=3.5, line=1, f=2)
    mtext(paste("Subject No.",x),4,cex=2, line=1, f=2)}
  
# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 3
# Promediando TODOS LOS SUJETOS 
# se obtienen 8 gráficos donde se comparan
# A' y B'' en Linea Base vs Magnitud, en cada condición
#######################################
layout(matrix(1:4,ncol=2, byrow=TRUE))

Par_A_LB <- NULL
Par_A_M <- NULL
Par_B_LB <- NULL
Par_B_M <- NULL

for(a in sort(unique(Condicion))){
  print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion))){
    Sesion <- b
    Hits_ <- sum(Hits[Condicion==a&TipoSesion==b])
    FA_ <- sum(FA[Condicion==a&TipoSesion==b]) 
    Signal_ <- sum(Signal[Condicion==a&TipoSesion==b])
    Noise_ <- sum(Noise[Condicion==a&TipoSesion==b])
    H_rate <- Hits_/Signal_
    FA_rate <- FA_/Noise_
    if(FA_rate > H_rate){
      A_ <- 0.5- ( ((FA_rate-H_rate)*(1+FA_rate-H_rate)) / ((4*FA_rate)*(1-H_rate)) )
    } else {
      A_ <- 0.5+ ( ((H_rate-FA_rate)*(1+H_rate-FA_rate)) / ((4*H_rate)*(1-FA_rate)) )}
    if(FA_rate > H_rate){
      B_ <- (((FA_rate*(1-FA_rate))-(H_rate*(1-H_rate)))/((FA_rate*(1-FA_rate))+(H_rate*(1-H_rate))))
    } else {
      B_ <- (((H_rate*(1-H_rate))-(FA_rate*(1-FA_rate)))/((H_rate*(1-H_rate))+(FA_rate*(1-FA_rate))))}
    
    A_ <- round(A_,3)
    B_ <- round(B_,3)
    H_rate <- round(H_rate,3)
    FA_rate <- round(FA_rate,3)
    
    valores_P<- data.frame(cbind(b, H_rate, FA_rate, A_, B_))   #Acomodamos los valores en un arreglo
    colnames(valores_P) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valores_P)

    if(b=='LB'){
      Par_A_LB <- A_
      Par_B_LB <- B_
    } else {
      Par_A_M <- A_
      Par_B_M <- B_
    }}
  
  Parejas_A <- c(rbind(Par_A_LB, Par_A_M))
  Parejas_B <- c(rbind(Par_B_LB, Par_B_M))
  
  barplot(Parejas_A, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Pre-Fed"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[1]/2),paste(Parejas_A[1]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A[2]/2),paste(Parejas_A[2]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Pre-Fed"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[2]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext(a,4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), outer = TRUE, line = -2)}





# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 4
# Promediando TODOS los sujetos
# se comparan A' y B'' en Linea Base vs Magnitud, presentando
# todas las condiciones en un solo gráfico
#######################################
layout(matrix(1:2,ncol=2, byrow=TRUE))
  
Par_A_LB <- NULL
Par_A_M <- NULL
Par_B_LB <- NULL
Par_B_M <- NULL
  
for(a in sort(unique(Condicion))){
  print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion))){
    Sesion <- b
    Hits_ <- sum(Hits[Condicion==a&TipoSesion==b])
    FA_ <- sum(FA[Condicion==a&TipoSesion==b]) 
    Signal_ <- sum(Signal[Condicion==a&TipoSesion==b])
    Noise_ <- sum(Noise[Condicion==a&TipoSesion==b])
    H_rate <- Hits_/Signal_
    FA_rate <- FA_/Noise_
    if(FA_rate > H_rate){
      A_ <- 0.5- ( ((FA_rate-H_rate)*(1+FA_rate-H_rate)) / ((4*FA_rate)*(1-H_rate)) )
    } else {
      A_ <- 0.5+ ( ((H_rate-FA_rate)*(1+H_rate-FA_rate)) / ((4*H_rate)*(1-FA_rate)) )}
    if(FA_rate > H_rate){
      B_ <- (((FA_rate*(1-FA_rate))-(H_rate*(1-H_rate)))/((FA_rate*(1-FA_rate))+(H_rate*(1-H_rate))))
    } else {
      B_ <- (((H_rate*(1-H_rate))-(FA_rate*(1-FA_rate)))/((H_rate*(1-H_rate))+(FA_rate*(1-FA_rate))))}
      
    A_ <- round(A_,3)
    B_ <- round(B_,3)
    H_rate <- round(H_rate,3)
    FA_rate <- round(FA_rate,3)
      
    valores_P<- data.frame(cbind(b, H_rate, FA_rate, A_, B_))   #Acomodamos los valores en un arreglo
    colnames(valores_P) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valores_P)
    
    if(b=='LB'){
      Par_A_LB[a] <- A_
      Par_B_LB[a] <- B_
    } else {
      Par_A_M[a] <- A_
      Par_B_M[a] <- B_
    }}}

  
Parejas_A <- c(rbind(Par_A_LB, Par_A_M))
Parejas_B <- c(rbind(Par_B_LB, Par_B_M))
  
  barplot(Parejas_A, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  if(Paty_Duracion=='Intermedias'){
    axis(1,at=c(1.3,3.7),labels=c("1vs4", "2vs8"), font=2)} else{
    axis(1,at=c(1.3,3.7),labels=c("0.5vs2", "3vs12"), font=2)}
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[1]-.07),paste(Parejas_A[1]),cex=.9,col='black',f=2, srt=90)
  text(1.9,(Parejas_A[2]-.07),paste(Parejas_A[2]),cex=.9,col='white',f=2, srt=90)
  text(3.08,(Parejas_A[3]-.07),paste(Parejas_A[3]),cex=.9,col='black',f=2, srt=90)
  text(4.26,(Parejas_A[4]-.07),paste(Parejas_A[4]),cex=.9,col='white',f=2, srt=90)
  text(5.5,(Parejas_A[5]-.07),paste(Parejas_A[5]),cex=.9,col='black',f=2, srt=90)
  text(6.67,(Parejas_A[6]-.07),paste(Parejas_A[6]),cex=.9,col='white',f=2, srt=90)
  text(7.9,(Parejas_A[7]-.07),paste(Parejas_A[7]),cex=.9,col='black',f=2, srt=90)
  text(9.1,(Parejas_A[8]-.07),paste(Parejas_A[8]),cex=.9,col='white',f=2, srt=90)
  lines(c(3.4, 4.1),c(1,1), lwd=3, lty=1, col="deepskyblue4")
  lines(c(0.5, 1.2),c(1,1), lwd=3, lty=1, col="cadetblue2")
  text(1.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
  text(4.4, 1, labels="Pre-Fed", offset=0, cex = 0.9, pos=4)
  mtext("A'",3,cex=3.5, line=1, f=2)
  
  barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("deepskyblue4", "cadetblue2"))
  if(Paty_Duracion=='Intermedias'){
    axis(1,at=c(1.3,3.7),labels=c("1vs4", "2vs8"), font=2)} else{
    axis(1,at=c(1.3,3.7),labels=c("0.5vs2", "3vs12"), font=2)}
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.5,paste(Parejas_B[1]),cex=.9,col='black',f=2, srt=90)
  text(1.9,0.5,paste(Parejas_B[2]),cex=.9,col='black',f=2, srt=90)
  text(3.08,0.5,paste(Parejas_B[3]),cex=.9,col='black',f=2, srt=90)
  text(4.26,0.5,paste(Parejas_B[4]),cex=.9,col='black',f=2, srt=90)
  text(5.5,0.5,paste(Parejas_B[5]),cex=.9,col='black',f=2, srt=90)
  text(6.67,0.5,paste(Parejas_B[6]),cex=.9,col='black',f=2, srt=90)
  text(7.9,0.5,paste(Parejas_B[7]),cex=.9,col='black',f=2, srt=90)
  text(9.1,0.5,paste(Parejas_B[8]),cex=.9,col='black',f=2, srt=90)
  lines(c(3.4, 4.1),c(1,1), lwd=3, lty=1, col="deepskyblue4")
  lines(c(0.5, 1.2),c(1,1), lwd=3, lty=1, col="cadetblue2")
  text(1.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
  text(4.4, 1, labels="Pre-Fed", offset=0, cex = 0.9, pos=4)
  mtext("B''",3,cex=3.5, line=1, f=2)
  mtext("ALL subjects", 4,cex=2, line=1, f=2)