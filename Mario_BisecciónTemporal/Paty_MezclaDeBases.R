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
# # # # # # # #  Parte I
# # # # # # # #  Cargamos los paty
####################################
setwd("C:/Users/Alejandro/Desktop/Felisa/Proyectos/Mario_BisecciónTemporal")# Directorio de trabajo
rm(list=ls())  #Reseteamos la consola
dir()          #Imprimimos los archivos contenidos en el directorio en la consola
archive_1 <-'Datos_Paty_Ext.csv'  
archive_2 <-'Datos_Paty_Int.csv'  
paty <- read.csv(archive_2) #DEFAULT: Duraciones intermedias
paty_ex <- read.csv(archive_1) #Duraciones Extremas (N=2)


########################################
# # # # # # # #  Parte II
# # # # # # # #  Especificamos variables
########################################
Sujeto <- paty$sujeto         #Del archivo 'paty', identificamos los datps en la columna 'Sujeto' como variable Sujeto
Condicion <- paty$grupo       #Del archivo 'paty', identificamos los datos en la columna 'Grupo' como variable Condición
TipoSesion <- paty$condición  #etc, etc, etc
Dia<- paty$dia
Hits <- paty$hit
FA <- paty$fa
CRej <- paty$rechazo
Miss <- paty$miss
Noise <- paty$ensayos_largo
Signal <- paty$ensayos_cortos

Sujeto_e <- paty_ex$sujeto         #Del archivo 'paty', identificamos los paty en la columna 'Sujeto' como variable Sujeto
Condicion_e <- paty_ex$grupo       #Del archivo 'paty', identificamos los paty en la columna 'Grupo' como variable Condición
TipoSesion_e <- paty_ex$condición  #etc, etc, etc
Dia_e <- paty_ex$dia
Hits_e <- paty_ex$hit
FA_e <- paty_ex$fa
CRej_e <- paty_ex$rechazo
Miss_e <- paty_ex$miss
Noise_e <- paty_ex$ensayos_largo
Signal_e <- paty_ex$ensayos_cortos

Signal_Is <- c(rep('Corto', length(Sujeto)))
TotalSujetos <- length(unique(Sujeto))
Sesiones <- c(rep('LB',10), rep('M',10))



###################################         
###### G R A F I C O
###################################         
layout(matrix(1:8,ncol=4, byrow=TRUE))

Par_A_LB <- NULL
Par_A_M <- NULL
Par_B_LB <- NULL
Par_B_M <- NULL

Par_A_LB_e <- NULL
Par_A_M_e <- NULL
Par_B_LB_e <- NULL
Par_B_M_e <- NULL


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
    d_<- qnorm(H_rate,0,1)-qnorm(FA_rate,0,1)
    Ad_<-pnorm(d_/(sqrt(2)))
    if(FA_rate > H_rate){
      A_ <- 0.5- ( ((FA_rate-H_rate)*(1+FA_rate-H_rate)) / ((4*FA_rate)*(1-H_rate)) )
    } else {
      A_ <- 0.5+ ( ((H_rate-FA_rate)*(1+H_rate-FA_rate)) / ((4*H_rate)*(1-FA_rate)) )}
    k_<-qnorm(1-FA_rate,0,1)               
    beta_<-dnorm(k_,d_,1)/dnorm(k_,0,1)             
    c_<-k_-(d_/2)
    if(FA_rate > H_rate){
      B_ <- (((FA_rate*(1-FA_rate))-(H_rate*(1-H_rate)))/((FA_rate*(1-FA_rate))+(H_rate*(1-H_rate))))
    } else {
      B_ <- (((H_rate*(1-H_rate))-(FA_rate*(1-FA_rate)))/((H_rate*(1-H_rate))+(FA_rate*(1-FA_rate))))}
    
    A_ <- round(A_,3)
    B_ <- round(B_,3)
    Beta_<-round(beta_,3)
    D_ <- round(d_,2)
    C_<- round(c_,3)
    A_d_ <- round(Ad_,3)
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
  
for(a in sort(unique(Condicion_e))){
  print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion_e))){
    Sesion <- b
    Hits_ex <- sum(Hits_e[Condicion_e==a&TipoSesion_e==b])
    FA_ex <- sum(FA_e[Condicion_e==a&TipoSesion_e==b]) 
    Signal_ex <- sum(Signal_e[Condicion_e==a&TipoSesion_e==b])
    Noise_ex <- sum(Noise_e[Condicion_e==a&TipoSesion_e==b])
    H_rate_e <- Hits_ex/Signal_ex
    FA_rate_e <- FA_ex/Noise_ex
    d_e<- qnorm(H_rate_e,0,1)-qnorm(FA_rate_e,0,1)
    Ad_e<-pnorm(d_e/(sqrt(2)))
    if(FA_rate_e > H_rate_e){
      A_e <- 0.5- ( ((FA_rate_e-H_rate_e)*(1+FA_rate_e-H_rate_e)) / ((4*FA_rate_e)*(1-H_rate_e)) )
    } else {
      A_e <- 0.5+ ( ((H_rate_e-FA_rate_e)*(1+H_rate_e-FA_rate_e)) / ((4*H_rate_e)*(1-FA_rate_e)) )}
    k_e<-qnorm(1-FA_rate_e,0,1)               
    beta_e<-dnorm(k_e,d_e,1)/dnorm(k_e,0,1)             
    c_e<-k_e-(d_e/2)
    if(FA_rate_e > H_rate_e){
      B_e <- (((FA_rate_e*(1-FA_rate_e))-(H_rate_e*(1-H_rate_e)))/((FA_rate_e*(1-FA_rate_e))+(H_rate_e*(1-H_rate_e))))
    } else {
      B_e <- (((H_rate_e*(1-H_rate_e))-(FA_rate_e*(1-FA_rate_e)))/((H_rate_e*(1-H_rate_e))+(FA_rate_e*(1-FA_rate_e))))}
    
    A_ex <- round(A_e,3)
    B_ex <- round(B_e,3)
    Beta_ex<-round(beta_e,3)
    D_ex <- round(d_e,2)
    C_ex<- round(c_e,3)
    A_d_ex <- round(Ad_e,3)
    H_rate_ex <- round(H_rate_e,3)
    FA_rate_ex <- round(FA_rate_e,3)
    
    valores_P_e<- data.frame(cbind(b, H_rate_ex, FA_rate_ex, A_ex, B_ex))   #Acomodamos los valores en un arreglo
    colnames(valores_P_e) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    print(valores_P_e)
    
    
    if(b=='LB'){
      Par_A_LB_e[a] <- A_ex
      Par_B_LB_e[a] <- B_ex
    } else {
      Par_A_M_e[a] <- A_ex
      Par_B_M_e[a] <- B_ex
    }}}
  
  Parejas_A <- c(cbind(rbind(Par_A_LB, Par_A_M),rbind(Par_A_LB_e, Par_A_M_e)))
  Parejas_B <- c(cbind(rbind(Par_B_LB, Par_B_M),rbind(Par_B_LB_e, Par_B_M_e)))
  
  barplot(Parejas_A[5:6], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[5]/2),paste(Parejas_A[5]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A[6]/2),paste(Parejas_A[6]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B[5:6], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[5]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[6]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext("0.5v2",4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), outer = TRUE, line = -2)
  
  barplot(Parejas_A[1:2], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[1]/2),paste(Parejas_A[1]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A[2]/2),paste(Parejas_A[2]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B[1:2], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[2]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext("1v4",4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), outer = TRUE, line = -2)
  
  barplot(Parejas_A[3:4], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[3]/2),paste(Parejas_A[3]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A[4]/2),paste(Parejas_A[4]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B[3:4], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[3]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[4]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext("2v8",4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), outer = TRUE, line = -2)
  
  barplot(Parejas_A[7:8], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[7]/2),paste(Parejas_A[7]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A[8]/2),paste(Parejas_A[8]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B[7:8], main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitud"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[7]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[8]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext("3v12",4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), outer = TRUE, line = -2)