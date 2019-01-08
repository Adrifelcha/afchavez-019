##########################################################
# Codigo base para el análisis de datos obtenidos en el experimento presentado por Pérez et al (2017)
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
# # # # # PARTE I:  Cargamos los datos
####################################
setwd("C:/Users/Adriana/Desktop/Felisa/Proyectos/Mario_BisecciónTemporal") #Directorio de trabajo
rm(list=ls())  #Reseteamos  el espacio de trabajo
dir()          #Imprimimos en la consola los archivos contenidos en el directorio
archive <-'Datos_Mario_.csv'  #Señalamos el archivo que contiene los datos
datos <- read.csv(archive)    #'Leemos' el archivo

########################################
# # # # # PARTE II:  Especificamos variables
########################################
Sujeto <- datos$Sujeto         #Del archivo 'datos', identificamos los datos en la columna 'Sujeto' como variable Sujeto
Condicion <- datos$Grupo       #Del archivo 'datos', identificamos los datos en la columna 'Grupo' como variable Condición
TipoSesion <- datos$Condicion  #etc, etc, etc
Dia<-datos$Día


Hits <- NULL #Creamos un conjunto de arreglos vacíos 
FA <- NULL   #que posteriormente se van a llenar con datos provenientes
CRej <- NULL #de nuestro archivo
Miss <- NULL
Noise <- NULL
Signal <- NULL

LargoEsSignal <- c(5, 20, 33)  #Especificamos cuáles Sujetos estuvieron en el grupo donde LARGO es Señal

#Llenamos los arreglos vacíos con una condicional
for(i in 1:length(Dia)){                      #Para todos los datos en la columna Día (el largo del csv)
  if(Sujeto[i] %in% LargoEsSignal == TRUE){   #si el Sujeto aparece en el grupo donde Largo es Señal 
    Hits[i] <- datos$Largo_enLargo[i]         
    FA[i] <- datos$Largo_enCorto[i]           #Los Hits, FA, etc, etc van a estar
    CRej[i] <- datos$Corto_enCorto[i]         #definidos en función a que la Señal
    Miss[i] <- datos$Corto_enLargo[i]         #son las duraciones Largas
    Noise[i] <- datos$EnsayosCortos[i] 
    Signal[i] <- datos$EnsayosLargos[i]
  } else {
    Hits[i] <- datos$Corto_enCorto[i]
    FA[i] <- datos$Corto_enLargo[i]           #De lo contrario (si el sujeto NO esta en el grupo Largo es Señal)
    CRej[i] <- datos$Largo_enLargo[i]         #definimos los Hits, FA, etc, etc a partir de la definición
    Signal[i] <- datos$EnsayosCortos[i]       #de la duración Corta como Señal
    Noise[i] <- datos$EnsayosLargos[i]}}

Sesiones <- c(rep('LB',10), rep('M',10))





########################################
# # # # # Parte III:  G R A F I C O S
########################################


# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 1
# Por cada Sujeto, se obtienen 8 gráficos donde se comparan
# A' y B'' en Linea Base vs Magnitud, en cada condición
# (un gráfico por cada par de duraciones)
#######################################          
layout(matrix(1:8,ncol=4, byrow=TRUE))

for(x in sort(unique(datos$Sujeto))){
  print('===================================================')
  print(c('Sujeto:', x))
  print('===================================================')
  for(a in sort(unique(Condicion))){
    print(c('========> Magnitud:', a))
      Sesion <- c('LB','M')
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
      axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
      axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
      text(0.72,(A_TS[1]/2),paste(A_TS[1]),cex=1.2,col='black',f=2)
      text(1.9,(A_TS[2]/2),paste(A_TS[2]),cex=1.2,col='white',f=2)
      mtext("A'",1,cex=1.3, line=3, f=2)

      barplot(B_TS, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
      axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
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

for(x in sort(unique(datos$Sujeto))){
  print('===================================================')
  print(c('Sujeto:', x))
  print('===================================================')
  for(a in sort(unique(Condicion))){
    print(c('========> Magnitud:', a))
    Sesion <- c('LB','M')
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
  axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A[1]-.07),paste(Parejas_A[1]),cex=.9,col='black',f=2, srt=90)
  text(1.9,(Parejas_A[2]-.07),paste(Parejas_A[2]),cex=.9,col='white',f=2, srt=90)
  text(3.08,(Parejas_A[3]-.07),paste(Parejas_A[3]),cex=.9,col='black',f=2, srt=90)
  text(4.26,(Parejas_A[4]-.07),paste(Parejas_A[4]),cex=.9,col='white',f=2, srt=90)
  text(5.5,(Parejas_A[5]-.07),paste(Parejas_A[5]),cex=.9,col='black',f=2, srt=90)
  text(6.67,(Parejas_A[6]-.07),paste(Parejas_A[6]),cex=.9,col='white',f=2, srt=90)
  text(7.9,(Parejas_A[7]-.07),paste(Parejas_A[7]),cex=.9,col='black',f=2, srt=90)
  text(9.1,(Parejas_A[8]-.07),paste(Parejas_A[8]),cex=.9,col='white',f=2, srt=90)
  lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
  lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="cadetblue2")
  text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
  text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
  mtext("A'",3,cex=3.5, line=1, f=2)
  
  barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.5,paste(Parejas_B[1]),cex=.9,col='black',f=2, srt=90)
  text(1.9,0.5,paste(Parejas_B[2]),cex=.9,col='black',f=2, srt=90)
  text(3.08,0.5,paste(Parejas_B[3]),cex=.9,col='black',f=2, srt=90)
  text(4.26,0.5,paste(Parejas_B[4]),cex=.9,col='black',f=2, srt=90)
  text(5.5,0.5,paste(Parejas_B[5]),cex=.9,col='black',f=2, srt=90)
  text(6.67,0.5,paste(Parejas_B[6]),cex=.9,col='black',f=2, srt=90)
  text(7.9,0.5,paste(Parejas_B[7]),cex=.9,col='black',f=2, srt=90)
  text(9.1,0.5,paste(Parejas_B[8]),cex=.9,col='black',f=2, srt=90)
  lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="darkseagreen4")
  lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="darkseagreen3")
  text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
  text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
  mtext("B''",3,cex=3.5, line=1, f=2)
  mtext(paste("Subject No.",x),4,cex=2, line=1, f=2)}

# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 3
# Promediando TODOS LOS SUJETOS (Señal-Corto Y Señal-Largo) 
# se obtienen 8 gráficos donde se comparan
# A' y B'' en Linea Base vs Magnitud, en cada condición
#######################################
layout(matrix(1:8,ncol=4, byrow=TRUE))

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
      Par_A_LB <- A_
      Par_B_LB <- B_
    } else {
      Par_A_M <- A_
      Par_B_M <- B_
    }}
  
  Parejas_A <- c(rbind(Par_A_LB, Par_A_M))
  Parejas_B <- c(rbind(Par_B_LB, Par_B_M))
  
  barplot(Parejas_A, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,0.6,paste(Parejas_A[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_A[2]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B[2]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext(a,4,cex=1.3, line=1, f=2)
  title(paste("All Subjects"), sub="Long & Short as signal", outer = TRUE, line = -2)}


  
# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 4
# Promediando TODOS los sujetos (Corto Señal y Largo Señal)
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
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
text(0.72,(Parejas_A[1]-.07),paste(Parejas_A[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,(Parejas_A[2]-.07),paste(Parejas_A[2]),cex=.9,col='white',f=2, srt=90)
text(3.08,(Parejas_A[3]-.07),paste(Parejas_A[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,(Parejas_A[4]-.07),paste(Parejas_A[4]),cex=.9,col='white',f=2, srt=90)
text(5.5,(Parejas_A[5]-.07),paste(Parejas_A[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,(Parejas_A[6]-.07),paste(Parejas_A[6]),cex=.9,col='white',f=2, srt=90)
text(7.9,(Parejas_A[7]-.07),paste(Parejas_A[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,(Parejas_A[8]-.07),paste(Parejas_A[8]),cex=.9,col='white',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="cadetblue2")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("A'",3,cex=3.5, line=1, f=2)

barplot(Parejas_B, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
text(0.72,0.5,paste(Parejas_B[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,0.5,paste(Parejas_B[2]),cex=.9,col='black',f=2, srt=90)
text(3.08,0.5,paste(Parejas_B[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,0.5,paste(Parejas_B[4]),cex=.9,col='black',f=2, srt=90)
text(5.5,0.5,paste(Parejas_B[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,0.5,paste(Parejas_B[6]),cex=.9,col='black',f=2, srt=90)
text(7.9,0.5,paste(Parejas_B[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,0.5,paste(Parejas_B[8]),cex=.9,col='black',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="darkseagreen4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="darkseagreen3")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("B''",3,cex=3.5, line=1, f=2)
mtext("ALL subjects (Long&Short)",4,cex=2, line=1, f=2)

# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 5
# Promediando TODOS los sujetos (Corto Señal y Largo Señal)
# se comparan A' y B'' en Linea Base vs Magnitud, presentando
# Un gráfico por cada condición
#######################################
layout(matrix(1:8,ncol=4, byrow=TRUE))

Par_A_LB_L <- NULL
Par_B_LB_L <- NULL
Par_A_LB_C <- NULL
Par_B_LB_C <- NULL

Par_A_M_L <- NULL
Par_B_M_L <- NULL
Par_A_M_C <- NULL
Par_B_M_C <- NULL

v_A_LS <- NULL
v_B_LS <- NULL
v_H_rate_LS <- NULL
v_FA_rate_LS <- NULL

v_A_CS <- NULL
v_B_CS <- NULL
v_H_rate_CS <- NULL
v_FA_rate_CS <- NULL

for(a in sort(unique(Condicion))){
  print(c('==============================> Condición:', a))
  for(b in sort(unique(TipoSesion))){
    Hits_LS <- sum(Hits[Condicion==a&TipoSesion==b &Sujeto %in% LargoEsSignal ==TRUE])
    FA_LS <- sum(FA[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE]) 
    Signal_LS <- sum(Signal[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE])
    Noise_LS <- sum(Noise[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE])
    H_rate_LS <- Hits_LS/Signal_LS
    FA_rate_LS <- FA_LS/Noise_LS
    
    Hits_CS <- sum(Hits[Condicion==a&TipoSesion==b &Sujeto %in% LargoEsSignal ==FALSE])
    FA_CS <- sum(FA[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE]) 
    Signal_CS <- sum(Signal[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE])
    Noise_CS <- sum(Noise[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE])
    H_rate_CS <- Hits_CS/Signal_CS
    FA_rate_CS <- FA_CS/Noise_CS
    
    if(FA_rate_LS > H_rate_LS){
      A_LS <- 0.5- ( ((FA_rate_LS-H_rate_LS)*(1+FA_rate_LS-H_rate_LS)) / ((4*FA_rate_LS)*(1-H_rate_LS)))
    } else {
      A_LS <- 0.5+ ( ((H_rate_LS-FA_rate_LS)*(1+H_rate_LS-FA_rate_LS)) / ((4*H_rate_LS)*(1-FA_rate_LS)))}
    if(FA_rate_CS > H_rate_CS){
      A_CS <- 0.5- ( ((FA_rate_CS-H_rate_CS)*(1+FA_rate_CS-H_rate_CS)) / ((4*FA_rate_CS)*(1-H_rate_CS)))
    } else {
      A_CS <- 0.5+ ( ((H_rate_CS-FA_rate_CS)*(1+H_rate_CS-FA_rate_CS)) / ((4*H_rate_CS)*(1-FA_rate_CS)))}
    
    if(FA_rate_LS > H_rate_LS){
      B_LS <-(((FA_rate_LS*(1-FA_rate_LS))-(H_rate_LS*(1-H_rate_LS)))/((FA_rate_LS*(1-FA_rate_LS))+(H_rate_LS*(1-H_rate_LS))))
    } else {
      B_LS <-(((H_rate_LS*(1-H_rate_LS))-(FA_rate_LS*(1-FA_rate_LS)))/((H_rate_LS*(1-H_rate_LS))+(FA_rate_LS*(1-FA_rate_LS))))}
    if(FA_rate_CS > H_rate_CS){
      B_CS <-(((FA_rate_CS*(1-FA_rate_CS))-(H_rate_CS*(1-H_rate_CS)))/((FA_rate_CS*(1-FA_rate_CS))+(H_rate_CS*(1-H_rate_CS))))
    } else {
      B_CS <-(((H_rate_CS*(1-H_rate_CS))-(FA_rate_CS*(1-FA_rate_CS)))/((H_rate_CS*(1-H_rate_CS))+(FA_rate_CS*(1-FA_rate_CS))))}
    
    v_A_LS <- round(A_LS,3)
    v_B_LS <- round(B_LS,3)
    v_H_rate_LS <- round(H_rate_LS,3)
    v_FA_rate_LS <- round(FA_rate_LS,3)
    
    v_A_CS <- round(A_CS,3)
    v_B_CS <- round(B_CS,3)
    v_H_rate_CS <- round(H_rate_CS,3)
    v_FA_rate_CS <- round(FA_rate_CS,3)
    
    if(b=='LB'){
      Par_A_LB_L[a] <- v_A_LS
      Par_B_LB_L[a] <- v_B_LS
      Par_A_LB_C[a] <- v_A_CS
      Par_B_LB_C[a] <- v_B_CS
    } else {
      Par_A_M_L[a] <- v_A_LS
      Par_B_M_L[a] <- v_B_LS
      Par_A_M_C[a] <- v_A_CS
      Par_B_M_C[a] <- v_B_CS}}
  
  Sesiones <- c('LB', 'M')
  valores_C<- data.frame(cbind(Sesiones, v_H_rate_CS, v_FA_rate_CS, v_A_CS, v_B_CS))   #Acomodamos los valores en un arreglo
  colnames(valores_C) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
  
  valores_L<- data.frame(cbind(Sesiones, v_H_rate_LS, v_FA_rate_LS, v_A_LS, v_B_LS))   #Acomodamos los valores en un arreglo
  colnames(valores_L) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
  
  print("========== > Largo es Señal")
  print(valores_L)
  print("========== > Corto es Señal")
  print(valores_C)}

for(d in c(1:4)){
  Parejas_A_L <- c(rbind(Par_A_LB_L[d], Par_A_M_L[d]))
  Parejas_B_L <- c(rbind(Par_B_LB_L[d], Par_B_M_L[d]))
  
  barplot(Parejas_A_L, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A_L[1]/2),paste(Parejas_A_L[1]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A_L[2]/2),paste(Parejas_A_L[2]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B_L, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B_L[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B_L[2]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext(sort(unique(Condicion))[d],4,cex=1.3, line=1, f=2)
  title("All Subjects", sub="Long is Signal", outer = TRUE, line = -2)
}

for(d in c(1:4)){
  Parejas_A_C <- c(rbind(Par_A_LB_C[d], Par_A_M_C[d]))
  Parejas_B_C <- c(rbind(Par_B_LB_C[d], Par_B_M_C[d])) 
  
  barplot(Parejas_A_C, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
  text(0.72,(Parejas_A_C[1]/2),paste(Parejas_A_C[1]),cex=1.2,col='black',f=2)
  text(1.9,(Parejas_A_C[2]/2),paste(Parejas_A_C[2]),cex=1.2,col='white',f=2)
  mtext("A'",1,cex=1.3, line=3, f=2)
  
  barplot(Parejas_B_C, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
  axis(1,at=c(0.72,1.9),labels=c("Baseline", "Magnitude"), font=2)
  axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
  text(0.72,0.6,paste(Parejas_B_C[1]),cex=1.2,col='black',f=2)
  text(1.9,0.6,paste(Parejas_B_C[2]),cex=1.2,col='black',f=2)
  mtext("B''",1,cex=1.5, line=3, f=2)
  mtext(sort(unique(Condicion))[d],4,cex=1.3, line=1, f=2)
  title("All Subjects", "Short is signal", outer = TRUE, line = -2)
}


# # # # # # # # # # # # # # # # # # # #
#          GRAFICO 6
# Promediando TODOS los sujetos (Corto Señal y Largo Señal)
# se comparan A' y B'' en Linea Base vs Magnitud, presentando
# todas las condiciones en un solo gráfico
#######################################

layout(matrix(1:2,ncol=2, byrow=TRUE))

Par_A_LB_L <- NULL
Par_B_LB_L <- NULL
Par_A_LB_C <- NULL
Par_B_LB_C <- NULL

Par_A_M_L <- NULL
Par_B_M_L <- NULL
Par_A_M_C <- NULL
Par_B_M_C <- NULL

Sesion <- c("LB", "M")

for(a in sort(unique(Condicion))){
  print(c('========> Magnitud:', a))
  for(b in sort(unique(TipoSesion))){
    Hits_LS <- sum(Hits[Condicion==a&TipoSesion==b &Sujeto %in% LargoEsSignal ==TRUE])
    FA_LS <- sum(FA[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE]) 
    Signal_LS <- sum(Signal[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE])
    Noise_LS <- sum(Noise[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==TRUE])
    H_rate_LS <- Hits_LS/Signal_LS
    FA_rate_LS <- FA_LS/Noise_LS
    
    Hits_CS <- sum(Hits[Condicion==a&TipoSesion==b &Sujeto %in% LargoEsSignal ==FALSE])
    FA_CS <- sum(FA[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE]) 
    Signal_CS <- sum(Signal[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE])
    Noise_CS <- sum(Noise[Condicion==a&TipoSesion==b & Sujeto %in% LargoEsSignal ==FALSE])
    H_rate_CS <- Hits_CS/Signal_CS
    FA_rate_CS <- FA_CS/Noise_CS
    
    if(FA_rate_LS > H_rate_LS){
      A_LS <- 0.5- ( ((FA_rate_LS-H_rate_LS)*(1+FA_rate_LS-H_rate_LS)) / ((4*FA_rate_LS)*(1-H_rate_LS)))
    } else {
      A_LS <- 0.5+ ( ((H_rate_LS-FA_rate_LS)*(1+H_rate_LS-FA_rate_LS)) / ((4*H_rate_LS)*(1-FA_rate_LS)))}
    if(FA_rate_CS > H_rate_CS){
      A_CS <- 0.5- ( ((FA_rate_CS-H_rate_CS)*(1+FA_rate_CS-H_rate_CS)) / ((4*FA_rate_CS)*(1-H_rate_CS)))
    } else {
      A_CS <- 0.5+ ( ((H_rate_CS-FA_rate_CS)*(1+H_rate_CS-FA_rate_CS)) / ((4*H_rate_CS)*(1-FA_rate_CS)))}
    
    if(FA_rate_LS > H_rate_LS){
      B_LS <-(((FA_rate_LS*(1-FA_rate_LS))-(H_rate_LS*(1-H_rate_LS)))/((FA_rate_LS*(1-FA_rate_LS))+(H_rate_LS*(1-H_rate_LS))))
    } else {
      B_LS <-(((H_rate_LS*(1-H_rate_LS))-(FA_rate_LS*(1-FA_rate_LS)))/((H_rate_LS*(1-H_rate_LS))+(FA_rate_LS*(1-FA_rate_LS))))}
    if(FA_rate_CS > H_rate_CS){
      B_CS <-(((FA_rate_CS*(1-FA_rate_CS))-(H_rate_CS*(1-H_rate_CS)))/((FA_rate_CS*(1-FA_rate_CS))+(H_rate_CS*(1-H_rate_CS))))
    } else {
      B_CS <-(((H_rate_CS*(1-H_rate_CS))-(FA_rate_CS*(1-FA_rate_CS)))/((H_rate_CS*(1-H_rate_CS))+(FA_rate_CS*(1-FA_rate_CS))))}
    
    A_LS <- round(A_LS,3)
    B_LS <- round(B_LS,3)
    H_rate_LS <- round(H_rate_LS,3)
    FA_rate_LS <- round(FA_rate_LS,3)
    
    A_CS <- round(A_CS,3)
    B_CS <- round(B_CS,3)
    H_rate_CS <- round(H_rate_CS,3)
    FA_rate_CS <- round(FA_rate_CS,3)
    
    valores_C<- data.frame(cbind(b, H_rate_CS, FA_rate_CS, A_CS, B_CS))   #Acomodamos los valores en un arreglo
    colnames(valores_C) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    
    valores_L<- data.frame(cbind(b, H_rate_LS, FA_rate_LS, A_LS, B_LS))   #Acomodamos los valores en un arreglo
    colnames(valores_L) <- c("Sesion",'Hits', 'FA', "A'", "B'") #, "D'","A_d'","A'","Beta", "C", "B''")
    
    print("==============================================Largo es Señal")
    print(valores_L)
    print("==============================================Corto es Señal")
    print(valores_C)
    
    if(b=='LB'){
      Par_A_LB_L[a] <- A_LS
      Par_B_LB_L[a] <- B_LS
      Par_A_LB_C[a] <- A_CS
      Par_B_LB_C[a] <- B_CS
    } else {
      Par_A_M_L[a] <- A_LS
      Par_B_M_L[a] <- B_LS
      Par_A_M_C[a] <- A_CS
      Par_B_M_C[a] <- B_CS}}
  
  Parejas_A_L <- c(rbind(Par_A_LB_L, Par_A_M_L))
  Parejas_B_L <- c(rbind(Par_B_LB_L, Par_B_M_L))
  Parejas_A_C <- c(rbind(Par_A_LB_C, Par_A_M_C))
  Parejas_B_C <- c(rbind(Par_B_LB_C, Par_B_M_C))
}

barplot(Parejas_A_L, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
text(0.72,(Parejas_A_L[1]-.07),paste(Parejas_A_L[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,(Parejas_A_L[2]-.07),paste(Parejas_A_L[2]),cex=.9,col='white',f=2, srt=90)
text(3.08,(Parejas_A_L[3]-.07),paste(Parejas_A_L[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,(Parejas_A_L[4]-.07),paste(Parejas_A_L[4]),cex=.9,col='white',f=2, srt=90)
text(5.5,(Parejas_A_L[5]-.07),paste(Parejas_A_L[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,(Parejas_A_L[6]-.07),paste(Parejas_A_L[6]),cex=.9,col='white',f=2, srt=90)
text(7.9,(Parejas_A_L[7]-.07),paste(Parejas_A_L[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,(Parejas_A_L[8]-.07),paste(Parejas_A_L[8]),cex=.9,col='white',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="cadetblue2")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("A'",1,cex=1.3, line=3, f=2)

barplot(Parejas_B_L, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
text(0.72,0.5,paste(Parejas_B_L[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,0.5,paste(Parejas_B_L[2]),cex=.9,col='black',f=2, srt=90)
text(3.08,0.5,paste(Parejas_B_L[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,0.5,paste(Parejas_B_L[4]),cex=.9,col='black',f=2, srt=90)
text(5.5,0.5,paste(Parejas_B_L[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,0.5,paste(Parejas_B_L[6]),cex=.9,col='black',f=2, srt=90)
text(7.9,0.5,paste(Parejas_B_L[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,0.5,paste(Parejas_B_L[8]),cex=.9,col='black',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="darkseagreen4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="darkseagreen3")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("B''",1,cex=1.5, line=3, f=2)
mtext("Long is considered Signal",4,cex=1.3, line=1, f=2)
title(main=paste("All Subjects"), outer = TRUE, line = -2)

barplot(Parejas_A_C, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(0,1.05), axes = FALSE, col =c("cadetblue2", "deepskyblue4"))
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0", "0.25", "0.5", "0.75", "1"),las=1)
text(0.72,(Parejas_A_C[1]-.07),paste(Parejas_A_C[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,(Parejas_A_C[2]-.07),paste(Parejas_A_C[2]),cex=.9,col='white',f=2, srt=90)
text(3.08,(Parejas_A_C[3]-.07),paste(Parejas_A_C[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,(Parejas_A_C[4]-.07),paste(Parejas_A_C[4]),cex=.9,col='white',f=2, srt=90)
text(5.5,(Parejas_A_C[5]-.07),paste(Parejas_A_C[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,(Parejas_A_C[6]-.07),paste(Parejas_A_C[6]),cex=.9,col='white',f=2, srt=90)
text(7.9,(Parejas_A_C[7]-.07),paste(Parejas_A_C[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,(Parejas_A_C[8]-.07),paste(Parejas_A_C[8]),cex=.9,col='white',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="deepskyblue4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="cadetblue2")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("A'",1,cex=1.3, line=3, f=2)

barplot(Parejas_B_C, main = "", xlab = "", ylab = "", font.lab=2, ylim = c(-1,1.05), axes = FALSE, col =c("darkseagreen3", "darkseagreen4"))
axis(1,at=c(1.3,3.7,6.1,8.5),labels=c("1vs4", "2vs8", "3vs12", "5vs2"), font=2)
axis(2,at=c(0, -0.5, 0.5, -1, 1),labels=c("0", "-0.5", "0.5", "-1","1"),las=1)
text(0.72,0.5,paste(Parejas_B_C[1]),cex=.9,col='black',f=2, srt=90)
text(1.9,0.5,paste(Parejas_B_C[2]),cex=.9,col='black',f=2, srt=90)
text(3.08,0.5,paste(Parejas_B_C[3]),cex=.9,col='black',f=2, srt=90)
text(4.26,0.5,paste(Parejas_B_C[4]),cex=.9,col='black',f=2, srt=90)
text(5.5,0.5,paste(Parejas_B_C[5]),cex=.9,col='black',f=2, srt=90)
text(6.67,0.5,paste(Parejas_B_C[6]),cex=.9,col='black',f=2, srt=90)
text(7.9,0.5,paste(Parejas_B_C[7]),cex=.9,col='black',f=2, srt=90)
text(9.1,0.5,paste(Parejas_B_C[8]),cex=.9,col='black',f=2, srt=90)
lines(c(5.7, 7.3),c(1,1), lwd=3, lty=1, col="darkseagreen4")
lines(c(0.7, 2.3),c(1,1), lwd=3, lty=1, col="darkseagreen3")
text(2.5, 1, labels="Baseline", offset=0, cex = 0.9, pos=4)
text(7.5, 1, labels="Magnitude", offset=0, cex = 0.9, pos=4)
mtext("B''",1,cex=1.5, line=3, f=2)
mtext("Short is considered Signal",4,cex=1.3, line=1, f=2)
title(main=paste("All Subjects"), outer = TRUE, line = -2)