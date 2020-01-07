setwd("C:/Users/asus/Desktop/afchavez-019/SEP/Análisis/Descriptivos")
Datos <- read.csv("Ejemplo_BD_Aplicacion.csv")     #Archivo con las respuestas de los estudiantes

#Información sobre los aspirantes
Figura <- Datos$figura
Nivel <- Datos$nivel
Grupo <- Datos$grupo

#Datos de identificación de SEDE
Sede_Nombre <- Datos$sede
Sede_Dirección <- Datos$dir_sede
Sede_Grupo <- Datos$grupo
Sede_Municipio <- Datos$munic_sede

Sede_Serial <- NULL
for(z in 1:length(unique(Sede_Dirección))){
  Sede_Serial[z] <- z
  Dir[z] <- as.character(unique(Sede_Dirección)[z])
}
No_Sedes <- max(Sede_Serial)



#Identificando entidades
Serial_edo <- Datos$ent_sede
#######unique(Sede_Municipio[which(Serial_edo==01)])
Entidad <- NULL
Entidad[which(Serial_edo==01)] <- 'Aguascalientes' 
Entidad[which(Serial_edo==02)] <- 'Baja California'
Entidad[which(Serial_edo==03)] <- 'Baja California Sur'
Entidad[which(Serial_edo==04)] <- 'Campeche'
Entidad[which(Serial_edo==05)] <- 'Coahuila'
Entidad[which(Serial_edo==06)] <- 'Colima'
Entidad[which(Serial_edo==07)] <- 'Chiapas'
Entidad[which(Serial_edo==08)] <- 'Chihuahua'
Entidad[which(Serial_edo==09)] <- 'CDMX'
Entidad[which(Serial_edo==10)] <- 'Durango'
Entidad[which(Serial_edo==11)] <- 'Guanajuato'
Entidad[which(Serial_edo==12)] <- 'Guerrero'
Entidad[which(Serial_edo==13)] <- 'Hidalgo'
Entidad[which(Serial_edo==14)] <- 'Jalisco'
Entidad[which(Serial_edo==15)] <- 'Edo. México'
Entidad[which(Serial_edo==16)] <- 'Michoacán'
Entidad[which(Serial_edo==17)] <- 'Morelos'
Entidad[which(Serial_edo==18)] <- 'Nayarit'
Entidad[which(Serial_edo==19)] <- 'Monterrey'
Entidad[which(Serial_edo==32)] <- 'Zacatecas'
Entidad[which(Serial_edo==21)] <- 'Puebla'
Entidad[which(Serial_edo==22)] <- 'Queretaro'
Entidad[which(Serial_edo==23)] <- 'Quintana Roo'
Entidad[which(Serial_edo==24)] <- 'San Luis Potosi'
Entidad[which(Serial_edo==25)] <- 'Sinaloa'
Entidad[which(Serial_edo==26)] <- 'Sonora'
Entidad[which(Serial_edo==27)] <- 'Tabasco'
Entidad[which(Serial_edo==28)] <- 'Tamaulipas'
Entidad[which(Serial_edo==29)] <- 'Tlaxcala'
Entidad[which(Serial_edo==30)] <- 'Veracruz'
Entidad[which(Serial_edo==31)] <- 'Yucatan'



f <- 1
Sum_Estado <- NULL
Sum_EB <- rep("-", No_Sedes)
Sum_EMS <- rep("-", No_Sedes)
Sum_Contador <- NULL
Sum_Contador_EB <- NULL
Sum_Contador_EMS <- NULL
Num_Edo <- NULL
Sum_Fechas <- NULL
Sum_Grupos <- NULL
Dir <- NULL
Min_Ap <- NULL
Max_Ap <- NULL
Average_Ap <- NULL
Min_Ap_EB <- rep("-", No_Sedes)
Max_Ap_EB <- rep("-", No_Sedes)
Average_Ap_EB <- rep("-", No_Sedes)
Min_Ap_EMS <- rep("-", No_Sedes)
Max_Ap_EMS <- rep("-", No_Sedes)
Average_Ap_EMS <- rep("-", No_Sedes)
Sum_Doc <- rep("-", No_Sedes)
Sum_TecDoc <- rep("-", No_Sedes)
Sum_ATP <- rep("-", No_Sedes)
Sum_Sup <- rep("-", No_Sedes)
Sum_Dir <- rep("-", No_Sedes)
Sum_Doc_EB <- rep("-", No_Sedes)
Sum_TecDoc_EB <- rep("-", No_Sedes)
Sum_ATP_EB <- rep("-", No_Sedes)
Sum_Sup_EB <- rep("-", No_Sedes)
Sum_Dir_EB <- rep("-", No_Sedes)
Sum_Doc_EMS <- rep("-", No_Sedes)
Sum_TecDoc_EMS <- rep("-", No_Sedes)
Sum_ATP_EMS <- rep("-", No_Sedes)
Sum_Sup_EMS <- rep("-", No_Sedes)
Sum_Dir_EMS <- rep("-", No_Sedes)
Fecha_1 <- rep("-", No_Sedes)
Fecha_2 <- rep("-", No_Sedes)
Fecha_3 <- rep("-", No_Sedes)
Fecha_4 <- rep("-", No_Sedes)
Fecha_5 <- rep("-", No_Sedes)
Fecha_6 <- rep("-", No_Sedes)
Fecha_7 <- rep("-", No_Sedes)
Fecha_8 <- rep("-", No_Sedes)
Fecha_9 <- rep("-", No_Sedes)
Fecha_10 <- rep("-", No_Sedes)
Fecha_11 <- rep("-", No_Sedes)
Fecha_12 <- rep("-", No_Sedes)
C_Fecha_1 <- rep("-", No_Sedes)
C_Fecha_2 <- rep("-", No_Sedes)
C_Fecha_3 <- rep("-", No_Sedes)
C_Fecha_4 <- rep("-", No_Sedes)
C_Fecha_5 <- rep("-", No_Sedes)
C_Fecha_6 <- rep("-", No_Sedes)
C_Fecha_7 <- rep("-", No_Sedes)
C_Fecha_8 <- rep("-", No_Sedes)
C_Fecha_9 <- rep("-", No_Sedes)
C_Fecha_10 <- rep("-", No_Sedes)
C_Fecha_11 <- rep("-", No_Sedes)
C_Fecha_12 <- rep("-", No_Sedes)
EB_Fecha_1 <- rep("-", No_Sedes)
EB_Fecha_2 <- rep("-", No_Sedes)
EB_Fecha_3 <- rep("-", No_Sedes)
EB_Fecha_4 <- rep("-", No_Sedes)
EB_Fecha_5 <- rep("-", No_Sedes)
EB_Fecha_6 <- rep("-", No_Sedes)
EB_Fecha_7 <- rep("-", No_Sedes)
EB_Fecha_8 <- rep("-", No_Sedes)
EB_Fecha_9 <- rep("-", No_Sedes)
EB_Fecha_10 <- rep("-", No_Sedes)
EB_Fecha_11 <- rep("-", No_Sedes)
EB_Fecha_12 <- rep("-", No_Sedes)
EB_C_Fecha_1 <- rep("-", No_Sedes)
EB_C_Fecha_2 <- rep("-", No_Sedes)
EB_C_Fecha_3 <- rep("-", No_Sedes)
EB_C_Fecha_4 <- rep("-", No_Sedes)
EB_C_Fecha_5 <- rep("-", No_Sedes)
EB_C_Fecha_6 <- rep("-", No_Sedes)
EB_C_Fecha_7 <- rep("-", No_Sedes)
EB_C_Fecha_8 <- rep("-", No_Sedes)
EB_C_Fecha_9 <- rep("-", No_Sedes)
EB_C_Fecha_10 <- rep("-", No_Sedes)
EB_C_Fecha_11 <- rep("-", No_Sedes)
EB_C_Fecha_12 <- rep("-", No_Sedes)
EMS_Fecha_1 <- rep("-", No_Sedes)
EMS_Fecha_2 <- rep("-", No_Sedes)
EMS_Fecha_3 <- rep("-", No_Sedes)
EMS_Fecha_4 <- rep("-", No_Sedes)
EMS_Fecha_5 <- rep("-", No_Sedes)
EMS_Fecha_6 <- rep("-", No_Sedes)
EMS_Fecha_7 <- rep("-", No_Sedes)
EMS_Fecha_8 <- rep("-", No_Sedes)
EMS_Fecha_9 <- rep("-", No_Sedes)
EMS_Fecha_10 <- rep("-", No_Sedes)
EMS_Fecha_11 <- rep("-", No_Sedes)
EMS_Fecha_12 <- rep("-", No_Sedes)
EMS_C_Fecha_1 <- rep("-", No_Sedes)
EMS_C_Fecha_2 <- rep("-", No_Sedes)
EMS_C_Fecha_3 <- rep("-", No_Sedes)
EMS_C_Fecha_4 <- rep("-", No_Sedes)
EMS_C_Fecha_5 <- rep("-", No_Sedes)
EMS_C_Fecha_6 <- rep("-", No_Sedes)
EMS_C_Fecha_7 <- rep("-", No_Sedes)
EMS_C_Fecha_8 <- rep("-", No_Sedes)
EMS_C_Fecha_9 <- rep("-", No_Sedes)
EMS_C_Fecha_10 <- rep("-", No_Sedes)
EMS_C_Fecha_11 <- rep("-", No_Sedes)
EMS_C_Fecha_12 <- rep("-", No_Sedes)


for (a in 1:length(unique(Sede_Dirección))){
  f <- a
  Dir[a] <- as.character(unique(Sede_Dirección)[a])
  Sum_Contador[a] <- length(which(Sede_Dirección==Dir[a]))
  Sum_Contador_EB[a] <- length(which(Nivel[which(Sede_Dirección==Dir[a])]=="BASICA"))
  Sum_Contador_EMS[a] <- length(which(Nivel[which(Sede_Dirección==Dir[a])]=="MEDIA SUPERIOR"))
  Aplicantes <- NULL
  Aplicantes_EB <- NULL
  Aplicantes_EMS <- NULL
  Fecha <- NULL
    if((length(unique(Entidad[which(Sede_Dirección==Dir[a])]))==1)==TRUE){
        Num_Edo[a] <- "1"}
    else{
        Num_Edo[a] <- "Más de uno"}
        if(Num_Edo[a]=="Más de uno"){
            print("Más de un estado")
            print(Dir[a])
            print(unique(Sede_Municipio[Sede_Dirección==Dir[a]]))}
        else{
            print(f)}
  Sum_Estado[a] <- as.character(unique(Entidad[which(Sede_Dirección==Dir[a])]))  
    
  
  for(k in 1:length(unique(Figura[which(Sede_Dirección==Dir[a])]))){
    if(unique(Figura[which(Sede_Dirección==Dir[a])])[k]=="DIRECTIVO"){
      Sum_Dir[a] <- length(which(Figura[which(Sede_Dirección==Dir[a])]=="DIRECTIVO"))
      Sum_Dir_EB[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]=="DIRECTIVO"))
      Sum_Dir_EMS[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]=="DIRECTIVO"))
    }else{}
    if(unique(Figura[which(Sede_Dirección==Dir[a])])[k]=="SUPERVISOR"){
      Sum_Sup[a] <- length(which(Figura[which(Sede_Dirección==Dir[a])]=="SUPERVISOR"))
      Sum_Sup_EB[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]=="SUPERVISOR"))
      Sum_Sup_EMS[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]=="SUPERVISOR"))
    }else{}
    if(unique(Figura[which(Sede_Dirección==Dir[a])])[k]=="DOCENTE"){
      Sum_Doc[a] <- length(which(Figura[which(Sede_Dirección==Dir[a])]=="DOCENTE"))
      Sum_Doc_EB[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]=="DOCENTE"))
      Sum_Doc_EMS[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]=="DOCENTE"))
    }else{}
    if(unique(Figura[which(Sede_Dirección==Dir[a])])[k]=="TECNICO DOCENTE"){
      Sum_TecDoc[a] <- length(which(Figura[which(Sede_Dirección==Dir[a])]=="TECNICO DOCENTE"))
      Sum_TecDoc_EB[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]=="TECNICO DOCENTE"))
      Sum_TecDoc_EMS[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]=="TECNICO DOCENTE"))
    }else{}
    if(unique(Figura[which(Sede_Dirección==Dir[a])])[k]=="ASESOR TECNICO PEDAGOGICO"){
      Sum_ATP[a] <- length(which(Figura[which(Sede_Dirección==Dir[a])]=="ASESOR TECNICO PEDAGOGICO"))
      Sum_ATP_EB[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]=="ASESOR TECNICO PEDAGOGICO"))
      Sum_ATP_EMS[a] <- length(which(Figura[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]=="ASESOR TECNICO PEDAGOGICO"))
    }else{}
  }
  
  
    for(c in 1:length(unique(Nivel[which(Sede_Dirección==Dir[a])]))){
        if(unique(Nivel[which(Sede_Dirección==Dir[a])])[c]=="BASICA"){
              Sum_EB[a] <-'Sí' 
              }
          else{}
        if(unique(Nivel[which(Sede_Dirección==Dir[a])])[c]=="MEDIA SUPERIOR"){
              Sum_EMS[a] <-'Sí' 
              }
          else{}
      
      
      for(g in 1:length(unique(Sede_Grupo[which(Sede_Dirección==Dir[a])]))){
      Sum_Grupos[a]<- g
        }
      
        for(b in 1:length(unique(Datos$fecha_apl1[which(Sede_Dirección==Dir[a])]))){
          Fecha[b] <- as.character(unique(Datos$fecha_apl1[which(Sede_Dirección==Dir[a])])[b])
          Sum_Fechas[a] <- b 
          Aplicantes[b] <- length(which(Datos$fecha_apl1[which(Sede_Dirección==Dir[a])]==Fecha[b]))
          Aplicantes_EB[b] <- length(which(Datos$fecha_apl1[which(Sede_Dirección==Dir[a] & Nivel=="BASICA")]==Fecha[b]))
          Aplicantes_EMS[b] <- length(which(Datos$fecha_apl1[which(Sede_Dirección==Dir[a] & Nivel=="MEDIA SUPERIOR")]==Fecha[b]))
          
          if(b==1){
            Fecha_1[a] <- Fecha[b]
            C_Fecha_1[a] <- Aplicantes[b]
            EB_Fecha_1[a] <- Fecha[b]
            EB_C_Fecha_1[a] <- Aplicantes_EB[b]
            EMS_Fecha_1[a] <- Fecha[b]
            EMS_C_Fecha_1[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==2){
            Fecha_2[a] <- Fecha[b]
            C_Fecha_2[a] <- Aplicantes[b]
            EB_Fecha_2[a] <- Fecha[b]
            EB_C_Fecha_2[a] <- Aplicantes_EB[b]
            EMS_Fecha_2[a] <- Fecha[b]
            EMS_C_Fecha_2[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==3){
            Fecha_3[a] <- Fecha[b]
            C_Fecha_3[a] <- Aplicantes[b]
            EB_Fecha_3[a] <- Fecha[b]
            EB_C_Fecha_3[a] <- Aplicantes_EB[b]
            EMS_Fecha_3[a] <- Fecha[b]
            EMS_C_Fecha_3[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==4){
            Fecha_4[a] <- Fecha[b]
            C_Fecha_4[a] <- Aplicantes[b]
            EB_Fecha_4[a] <- Fecha[b]
            EB_C_Fecha_4[a] <- Aplicantes_EB[b]
            EMS_Fecha_4[a] <- Fecha[b]
            EMS_C_Fecha_4[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==5){
            Fecha_5[a] <- Fecha[b]
            C_Fecha_5[a] <- Aplicantes[b]
            EB_Fecha_5[a] <- Fecha[b]
            EB_C_Fecha_5[a] <- Aplicantes_EB[b]
            EMS_Fecha_5[a] <- Fecha[b]
            EMS_C_Fecha_5[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==6){
            Fecha_6[a] <- Fecha[b]
            C_Fecha_6[a] <- Aplicantes[b]
            EB_Fecha_6[a] <- Fecha[b]
            EB_C_Fecha_6[a] <- Aplicantes_EB[b]
            EMS_Fecha_6[a] <- Fecha[b]
            EMS_C_Fecha_6[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==7){
            Fecha_7[a] <- Fecha[b]
            C_Fecha_7[a] <- Aplicantes[b]
            EB_Fecha_7[a] <- Fecha[b]
            EB_C_Fecha_7[a] <- Aplicantes_EB[b]
            EMS_Fecha_7[a] <- Fecha[b]
            EMS_C_Fecha_7[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==8){
            Fecha_8[a] <- Fecha[b]
            C_Fecha_8[a] <- Aplicantes[b]
            EB_Fecha_8[a] <- Fecha[b]
            EB_C_Fecha_8[a] <- Aplicantes_EB[b]
            EMS_Fecha_8[a] <- Fecha[b]
            EMS_C_Fecha_8[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==9){
            Fecha_9[a] <- Fecha[b]
            C_Fecha_9[a] <- Aplicantes[b]
            EB_Fecha_9[a] <- Fecha[b]
            EB_C_Fecha_9[a] <- Aplicantes_EB[b]
            EMS_Fecha_9[a] <- Fecha[b]
            EMS_C_Fecha_9[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==10){
            Fecha_10[a] <- Fecha[b]
            C_Fecha_10[a] <- Aplicantes[b]
            EB_Fecha_10[a] <- Fecha[b]
            EB_C_Fecha_10[a] <- Aplicantes_EB[b]
            EMS_Fecha_10[a] <- Fecha[b]
            EMS_C_Fecha_10[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==11){
            Fecha_11[a] <- Fecha[b]
            C_Fecha_11[a] <- Aplicantes[b]
            EB_Fecha_11[a] <- Fecha[b]
            EB_C_Fecha_11[a] <- Aplicantes_EB[b]
            EMS_Fecha_11[a] <- Fecha[b]
            EMS_C_Fecha_11[a] <- Aplicantes_EMS[b]
          }else{}
          if(b==12){
            Fecha_12[a] <- Fecha[b]
            C_Fecha_12[a] <- Aplicantes[b]
            EB_Fecha_12[a] <- Fecha[b]
            EB_C_Fecha_12[a] <- Aplicantes_EB[b]
            EMS_Fecha_12[a] <- Fecha[b]
            EMS_C_Fecha_12[a] <- Aplicantes_EMS[b]
          }else{}
          }}
  Min_Ap[a] <- min(Aplicantes)
  Max_Ap[a] <- max(Aplicantes)
  Average_Ap[a] <- round(mean(Aplicantes),0)
  Min_Ap_EB[a] <- min(Aplicantes_EB)
  Max_Ap_EB[a] <- max(Aplicantes_EB)
  Average_Ap_EB[a] <- round(mean(Aplicantes_EB),0)
  Min_Ap_EMS[a] <- min(Aplicantes_EMS)
  Max_Ap_EMS[a] <- max(Aplicantes_EMS)
  Average_Ap_EMS[a] <- round(mean(Aplicantes_EMS),0)}


#Por estado
Est <- NULL
Grupo_xSede <- NULL
Grupo_xEstado <- NULL
for(e in 1:length(unique(Sum_Estado))){
  Est[e] <- as.character(as.character(unique(Sum_Estado)[e]))
  Grupo_xSede[e] <- sum(Sum_Grupos[which(Sum_Estado==Est[e])]) 
  Grupo_xEstado[e] <- length(unique(Grupo[which(Entidad==Est[e])]))
  print(Est[e])
  print(Grupo_xSede[e])
  print(Grupo_xEstado[e])
}

Sum_Edo <- matrix(, nrow= e, ncol=3)
Sum_Edo[,1] <- Est
Sum_Edo[,2] <- Grupo_xEstado
Sum_Edo[,3] <- Grupo_xSede
colnames(Sum_Edo) <- c("Estado", "Grupos por estado", "Suma grupos por sede") #Asignamos los Headers
write.csv(Sum_Edo,"Summary-Edo.csv",row.names = FALSE) #Creamos un csv con nuestra matriz




Summary <- matrix(, nrow = a, ncol = 105)  #Creamos una matriz vacía
Summary[,1] <- Sum_Estado
Summary[,2] <- Dir
Summary[,3] <- Sum_EB
Summary[,4] <- Sum_EMS
Summary[,5] <- Sum_Contador
Summary[,6] <- Sum_Contador_EB
Summary[,7] <- Sum_Contador_EMS
Summary[,8] <- Sum_Doc
Summary[,9] <- Sum_TecDoc
Summary[,10] <- Sum_ATP
Summary[,11] <- Sum_Dir
Summary[,12] <- Sum_Sup
Summary[,13] <- Sum_Fechas
Summary[,14] <- Min_Ap
Summary[,15] <- Max_Ap
Summary[,16] <- Average_Ap
Summary[,17] <- Sum_Doc_EB
Summary[,18] <- Sum_TecDoc_EB
Summary[,19] <- Sum_ATP_EB
Summary[,20] <- Sum_Dir_EB
Summary[,21] <- Sum_Sup_EB
Summary[,22] <- Sum_Doc_EMS
Summary[,23] <- Sum_TecDoc_EMS
Summary[,24] <- Sum_ATP_EMS
Summary[,25] <- Sum_Dir_EMS
Summary[,26] <- Sum_Sup_EMS
Summary[,27] <- Fecha_1
Summary[,28] <- C_Fecha_1
Summary[,29] <- Fecha_2
Summary[,30] <- C_Fecha_2
Summary[,31] <- Fecha_3
Summary[,32] <- C_Fecha_3
Summary[,33] <- Fecha_4
Summary[,34] <- C_Fecha_4
Summary[,35] <- Fecha_5
Summary[,36] <- C_Fecha_5
Summary[,37] <- Fecha_6
Summary[,38] <- C_Fecha_6
Summary[,39] <- Fecha_7
Summary[,40] <- C_Fecha_7
Summary[,41] <- Fecha_8
Summary[,42] <- C_Fecha_8
Summary[,43] <- Fecha_9
Summary[,44] <- C_Fecha_9
Summary[,45] <- Fecha_10
Summary[,46] <- C_Fecha_10
Summary[,47] <- Fecha_11
Summary[,48] <- C_Fecha_11
Summary[,49] <- Fecha_12
Summary[,50] <- C_Fecha_12
Summary[,51] <- EB_Fecha_1
Summary[,52] <- EB_C_Fecha_1
Summary[,53] <- EB_Fecha_2
Summary[,54] <- EB_C_Fecha_2
Summary[,55] <- EB_Fecha_3
Summary[,56] <- EB_C_Fecha_3
Summary[,57] <- EB_Fecha_4
Summary[,58] <- EB_C_Fecha_4
Summary[,59] <- EB_Fecha_5
Summary[,60] <- EB_C_Fecha_5
Summary[,61] <- EB_Fecha_6
Summary[,62] <- EB_C_Fecha_6
Summary[,63] <- EB_Fecha_7
Summary[,64] <- EB_C_Fecha_7
Summary[,65] <- EB_Fecha_8
Summary[,66] <- EB_C_Fecha_8
Summary[,67] <- EB_Fecha_9
Summary[,68] <- EB_C_Fecha_9
Summary[,69] <- EB_Fecha_10
Summary[,70] <- EB_C_Fecha_10
Summary[,71] <- EB_Fecha_11
Summary[,72] <- EB_C_Fecha_11
Summary[,73] <- EB_Fecha_12
Summary[,74] <- EB_C_Fecha_12
Summary[,75] <- EMS_Fecha_1
Summary[,76] <- EMS_C_Fecha_1
Summary[,77] <- EMS_Fecha_2
Summary[,78] <- EMS_C_Fecha_2
Summary[,79] <- EMS_Fecha_3
Summary[,80] <- EMS_C_Fecha_3
Summary[,81] <- EMS_Fecha_4
Summary[,82] <- EMS_C_Fecha_4
Summary[,83] <- EMS_Fecha_5
Summary[,84] <- EMS_C_Fecha_5
Summary[,85] <- EMS_Fecha_6
Summary[,86] <- EMS_C_Fecha_6
Summary[,87] <- EMS_Fecha_7
Summary[,88] <- EMS_C_Fecha_7
Summary[,89] <- EMS_Fecha_8
Summary[,90] <- EMS_C_Fecha_8
Summary[,91] <- EMS_Fecha_9
Summary[,92] <- EMS_C_Fecha_9
Summary[,93] <- EMS_Fecha_10
Summary[,94] <- EMS_C_Fecha_10
Summary[,95] <- EMS_Fecha_11
Summary[,96] <- EMS_C_Fecha_11
Summary[,97] <- EMS_Fecha_12
Summary[,98] <- EMS_C_Fecha_12
Summary[,99] <- Min_Ap_EB
Summary[,100] <- Max_Ap_EB
Summary[,101] <- Average_Ap_EB
Summary[,102] <- Min_Ap_EMS
Summary[,103] <- Max_Ap_EMS
Summary[,104] <- Average_Ap_EMS
Summary[,105] <- Sum_Grupos

colnames(Summary) <- c("Estado", "Sede - Dirección", "EB", "EMS", "Total Adscritos", "Cantidad EB", "Cantidad EMS", "No. Docentes", "No. TecDoc", "No. ATP", "No. Dir", "No. Sup ", "Fechas de aplicación", "Mínimo", "Máximo", "Promedio", "No. Docentes EB", "No. TecDoc EB", "No. ATP EB", "No. Dir EB", "No. Sup EBS", "No. Docentes EMS", "No. TecDoc EMS", "No. ATP EMS", "No. Dir EMS", "No. Sup EMS", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "Fecha", "Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EB Fecha", "EB Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "EMS Fecha", "EMS Contador", "Min EB", "Max EB", "Mean EB", "Min EMS", "Max EMS", "Mean EMS", "No. Grupos") #Asignamos los Headers
write.csv(Summary,"Summary.csv",row.names = FALSE) #Creamos un csv con nuestra matriz
