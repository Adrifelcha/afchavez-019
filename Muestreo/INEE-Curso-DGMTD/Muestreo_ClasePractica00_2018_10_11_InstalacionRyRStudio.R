#################################################################
#################################################################
#################################################################
###
### Curso de Muestreo - INEE
###
### Clase Practica 00 (Instalacion de R y R-Studio)
###
### Emilio Lopez Escobar (http://www.info-Emilio.NET)
### emilio@numerika.mx
### Ciudad de México. Octubre 11, 2018
###
#################################################################
#################################################################
#################################################################



#Instalacion de R en el equipo.
#################################################################
#Este se encuentra más rápidamente en el siguiente servidor mexicano:
#http://cran.itam.mx



#(Si tienen curiosidad esta es la página principal de R: http://www.r-project.org)



#Hay que seleccionar la versión de R según el sistema que se esté utilizando.



#Bajar, ejecutar la instalación y seguir las instrucciones del instalador de Windows.



#Importante: Cuando pregunte el instalador el tipo de formato que se desea para la ayuda hay que elegir html (o html2). Es más fácil navegar por la ayuda con el navegador.



#Posteriormente, si se desea, instalar R-Studio (opcional, sugerido). Está aquí:
#http://rstudio.org



#Importante: Para la instalación de R-Studio, tiene que haberse instalado antes R.
#R-Studio es una "mascara" de R que lo hace más amigable.
#Propiamente, no es necesario para ejecutar R, es opcional.



#Una vez instalado R (y en su caso R-Studio), hay que ejecutar R (o R-Studio si se instaló, directamente sin ejecutar antes a R).
#Dentro de R (o R-Studio), en la línea de comandos, hay que aprender 2 comandos básicos que necesitaremos para saber donde estamos trabajando.



#El primer comando indica el directorio de trabajo actual:
getwd()



#Y otro que me permite manualmente determinar el directorio que yo quiero utilizar para trabajar. Por ejemplo, si quiero trabajar en una carpeta llamada R, en el disco F.
#(Ojo, la carpeta que se indica debe de existir.
#Notar que las diagonales que se utilizan son diagonales NO INVERSAS, de división. Así se indican las carpetas en R bajo Windows. También, no olvidar las comillas al inicio y al final.):
setwd("C:/Users/Maley/Dropbox/2017_10octubre_Emilio/CURSOS/INEGI/08_R")



#Otra forma es hacer esto con el mouse...



#Una vez determinado el directorio de trabajo hay que colocar allí los archivos de datos que se van a leer.



#También, es en esa carpeta donde se guardaran las cosas que guarden.
