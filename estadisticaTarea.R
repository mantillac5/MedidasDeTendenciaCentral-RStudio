library(readr)
library(tidyverse)
library(datasets)
library(modeest)
choose.files()

#*************************POBLACION*********************

ruta_csv <-"C:\\Users\\PONI\\Desktop\\ProbabilidadYEstadistica\\empleoyocupacion.csv"
tasaDesocupacion <- read.csv(ruta_csv)
head(tasaDesocupacion)


#********************MUESTRA**************************

#aqui opmito la opcion de realizar la "muestra" de datos
#ya que considero que son muy pocos datos los que tengo

#ordeno los datos de menor a mayo y lo guardo en una variable 
tasaOrdenado <- tasaDesocupacion %>% arrange(Indicador)%>% head(199)


#saco la frecuencia absoluta
DatosyFrec <- as.data.frame(table(tasaOrdenado$Indicador))
Posicion <- DatosyFrec
#****************FRECUENCIA SIN AGRUPAR*************************

#Saco las demas frecuencias
Frecuencias_Sin_Agr <- transform(DatosyFrec,
                   FreqAC=cumsum(DatosyFrec$Freq),#tabla acumulada
                   Rel=round(prop.table(DatosyFrec$Freq),3),#tabla relativa
                   RelAC=round(cumsum(prop.table(DatosyFrec$Freq)),3))#Frecuencia relativa   

#******************TENDENCIA CENTRAL************************************

#lo que hacemos es convertir la variable DatosyFrec en numerico
str(DatosyFrec)
db <- DatosyFrec
db <- gsub(",",".", db$Var1 , fixed = TRUE)
Var1Num= as.numeric(db)

#Creo una nueva variable llamada DatxFrec(Datos * Frecuencia)
DatosyFrec$DatxFrec=Var1Num * DatosyFrec$Freq

DatosyFrec$Media <- mean(DatosyFrec$DatxFrec)#media
DatosyFrec$Mediana <- median(DatosyFrec$DatxFrec)#mediana
#mlv(DatosyFrec$DatxFrec,method = "discrete")#Moda
#modes(DatosyFrec$DatxFrec)
TendenciaCentral <- DatosyFrec

#***********************Posicion************************

Posicion$FrecRelAcum <- cumsum(prop.table(Posicion$Freq)) #frecuencia relativa acumulada
  

