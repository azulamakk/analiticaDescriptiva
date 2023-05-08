#De la base de datos "data.csv"
library(tidyverse)
library(funModeling)
#1) Seleccione de 2 ó 3 variables y realice una breve descripción estadistica que contenga:
#Medidas de tendencia central, variacion o frecuencia.
  #Algún interrogante que pueda ser contestado con esos datos que vincule:
    #a) Dos variables numericas
    #b) Una categorica y una numerica:
        #b.1) Discretizando la numerica
        #c.1) Analizando las distribuciones de la numerica por categoría
    #c) Dos categoricas
    #d) Al menos 2 Gráficos relevantes realizados con ggplot2 (el resto puede ser realizado con las herramientas de R-base)

#2) 
  #a) Detectar missing de la columna "Explicit" 
  #b) Outliers en la columna "duration".
  #b) Missings y Outliers en la columna "year".
#Recuerde:
# Verficar si los outliers refieren a algun dato real o es un faltante
# Verificar si los faltantes son aleatorios o sistematicos.
# Imputar los faltantes con linear regression ó logistical regression según corresponda.

#3) En la base de datos "znorte_properati.csv"
#a) Repita el ejercicio 1) sobre esta base.
#b) Estudie los missings y outliers de la variable "price_aprox_local_currency"
#tip: tenga en cuenta su "conocimiento contextual" (¿tipo de cambio implicito?)

#4) Procesamiento de texto
#a) Para la base "spotify.csv":
#Vea que la variable "genre" puede tener distintas convinaciones de generos de música,
#por ejemplo: "pop, rock", "pop, country" o "rock, metal"
#Cree 2 columnas adicionales llamadas:
  #"rock" que tome 1 si la cancion es del genero rock, 0 si no.
  #"pop"  que tome 1 si la cancion es del genero pop, 0 si no.
#Es decir, extraiga los sub-generos pop y rock de la columna "genre"

#b) Para la base "znorte_properati.csv":
#Toma la columna "place_with_parent_names" y separarla en las variables :
  #a.1) "Pais", "Provincia", "Region"


