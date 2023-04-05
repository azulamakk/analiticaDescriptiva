#Modelo-Examen R.

#1) Operaciones de Vectores

#a) Dados los vectores:
a = c(1,3,5,2,5,7,3,5,7,8,6,4,4)
b = c(3,5,3,5,7,4,6,6)
#Explique que hace R al:
a*b

length(a)
length(b)
# Lo que hace R al multiplicar estos vectores es en primer lugar multiplicar un elemento
# del vector a con un elemento del vector b en la misma posicion. Asi sucesivamente hasta llegar
# a la posicion 8, donde termina el vector b
# De ahi en adelante, lo que R realiza es la multiplicacion del vector a por el b desde su 
# primera posicion, por ende sabemos que en a[9] el vector a se multiplicar por b[1], en la
# posicion a[10] por b[2] y asi sucesivamente hasta finalizar con el vector a.
# En otras palabras, cuando R alcanza el maximo del menor vector, sigue multiplicado al 
# vector de mayor longitus por el de menor, comenzando el menor vector las veces que sea suficiente hasta
# completar el total de posiciones del mayor vector.
# Al ser que la longitud del vector a no es divisible por el vector b, nos tira un mensaje de alerta.

#b) Obtenga los elementos pares del vector "a", usando una mascara booleana
# Procedemos a obtener la marcara booleana en funcion a si los elementos del vector a son pares o no
sonPar <- c()
for(i in 1:length(a)){
  if(a[i]%%2==0){
    elemento = TRUE
  }else{
    elemento = FALSE
  }
  sonPar <- append(sonPar, elemento)
}
sonPar 

# Creamos un nuevo vector con los datos TRUE (es decir, los pares)
pares <- c()
for(i in 1:length(sonPar)){
  if(sonPar[i]==TRUE){
    pares <- append(pares, a[i])
  }
}
pares

#2) Funciones y loops:Defina una funcion que cree el factorial de un numero entero. Si no se pasa ningun parametro, 
#debe usar el numero 10 como default
#Recuerde que el factorial de 5, se denota 5! y se calcula
1*2*3*4*5

factorialFunc <- function(x = 10){
  resultado = 1
  for(i in 1:x){
    resultado = resultado * i
  }
  return(resultado)
}
factorialFunc()

#3) Data Frames y dplyr

#   El dataset netflix.csv tiene la columnas 
#names=nombre de la pelicula
#release_year = anio de estreno
#maturity = clasificacion parental
#hours = horas de duracion
#minutes = horas de duracion
library(dplyr)
#a) Cargue en memoria el archivo "netflix.csv" y asignelo a una variable
netflix <- read.csv("~/Desktop/Analitica Descriptiva/Parciales/Parcial 1 Makk/netflix.csv")
View(netflix)

#b) Cree una columna que indique la duracion total en minutos (hours*60+minutes)
dfNetflix <- netflix %>% mutate(duracionTotal = (hours*60+minutes))
# Asignamos el data frame a una variable ya que lo usaremos en el siguiente ejercicio

#c) ?Cual es el a?o con mayor duracion promedio? 
# [obtener duracion total promedio con la columna anterior, agrupar por a?o, calcular la duracion promedio, y ordenar de mayor a menor]
#?cual es el a?o con mayor variabilidad? [sd(x) calcula el desvio estandar del vector x]

# Se nos pide primeramente calcular la duracion promedio de todas las peliculas
duracionPromedio <- mean(dfNetflix$duracionTotal)
duracionPromedio

# Seguimos por la agrupacion por anio, obtencion de duracion media y ordenamiento de mayor a menor en funcion a ello
anioYDuracionMedia <- dfNetflix %>% group_by(release_year) %>% summarise(duracionMedia = mean(duracionTotal)) %>% arrange(-duracionMedia)
#El anio con mayor duracion promedio es
anioYDuracionMedia[1,]
# 2004, con una duracion media de 154

# Realizamos el mismo proceso que anteriormente para el desvio
anioYDesvio <- dfNetflix %>% group_by(release_year) %>% summarise(desvio = sd(duracionTotal)) %>% arrange(-desvio)
#El anio con mayor desvio es 
anioYDesvio[1,]
# 2001, con un desvio de 68,1

#d) De las peliculas para mayores de 13 anios (maturity U/A 13+), el a?o de la duracion promedio mas
#alta, es igual a la poblacion general? [operacion anterior + filtro]
mayorA13 <- dfNetflix %>% filter(maturity_rating == 'U/A 13+') %>% group_by(release_year) %>% summarise(duracionMedia = mean(duracionTotal)) %>% arrange(-duracionMedia)
mayorA13[1,]

# La duracion promedio mas alta ocurre en el anio 2001, con una duracion promedio de 209 minutos
# En el ejercicio anterior pudimos observar que en la totalidad de la base (sin filtro), el anio
# con mayor duracion promedio fue 2004, con 154 minutos.
# Dado que 154 < 209, podemos decir que el maximo de duracion promedio de las peliculas para mayores
# a 13 anios es mayor que sin esta restriccion

#e) Realice una pregunta al dataset, escriba el c?digo que la responda 
# y explique como funciona

# ¿Cuales son las categorias (excluyendo a aquella sin restriccion) y los anios que han obtenido una menor duracion promedio

menorPromXCatYAnio <- dfNetflix %>% filter(maturity_rating != 'U') %>% group_by(release_year, maturity_rating) %>%
  summarise(promedioCat = mean(duracionTotal)) %>% arrange(promedioCat)
head(menorPromXCatYAnio)

# Para realizar esta funcion se obtiene el data frame con la duracionTotal calculada previamente.
# Antes de empezar a trabajar, filtramos a la categoria 'U', debido a que unicamente nos vamos a
# enfocar para este analisis en contenido con control parental. 
# Luego, agrupamos por anio de lanzamiento y su categoria simultaneamente.
# A partir de ese reagrupamiento, obtenemos el promedio por categoria y año y ordenamos 
# a partir de este numero de menor a mayor. Ordenarlo de esta manera nos permite obtener los 
# 6 primeros minimos gracias a la funcion head, lo que hace evidente que en su amplia mayoria 
# los minimos se presentan para aquellas con un rating de 'U/A 7+' (5 de 6 perteneciendo a tal rating)
