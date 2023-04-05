#Primer Examen segundo semestre 2022
setwd("C:/Users/paula/Desktop/R FILES")
library(tidyverse)
dt = read_csv('starbucks.csv') 
View(dt)

##1) VECTORES Y ARRAYS
#a) Indique las dimensiones de la base de datos
dim(dt)
#b) Realice un filtro del vector correspondiente a la columna "Cholesterol_mg" 
# usando una mascara booleana que indique si el valor del vector es mayor o menor al promedio
a <- dt$Cholesterol_mg
b <- dt$Cholesterol_mg < mean(dt$Cholesterol_mg)

#c) Seleccione las columnas par (la segunda, la cuarta,...)

col <- dim(dt)[2] #me tira la cantidad de columnas
col_pares <- seq(2,col, by = 2) #me genera un vector desde 2 hasta 18, yendo de 2 en 2.

solo_columnas_pares <- dt[,c(col_pares)] #me muestra todas las filas de las columnas pares
solo_columnas_pares

#2) DPLYR 
#a) ?Cual es la 'Beverage_prep' que tiene mayor proporci?n de bebidas con sodio?
head(dt %>% filter(Sodium_mg > 0) %>% 
  group_by(Beverage_prep) %>% 
  summarise(Proporcion=n()), 1)

#b) ?Cuantas, de las bebidas con leche de soja, no tienen cafeina? ?cuantas tienen hasta 100? ?cuantas m?s de 100?
dt2 <- dt %>% filter(Beverage_prep == "Soymilk") #primero filtro las leches de soja y los resultados los almaceno en un nuevo dataframe ->"dt2"
dt2 %>% mutate(
  cuanta_cafeina = case_when(
    dt2$Caffeine_mg == 0 ~ "no tienen",
    dt2$Caffeine_mg <= 100 ~ "menor o igual a 100",
    dt2$Caffeine_mg > 100 ~ "mas de 100"
  ) #hasta aca agregue una columna nueva a dt2 con las condiciones del case_when
) %>% group_by(cuanta_cafeina) %>% summarise(count=n()) #agrupo por la nueva columna y cuento cuantos casos hay para cada grupo

#3) GGPLOT
#a) Realice un an?lisis gr?fico de la distribuci?n de Calorias por
#Beverage_category. Realice un breve an?lisis respecto a sus descubrimientos.
dt %>% ggplot(aes(Calories, fill = Beverage_category)) + geom_bar()

#b) Realice un an?lisis gr?fico de la relaci?n entre las calorias y 
# las grasas totales. Seleccione algun "Beverage" y comparelo con el total
#(o con el resto).Realice un breve an?lisis respecto a sus descubrimientos.


#4) FUNCIONES
#En matem?ticas, los n?meros coprimos (n?meros primos entre s? o primos relativos) son dos n?meros enteros 
#a y b que no tienen ning?n factor primo en com?n:

#Por ejemplo, 6 y 19 son coprimos, pero 6 y 27 no lo son porque ambos son divisibles por 3
#a) Genere una funci?n que determine si 2 numeros son co-primos.

#coprimos = son aquellos pares de números que no tienen divisores en común, salvo el 1

calculador_de_coprimos <- function(a,b){
  divisores_a <- c()
  for (i in 2:a){
    if (a%%i == 0){
      divisores_a <- append(divisores_a, i)
    }
  } #genero un vector con los divisores del numero a
  divisores_b <- c()
  for (i in 2:b){
    if (b%%i == 0){
      divisores_b <- append(divisores_b, i)
    }
  } #genero un vector con los divisores del numero b
  x <- divisores_a %in% divisores_b #me fijo cuales divisores de a estan en el vector de los divisores de b, y los valores booleanos los almaceno en x
  y <- divisores_b %in% divisores_a #me fijo cuales divisores de b estan en el vector de los divisores de a y los valores booleanos los almaceno en y
  
  es_coprimo = "son co-primos"
  for (i in x){
    if (i == TRUE){
      es_coprimo = "no son co-primos"
    }
  } #me fijo en x si hay algun valor TRUE, si es así entonces los numeros no son co-primos
  for (i in y){
    if (i == TRUE){
      es_coprimo = "no son co-primos"
    }
  }#me fijo en y si hay algun valor TRUE, si es así entonces los numeros no son co-primos
  return(es_coprimo)
}

calculador_de_coprimos(6,27)


##5) CONTROL DE FLUJO

#Ejercicio: Encuentre los primeros 10 numeros co-primos de los primeros 10 numeros de la serie de fibonacci
#a) Realice el ejercicio con un "for" loop
#b) Realice el ejercicio con un "while" loop

#voy a estar usando los primeros 10 numeros de fibonacci despues del 0 (porque no se puede dividir).
numeros_fibo = c(0, 1) 
i = 1

while(length(numeros_fibo)<10){
  if(length(numeros_fibo)<=1){
    numeros_fibo=append(numeros_fibo,1)    
  }
  else{
    next_fibo = numeros_fibo[i-1]+numeros_fibo[i-2]
    numeros_fibo=append(numeros_fibo,next_fibo)
  }
  i = i +1
}
numeros_fibo

#la idea es crear una lista de longitud 10 en donde cada elemento de la lista es un vector con los 10 primeros coprimos que le siguen

lista_vacia <- vector("list", length = 10) 
lista_vacia
coprimos <- c()


for (i in 1:length(lista_vacia)){
  while (length(coprimos) < 10){
    d <- i + 1
    if(calculador_de_coprimos(i,d) == "son co-primos"){
      coprimos <- append(coprimos, d)
    }
  }
  lista_vacia[[i]] <- c(coprimos)
}

lista_vacia
