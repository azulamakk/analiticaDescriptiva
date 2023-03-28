#Modelo-Examen R.
library(dplyr)
#1) Operaciones de Vectores

#a) Dividir el vector correspondiente a la columna Sepal.Length por Sepal.Width y asignarlo a una variable

n <- nrow(iris)
division <- c()
for(i in 1:n){
  division <- iris$Sepal.Length /iris$Sepal.Width
}
division

#b) Obtener la secuencia de numeros pares de 10mil a 100mil y asignarlo a una variable
pares <- seq(10000, 100000, 2)
pares

#c) Realice una mascara booleana para filtrar el vector
iris %>% mutate(esViginica = Species == 'virginica')

#tal que queden solo la especie "virginica" 

#2) Funciones
#a) Crear una funcion que tome como parametro un numero e indique si es divisible por 5.
divisibleX5 <- function(numero){
  resultado = TRUE
  if(floor(numero/5)!= numero/5){
    resultado = FALSE
  }
  return(resultado)
}
divisibleX5(7)

#c) Creer una funcion que tome como parametro un vector e indique cuales son los valores mayor al promedio y cuales menores

#el output debe ser un vector de la misma longitud que el original y que cada elemento 
#sea "TRUE" si es mayor al promedio y "FALSE" si es menor al promedio
menorAPromedio <- function(vectorDado){
  promedio <- mean(vectorDado)
  n <- length(vectorDado)
  vectorCumple <- c()
  for(i in 1:n){
    cumple = FALSE
    if(vectorDado[i] > promedio){
      cumple = TRUE
    }
  vectorCumple = append(vectorCumple, cumple)
  }
  return(vectorCumple)
}
vectordado <- c(1,10,1,10,20)
menorAPromedio(vectordado)

#3) Data Frames y dplyr

#   El dataset datasets::ToothGrowth tiene los resultados de un experimiento que busca 
#   evaluar que ocurre con los dientes de conejillos de indias al agregar dos tipos de vitaminas a su dieta 
#Columnas
#[,1]	len	numeric: longitud de los dientes
#[,2]	supp	factor: Que tipo vitamina se agrego (VC or OJ) 
#[,3]	dose	numeric	: Dosis, Cuanta vitamina se agrego en milligramos/dia
#Responder:

View(datasets::ToothGrowth)
#a) ?Cual es la longitud promedio de los dientes por cada tipo de vitamina?
datasets::ToothGrowth
ToothGrowth %>% group_by(supp) %>% summarise(mean(len))

#b) Obtener la fila correspondiente al conejillo de indias con el diente mas largo.
n <- nrow(ToothGrowth)
for(i in 1:n){
  maximo <- max(len)
  if(ToothGrowth$len[i] == maximo){
    superador = ToothGrowth[i,]
  }
}
superador

#c) Ordernar el dataset de menor a mayor de acuerdo a la longitud de los dientes.
ToothGrowth %>% arrange(len)
# De mayor a menor 
ToothGrowth %>% arrange(-len)

#d) Usar la funcion creada en el punto 2.c para crear una oolulmna nueva que diga si
# la longitud de los dientes es menor o es mayor al promedio
ToothGrowth %>% mutate(menorAMedia = menorAPromedio(len))

# 1) Filtrar por aquellos menores al promedio, ordenarlos de mayor a menor en funcion 
# de len, y extraer la dosis promedio por suplemento del dataframe resultante
ToothGrowth %>% filter(menorAPromedio(len) == TRUE) %>% arrange(len)
dosisPromedio <- ToothGrowth %>% filter(menorAPromedio(len) == TRUE) %>% arrange(-len) %>% group_by(supp) %>% summarise(len_promedio=mean(dose))
dosisPromedio
dosisPromedio$mean(len)
colnames(dosisPromedio) 
#Usando el for-loop y estructuras condicionales (if)
#1) ?Es 77466871 un numero primo? Si la respuesta es NO, ?cuales son sus divisores?
divisores = c()
for(i in 2:77466890){
  if(77466871/i == floor(77466871/i)){
    divisores = append(divisores, i)
  }
}
if(length(divisores)>0){
  respuesta = 'No es Primo'
  respuesta
  divisores
}else{respuesta = 'Es Primo'}

primo = function(num){
  
  nums = 2:(num-1)
  
  print(nums)
  
  a = num%%nums==0
  
  return(sum(a))
  
}