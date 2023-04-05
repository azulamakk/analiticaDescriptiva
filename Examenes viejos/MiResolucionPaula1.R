#Primer Examen segundo semestre 2022
setwd("C:/Users/paula/Desktop/R FILES")
library(tidyverse)
dt = read_csv('starbucks.csv') 
View(dt)

##1) VECTORES Y ARRAYS
#a) Indique las dimensiones de la base de datos

#b) Realice un filtro del vector correspondiente a la columna "Cholesterol_mg" 
# usando una mascara booleana que indique si el valor del vector es mayor o menor al promedio


#c) Seleccione las columnas par (la segunda, la cuarta,...)

#2) DPLYR 
#a) ?Cual es la 'Beverage_prep' que tiene mayor proporci?n de bebidas con sodio?

#b) ?Cuantas, de las bebidas con leche de soja, no tienen cafeina? ?cuantas tienen hasta 100? ?cuantas m?s de 100?

#3) GGPLOT
#a) Realice un an?lisis gr?fico de la distribuci?n de Calorias por
#Beverage_category. Realice un breve an?lisis respecto a sus descubrimientos.


#b) Realice un an?lisis gr?fico de la relaci?n entre las calorias y 
# las grasas totales. Seleccione algun "Beverage" y comparelo con el total
#(o con el resto).Realice un breve an?lisis respecto a sus descubrimientos.


#4) FUNCIONES
#En matem?ticas, los n?meros coprimos (n?meros primos entre s? o primos relativos) son dos n?meros enteros 
#a y b que no tienen ning?n factor primo en com?n:

#Por ejemplo, 6 y 19 son coprimos, pero 6 y 27 no lo son porque ambos son divisibles por 3
#a) Genere una funci?n que determine si 2 numeros son co-primos.

#coprimos = son aquellos pares de números que no tienen divisores en común, salvo el 1

##5) CONTROL DE FLUJO

#Ejercicio: Encuentre los primeros 10 numeros co-primos de los primeros 10 numeros de la serie de fibonacci
#a) Realice el ejercicio con un "for" loop
#b) Realice el ejercicio con un "while" loop
# Define a function to check if two numbers are coprime
is_coprime <- function(a, b) {
  gcd <- function(x, y) {
    if (y == 0) {
      return(x)
    } else {
      return(gcd(y, x %% y))
    }
  }
  return(gcd(a, b) == 1)
}

# Generate the first 13 Fibonacci numbers
fibonacci <- numeric(13)
fibonacci[1] <- 1
fibonacci[2] <- 1
for (i in 3:13) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
}

# Find the first 10 coprimes of the Fibonacci numbers
coprimes <- numeric(10)
count <- 0
for (i in 1:13) {
  for (j in (i+1):13) {
    if (is_coprime(fibonacci[i], fibonacci[j])) {
      count <- count + 1
      coprimes[count] <- fibonacci[i]
      if (count == 10) {
        break
      }
    }
  }
  if (count == 10) {
    break
  }
}

# Print the results
print(coprimes)


# Find the first 10 coprimes of each Fibonacci number
for (i in 1:13) {
  coprimes <- numeric(10)
  count <- 0
  for (j in 1:13) {
    if (i != j && is_coprime(fibonacci[i], fibonacci[j])) {
      count <- count + 1
      coprimes[count] <- fibonacci[j]
      if (count == 10) {
        break
      }
    }
  }
  # Print the coprimes of each Fibonacci number
  cat("The first 10 coprimes of Fibonacci", i, "are:", coprimes, "\n")
}

# Define a function to check if two numbers are coprime
is_coprime <- function(a, b) {
  gcd <- function(x, y) {
    if (y == 0) {
      return(x)
    } else {
      return(gcd(y, x %% y))
    }
  }
  return(gcd(a, b) == 1)
}

# Generate the first 13 Fibonacci numbers
fibonacci <- numeric(13)
fibonacci[1] <- 1
fibonacci[2] <- 1
for (i in 3:13) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
}

# Find the first 10 coprimes of each Fibonacci number
i <- 1
while (i <= 13) {
  coprimes <- numeric(10)
  count <- 0
  j <- 1
  while (j <= 13 && count < 10) {
    if (i != j && is_coprime(fibonacci[i], fibonacci[j])) {
      count <- count + 1
      coprimes[count] <- fibonacci[j]
    }
    j <- j + 1
  }
  # Print the coprimes of each Fibonacci number
  cat("The first 10 coprimes of Fibonacci", i, "are:", coprimes, "\n")
  i <- i + 1
}
