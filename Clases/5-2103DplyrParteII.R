#Ejercicios

View(iris)
# 1) Selecciona la mitad de las columnas impares de iris 
nuevoSet1 <- data.frame()
nuevoSet <- data.frame(iris$Sepal.Length,iris$Petal.Length,iris$Species)

# 2) Selecciona las primeras 5 filas y las ultimas  5 filas de iris.
head(iris)
tail(iris)

# 2) Selecciona las primeras 5 filas y las ultimas  3 columnas de iris.
head(iris$Petal.Length,iris$Petal.Width,iris$Species)
punto2 <- data.frame(head(iris$Petal.Length), head(iris$Petal.Width), head(iris$Species))

# 3) Selecciona la mitad de las filas de iris de forma alternada empezando por la primera
filasImpares <- c()
n <- nrow(iris)
for(i in 1:n){
  if(i/2 != 0){
    filasImpares = append(filasImpares, iris[i])
  }
}
filasImpares

filasImpares2 <- c()
filasImpares2 <- iris[seq(1, 150, 2),]
filasImpares2

# 4) Utilice dos alternativas para realizar un muestreo aleatorio de tamnio 20 de las filas de iris
muestreo1 <- iris[sample(nrow(iris),20,replace=FALSE, prob = NULL),]
muestreo1

indiceMuestreo <- c()
indiceMuestreo <- floor(runif(20, 1, 150))
muestreo2 <- iris[indiceMuestreo,] 

# 5) realice una copia de iris con el muestreo aleatorio para cada especie de 5 filas para cada una 
#    de las espacies de las  columnas "Petal.Length" y "Species"
iris[c(runif(5,1,50), runif(5,1,100), runif(5,1,150)), c('Petal.Length', 'Species')]

titanic <- read.csv("~/Desktop/Analitica Descriptiva/Clases/titanic.csv")

