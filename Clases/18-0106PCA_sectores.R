## ejemplos de PCA

rm(list=ls())

library(readxl)
EMAE <- read_excel("C:/Users/HP/Desktop/Carpetas/ITBA/clase/Primer Q 2021/16- Reduccion de dimensionalidad/Clase/actividad.xlsx")

library("corrplot")

M <- cor(EMAE[,], method = "pearson")  #permite ejecutar una matriz de correlación
corrplot(M, method = "ellipse")

# A continuación utilizamos la función prcomp() para obtener
# las componentes principales:
options(scipen=999)

pcaemae <- prcomp(EMAE,scale=T) 
# Scale = False -> utilizamos la matriz de COVARIANZA para obtener 
# las componentes! Veamos que ocurre:

plot(pcaemae)


#Veamos que ocurre:
summary(pcaemae)

# Desviación estándar: estos son los valores propios 
# en nuestro caso, ya que los datos se han centrado y 
# escalado (estandarizado)

#pca.Europa2$sdev # Varianza de cada componente.

#Proporción de varianza: es la cantidad de varianza que el
#componente representa en los datos, es decir. 
#¡PC1 representa> 46% de la varianza total 

#Proporción acumulada:es la cantidad acumulada de varianza
#explicada. Si usáramos los primeros 3 componentes, 
#tendríamos mas de 78% de la varianza total en los datos.

pcaemae$rotation

Rotacion<-pcaemae$rotation # cargas de cada componente
#(la contrib de cada varaible al componente)
CargaPC1<-Rotacion[,1]# como se compone el primer componente principal
#(la contrib de cada varaible al primer componente principal)
CargaPC1

#como calculo la primer componente principal?


PCATotal<-pcaemae$x
dim(PCATotal)


IndiceEMAE <- read_excel("C:/Users/HP/Desktop/Carpetas/ITBA/clase/Primer Q 2021/16- Reduccion de dimensionalidad/Clase/EMAE.xlsx")

# de la funcion pca.Europa2$x (primer columna de PCATotal en este caso)
PC1<-PCATotal[,1]
plot(PC1) # es solo un set de datos para cada registro


#datos_recuperados[, 1] <- PC1 + mean(datos$X1)
plot(EMAE$f)

plot( c(-1,170), c(-15,15), type = "n", xlab = "Tiempos",
      ylab="Efecto", main = "Líneas temporales" )

# primer Componente ppal
lines( PC1,
       lwd = 0.7,
       lty = 1,
       col = "black",
       pch = 1 )

# F - Construcción
lines( EMAE$f, 
       lwd = 1.5,
       lty = 2,
       col = "darkorange1" )

# N - Servicios Sociales y de Salud
lines( EMAE$n,
       lwd = 1.3,
       lty = 3,
       col = "green4")






##https://rpubs.com/elvirafj/tabulaRgrafLineas












# multiplicando la matriz de rotación por el dataset

CargaPC1vector<-matrix(CargaPC1,nrow = 7,ncol = 1) # convertimos en un vector

library(scales)
base.escalada<- scale(Europa2,center=T,scale=T)
PC1calculado<-as.matrix(base.escalada)%*%CargaPC1vector

Compara<-cbind(PC1calculado,PC1)
View(Compara)
plot(Compara)


### como elejir la cantidad de componentes ppales?


#Como estandarizamos nuestros datos y ahora tenemos los 
#valores propios correspondientes de cada PC, en realidad 
#podemos usarlos para trazar un límite para nosotros. 
#Dado que un valor propio <1 significaría que el PC 
#en realidad explica menos de una variable explicativa, 
#nos gustaría descartarlos. 
#Si nuestros datos son adecuados para PCA, deberíamos poder descartar 
#los CP mientras conservamos al menos el 70% de la varianza acumulada.

screeplot(pca.Europa2, type = "l", npcs = 5, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(pca.Europa2$sdev^2 / sum(pca.Europa2$sdev^2))
plot(cumpro[0:7], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 3, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)

#Notamos que los primeros 3 componentes tienen un valor propio> 1 y 
#explican casi el 80% de la varianza,
#se redujo la dimensionalidad de 7 a 3 perdiendo el 20% de info

### recurde que con los primeros dos compomentes PPales tenemos el 63%

plot(pca.Europa2$x[,1],pca.Europa2$x[,2], xlab="PC1 (46.1%)", ylab = "PC2 (16.9%)", main = "PC1 / PC2 - plot")



### clasificacion
euroclust<-hclust(dist(Europa[-1]))
plot(euroclust, labels=Europa$Country)

# compute divisive hierarchical clustering
library("cluster")
library("ggplot2")
library("factoextra")

hc4 <- diana(Europa2)

# Divise coefficient
hc4$dc

pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

clust <- cutree(hc4, k = 2)
fviz_cluster(list(data = Europa2, cluster = clust)) 



### cluster PCA
as.factor(clust)
fviz_pca_ind(pca.Europa2, geom.ind = "point", pointshape = 21, 
               pointsize = 2,fill.ind = clust) 



# Analisis Factorial

library("scales")
Europa3= scale(Europa2)

pca.Europa2 <- prcomp(Europa2,scale=T)
summary(pca.Europa2)

pca.Europa3 <- prcomp(Europa3,scale=F)
summary(pca.Europa3)

cor.E3=round(cor(Europa3),3)
cor.E3

det(cor.E3)

# cercano a 0, indica alta multicolinealidad entre las variables. 
# igual a 0 (matriz no singular). 


# Supuesto de multicolinealidad test de esfericidad de Bartlett busca contrastar 
# la hipótesis nula de que la matriz de correlaciones es igual a una matriz 
# de identidad

library(psych)
cortest.bartlett(cor.E3,n=nrow(Europa3))

# el resultado indica que no puedo aceptar la hipotesis nula que las varialbes no estan correlacionadas

KMO(cor.E3)


# 0,90 > KMO Muy bueno 
# 0,90 > KMO > 0,80 Bueno 
# 0,80 > KMO > 0,70 Aceptable 
# 0,70 > KMO > 0,60 Mediocre o regular 
# 0,60 > KMO > 0,50 Malo 
# 0,50 > KMO Inaceptable

aucor=eigen(cor.E3)
aucor
