rm(list=ls())
library("corrplot")

library(readxl)
precios <- read_excel("C:/Users/HP/Desktop/Carpetas/ITBA/clase/Primer Q 2021/16- Reduccion de dimensionalidad/Clase/precios.xlsx")


dim(precios)
cor(precios)

M <- cor(precios, method = "pearson")  #permite ejecutar una matriz de correlación
corrplot(M, method = "ellipse")

# A continuación utilizamos la función prcomp() para obtener
# las componentes principales:
options(scipen=999)


pca.precio <- prcomp(precios,scale=FALSE) 
# Scale = False -> utilizamos la matriz de COVARIANZA para obtener 
# las componentes! Veamos que ocurre:


plot(pca.precio)

summary(pca.precio)

pca.precio <- prcomp(precios,scale=T) 
# Scale = False -> utilizamos la matriz de COVARIANZA para obtener 
# las componentes! Veamos que ocurre:


# Los elementos "center" y "scale" se corresponden con las medias y 
# las desviaciones estándar originales de las variables previo escalado 
# e implementación del PCA. 

# La matriz "rotation" proporciona los loadings 
# de los componentes principales (cada columna contiene el vector de 
# loadings de cada componente principal). La función los denomina matriz 
# de rotación ya que si multiplicáramos la matriz de datos por 
# datos$rotation, obtendríamos las coordenadas de los datos en el nuevo 
# sistema rotado de coordenadas. Estas coordenadas se corresponden con los 
# scores de los componentes principales.


# Muestra de los primeros 6 elementos del vector de loadings de los 5 primeros componentes


head(pca.precio$rotation)[, 1:5]

dim(pca.precio$rotation)



summary(pca.precio)


library(factoextra)

fviz_screeplot(pca.precio, addlabels = TRUE, ylim = c(0, 50))





fviz_pca_ind(pca.precio, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 


fviz_pca_var(pca.precio, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)


# Contribución de variables
fviz_contrib(pca.precio, choice = "var", axes = 1, top = 10)

fviz_contrib(pca.precio, choice = "var", axes = 2, top = 10)

PCATotal=pca.precio$x
dim(PCATotal)

PC1<-PCATotal[,1]
plot(PC1) # es solo un set de datos para cada registro
PC2<-PCATotal[,2]

#datos_recuperados[, 1] <- PC1 + mean(datos$X1)
plot(precios$p36)

plot( c(0,40), c(-10,25), type = "n", xlab = "Tiempo",
      ylab="Efecto", main = "Líneas temporales" )

# primer Componente ppal
lines( PC1,
       lwd = 0.7,
       lty = 1,
       col = "black",
       pch = 1 )


# p36 - 
lines( precios$p36, 
       lwd = 1.5,
       lty = 2,
       col = "darkorange1" )

# p1 - 
lines( precios$p1,
       lwd = 1.5,
       lty = 3,
       col = "green4")


# p24 - 
lines( precios$p24,
       lwd = 1.3,
       lty = 3,
       col = "red")

# p16 - 
lines( precios$p12,
       lwd = 1.3,
       lty = 3,
       col = "blue")

