#Examen 3 - modelo
library("ISLR")
library("MASS")
library(tidyverse)

#NOTA: Desarrolle más las ideas y conclusiones en el examen.

#1) k-medias y Regresiones:
#Con la base de datos USArrest:
# a) Haga un resumen estadistico para todo el conjunto
summary(USArrests)
# b) Determine la cantidad de agrupamientos optimos
#Antes de determinar los agrupamientos optimos, hace falta estandarizar los datos
# Estandarizar el dataset
USArrestss=scale(USArrests)
K.max=10
wss=c()

for(i in 1:K.max){
  irisclasses= kmeans(USArrestss,i,nstart = 15)
  wss[i] = irisclasses$tot.withinss   #in irisclass,total wss distance allocated 
}

plot(1:K.max, wss, 
     type="b",pch = 12,   #b means both(line,point), pch: specify symbol used along with plot
     xlab= "Number of clusters",
     ylab="within ss/ total ss")
km.out =kmeans (USArrestss,2)
# c) Realice un examen estadistico por grupo, comparela con la del conjunto
subset = USArrests %>% group_by(cluster) %>% summarise(
  mean(Murde) #Etc
) 


#2) PCA 
#a) Analice la varianza de los datos.
apply(USArrests , 2, var)
#b) realice un analisis de PCA
pr.out =prcomp (USArrests , scale =TRUE) #escalar? si? no?
#c) Calcule la matriz de rotacion y comente para que le sirve la misma
pr.out$rotation
#d) Graficando los Componentes y la Matriz de Rotacion:
  #En el primer cuadrante, ¿Que tipo de estados hay?¿En el 2do?
biplot (pr.out , scale =0)

#2) MCA
library("FactoMineR")
library("factoextra")
df = read_csv('MaskBeliefs.csv')

res.mca <- MCA(df, graph = T)
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())


h3=cbind(df,res.mca$svd$U[,1:4] %>% as_tibble())
#a) ¿Cuanto del segundo componente se explica por el Genero?
corratioV2 = lm(V2~Gender, h3) %>% summary()
#b) ¿Cuanto del primer componente se explica por creer que evita el contagio?
corratioV3 = lm(V1~PreventSpread, h3) %>% summary()
#c) Analizando el segundo componente, ¿cual es el vinculo entre el genero y el motivo del uso del barbijo? (que categorías estan cercas unas de otras)
#Los varones estan mas cerca de usarlo para protegerse a si mismos Ó para proteger a otros
#las mujeres mas para proteger tanto a si mismo como a otros.

#d) Caracterizar a la persona con un primer componente alto. 
#Usa el barbijo solo por obligacion, sale a comer seguido, realmente 
#no cree que los barbijos prevengan el contagio.
