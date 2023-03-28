## Tercer Examen Recuperatorio - Paula Gonzalez

install.packages("NbClust")
library(NbClust)
library("MASS")
library(tidyverse)
library(dplyr)
library("FactoMineR")
library("factoextra")



diabetes = read_csv('diabetes.csv')
#1 = diabetico
#0 = no diabetico


#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1) Haga un listado de las variables que componen el dataset

lista_var_diabetes <- list(ls(diabetes))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2) La funcion siguiente tiene una no linealidad  
#    Explique el significado de esta variable, como se interpreta 
lm1 <- lm(BloodPressure ~ Glucose*Insulin+Age, diabetes)
summary(lm1)

#Lo que se busca con la variable Glucose*Insulin no es medir una regresión lineal, sino agregarle 
#niveles al modelo, en este caso multiplicando la glucosa por la insulina, es conseguir un mejor 
#ajuste a las observaciones. 

#Haciendo el summary, se puede observar que esta variable es estadisticamente significativa ya que tiene un p-valor < 0.05.
#Dado que el coeficiente de la variable Glucose*Insulin es negativo, el valor que maximize la presión en la sangre estara oscilando el 0 mientras que 
#el que lo minimiza se ira a valores grandes ya sean positivos o negativos. 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3) Realice una regresi?n que indique si un paciente tendr? o no diabetes,
#selecciones al menos 5 variables, y explique como afectan la probabilidad de tener 
#diabetes

lm2 <- lm(Outcome ~ Glucose+BloodPressure+SkinThickness+Insulin+Age , data = diabetes )
summary(lm2)

#El modelo es valido dado que todos sus coeficientes son estadisticamente significativos con una confianza 
#minima del 99% para todos sus casos. La variabilidad de la diabetes en los pacientes explicada por nuestras variables
#se puede ver en el Adjusted R-squared y es del 23.54% la cual la considero baja. El error residual del modelo se puede
#observar en el Multiple R-squared y es de 0.2403.

#Glucose: Si esta variable aumenta en 1 unidad la probabilidad de ser diabético aumenta en 0.006 unidades. 
#El coeficiente de esta variable es significativo para un alfa (prob de error de tipo 1) = 0.05.

#BloodPressure: Si esta variable aumenta en 1 unidad la probabilidad de ser diabético disminuye en 0.0013 unidades. 
#El coeficiente de esta variable no es significativa para un alfa (prob de error de tipo 1) = 0.05.

#SkinThickness: Si esta variable aumenta en 1 unidad la probabilidad de ser diabético aumenta en 0.0029 unidades. 
#El coeficiente de esta variable es significativo para un alfa (prob de error de tipo 1) = 0.05.

#Insulin: Si esta variable aumenta en 1 unidad la probabilidad de ser diabético disminuye en 0.0002 unidades. 
#El coeficiente de esta variable no es significativo para un alfa (prob de error de tipo 1) = 0.05.

#Age: Si esta variable aumenta en 1 unidad la probabilidad de ser diabético aumenta en 0.0057 unidades. 
#El coeficiente de esta variable es significativo para un alfa (prob de error de tipo 1) = 0.05.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 4) Realice un analisis de componentes pricipales de

brand.ratings <- read.csv("http://goo.gl/IQl8nc") %>% as_tibble()

#     4.a) Defina la cantidad optima de componentes ppales

#Para hacer un PCA, necesitamos que todas las variables sean numericas
df <- subset(brand.ratings, select = -c(brand))
cor(df)

# Analizando la matriz de correlaciones entre las variables, se puede notar que hay varios casos en los que
#se presenta un coeficiente de correlacion alto, por eso que se elige realizar un PCA.

pca =prcomp(df, scale =TRUE)
pca$sdev

#Dado el output del código de la linea 81, elijo una cantidad de 3 componentes principales.


#     4.b) Explique el criterio

#Elejí 3 componentes principales ya que sus autovalores son mayores a 1, y esto explica más variabilidad al modelo de la que incorporan proporcionalmente.


#     4.c) Cuanto explica el primer componente ppal?

c <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(c[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 10, col="blue", lty=5)
abline(h = 0.95, col="blue", lty=5)
legend("topleft", col=c("blue"), lty=5, cex=0.6)

#El primer componente principal explica poco menos del 40% de la varianza. 


#     4.d) Cuantos componentes ppales debe reunir para explicar 
#          el 85% de la variabilidad conjunta

#Para explicar el 85% de la variablidad conjunta se deberian usar 5 componentes.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 5) ###KMEAN
#Con la base de datos Diabetes:
  # 5a) Haga un resumen estadistico para todo el conjunto

summary(diabetes)

#Elijo las primeras tres variables para resumir:

#Pregnancies: El min de Pregnancies es de 0 y el max es de 17 embarazos, por lo que presentan una dispersion baja a primera vista. 
#La mediana (3) es bastante parecida a la media (3.845) por lo que su distribucion se asemeja a una normal.

#Glucose: El min de la glucosa en los pacientes es de 0 y el max es de 199 por lo que presentan una dispersion alta a primera vista. 
#La mediana (117) difiere apenas de la media (120.9) por lo que nuestra distribucion levemente asimétrica positiva. 

#BloodPressure: El min de la presión en sangre es de 0 y el max es de 122 por lo que presentan una dispersion alta a primera vista. 
#La mediana (72) es muy parecida a la media (69.11) por lo que su distribucion es levemente asimétrica negativa.


  # 5b) Determine la cantidad de agrupamientos optimos, ?que metodo utilizo? ?por que?

#Antes de determinar los agrupamientos optimos, hace falta estandarizar los datos

diabetes_estandar <- scale(diabetes)

K.max=10
wss=c()

for(i in 1:K.max){
  irisclasses= kmeans(diabetes_estandar,i, nstart = 15)
  wss[i] = irisclasses$tot.withinss   #in irisclass,total wss distance allocated 
}

plot(1:K.max, wss, 
     type="b",pch = 12,   #b means both(line,point), pch: specify symbol used along with plot
     xlab= "Number of clusters",
     ylab="within ss/ total ss")


#Observando este gráfico no me quedo muy claro si el numero óptimo de K es 3 o 4.
#Por eso, ejecute otro gráfico. 

fviz_nbclust(diabetes_estandar, kmeans, method = "wss")

#Observando el gráfico de la linea del codigo 153, confirmo que efectivamente el número óptimo de
#de clusters es de K = 3.

km <- kmeans(diabetes_estandar, 3) #aca se define la cant de clusters

diabetes_estandar$cluster = km$cluster


  # 5c) Realice un examen estadistico por grupo, comparela con la del conjunto.

diabetes2 <- as.data.frame(diabetes) %>% group_by(cluster) %>% summarise(
  
  min_Pregnancies = min(Pregnancies),
  median_Pregnancies = median(Pregnancies),
  mean_Pregnancies = mean(Pregnancies),
  max_Pregnancies = max(Pregnancies), 
  
  min_Glucose = min(Glucose),
  median_Glucose = median(Glucose),
  mean_Glucose = mean(Glucose),
  max_Glucose = max(Glucose),
  
  min_BloodPressure = min(BloodPressure),
  median_BloodPressure = median(BloodPressure),
  mean_BloodPressure = mean(BloodPressure),
  max_BloodPressure = max(BloodPressure),
  
  min_SkinThickness = min(SkinThickness),
  median_SkinThickness = median(SkinThickness),
  mean_SkinThickness = mean(SkinThickness),
  max_SkinThickness = max(SkinThickness),
  
  min_Insulin = min(Insulin),
  median_Insulin = median(Insulin),
  mean_Insulin = mean(Insulin),
  max_Insulin = max(Insulin)
  
) 

diabetes_estandar %>% summarise(
  
  min_Pregnancies = min(Pregnancies),
  median_Pregnancies = median(Pregnancies),
  mean_Pregnancies = mean(Pregnancies),
  max_Pregnancies = max(Pregnancies), 
  
  min_Glucose = min(Glucose),
  median_Glucose = median(Glucose),
  mean_Glucose = mean(Glucose),
  max_Glucose = max(Glucose),
  
  min_BloodPressure = min(BloodPressure),
  median_BloodPressure = median(BloodPressure),
  mean_BloodPressure = mean(BloodPressure),
  max_BloodPressure = max(BloodPressure),
  
  min_SkinThickness = min(SkinThickness),
  median_SkinThickness = median(SkinThickness),
  mean_SkinThickness = mean(SkinThickness),
  max_SkinThickness = max(SkinThickness),
  
  min_Insulin = min(Insulin),
  median_Insulin = median(Insulin),
  mean_Insulin = mean(Insulin),
  max_Insulin = max(Insulin)
  
)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

##MCA

##  6) Discretice las variables de brand.ratings y realice 
# un an?lisis de MCA con ellas
#a) ?Cuanta variabilidad explican los primeros 2 componentes principales?

View(brand.ratings)
summary(brand.ratings)

disc_br <- brand.ratings
disc_br <- disc_br %>% mutate(
  perform = case_when(
    disc_br$perform < 2 ~ "bajo",
    disc_br$perform < 4 ~ "medio bajo",
    disc_br$perform < 6 ~ "mediano",
    disc_br$perform < 8 ~ "medio alto",
    disc_br$perform <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  leader = case_when(
    disc_br$leader < 2 ~ "bajo",
    disc_br$leader < 4 ~ "medio bajo",
    disc_br$leader < 6 ~ "mediano",
    disc_br$leader < 8 ~ "medio alto",
    disc_br$leader <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  latest = case_when(
    disc_br$latest < 2 ~ "bajo",
    disc_br$latest < 4 ~ "medio bajo",
    disc_br$latest < 6 ~ "mediano",
    disc_br$latest < 8 ~ "medio alto",
    disc_br$latest <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  fun = case_when(
    disc_br$fun < 2 ~ "bajo",
    disc_br$fun < 4 ~ "medio bajo",
    disc_br$fun < 6 ~ "mediano",
    disc_br$fun < 8 ~ "medio alto",
    disc_br$fun <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  serious = case_when(
    disc_br$serious < 2 ~ "bajo",
    disc_br$serious < 4 ~ "medio bajo",
    disc_br$serious < 6 ~ "mediano",
    disc_br$serious < 8 ~ "medio alto",
    disc_br$serious <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  bargain = case_when(
    disc_br$bargain < 2 ~ "bajo",
    disc_br$bargain < 4 ~ "medio bajo",
    disc_br$bargain < 6 ~ "mediano",
    disc_br$bargain < 8 ~ "medio alto",
    disc_br$bargain <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  value = case_when(
    disc_br$value < 2 ~ "bajo",
    disc_br$value < 4 ~ "medio bajo",
    disc_br$value < 6 ~ "mediano",
    disc_br$value < 8 ~ "medio alto",
    disc_br$value <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  trendy = case_when(
    disc_br$trendy < 2 ~ "bajo",
    disc_br$trendy < 4 ~ "medio bajo",
    disc_br$trendy < 6 ~ "mediano",
    disc_br$trendy < 8 ~ "medio alto",
    disc_br$trendy <= 10 ~ "alto",
  )
)
disc_br <- disc_br %>% mutate(
  rebuy = case_when(
    disc_br$rebuy < 2 ~ "bajo",
    disc_br$rebuy < 4 ~ "medio bajo",
    disc_br$rebuy < 6 ~ "mediano",
    disc_br$rebuy < 8 ~ "medio alto",
    disc_br$rebuy <= 10 ~ "alto",
  )
)
View(disc_br)
disc_br <- disc_br %>% select(-brand)

mca_i <- MCA(disc_br, graph = T) 

fviz_mca_var(mca_i, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

summary(mca_i) #muestra la cantidad de dimensiones

#?porque difiere del an?lisis de PCA?
