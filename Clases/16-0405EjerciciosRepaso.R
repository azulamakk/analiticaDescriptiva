#De la base de datos "data.csv"
library(tidyverse)
library(funModeling)
library(dplyr)
spotify <- read.csv("~/Desktop/BackupClases/spotify(1).csv")
View(spotify)
#1) Seleccione de 2 ? 3 variables y realice una breve descripci?n estadistica que contenga:
#Medidas de tendencia central, variacion o frecuencia.
  #Alg?n interrogante que pueda ser contestado con esos datos que vincule:
    #a) Dos variables numericas
    #b) Una categorica y una numerica:
        #b.1) Discretizando la numerica
        #c.1) Analizando las distribuciones de la numerica por categor?a
    #c) Dos categoricas
    #d) Al menos 2 Gr?ficos relevantes realizados con ggplot2 (el resto puede ser realizado con las herramientas de R-base)

#Valores centrales
media <- mean(spotify$duration_ms)
mediana <- median(spotify$duration_ms)
varianza <- var(spotify$duration_ms)


# a) Dos variables numericas
# Existe relacion entre la duracion de la cancion con la popularidad?

# Planteo de hipotesis
# Hipotesis coloquial: no existe relacion entre la popularidad y la duracion
# Hipotesis formal: Existe una correlacion entre la cancion y la popularidad

spotify%>% ggplot(aes(x=duration_ms,y=popularity))+geom_point(color="deeppink3")+geom_smooth(method="lm", color='deeppink4')
# No se puede indicar una clara correlacion entre ambas mirando el grafico

cor.test(spotify$duration_ms, spotify$popularity, method='spearman')
# No existe suficiente evidencia estadistica para afimar que hay una correlacion entre ambas variables

# b) Una categorica y una numerica
# b.1) Discretizando numerica
hist(spotify$energy)
nsdata <- spotify %>% select(energy, explicit)

nnsdata <- nsdata %>% mutate(tipoEnergia = case_when(
  energy < 0.6 ~ 'Quedado', 
  energy < 0.8 ~ 'Medio',
  T ~ 'Punchi'
))

# Hipotesis coloquial: El tipo de energia es dependinte de si una cancion es explicita o no
# Hipotesis formal: La clase de energia no es independiente con respecto a ser explicita o no

nnsdata1 <- nnsdata %>% group_by(explicit, tipoEnergia) %>% summarise(n=n())

nnsdata1 %>% ggplot(aes(x = tipoEnergia, y=n, fill= explicit))+
  geom_bar(stat="identity",position="fill")+labs(x = "Tipo de Energia", y = "Explicit",title = "Energia y Explicit")

chisq.test(nnsdata$explicit, nnsdata$tipoEnergia)

# Interpretacion formal: Existe sufienciente evidencia estadistica como para afirmar que existe una correlacion entre si.

# b) que genero es el que tiene mayor energia?


# c) existe una relacion entre el genero de la cancion con la frecuencia 
library(ggplot2)
ggplot() + geom_point(aes(spotify$duration_ms,spotify$popularity))

#2) 
  #a) Detectar missing de la columna "Explicit" 
  #b) Outliers en la columna "duration".
  #b) Missings y Outliers en la columna "year".
#Recuerde:
# Verficar si los outliers refieren a algun dato real o es un faltante
# Verificar si los faltantes son aleatorios o sistematicos.
# Imputar los faltantes con linear regression ? logistical regression seg?n corresponda.

#3) En la base de datos "znorte_properati.csv"
#a) Repita el ejercicio 1) sobre esta base.
#b) Estudie los missings y outliers de la variable "price_aprox_local_currency"
#tip: tenga en cuenta su "conocimiento contextual" (?tipo de cambio implicito?)

#4) Procesamiento de texto
#a) Para la base "spotify.csv":
#Vea que la variable "genre" puede tener distintas convinaciones de generos de m?sica,
#por ejemplo: "pop, rock", "pop, country" o "rock, metal"
#Cree 2 columnas adicionales llamadas:
  #"rock" que tome 1 si la cancion es del genero rock, 0 si no.
  #"pop"  que tome 1 si la cancion es del genero pop, 0 si no.
#Es decir, extraiga los sub-generos pop y rock de la columna "genre"
for (i in 1:length(spotify$genre)){
  canc <- unlist(strsplit(spotify$genre[i],", "))
  spotify$rock[i] <- ifelse("rock"%in%canc,1,0)
  spotify$pop[i] <- ifelse("pop"%in%canc,1,0)
}

#b) Para la base "znorte_properati.csv":
#Toma la columna "place_with_parent_names" y separarla en las variables :
  #a.1) "Pais", "Provincia", "Region"


prop <- read.csv("~/Desktop/BackupClases/znorte_properati(1).csv")
l = strsplit(prop$place_with_parent_names,"\\|")

for(i in l){
  pais = c(pais,i[2])
  l1 = strsplit(i[3],' ')
  provincia = c(provincia,l1[[1]][1])
  region = c(region,l1[[1]][2])
}
prop['pais'] = pais
prop['provincia'] = provincia
prop['region'] = region

