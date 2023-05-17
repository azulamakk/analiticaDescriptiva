#De la base de datos "data.csv"
library(tidyverse)
library(funModeling)
#1) Seleccione de 2 ? 3 variables y realice una breve descripci?n estadistica que contenga:
#Medidas de tendencia central, variacion o frecuencia.
  #Alg?n interrogante que pueda ser contestado con esos datos que vincule:
    #a) Dos variables numericas
    #b) Una categorica y una numerica:
        #b.1) Discretizando la numerica
        #c.1) Analizando las distribuciones de la numerica por categor?a
    #c) Dos categoricas
    #d) Al menos 2 Gr?ficos relevantes realizados con ggplot2 (el resto puede ser realizado con las herramientas de R-base)
spoti = read.csv("~/Desktop/BackupClases/spotify(1).csv")
#a) Hipotesis:
#Coloquial: "Cuanto mas bailable sea una cancion, mas popular es"
#Formal: "Existe una correlacion positiva entre "danceability" y "popularity" 
#Test: Test de Correlacion. 
#Recordatorio: Tanto Pearson como Spearman testean monotonicidad pero Spearman solo asume linearidad. 
#Veamos los datos para ver cual es mejor (o ambos)
spoti %>% ggplot(aes(danceability, popularity)) +geom_point()+
  geom_smooth()
#No parece haber relaciones monotonicas entre las relaciones. Se prueban ambos tests:
cor.test(spoti$danceability,spoti$popularity, method = 'pearson')
cor.test(spoti$danceability,spoti$popularity, method = 'spearman')
#Dado el p-valor obtenido, no se puede afirmar que exista la relacion
#b) Coloquial: "Las canciones explicitas suelen ser mas largas"
#Formal: "El promedio de la longitud de las canciones (duration_ms) es
#mayor en las canciones donde "explicit==TRUE"
#Primero visualicemos este contexto:
spoti %>% ggplot(aes(x=explicit, y = duration_ms))+geom_boxplot()
spoti %>% ggplot(aes(fill=explicit, x = duration_ms))+geom_density(alpha = 0.4)
#En el boxplot parece que la hipotesis se cumple, en el grafico de densidad no,
#el test correspondiente es el de igualdad de medias "t-student"
t.test(spoti[spoti$explicit==T,]$duration_ms,spoti[spoti$explicit==F,]$duration_ms, alternative = 'greater')
#La H0: mean(duration_ms|explicit==T)<=mean(duration_ms|explicit==F)
#La H1: mean(duration_ms|explicit==T)>mean(duration_ms|explicit==F)
#Como el p-value es cercano a 0 (menor a .05), el test indica que, en promedio,
#las canciones explicitas son, en promedio, mas largas

#c) Coloquial: Es indiferente si la cancion es del siglo XX o XXI para saber si 
# Formal: El siglo es independiente de explcit
#Requerimientos: 
# calcular el siglo
# visualizar la distribucion de explicits por siglo
# test chi2
spoti['sigloXX'] = spoti$year<2000
spoti %>% group_by(sigloXX) %>% summarise(ex = mean(explicit,na.rm=T)) %>% ggplot()+
  geom_bar(aes(x=sigloXX, y = ex), stat = 'identity')
table(spoti$explicit,spoti$sigloXX) %>% plot()
table(spoti$explicit,spoti$sigloXX)/nrow(spoti)
chisq.test(table(spoti$explicit,spoti$sigloXX))
#El test indica que son independientes

#2) 
  #a) Detectar missing de la columna "Explicit" 
  #b) Outliers en la columna "duration".
  #b) Missings y Outliers en la columna "year".
#Recuerde:
# Verficar si los outliers refieren a algun dato real o es un faltante
# Verificar si los faltantes son aleatorios o sistematicos.
# Imputar los faltantes con linear regression ? logistical regression seg?n corresponda.
#a)
#Primero se detecta si el faltante es linealmente dependiente de alguna variable
mdet = glm(is.na(explicit)~duration_ms+popularity+energy+key+loudness+speechiness+acousticness+liveness+instrumentalness+valence+tempo, data = spoti, family='binomial')
mdet %>% summary()
#Hay significatividad en "popularity" y "speechiness", se eliminan para la imputacion
#Modelo de imputacion
impmodel = glm(explicit~duration_ms+energy+key+loudness+acousticness+liveness+instrumentalness+valence+tempo, data = spoti[!is.na(spoti$explicit),], family='binomial')
impmodel %>% summary()

#Generacion de imputacion
impexplicit = exp(predict(impmodel,spoti[is.na(spoti$explicit),]))>0.5
#Imputacion
spoti$explicit[is.na(spoti$explicit)] = impexplicit

#b)Deteccion de outliers por:
#Rango intercuartilico
boxplot(spoti$duration_ms)
q <- quantile(spoti$duration_ms, c(0.25, 0.75))

iqr <- q[2] - q[1]

lower <- q[1] - 1.5 * iqr
upper <- q[2] + 1.5 * iqr
outliers <- spoti[spoti$duration_ms < lower | spoti$duration_ms > upper,]
#Por percentil 99
p99 <- quantile(spoti$duration_ms, 0.99)
outliers <- spoti[spoti$duration_ms > p99,]
#Se pasan a minutos para evaluarlos mejor
spoti['minutes'] = spoti$duration_ms / 60000
boxplot(spoti$minutes)
#Los outliers no parecen ser falsos


#3) En la base de datos "znorte_properati.csv"
znorte_properati = read.csv("~/Desktop/BackupClases/znorte_properati(1).csv")
View(znorte_properati)
#a) Repita el ejercicio 1) sobre esta base.
#b) Estudie los missings y outliers de la variable "price_aprox_local_currency"
#tip: tenga en cuenta su "conocimiento contextual" (?tipo de cambio implicito?)
#A) similar al anterior
#B) Outliers: analisis similar al anterior
#B)Missings:
#Los missings, ahora que es una variable numerica
mdet = glm(is.na(price_aprox_local_currency)~rooms+property_type+surface_in_m2, data = znorte_properati, family='binomial')
mdet %>% summary()
#Modelo de imputacion
impmodel = lm(price_aprox_local_currency~rooms+property_type+surface_in_m2, 
              data = znorte_properati[!is.na(znorte_properati$price_aprox_local_currency),])
impmodel %>% summary()


#Generacion de imputacion
impprice = predict(impmodel,znorte_properati[is.na(znorte_properati$price_aprox_local_currency),])
#Imputacion
znorte_properati$price_aprox_local_currency[is.na(znorte_properati$price_aprox_local_currency)] = impprice


#4) Procesamiento de texto
#a) Para la base "spotify.csv":
#Vea que la variable "genre" puede tener distintas convinaciones de generos de m?sica,
#por ejemplo: "pop, rock", "pop, country" o "rock, metal"
#Cree 2 columnas adicionales llamadas:
  #"rock" que tome 1 si la cancion es del genero rock, 0 si no.
  #"pop"  que tome 1 si la cancion es del genero pop, 0 si no.
#Es decir, extraiga los sub-generos pop y rock de la columna "genre"
spoti['rock'] = grepl('rock',spoti$genre)
spoti['pop'] = grepl('pop',spoti$genre)
#Se puede crear una funcion para esto:
get_genre = function(df,x){
  df[x] = grepl(x,df$genre)
  df
}
spoti = get_genre(spoti,'R&B')

#b) Para la base "znorte_properati.csv":
#Toma la columna "place_with_parent_names" y separarla en las variables :
  #a.1) "Pais", "Provincia", "Region"

prop <- read.csv("~/Desktop/BackupClases/znorte_properati(1).csv")

l = strsplit(prop$place_with_parent_names,"\\|")
l
pais = c()
provincia = c()
region = c()
for(i in l){
  pais = c(pais,i[2])
  l1 = strsplit(i[3],' ')
  provincia = c(provincia,l1[[1]][1])
  region = c(region,l1[[1]][2])
}
prop['pais'] = pais
prop['provincia'] = provincia
prop['region'] = region
View(prop)
