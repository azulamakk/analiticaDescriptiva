#Recuperatorio del Segundo Parcial- Paula Gonzalez

library(tidyverse)
library(missForest)
library(rpart)
library(rpart.plot)

#1) ---- Missings y Outliers
dt <- data_recuperatorio_2

#A)¿Los faltantes de "Explicit" son aleatorios respecto a el largo de la cancion (duration_ms)?
#para evaluar la independencia entre dos variables se usa el chi-cuadrado.
#para usar ese test debemos crear una tabla con las dos variables a analizar.


t <- table(is.na(dt$explicit), dt$duration_ms)
chisq.test(t)

#podemos ver en el resultado que el p-valor es mayor a 0.05, por ende no se rechaza la hipotesis de que son independientes.
#Por ende inferimos que las variables son independientes.


#B)¿Hay outliers en la variable duration_ms? Estos valores atipicos, ¿se son independientes de la decada en la que se hicieron?
boxplot(dt$duration_ms,
        main="Boxplot de duration_ms",
        col="tomato",
        border="black",
        horizontal=TRUE
)
#graficamos el boxplot para ver si hay outliers y si, los hay.
#extraemos los outliers:
extraer_outliers = function(x){
  sup = quantile(x,0.75, na.rm = T)+IQR(x, na.rm = T)*1.5 #bigote de la derecha
  inf = quantile(x,0.25, na.rm = T)-IQR(x, na.rm = T)*1.5 #bigote de la izquierda
  outliers <- na.omit(x[(x > sup) | (x<inf)])
  return(outliers)
}
o <- extraer_outliers(dt$duration_ms)

#sacando al decada:
unique(data_recuperatorio_2$year)

numeros <- c()
years <- dt[,3]
for(i in 1:length(years)){
  ano <- as.character(dt[i,3])
  num <- substr(ano, star = 3, stop = 3)
  numeros <- append(numeros, num)
}

unique(numeros)
numeros <- as.numeric(numeros)
dt <- cbind(dt, numeros)

#agregamos la variable categórica de decada
dt <- dt %>% mutate(decada = case_when(numeros == 0 ~ "00s", 
                                       numeros == 9 ~ "90s",
                                       numeros == 1 ~ "10s", 
                                       numeros == 2 ~ "20s"))

#evaluando independencia entre outliers de duration_ms con la decada
#para eso quiero ver cuantos outliers hay por decada
o2 <- dt %>% filter(duration_ms %in% o) %>% select(duration_ms, decada)
o_x_decada <- o2 %>% group_by(decada) %>% summarise(cantidad=n())

#como se puede observar en la variable "o_x_decada" las década que mas outliers tiene es la de los 2000.  
#mientras que en segundo lugar las canciones de los 2010 tiene la mitad de outliers, y en los 90s solo hay 3.
#en los 2020 no hay canciones de longitud atípica.  
#Por lo tanto, se concluye en que hay una dependencia entre outliers de duration_ms con la decada.


#C) Seleccionar una metodologia de imputacion y aplicarla
dt <- dt %>% mutate(explicit_num = case_when(explicit == FALSE ~ 0, 
                                          explicit == TRUE ~ 1))
dt2 <- dt %>% select(-explicit, -genre, -decada)
dt2 = as.data.frame(dt2)
imp = missForest(dt2, verbose = TRUE, variablewise = FALSE)
imp
df_sin_na <- as.data.frame(imp$ximp)

for(i in 1:length(df_sin_na$explicit_num)){
  if(df_sin_na$explicit_num[i] < 0.5){
    df_sin_na$explicit_num[i] = 0
  } else {
    df_sin_na$explicit_num[i] = 1
  }
}

for(i in 1:length(df_sin_na$explicit_num)){
  if(df_sin_na$explicit_num[i] == 0){
    df_sin_na$explicit_num[i] = FALSE
  } else {
    df_sin_na$explicit_num[i] = TRUE
  }
}



#2) ---- Procesamiento de Texto
#A) De la columna "genre" extraiga el feature correspondiente a si corresponde
#a los generos: c("Folk","Rock","pop","hip hop")
generos <- c("folk","rock","pop","hip hop")
folk <- c()
rock <- c()
hip_hop <- c()
pop <- c()
genre <- dt$genre

for(i in 1:length(genre)){ #recorre cada elemento del vector
  palabra = ""
  for(j in 1:length(genre[i])){ #recorre cada caracter
    caracter <- genre[i,j] #agarra el caracter
    if (caracter == ","){
      
    }
    palabra <- append(palabra, caracter) 
    if(j == length(genre[i])){ #si caemos en el ultimo caracter
      if(palabra == "pop"){
        pop <- append(pop, 1)
      }
      if(palabra == "folk"){
        folk <- append(folk, 1)
      }
      if(palabra == "hip hop"){
        hip_hop <- append(hip_hop, 1)
      }
      if(palabra == "rock"){
        rock <- append(rock, 1)
      }
    }
  }
  
#nose


#3) ---- Test de Hipotesis
#Generar una hipotesis, determinar el tipo de test aplicable, e interpretar resultados de
#A)  Una variable numerica y una no-numerica

#quiero ver si el tempo de las canciones solo de pop en promedio es más elevado al tempo de las canciones que son sólo de hip hop.
#numerico es el tempo y lo categórico son los generos.

tempo_pop <- dt$tempo[dt$genre == "pop"]
tempo_hiphop <- dt$tempo[dt$genre == "hip hop"]
t.test(tempo_pop, tempo_hiphop, alternative = "greater")

#como el p-valor dio mayor a 0.05, no se rechaza la hipótesis nula que es que la media de las canciones de pop es mayor a la media de las canciones de hip hop. 
#por ende, podemos inferir que la la media de las canciones de pop es mayor a la media de las canciones de hip hop.

#B)  Una variable no-numerica y una no-numerica (puede usar los features extraidos en el punto anterior)

#quiero evaluar la independencia entre las variables de genero de música y decada.
#ambas variables no son numéricas.

chisq.test(table(dt$genre,dt$decada))

#como el p-valor dio menor a 0.05, se puede rechazar la hipotesis nula que es que las variables son independientes.
#por ende, se puede inferir que las variables son dependientes.

#C)  Una variable numerica y una numerica

#quiero hacer un test de correlacion para ver si el liveness tiene correlación con la variable valence.

cor.test(dt$liveness, dt$valence)

#como el p-valor dio mayor a 0.05, entonces no se rechaza la hipótesis nula de que hay correlación entre las variables. 
#por ende, se puede inferir que no hay correlacion entre las variables.



#4) ---- Arboles
#A) Realizar un analisis exploratorio usando un arbol de decision para la popularidad ("popularity") de la cancion
#e interpretarlo:


arbol1 <- rpart(popularity~., data = dt, method = "class")

rpart.plot(arbol1, main = "Arbol de Clasificacion: Popularity")

