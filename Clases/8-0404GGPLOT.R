#Usando ggplot:

library(ggplot2)
library(dplyr)
#1) Realizar un scatterplot de la base de datos de mtcars para comparar el peso (wt) y la acelearación (qsec)
mtcars<-datasets::mtcars
mtcars
mtcars %>% ggplot(aes(x=wt,y=qsec))+geom_point()

#2) Al plot anterior, agregar la linea de tendencia
mtcars %>% ggplot(aes(x=wt,y=qsec))+geom_point()+geom_smooth(method='lm')

#3) Realizar un histograma de la potencia (hp), divido por color de acuerdo a la cilindrada (cyl)
mtcars %>% ggplot(aes(x=hp,fill=cyl)) + geom_histogram()

mtcars %>% ggplot() + geom_histogram(aes(x=hp,fill=as.factor(cyl)))

#4) De la base de datos nycflights13::flights, realizar un analisis grafico de la cantidad de vuelos según destinos.
nycflights13::flights %>% ggplot(aes(x=dest))+geom_bar()

#5) Realizar el gráfico de variables numericas usando funModeling para la base nycflights13::flights
nycflights13::flights %>% group_by(dest) %>% summarise(N=n()) %>% 
  arrange(-N) %>% head(10) %>% ggplot(aes(x= reorder(dest, -N), y = N))+geom_bar(stat="identity")