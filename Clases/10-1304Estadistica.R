

df= read.csv('/Users/azulmakk/Desktop/Analitica Descriptiva/Clases/spotify.csv')

library(dplyr)
df1 = df %>% filter(explicit==T) %>% group_by(year) %>% summarise(explicit = n())
df2 = df %>% filter(explicit==F) %>% group_by(year) %>% summarise(explicit = n())
df3 = df %>% group_by(year) %>% summarise(explicit = mean(explicit,na.rm = T))

#Si le sacamos el outlier
df3 %>% filter(year!=1998) %>% 
  ggplot(aes(year,explicit))+
  geom_smooth()+geom_point()

df3a = df3 %>% filter(year!=1998)
cor.test(df3a$year,df3a$explicit,method = "spearman")
cor.test(df3a$year,df3a$explicit,method = "pearson")

cor(df3a$year,df3a$explicit)
View(df)
ggplot()+
  geom_smooth(method='lm', aes(year, explicit), color='red', data = df1)+
  geom_smooth(method='lm',color='blue', aes(year, explicit), data = df2)

# a) tendencias de medida central 
mean(df$duration_ms)
median(df$duration_ms)
moda <- function(vector){
  valores_unicos <- unique(vector)
  cant_repeticiones <- tabulate(match(vector, valores_unicos))
  return(valores_unicos[cant_repeticiones==max(cant_repeticiones)])
}
moda(df$genre)
sd(df$duration_ms)

# b) incorporar al menos dos visualizaciones
library("funModeling")
funModeling::plot_num(df)

ggplot(data = df, aes(x=danceability,y=speechiness))+
    geom_point()+geom_smooth(color='purple')+geom_point()

# c) Pregunta entre variables numericas
cor.test(df$danceability, df$energy)

# d) Pregunta entre variables categoricas
chisq.test(df$mode, df$explicit) # No hay correlacion cuando el p-valor es menor a 0.05
# Formular la pregunta (puede ser hay correlacion o hay independencia -chi-cuadrado evalua independencia-)
# Hacer analisis grafico
# Elegir el test
df %>% filter(!is.na(explicit)) %>% group_by(explicit, mode) %>% count() %>%
  ggplot(aes(x=explicit, y=n, fill=as.factor(mode)))+geom_bar(stat = 'identity', position='fill')
# El position fill se hace cuando el y es n

# e) Pregunta entre variable numerica y categorica