# Examen Parcial 216/05/2023
# 61.589
# Azul de los Angeles Makk

#De la base de datos "ds_salaries.csv"
library(tidyverse)
library(funModeling)
library(ggplot2)
library(dplyr)
credit_customers <- read.csv("~/Desktop/Analitica Descriptiva/Parciales/Parcial 2 Makk/credit_customers.csv")
View(credit_customers)

#1) Procesamiento Texto:
#La variable "personal_status" contiene el sexo y el estado civil 
#a)Genere una columna que diga si es varon o mujer.
for (i in 1:length(credit_customers$personal_status)){
  generoYEstado <- unlist(strsplit(credit_customers$personal_status[i]," "))
  credit_customers$genero[i] <- ifelse("male"%in%generoYEstado,'varon','mujer')
}

#b)Genere una columna que indique el estado civil.

for (i in 1:length(credit_customers$personal_status)){
  generoYEstado <- strsplit(credit_customers$personal_status[i]," ")
  credit_customers$estado[i] <- generoYEstado[[1]][2]
}

#2) Genere  interrogante que pueda ser contestado con esos datos que vincule:
    #a) Dos variables numericas
    #b) Una categorica y una numerica:
    #c) Dos categoricas
#En todos los casos:
#1.Escriba la hipotesis
#2.En cada analisis, realice un previo analisis gr?fico relevante para adelantar alguna conclusion
#3.Seleccione el test adecuado
#4.Realice el test
#5.Interpretelo

# a) Se seleccionaran las variables credit_amount y age
# Se quiere averiguar si a mayor edad, mayor credit_amount. Por ende, se busca estudiar su correlacion
# Establecemos hipotesis:
# Hipotesis coloquial: cuanta mas es la duracion, mas es el monto del credito
# Hipotesis formal: existe una correlación positiva entre el monto del credito y su duracion

# A modo de analisis exploratorio, realizaremos un grafico:
credit_customers%>% ggplot(aes(x=duration,y=credit_amount))+geom_point(color="coral3")+geom_smooth(method="lm", color='coral1')
# Interpretacion: Podemos observar una clara homogeneidad ascendente
cor.test(credit_customers$credit_amount, credit_customers$duration, method = "pearson")
# Interpretación coloquial: se puede decir que cuantos más es la duracion, más alto es el monto del credito.
# Interpretación formal: el test es signifivativo entonces se afirma que hay evidencia estadistica para decir que el coef de corr es distinto de 0.

# b) Una categorica y una numerica
hist(credit_customers$age)
unique(credit_customers$job)
# Como variable categorica tomaremos la calificacion del cliente.  
# Para poder compararlas en primer lugar vamos a discretizar la variable numerica
nnds <- credit_customers %>% select(class,age)
nnds <- nnds %>% mutate(rangoEtario = case_when(
  age<30 ~ 'Joven',
  age<50 ~ 'Mediana Edad',
  T ~ 'Adulto Mayor'))

# Hipotesis coloquial: la calificacion del cliente esta relacionado con el rango etario.
# Hipotesis formal: la calificacion crediticia no es independiente con respecto al rango etario.

# Lo graficaremos usando ggplot
nnds%>% select(rangoEtario,class) %>% group_by(rangoEtario,class) %>% summarise(n=n()) %>% 
    ggplot(aes(x = rangoEtario, y=n, fill= class))+geom_bar(stat="identity",position="fill")+
    labs(x = "Trabajo", y = "Densidad",title = "Rango Etario y Trabajo")
# Interpretacion: Pareciera existir una correlacion en la que, a mayor edad, mayor es la tendencia a
# una buena calificacion

# Luego procederemos a hacer un test chi-cuadrado. Para ello debemos hacer los siguientes supuestos:
# - Los datos en cada grupo son independientes.
# - El taamaño de muestra adecuado 
# - Categorías son mutuamente excluyentes 
chisq.test(nnds$rangoEtario, nnds$class)

# Interpretación coloquial: se puede decir que la situacion laboral es dependiente del rango etario
# Interpretación formal: el test es significativo por lo que rechazamos la independencia y hay evidencia para afirmar que las variables son dependientes.

# c) Dos variables categoricas
# Para este caso tomaremos son genero y estado -obtenidas en el punto 1 del presente examen-.

# Hipotesis coloquial: eltipo de housing depende del estado civil
# Hipotesis formal: no hay independencia el tipo de housing y el estado civil
# Gráfico:
nnds1 <- credit_customers %>% select(housing,estado)
nnds1 %>% ggplot(aes(x = housing, fill = estado)) + geom_bar(position = "fill")
mosaicplot(~ housing+ estado, data = nnds1, shade = TRUE)

t <- table(nnds1$housing,nnds1$estado)
# Test de independencia chi cuadrado
chisq.test(t)

# Interpretación coloquial: hay dependencia entre el tipo de housing y el estado
# Interpretación formal: el test es significativo y hay evidancia estadistica para afirmar que la experiencia no es independiente del tipo de empleo

#3) 
  #a) Detectar missing de la columna "purpose". Detecte si es independiente de "credit_amount".
  #b) Outliers en la columna "credit_amount". Utilice metodos de intercuartilico y percentil y compare. (?Cuales son outliers con un m?todo y no en otro?)

# a) Missings
nnds2 <- credit_customers %>% select(purpose,credit_amount)
nnds2 <- nnds2 %>% mutate(na=is.na(purpose))
mean(nnds2$na) # Solo un 1% de los datos son NAs
t <- table(nnds2$na,nnds2$credit_amount) 

chisq.test(t) 
# El resultado del test es significativo por lo que hay suficiente evidencia para afirmar la dependecia

# b) Outliers
credit_customers %>% ggplot(aes(y=credit_amount))+geom_boxplot(color="darkslategray3", fill='deepskyblue4')+ 
  labs(y="Monto del Credito")+ ggtitle("Boxplot de montos de creditos solicitados")

# Utilizando el rango intercuartilico
q <- quantile(credit_customers$credit_amount, c(0.25, 0.75))
ric <- q[2] - q[1]

lower <- q[1] - 1.5 * ric
upper <- q[2] + 1.5 * ric
outliers <- credit_customers[credit_customers$credit_amount < lower | credit_customers$credit_amount > upper,]
nrow(outliers) # 72 outliers detectados

# Por Percentil 99:
P99 <- quantile(na.omit(credit_customers$credit_amount), 0.99) #14180.39 
outliersP99 <- credit_customers$credit_amount[na.omit(credit_customers$credit_amount) > P99]
length(outliersP99) # 10 outliers detectados
hist(outliers_p99)

# La significante diferencia de 62 registros se debe a los montos de credito que se ubican entre 
# la cota superior del rango intercuantilico (x0.75 + 1.5*ric) que en este caso 7882,37
# y entre el percentil 0.99, representado por 14180,39

#4) SMOTE
#a) Realizar un ejercicio de oversampling sobre la variable "class" 
#b) Comparar que las variables credit_amount antes y despues del oversampling,
#b.1) ?Tiene la misma media?
#b.2) ?Tiene el mismo desvio?

library(smotefamily)

dataCompleta <- credit_customers[complete.cases(credit_customers),]
dataCompleta$class <- as.factor(dataCompleta$class)
dataCompleta['good'] = as.factor(as.numeric(dataCompleta$class== 'good'))
mean(as.numeric(as.character(dataCompleta$good))) # 70% son 'good'

# Creamos un dataframe solo con las variables y un vector con las respuestas de la variable objetivo
predictorasVariables <- dataCompleta %>% select(duration,credit_amount) %>% as_tibble() 
respuestaVariable <- as.numeric(dataCompleta$good)   # Only select response variable

# El caso minoritario debe tener el nivel de factor de 1, por defecto lo tiene codificado como 0.
smotedData <- SMOTE(predictorasVariables,target= as.numeric(respuestaVariable),K=3)
oversampled = smotedData$data # los nuevos datos creados

# 4 b 1
nrow(credit_customers)
nrow(oversampled)

ggplot()+geom_density(aes(credit_customers$credit_amount), fill='deeppink',alpha = 0.5)+
  geom_density(aes(oversampled$credit_amount), fill='deepskyblue',alpha = 0.5)

# Otra manera de visualizarlo:
ggplot()+geom_density(aes(credit_customers$credit_amount), fill='deeppink',alpha = 0.5)+
  geom_density(aes(oversampled$credit_amount), fill='deepskyblue',alpha = 0.5)+
  scale_x_log10()

# Para la comparacion de medias realizaremos un test de T Student. Para ello debemos realizar los siguientes tres supuestos:
# 1) Los datos son independientes --> Al tenes diferentes lengths, no se puede realizar un test de correlacion
# 2) Los datos en cada grupo siguen una distribución normal 
shapiro.test(credit_customers$credit_amount)
shapiro.test(oversampled$credit_amount)
# En ambos casos el valor p es menor que un nivel de significancia dado por lo que 
# se puede concluir que existe una relación significativa entre las variables.
# 3) Homogeneidad de varianza (varianzas iguales) 
var.test(credit_customers$credit_amount,oversampled$credit_amount)
# Dado que el valor p es menos que el nivel de significancia dado, se concluye que hay evidencia 
# estadística para afirmar que las varianzas entre los grupos o muestras son diferentes. 
# En otras palabras, hay una diferencia significativa en las varianzas entre los grupos.

# Debido a que el tercer supuesto no se cumple, realizaremos el test de Welch
# o t-test sin asunción de igualdad de varianzas, debido a que tal es una alternativa al 
#t-test estándar cuando las varianzas de los dos grupos que se están comparando son diferentes.
t.test(credit_customers$credit_amount,oversampled$credit_amount, var.equal = FALSE)
# Dado que su resultado es no significativo, no hay suficiente evidencia para afirmar que las medias de los grupos son diferentes.

