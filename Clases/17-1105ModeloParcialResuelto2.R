#De la base de datos "ds_salaries.csv"
library(tidyverse)
library(funModeling)
data <- read.csv("~/Desktop/BackupClases/ds_salaries.csv")

# ------------------------------------------------------------------------------
#1) Genere  interrogante que pueda ser contestado con esos datos que vincule:
    #a) Dos variables numericas
    #b) Una categorica y una numerica:
        #b.1) Discretizando la numerica
        #c.1) Analizando las distribuciones de la numerica por categor?a
    #c) Dos categoricas

#En todos los casos:
#1.Escriba la hipotesis de forma coloquial y formal
#2.En cada analisis, realice un previo analisis gr?fico relevante para adelantar alguna conclusion
#3.Seleccione el test adecuado
#4.Realice el test
#5.Interpretelo de forma coloquial y formal

# A) Dos variables numéricas
shapiro.test(as.numeric(data$salary))
unique(data$employee_residence)
options(scipen = 999)
reg <- data %>% group_by(employee_residence) %>%  summarise( n = n(),promedio = mean(salary)) 

# Hipotesis coloquial: cuantos más residentes hay, mas alto es el salario promedio
# Hipotesis formal: existe una correlación positiva entre la cantidad de residentes y el salario promedio
# Gráfico 
reg%>% ggplot(aes(x=n,y=promedio))+geom_point(color="coral1")+scale_x_log10()+scale_y_log10()+geom_smooth(method="lm", color='coral3') # no hay indicios de que exista la correlación entre las variables
# Test de correlación
cor.test(log(reg$n),log(reg$promedio),method="spearman")
# Interpretación coloquial: se puede decir que cuantos más residentes hay, más alto es el salario promedio, aunque esta relación no es muy fuerte.
# Interpretación formal: el test es signifivativo entonces se afirma que hay evidencia estadistica para decir que el coef de corr es distinto de 0.

# B) Una categórica y una númerica

# b1
hist(log(data$salary))
regb <- data %>% select(salary,company_size)
regb <- regb %>% mutate(Clase_salario = case_when(
  salary<100000 ~ 1,
  salary<200000 ~ 2,
  T ~ 3))

# Hipotesis coloquial: el rango de salario es dependiente del tamaño de la empresa.
# Hipotesis formal: la clase de salario no es independiente con respecto al tamaño de la empresa
# Gráfico
regb %>% select(Clase_salario,company_size) %>% group_by(Clase_salario,company_size) %>% summarise(n=n()) %>% 
  ggplot(aes(x = Clase_salario, y=n, fill= company_size))+
  geom_bar(stat="identity",position="fill")+
  labs(x = "Clase salario", y = "Densidad",title = "Clase salario y Company Size")
# pareciera ser que la mayoría de los salarios provenientes de compañías grandes están en las clases 1 y 3.
# Test de chi cuadrado
table(regb$company_size,regb$Clase_salario)
chisq.test(table(regb$company_size,regb$Clase_salario))
# Interpretación coloquial: se puede decir que el rango de salario es dependiente del tamaño de la empresa
# Interpretación formal: el test es significativo por lo que rechazamos la independencia y hay evidencia para afirmar que las variables son dependientes.

# b2
# Hipotesis coloquial: la dispersión de los salarios cambia de acuerdo al tamaño de la empresa
# Hipotesis formal: las varianzas de los salarios son diferentes dependiendo del tamaño de la empresa
# Gráfico
regb %>% ggplot(aes(x=log(salary), fill=company_size))+
  geom_density(aes(y=..density..),position = "identity",alpha=0.5)+
  labs(x = "Salario", y = "Densidad")
# intepretacion del grafico: se ve que los salarios de las compañías de tamaño L y M tienen dispersiones parecidas, mientras que los de las compañías S parecen estar un poco más dispersos.
# Test de barlett
sal_L <- regb$salary[regb$company_size == "L"]
sal_M <- regb$salary[regb$company_size == "M"]
sal_S <- regb$salary[regb$company_size == "S"]
bartlett.test(list(sal_L,sal_M,sal_S))

# Interpretación coloquial: los salarios tienen diferente dispersión en función del tamaño de la empresa
# Interpretación formal: el test es significativo por lo que se rechaza que todas las varianzas sean iguales. Hay evidencia para afirmar que no tienen varianzas iguales.

# D) Dos categóricas

# Hipotesis coloquial: el tipo de empleo depende de la experiencia del empleado
# Hipotesis formal: no hay independencia entre el tipo de empleo y la experiencia del empleado
# Gráfico:
regd <- data %>% select(employment_type,experience_level)
t <- table(regd$employment_type,regd$experience_level)
plot(t)
# Test de independencia chi cuadrado
chisq.test(t)

# Interpretación coloquial: hay dependencia entre el nivel de experiencia y el tipo de empleo
# Interpretación formal: el test es significativo y hay evidancia estadistica para afirmar que la experiencia no es independiente del tipo de empleo


# ------------------------------------------------------------------------------
#2) 
  #a) Detectar missing de la columna "salary_in_usd". Detecte si es independiente de "salary_currency"
  #determine la mejor forma de imputarlo sin usar un modelo.
  #b) Outliers en la columna "salary_in_usd". Utilice metodos de intercuartilico y percentil.
  #Encuentre alguna explicacion plausible.

# A: MISSINGS
data2 <-  data %>% select(salary_in_usd,salary_currency)
data2 <- data2 %>% mutate(na=is.na(salary_in_usd))
mean(data2$na) # hay pocos NAs = 0.7% 
t <- table(data2$na,data2$salary_currency) 
chisq.test(t) # el test es significativo y que hay evidencia para rechazar la independencia
# Imputar los NAs
media_eur <- mean(na.omit(data2$salary_in_usd[data2$salary_currency=="EUR"]))
# Como todos los faltantes son de salary_currency = EUR --> se imputa con la media de los EUR
# Para imputarlo sería: data$salary_in_usd <- ifelse(is.na(data$salary_in_usd),media_eur,data$salary_in_usd)

# B: OUTLIERS
data %>% ggplot(aes(y=salary_in_usd))+
  geom_boxplot(color="darkslategray3", fill='deepskyblue4')+
  labs(y="Salario en USD")+
  ggtitle("Boxplot")
# se ven valores atípicos 
# Por IQR:
stats <- boxplot.stats(data$salary_in_usd, coef = 1.5)
outliers_iqr <- stats$out
length(outliers_iqr) # 63 outliers detectados
hist(outliers_iqr)

# Por Percentil 99:
p99 <- quantile(na.omit(data$salary_in_usd), 0.99)
outliers_p99 <- data$salary_in_usd[na.omit(data$salary_in_usd) > p99]
length(outliers_p99) # 63 outliers detectados
hist(outliers_p99)

# Los outliers no parecen ser falsos

# ------------------------------------------------------------------------------
#3) Procesamiento la variable "job_title" contiene el nombre del 
#titulo. Genere 3 columnas indicando si el encuestado es "Scientist","Anlyst" o "Engineer" 
#Todos los registros deben caber en alguna columna por lo menos, donde no lo haya, 
#use su criterio para imputarlo, es decir, "ETL Developer" podr?a ser un analista
#y "Machine Learning Developer" un ingeniero
library(syuzhet)

job <- data %>% select(job_title)

for (i in 1:length(job$job_title)){
  jobvec <- unlist(strsplit(job$job_title[i]," "))
  job$Scientist[i] <- ifelse("Scientist"%in%jobvec,1,0)
  job$Engineer[i] <- ifelse("Engineer"%in%jobvec,1,0)
  job$Analyst[i] <- ifelse("Analyst"%in%jobvec %in%jobvec,1,0)
}

vec <- unlist(unique(job %>% filter(Scientist==0,Engineer==0,Analyst==0) %>% select(job_title)))
length(vec) # vec guarda los puestos de trabajo que están indefinidos

# Todo el for completo queda:
for (i in 1:length(job$job_title)){
  jobvec <- unlist(strsplit(job$job_title[i]," ")) # podría haber usado grepl()
  if ("Scientist"%in%jobvec ||"Engineer"%in%jobvec||"Analyst"%in%jobvec){
      job$Scientist[i] <- ifelse("Scientist"%in%jobvec,1,0) # con la funcion sería: grepl("Scientist",job$job_title[i])
      job$Engineer[i] <- ifelse("Engineer"%in%jobvec,1,0) # grepl("Engineer",job$job_title[i])
      job$Analyst[i] <- ifelse("Analyst"%in%jobvec,1,0) # grepl("Analyst",job$job_title[i])
  } else{
    job$Scientist[i] <- ifelse(job$job_title[i]%in%vec[1:11],1,0)
    job$Engineer[i] <- ifelse(job$job_title[i]%in%vec[12:23],1,0)
    job$Analyst[i] <- ifelse(job$job_title[i]%in%vec[24:34],1,0) 
  }
}
# ------------------------------------------------------------------------------

#4) SMOTE
#a) Realizar un ejercicio de oversampling 

library(smotefamily)

data_completa <- data[complete.cases(data),]
data_completa$employment_type <- as.factor(data_completa$employment_type)
data_completa['ft'] = as.factor(as.numeric(data_completa$employment_type == 'FT'))
mean(as.numeric(as.character(data_completa$ft))) # 99% de FT

# Crear un dataframe solo on las variables y un vector con las respuestas de la variable objetivo
predictoras_variables <- data_completa %>% select(salary,remote_ratio) %>% as_tibble() 
respuesta_variable <- as.numeric(data_completa$ft)   # Only select response variable
# El caso minoritario debe tener el nivel de factor de 1, por defecto lo tiene codificado como 0.
#levels(respuesta_variable) <- c('1', '0') 
smoted_data <- SMOTE(predictoras_variables,target= as.numeric(respuesta_variable),K=3)
oversampled = smoted_data$data # los nuevos datos creados

#b) Comparar que las variables salary_in_usd,remote_ratio antes y despues del oversampling,
#b.1) ?Tiene la misma media?

# hacer t.test()
nrow(data)
nrow(oversampled)

ggplot()+geom_density(aes(data$salary), fill='red',alpha = 0.5)+
  geom_density(aes(oversampled$salary), fill='blue',alpha = 0.5)+
  scale_x_log10()

t.test(data$salary,oversampled$salary)
var.test(data$salary,oversampled$salary)
#b.2) ?Tiene el mismo desvio?

ggplot()+geom_density(aes(data$remote_ratio), fill='red',alpha = 0.5)+
  geom_density(aes(oversampled$remote_ratio), fill='blue',alpha = 0.5)
t.test(data$remote_ratio,oversampled$remote_ratio)
var.test(data$remote_ratio,oversampled$remote_ratio)
