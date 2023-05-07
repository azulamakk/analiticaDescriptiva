# Ejercicio SMOTE

rm(list=ls())
gc()
setwd("~/Desktop/Analitica Descriptiva/Clases")
library(ggplot2)
library(knitr)
suppressMessages(library(dplyr))
library(readr)


# Leer el archivo y convertir en factor

credit_card_data <-read.csv("creditcard.csv")
credit_card_data$Class<-as.factor(credit_card_data$Class) # convert class to factor
levels(credit_card_data$Class) <- c('Legitimate', 'Fraud') # names of factors
summary(credit_card_data$Class)
prop.table(table(credit_card_data$Class))


# Gráfico de los datos
options(scipen=10000) # remove scientific notation when viewing plot
ggplot(data = credit_card_data, aes(fill = Class)) +
  geom_bar(aes(x = Class))+
  ggtitle("Number of samples in each class", subtitle = "Original dataset")+
  xlab("")+
  ylab("Samples")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# scatter plot original dataset
ggplot(data = credit_card_data, aes(x = V4, y = V7, colour = Class))+
  geom_point()+
  ggtitle("Original dataset")+
  xlab("V4")+
  ylab("V7")+
  geom_point() +
  xlim(-5, 15)+
  ylim(-50, 50)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank())

# Crear un dataframe solo on las variables y un vector con las respuestas de la variable objetivo
predictor_variables <- credit_card_data[,-31] # Select everything except response
response_variable <- credit_card_data$Class   # Only select response variable
# El caso minoritario debe tener el nivel de factor de 1, por defecto lo tiene codificado como 0.
levels(response_variable) <- c('0', '1') 


# 1- Recopilar más datos
# 2- Utilizar las métricas de evaluación correctas
# 3- Undersampling o submuestreo
# 4- El sobremuestreo (SMOTE / ADASYN / DB SMOTE)


# SMOTE

#install.packages("smotefamily")
library(smotefamily)
# El valor K es el número de vecinos más cercanos elegidos para cada caso objetivo en clases minoritarias, 
# y el valor de tamaño duplicado es el número de veces que se duplica el tamaño de la clase minoritaria.

dup_size=284315/492
dup_size

smote_result = SMOTE(predictor_variables,target = response_variable, K = 3, dup_size = 5)

oversampled = smote_result$data # los nuevos datos creados
str(oversampled)

# Gráficos
options(scipen=10000) # remove scientific notation when viewing plot
ggplot(data = oversampled, aes(fill = class)) +
  geom_bar(aes(x = class))+
  ggtitle("Number of samples in each class", subtitle = "Original dataset")+
  xlab("")+
  ylab("Samples")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# scatter plot dataset
ggplot(data = oversampled, aes(x = V4, y = V7, colour = class))+
  geom_point()+
  ggtitle("Oversampled")+
  xlab("V4")+
  ylab("V7")+
  geom_point() +
  xlim(-5, 15)+
  ylim(-50, 50)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank())

TS2<- filter(oversampled, oversampled$class == 1)
nrow(TS2)
prop.table(table(oversampled$class))

#ADASYN Balanced
ADASed <- ADAS(predictor_variables,response_variable,K = 3)
ADASed <- ADASed$data  # extract only the balanced dataset
ADASed$class <- as.factor(ADASed$class)
dim(ADASed)

# Get the count and proportion of each classes
prop.table(table(ADASed$class)) # en ADASYN se hace 50% / 50%

# Scatter Plot ADASYN
ggplot(data = ADASed, aes(x = V4, y = V7, colour = class))+
  geom_point()+
  ggtitle("Adasyn")+
  xlab("V4")+
  ylab("V7")+
  geom_point() +
  xlim(-5, 15)+
  ylim(-50, 50)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank())

# DENSITY BASED SMOTE IMPLEMENTATION

#Density based SMOTE
DBSMOTed <- DBSMOTE(predictor_variables,response_variable)
DBSMOTed <- DBSMOTed$data # extract only the balanced dataset
DBSMOTed$class <- as.factor(DBSMOTed$class)

head(DBSMOTed)

prop.table(table(DBSMOTed$class))

# Ejercicio en Clase de SMOTE

# Caso 1
smote_result_2 = SMOTE(predictor_variables,target = response_variable, K = 3, dup_size = 50)

oversampled_2 = smote_result_2$data # los nuevos datos creados
str(oversampled_2)

ggplot(data = oversampled_2, aes(x = V4, y = V7, colour = class))+
  geom_point()+
  ggtitle("Oversampled K 3")+
  xlab("V4")+
  ylab("V7")+
  geom_point() +
  xlim(-5, 15)+
  ylim(-50, 50)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank())

# Caso 2 
smote_result_3 = SMOTE(predictor_variables,target = response_variable, K = 10, dup_size = 50)

oversampled_3 = smote_result_3$data # los nuevos datos creados
str(oversampled_3)

ggplot(data = oversampled_3, aes(x = V4, y = V7, colour = class))+
  geom_point()+
  ggtitle("Oversampled K 10")+
  xlab("V4")+
  ylab("V7")+
  geom_point() +
  xlim(-5, 15)+
  ylim(-50, 50)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank())

