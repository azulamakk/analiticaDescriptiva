library(dplyr)
library(missForest)
library(tidyverse)
#iris_miss = prodNA(iris,0.2)
#iris_imp = missForest(iris_miss)
library(readxl)
bweight_missing <- read_excel("ITBA/GitHub/2- Analisis Exploratorio/Missing-Outliers/Scripts/bweight_missing.XLS")

# porcentaje de faltantes
bweight_missing$married %>% is.na() %>% mean()

bweight_missing_logit_imp = bweight_missing

#Testeo que variables estan significativamente asociadas a la falta del
#dato en la variable "married" (o sea que mi variable dependiente es is.na(married))

#regresion para explicar en funcion del resto de las variables si falta o no falta 

#que tan correlacionado estan el valor que tiene el resto de las variables con el faltante 
#quiero ver si me falta el dato de si esta casada esta vinculado con el valor que toma la variable

model_imp_logit = glm(is.na(as.factor(married))~.,
                      data = bweight_missing_logit_imp, family = 'binomial')

#¿si falta el dato de si esta casada, depende de alguna de las demas variables?

#Eliminar las variables estadisticamente significativas de la falta (es decir, del modelo anterior)
# elimias porque estas introduciendo sesgos, ya que esas variables estan relacionadas 

#no puedo usar estas variables para imputar, estan vinculadasa la falta o la no falta 
# las que faltan estan sub o sobre representadas

#variable dependiente --> si falta el dato o no 

bweight_missing_logit_imp = bweight_missing %>%  select(-smoke,-cigsper)

#quiero predecir el married en funcion del resto de mis variables, habiendo eliminado las var anteriores

model_imp_logit = glm(as.factor(married)~.,
                      data = bweight_missing_logit_imp, family = 'binomial')


#donde falta el dato

bweight_miss=bweight_missing[is.na(bweight_missing$married),]

#uso el modelo para calcular la probabilidad de todas las personas que faltan

bweight_miss['married_prob'] = predict(model_imp_logit,bweight_miss, type='response') %>% as.numeric()
bweight_miss['married_prob']
bweight_miss %>% select(married,married_prob) %>% summary()

#si la probabilidad es mayora 0.5 me quedo con que estan casados

bweight_miss['married_imp'] = as.numeric(bweight_miss$married_prob>0.5)


##################################################################################################################

glm(as.factor(is.na(m_wtgain))~.,
    data = bweight_missing, family = 'binomial') %>% summary()

bweight_missing_lm_imp = bweight_missing #%>% select(-smoke,-cigsper)
model_imp_lm = lm(m_wtgain~.,
                  data = bweight_missing_lm_imp)
model_imp_lm %>% summary()

bweight_miss=bweight_missing[is.na(bweight_missing$m_wtgain),]
bweight_miss['m_wtgain_pred'] = predict(model_imp_lm,bweight_miss) %>% as.numeric()
