library(dplyr)
library(missForest)
library(tidyverse)
#iris_miss = prodNA(iris,0.2)
#iris_imp = missForest(iris_miss)
library(readxl)
bweight_missing <- read_excel("ITBA/GitHub/2- Analisis Exploratorio/Missing-Outliers/Scripts/bweight_missing.XLS")

bweight_missing$married %>% is.na() %>% mean()

bweight_missing_logit_imp = bweight_missing

#Testeo que variables estan significativamente asociadas a la falta del
#dato en la variable "married" (o sea que mi variable dependiente es is.na(married))
model_imp_logit = glm(is.na(as.factor(married))~.,
                      data = bweight_missing_logit_imp, family = 'binomial')



#Eliminar las variables estadisticamente significativas de la fala (es decir, del modelo anterior)
bweight_missing_logit_imp = bweight_missing %>%  select(-smoke,-cigsper)

model_imp_logit = glm(as.factor(married)~.,
                      data = bweight_missing_logit_imp, family = 'binomial')




bweight_miss=bweight_missing[is.na(bweight_missing$married),]


bweight_miss['married_prob'] = predict(model_imp_logit,bweight_miss, type='response') %>% as.numeric()
bweight_miss['married_prob']
bweight_miss %>% select(married,married_prob) %>% summary()

bweight_miss['married_imp'] = as.numeric(bweight_miss$married_prob>0.5)
######################

glm(as.factor(is.na(m_wtgain))~.,
    data = bweight_missing, family = 'binomial') %>% summary()


bweight_missing_lm_imp = bweight_missing #%>% select(-smoke,-cigsper)
model_imp_lm = lm(m_wtgain~.,
                  data = bweight_missing_lm_imp)
model_imp_lm %>% summary()

bweight_miss=bweight_missing[is.na(bweight_missing$m_wtgain),]
bweight_miss['m_wtgain_pred'] = predict(model_imp_lm,bweight_miss) %>% as.numeric()
