x1=seq(100)+rnorm(100,mean = 0,sd=10)
x2=200+seq(100)+rnorm(100,mean = 2,sd=10)
p=(exp(5+0.8*x1-0.15*x2)/(1+exp(5+0.8*x1-0.15*x2)))+runif(n = 100,min = -0.5,max = 0.5)
z=seq(length(p))

for (i in seq(length(p))) {
  if (p[i]>0.5){
    z[i]=1
  }else{
    z[i]=0
  }
}
y<-200+0.75*x1-1.5*x2+0.15*z +rnorm(100,mean = 0,sd=5)

df= cbind(x1,x2,z,y)
df=as.data.frame(df)

dff=df

dff$z[15]=NA
dff$z[25]=NA
dff$z[75]=NA

dfpred= dff[is.na(dff$z),]
dfpred 

dftoprove= dff[!is.na(dff$z),]
head(dftoprove)

library(dplyr)
library(tidyverse)
set.seed(1)

#create ID column
dftoprove$id <- 1:nrow(dftoprove)

#use 70% of dataset as training set and 30% as test set 
train <- dftoprove %>% sample_frac(0.70)
test  <- dplyr::anti_join(dftoprove, train, by = 'id')

train=train[,(names(train)!="id")]
test=train[,(names(train)!="id")]
logit=glm(z ~ x1 + x2 + y, data=train) 
summary(logit)

test$pred=predict(object = logit,newdata = test)
test$predbin = (test$pred > 0.5) * 1
head(test)

library(caret)
example <- confusionMatrix(as.factor(test$predbin), as.factor(test$z))
example

dff[is.na(dff)]=predict(object = logit,newdata = dfpred)
dff$z[dff$z>0.5] = 1
dff$z[dff$z<=0.5] = 0
head(dff)
dff[c(15,25,75),]
df[c(15,25,75),]

# Regresión Lineal ------------------------------------------------------------

dff$y[5]=NA
dff$y[50]=NA
dff$y[100]=NA
dff$y[90]=5000

nuevo <- dff[c(5,50,100),]

c(dff$y[5],dff$y[50],dff$y[90],dff$y[100])

# Modelo
modelolin=lm(y ~ x1 + x2 + z, dff)
summary(modelolin)

predict(modelolin, newdata= nuevo)

# Realice los siguientes pasos:
# Separo los registros de los datos faltantes y construyo un set de datos
# Sobre el conjunto de datos que no tiene faltantes, armo un conjunto de entrenamiento y otro de testeo
# Aplicamos la funcion glm sobre los datos de z enfuncion de el resto de los datso del sistema
# Evaluo que tan bueno es nuestro ejercicio condicional con una matriz de confusion y comparo lo obtenido con el resultado no condicional
# Reemplazo los valores faltantes por los predichos y los convierto en categorias


