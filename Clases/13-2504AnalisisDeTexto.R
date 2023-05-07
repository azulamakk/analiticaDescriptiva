rm(list=ls())
gc()

setwd('C:/Users/HP/Desktop/Carpetas/ITBA/clase/Primer Q 2021/Analisis_de_texto/Amazon_Unlocked_Mobile.csv')

library(tidyverse)
library(syuzhet)
library(readr)

listings <- read_csv('C:/Users/HP/Desktop/Carpetas/ITBA/clase/Primer Q 2021/Analisis_de_texto/Amazon_Unlocked_Mobile.csv/listings.csv')

#airbnb<-listings %>% sample_n(100) %>% select(amenities)

airbnb<-listings %>% sample_n(1000)


cleanFun <- function(htmlString) {
  #Saco los tags de html
  t=(gsub("\t","",gsub("\n","",gsub("<.*?>", "",gsub("@","", htmlString)))))
  #Lo paso a minuscula
  t=tolower(t)
  #Le saco los caracteres no alfanumericos
  t=str_replace_all(t, "[[:punct:]]", " ")
  #: ! ' # S % & ' ( ) * + , - . / : ; < = > ? @ [ / ] ^ _ { | } ~
  
  t
  
}

extract_amenities<-function(value,amtns){
  #Con la funcion limpia, separo las palabras en vectores de palabras
  txvector<-unlist(strsplit(value," "))
  #es necesario el unlist() porque sino despues no se puede buscar
  
  #declaro las amenities
  
  
  #Interseccion
  intrs<-intersect(txvector,amtns)
  
  #genero un dataframe vacio para ir poniendo los amenities
  y<-data.frame(matrix(ncol = length(amtns), nrow = 0))
  #le doy el nombre de las columnas con los amenitiesque agrego
  colnames(y)<-amtns
  
  #Me fijo cuales son los que estan
  ams<-(amtns%in%intrs)
  #y lo agrego a mi df de amenities
  y[1,] = ams
  y
}

amtns<-c("cable","parking","smoke","extinguisher")
amenities_df<-data.frame(matrix(ncol = length(amtns), nrow = 0))
colnames(amenities_df)<-amtns

#Esta es la opcion con un forloop
for(i in 1:nrow(airbnb)){
  #limpiar
  t <- cleanFun(airbnb$amenities[i])
  adf<-extract_amenities(t,amtns)
  amenities_df<-rbind(amenities_df,adf)

}
amenities_df<-ifelse(amenities_df==TRUE,1,0)
arbnb1<-cbind(airbnb, amenities_df)
