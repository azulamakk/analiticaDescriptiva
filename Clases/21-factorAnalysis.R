####
library(nFactors)
library(GPArotation)
library(tidyverse)
library(ggcorrplot)
library(RColorBrewer)
library(gplots)
library(semPlot)


brand.ratings <- read.csv("http://goo.gl/IQl8nc") %>% as_tibble()
brand.nm = brand.ratings[,1:9]
r.mat = cor(brand.nm)
eigens <- eigen(r.mat)
eigens$values
#Visualización de Correlacion

ggcorrplot(r.mat)

#Cantidad de factores 
#Autovalores
eigens <- eigen(r.mat)
eigens$values/sum(eigens$values) #Sugiere 2 con alrededor del 56% o 3 con el 68% 
(eigens$values/sum(eigens$values)) %>% enframe() %>% ggplot(aes(name,value))+
  geom_col()

#Scree test
eigendf = enframe(eigens$values)
eigendf$random = eigens$values

nS = nScree(r.mat) #Los distintos test sugieren, principalmente, 3 factores
plotnScree(nS) 
?nScree
#Una vez seleccionados 2 factores:
nfactors=2

f2 = factanal(brand.nm, factors=nfactors, rotation = 'oblimin',scores="Bartlett")
factanal(brand.nm, factors=nfactors+1)
semPaths(f2, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)


f2 = factanal(brand.nm, factors=nfactors, rotation = 'none',scores="Bartlett")

f2varimax = factanal(brand.nm, factors=nfactors, rotation = 'varimax')
f2default = factanal(brand.nm, factors=nfactors)
f2oblimin = factanal(brand.nm, factors=nfactors, rotation = 'oblimin')
f2none = factanal(brand.nm, factors=nfactors, rotation = 'none',scores="Bartlett")


brand.scores = f2$scores %>% as_tibble() #%>% ggplot(aes(Factor1,Factor2))+geom_point()

brand.scores["brand"] = brand.ratings[,10]
brand.fa.mean <- aggregate(. ~ brand, data=brand.scores, mean)
names(brand.fa.mean)=c("brand","precio","calidad")
rownames(brand.fa.mean)=brand.fa.mean[, 1]

brand.fa.mean %>% ggplot(aes(precio,calidad,col=brand))+geom_point()

brand.fa.mean <- brand.fa.mean[, -1]
heatmap.2(as.matrix(brand.fa.mean),
           col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
           cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand")






### Example 
library(haven)
dataset = read_sav("SAQ.sav")
dataset = dataset[,1:8]

#1 Statistics makes me cry
#2 My friends will think I'm stupid for not being able to cope with SPSS
#3 Standard deviations excite me
#4 I dream that Pearson is attacking me with correlation coefficients
#5 I don't understand statistics
#6 I have little experience of computers
#7 All computers hate me
#8 I have never been good at mathematics

dt = sapply(dataset,as.numeric) %>% as_tibble()
r.mat = cor(dt)
nS = nScree(r.mat) #Los distintos test sugieren, principalmente, 3 factores
plotnScree(nS) 

f2var = factanal(dt, factors=nfactors, rotation = 'varimax',scores="Bartlett")
semPaths(f2var, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)




library(funModeling)
data(heart_disease)
dt = heart_disease %>% as_tibble()
df_status(dt)
dt$oldpeak
dt = dt %>% select(age,resting_blood_pressure,oldpeak, heart_disease_severity, fasting_blood_sugar)
dt = sapply(dt, as.numeric) %>% as_tibble()
r.mat = cor(dt)

nS = nScree(r.mat) #Los distintos test sugieren, principalmente, 3 factores
plotnScree(nS) 
f2var = factanal(dt, factors=nfactors, rotation = 'varimax',scores="Bartlett")
semPaths(f2var, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)



library(Hmisc) # contains the `describe` function
cr = read_csv("Crime_R.csv")
r.mat = cor(cr)
dim(r.mat)
str(cr)
colnames(cr)
summary(cr)

cr1=scale(cr,TRUE,TRUE)
summary(cr1)
r.mat1 = cor(cr1)


#Autovalores
eigens <- eigen(r.mat) #Sugiere 6 


colnames(r.mat)

nS = nScree(r.mat) #Los distintos test sugieren, principalmente, 3 factores
plotnScree(nS) 


f2var = factanal(cr, factors=6, lower = 0.009, "none",scores="Bartlett", rotation = "varimax")
?factanal
f2var$

semPaths(f2var, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)


crime.scores = f2var$scores %>% as_tibble() #%>% ggplot(aes(Factor1,Factor2))+geom_point()
cr$Southern
crime.scores["Southern"] = cr["Southern"]
crime.fa.mean <- aggregate(. ~ Southern, data=crime.scores, mean)
rownames(crime.fa.mean)=crime.fa.mean[, 1]


crime.fa.mean <- crime.fa.mean[, -1]
heatmap.2(as.matrix(crime.fa.mean),
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand")




### Realizar analisis factorial
bweight = read_csv("Birthweight_reduced_kg_R.csv")
