#Analisis de Multicorrespondencia
library(tidyverse)
library("FactoMineR")
library("factoextra")
library(ncpen)
library(aweSOM)
library(soc.ca)

hobbies = read_delim("data_MCA_Hobbies.csv",  delim = ";")
hobbies_sup = hobbies %>% select(colnames(hobbies)[19:22])
hobbies = hobbies %>% select(colnames(hobbies)[1:18])
hobbies$TV=as.factor(hobbies$TV)

hobbies %>% View()
hobbies %>% mutate(
  Reading_binary = Reading=="y",
  Cinema_bin = Cinema=="y"
) %>% ggplot(aes(Reading_binary,TV ))+geom_point()+theme_classic()

hobbies_indicator = as_tibble(sapply(hobbies, to.indicators, exclude.base = F))
hobbies_indicator <- data.frame(do.call("cbind", hobbies_indicator))

colnames_indicator_matrix=c()
hobbies_temp=hobbies %>% select(-TV)
for(i in 1:length(colnames(hobbies_temp))){
  colnames_indicator_matrix=c(colnames_indicator_matrix,paste0(colnames(hobbies),"_n")[i],
  paste0(colnames(hobbies),"_y")[i])
  
}
colnames_indicator_matrix=c(colnames_indicator_matrix,"TV_0","TV_1","TV_2","TV_3","TV_4")
colnames(hobbies_indicator)=colnames_indicator_matrix

hobbies_indicator=as_tibble(hobbies_indicator)
hobbies=as_tibble(sapply(hobbies, as.factor))
mcah=MCA(hobbies,graph = T)
mcah$var
round(mcah$svd$vs**2/sum(mcah$svd$vs**2),3) %>% enframe() %>% 
  ggplot(aes(name,value))+geom_line()


mcah$var$contrib

dim(hobbies)
dim(mcah$svd$U)
dim(mcah$svd$V)

summary(mcah)
mcahC=summary(mcah)
mcahC
h3=cbind(hobbies_indicator,mcah$svd$U[,1:2] %>% as_tibble())
ggplot()+geom_point(aes(V1,V2,color=as.factor(Gardening_y)),alpha=0.35,data=h3)+
  labs(color="Gardening")+geom_point(aes(mean(h3$V1[h3$Gardening_y==1]),mean(h3$V2[h3$Gardening_y==1])))+
  geom_text(aes(label="Gardening_y",mean(h3$V1[h3$Gardening_y==1]),mean(h3$V1[h3$Gardening_y==1])))+
  geom_point(aes(mean(h3$V1[h3$Gardening_y==0]),mean(h3$V2[h3$Gardening_y==0])))+
  geom_text(aes(label="Gardening_n",mean(h3$V1[h3$Gardening_y==0]),mean(h3$V1[h3$Gardening_y==0])))+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)


ggplot()+geom_point(aes(V1,V2,color=as.factor(Gardening_y)),alpha=0.05,data=h3)+
  labs(color="Gardening")+geom_point(aes(0,V2,color=as.factor(Gardening_y)),size=2,data=h3)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)

ggplot()+geom_point(aes(V1,V2,color=as.factor(Gardening_y)),alpha=0.05,data=h3)+
  labs(color="Gardening")+geom_point(aes(V1,0,color=as.factor(Gardening_y)),size=2,data=h3)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)

corratioV2 = lm(V2~Gardening_n, h3) %>% summary()
corratioV2$adj.r.squared
corratioV1 = lm(V1~Gardening_n, h3) %>% summary()
corratioV1$adj.r.squared

corratioV2_y = lm(V2~Gardening_y, h3) %>% summary()
#Contribución Absoluta de la variable original Gardening a la construcción de V2
(corratioV2$adj.r.squared+corratioV2_y$adj.r.squared)/J
#Contribución Relativa de la variable original Gardening a la construcción de V2
(corratioV2$adj.r.squared+corratioV2_y$adj.r.squared)/(J*mcah$svd$vs[2])

pk = colSums(hobbies_indicator)/nrow(hobbies)
hobbies_indicator_x = as_tibble(t(t(hobbies_indicator)/pk))
hobbies_indicator_x=hobbies_indicator_x-1
complete_disjointed_table = as.matrix(hobbies_indicator_x)

hobbies_sup=hobbies_sup[,c(1,2)]
hobbies_sup_indicator = as_tibble(sapply(hobbies_sup, to.indicators, exclude.base = F))
#hobbies_sup_indicator <- data.frame(do.call("cbind", hobbies_indicator))
pk_sup = colSums(hobbies_sup_indicator)/nrow(hobbies_sup)
hobbies_indicator_x_sup = as_tibble(t(t(hobbies_sup_indicator)/pk))
hobbies_indicator_x_sup=hobbies_indicator_x_sup-1
complete_disjointed_table_sup = as.matrix(hobbies_indicator_x_sup)



#svd_cdt = svd(complete_disjointed_table)
svd_cdt = svd.triplet(complete_disjointed_table, row.w = 1/nrow(complete_disjointed_table), col.w = pk)
round(svd_cdt$vs**2/sum(svd_cdt$vs**2),4)



X.col.sup <- t(t(hobbies_sup_indicator)/colSums(hobbies_sup_indicator))
coord.col.sup <- crossprod(as.matrix(X.col.sup), svd_cdt$U) *sqrt(svd_cdt$vs)

a=(as.vector(X.col.sup[,1]))
b=(as.vector(svd_cdt$U[,1]))
a%*%b*sqrt(svd_cdt$vs)[1]

a=c(1,2,3)
b=c(4,5,6)
crossprod(a,b)


qualiproy=as_tibble(coord.col.sup[3:10,1:2])
qualiproy["edad"]=rownames(coord.col.sup)[3:10]

qualiproy%>% ggplot()+geom_line(aes(V1,V2))+geom_text(aes(V1,V2,label=edad),data=qualiproy)
hobbies = read_delim("data_MCA_Hobbies.csv",  delim = ";")

X.quanti.sup=hobbies$nb.activitees %>% enframe() %>% select(value)
dim(svd_cdt$U)
coord.quanti.sup <- matrix(NA, ncol(X.quanti.sup), 39)
coord.quanti.sup <- cov.wt(cbind.data.frame(svd_cdt$U, X.quanti.sup), 
                           cor = TRUE,  method = "ML")$cor[-(1:ncol(svd_cdt$U)), 
                                                           1:ncol(svd_cdt$U), 
                                                           drop = FALSE]

coord.quanti.sup[1:2] %>% t  %>% as_tibble() %>% ggplot(aes(V1,V2))+geom_point()+
  geom_vline(xintercept = 0)+geom_hline(yintercept = 0)

dim(svd_cdt$U[,1:2])
dim(mcah$svd$U[,1:2])
J=length(colnames(hobbies))
distance_inds = function(i,j){
  sum((pk/J)*((complete_disjointed_table[i,]-complete_disjointed_table[j,])**2))
}
d1 = c()
for(i in 2:8000){
  d1 = c(d1, distance_inds(1,i))
}

hobbies[c(1,which.min(d1)),] %>% View()
hobbies[c(1,which.max(d1)),] %>% View()

(svd_cdt$vs**2/sum(svd_cdt$vs**2)) %>% enframe() %>% filter(value>0.001) %>% ggplot()+geom_col(aes(name,value))+
  theme_linedraw()
f2 = as_tibble(svd_cdt$U[,1:2])
f2$V1=f2$V1/(svd_cdt$vs[1]**(0.5))
f2$V2=f2$V2/(svd_cdt$vs[2]**(0.5))
f2 %>% ggplot(aes(V1,V2))+geom_point()

library(FactoMineR)
hobbies = read_delim("data_MCA_Hobbies.csv",  delim = ";")
hobbies[,1:22]=as_tibble(sapply(hobbies[,1:22], as.factor))
hobbies$nb.activitees=as.numeric(hobbies$nb.activitees)
mca.res <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)


p1=fviz_mca_var(mcah,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

p1

fviz_screeplot(mcah, addlabels = TRUE, ylim = c(0, 20))
plot(mca.res,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),
     title="Graph of the active categories")
plot(mca.res,invisible=c("ind","var"),hab="quali", 
     palette=palette(c("blue","maroon","darkgreen","black","red")), 
     title="Graph of the supplementary categories")
plot(mca.res,choix="var",title="Graph of the variables")
plot(mca.res,choix="quanti.sup",title="Graph of the continuous variables")
dimdesc(mca.res)


library(tidyverse)

vote<-read.csv("house-votes-84.names")
vote<-read.csv("house-votes-84.data")

colnames(vote)=c("party","handicapped_infants","water_project_cost_sharing",
                 "adoption_of_the_budget_resolution","physician_fee_freeze",
                 "el_salvador_aid","religious_groups_in_schools","anti_satellite_test_ban",
                 "aid_to_nicaraguan_contras","mx_missile","immigration",
                 "synfuels_corporation_cutback","education_spending",
                 "superfund_right_to_sue","crime","duty_free_exports","export_administration_act_south_africa")
vote %>% View()
vote[vote=="?"]=NA
vote=vote[complete.cases(vote),]
mca.res.vote=MCA(vote %>% select(-party))

dt = cbind(mca.res.vote$svd$U[,1:4] %>% as_tibble(), vote["party"]) %>% as_tibble() #%>% ggplot(aes(V1, V2, col=party))+geom_point()

kmvote = kmeans(mca.res.vote$svd$U[,1:2],2,nstart = 25)
vote["c"] = kmvote$cluster
table(vote$c, vote$party)

vote$c[vote$c==1]="democrat"
vote$c[vote$c==2]="republican"
mean(vote$c==vote$party)


summary(mca.res.vote)
fviz_screeplot(mca.res.vote, addlabels = TRUE, ylim = c(0, 50))
p1=fviz_mca_var(mca.res.vote,
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE     # Avoid text overlapping
                )
p1
h3=cbind(vote,mca.res.vote$svd$U[,1:2] %>% as_tibble())
ggplot()+geom_point(aes(V1,V2,color=as.factor(party)),alpha=0.95,data=h3)+
  geom_text(aes(label="Democrat",mean(h3$V1[h3$party=="democrat"]),mean(h3$V1[h3$party=="democrat"])))+
  geom_text(aes(label="Republican",mean(h3$V1[h3$party!="democrat"]),mean(h3$V2[h3$party!="democrat"])))+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)



library(arules)
brand.ratings <- read.csv("http://goo.gl/IQl8nc")

brand.ratings=brand.ratings[, 1:9]




distr=function(x){
  x = as.numeric(x)
  x[x<6]="detractor"
  x[x>=6 & x<8]="neutro"
  x[x>=8]="promotor"
  x
}
distr(brand.ratings$perform)

brand.ratings_discr = as_tibble(sapply(brand.ratings,distr))
brand.ratings_discr$perform
brand.ratings_discr = as_tibble(sapply(brand.ratings,ntile,2))
brand.ratings_discr = sapply(brand.ratings_discr, as.factor)
brand.ratings_discr=as_tibble(brand.ratings_discr)
brand.ratings_discr["brand"]=brand.ratings$brand

mca.res.brand=MCA(brand.ratings_discr)
dim(mca.res.brand$svd$U)
mca.res.brand$svd$U[,1:2]
round(mca.res.brand$svd$vs**2/sum(mca.res.brand$svd$vs**2),3)


plot(mca.res.brand,invisible=c("ind","var"),hab="quali", 
     palette=palette(c("blue","maroon","darkgreen","black","red")), 
     title="Graph of the supplementary categories")

dim(mca.res.brand$svd$U)
dim(mca.res.brand$svd$V)
MCA(hobbies,quali.sup=19:22,quanti.sup=23)

rbind(brand.ratings_discr,mca.res.brand$svd$U[,1:2])

p1=fviz_mca_var(mca.res.brand,
                col.var = "contrib", # Color by contributions to the PC
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE     # Avoid text overlapping
)

h3=cbind(brand.ratings_discr,mca.res.brand$svd$U[,1:2] %>% as_tibble())
ggplot()+geom_point(aes(V1,V2,color=as.factor(value)),alpha=0.35,data=h3)+
  geom_point(aes(label="Arriba mediana (Value)",
                mean(h3$V1[h3$value=="2"]),
                mean(h3$V1[h3$value=="2"])))+
  geom_point(aes(label="Debajo mediana (Value)",
                mean(h3$V1[h3$value=="1"]),
                mean(h3$V2[h3$value=="1"])))+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)


brand.pca <- prcomp(brand.ratings, scale = T)
summary(brand.pc)


p2=fviz_pca_var(brand.pca,repel = TRUE)
gridExtra::grid.arrange(p1,p2,nrow=1)

brand.ratings_discr <- brand.ratings

discr = function(col){
  brand.ratings %>%
    mutate(col=ifelse(col<=6,'bajo',ifelse(col<=8,'neutro','alto'))) %>%
    select(col)
}
a = as_tibble(sapply(brand.ratings_discr,discr))
a %>% funModeling::df_status()
MCA(a)
discr(brand.ratings_discr["trendy"])
brand.ratings_discr["trendy"] <- brand.ratings %>%
  mutate(trendy=ifelse(trendy<=6,'bajo',ifelse(trendy<=8,'neutro','alto'))) %>%
  select(trendy)

brand.ratings_discr["perform"] <- brand.ratings %>%
  mutate(perform=ifelse(perform<=6,'bajo',ifelse(perform<=8,'neutro','alto'))) %>%
  select(perform)

brand.ratings_discr["leader"] <- brand.ratings %>%
  mutate(leader=ifelse(leader<=6,'bajo',ifelse(leader<=8,'neutro','alto'))) %>%
  select(leader)

brand.ratings_discr["latest"] <- brand.ratings %>%
  mutate(latest=ifelse(latest<=6,'bajo',ifelse(latest<=8,'neutro','alto'))) %>%
  select(latest)

brand.ratings_discr["fun"] <- brand.ratings %>%
  mutate(fun=ifelse(fun<=6,'bajo',ifelse(fun<=8,'neutro','alto'))) %>%
  select(fun)

brand.ratings_discr["serious"] <- brand.ratings %>%
  mutate(serious=ifelse(i<=6,'bajo',ifelse(i<=8,'neutro','alto'))) %>%
  select(serious)

brand.ratings_discr["bargain"] <- brand.ratings %>%
  mutate(bargain=ifelse(bargain<=6,'bajo',ifelse(bargain<=8,'neutro','alto'))) %>%
  select(bargain)

brand.ratings_discr["value"] <- brand.ratings %>%
  mutate(value=ifelse(value<=6,'bajo',ifelse(value<=8,'neutro','alto'))) %>%
  select(value)

brand.ratings_discr["rebuy"] <- brand.ratings %>%
  mutate(rebuy=ifelse(rebuy<=6,'bajo',ifelse(rebuy<=8,'neutro','alto'))) %>%
  select(rebuy) 
brand.ratings_discr=as_tibble(brand.ratings_discr)

for (col in colnames(brand.ratings)){
  brand.ratings_discr$col <- brand.ratings %>% 
    mutate(col=ifelse(col<6,'bajo',ifelse(col<9,'medio','alto')))%>% 
    select(col)
}

mca.res.brand = MCA(brand.ratings_discr)

brand.ratings_discr
dt = cbind(brand.ratings_discr[,1:4],brand.ratings[,5:7])
library(PCAmixdata)
brand.ratings_discr$perform %>% unique
brand.ratings_discr["perform"] %>% unique
PCAmixdata::PCAmix(X.quanti = brand.ratings[,1:5],
                   X.quali = brand.ratings_discr["perform"], rename.level = T)

split <- splitmix(dt)
X1 <- split$X.quanti 
X2 <- split$X.quali 
res.pcamix <- PCAmix(X.quanti=X1, X.quali=X2,rename.level=TRUE,
                     graph=FALSE)
par(mfrow=c(2,2))
plot(res.pcamix,choice="ind",coloring.ind=X2$perform,label=FALSE,
     posleg="bottomright", main="Observations")
plot(res.pcamix,choice="levels",xlim=c(-1.5,2.5), main="Levels")
plot(res.pcamix,choice="cor",main="Numerical variables")
plot(res.pcamix,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")