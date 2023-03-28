bocasSubte <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/bocas-subte/bocas-de-subte.csv', header=TRUE)
bocasSubte

View(bocasSubte)

dfBocasSubte <- data.frame(bocasSubte)
dfBocasSubte

head(bocasSubte)
tail(bocasSubte)
summary(bocasSubte)
colnames(bocasSubte)
nrow(bocasSubte)
ncol(bocasSubte)
length(bocasSubte)
unique(bocasSubte$estacion)