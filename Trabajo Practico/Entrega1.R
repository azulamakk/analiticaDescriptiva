library(tidyverse)
acceso_info_publica <- read_delim("~/Desktop/Analitica Descriptiva/Trabajo Practico/acceso_info_publica.csv"  , delim = ';')
View(acceso_info_publica)

dim(acceso_info_publica)
nrow(acceso_info_publica)
ncol(acceso_info_publica)

head(acceso_info_publica)
tail(acceso_info_publica)
colnames(acceso_info_publica)

# Analisis exporatorio de la informacion
unique(acceso_info_publica$dependencia)
unique(acceso_info_publica$tipo_solicitante)
unique(acceso_info_publica$categoria_tema)
unique(acceso_info_publica$dependencia_ministerio)
unique(acceso_info_publica$ministerio)
unique(acceso_info_publica$estado)
unique(acceso_info_publica$calidad_respuesta)

