library(tidyverse)
acceso_info_publica <- read_delim("~/Desktop/Analitica Descriptiva/Trabajo Practico/acceso_info_publica.csv"  , delim = ';')

dim(acceso_info_publica)
filas <- nrow(acceso_info_publica)
ncol(acceso_info_publica)

head(acceso_info_publica)
tail(acceso_info_publica)
colnames(acceso_info_publica)

#1------ Analis exploratorio: Informacion preliminar
unique(acceso_info_publica$dependencia)
unique(acceso_info_publica$tipo_solicitante)
unique(acceso_info_publica$categoria_tema)
unique(acceso_info_publica$dependencia_ministerio)
unique(acceso_info_publica$ministerio)
unique(acceso_info_publica$estado)
unique(acceso_info_publica$calidad_respuesta)



library(dplyr)
#2------ Preguntas a la base de datos
#2.1 Dependencia
#2.1.1 Cantidad de registros por dependencia, ordenado de mayor a menor
cantXDependencia <- acceso_info_publica %>% group_by(dependencia) %>% summarise(cantidad = n()) %>% filter(dependencia != 'NA') %>% arrange(-cantidad)
cantXDependencia

#2.1.2 Promedio de tiempo de respuesta en funcion a dependencia, ordenado de mayor a menor
promedioXdependencia <- acceso_info_publica %>% group_by(dependencia) %>% summarise(promedio = mean(tiempo_de_respuesta)) %>% 
    filter(promedio != 'NA') %>% arrange(-promedio)
promedioXdependencia

#2.1.3 Dependecia con menor tiempo de respuesta
menorTiempoDependencia <- promedioXdependencia %>% filter(promedio == min(promedio))
menorTiempoDependencia

#2.1.4 Dependecia con mayor tiempo de respuesta
mayorTiempoDependencia <- promedioXdependencia %>% filter(promedio == max(promedio))
mayorTiempoDependencia


#2.2 Tipo de solicitante
#2.2.1 Cantidad de registros por tipo de solicitante, ordenado de mayor a menor
cantXTipoSolicitante <- acceso_info_publica %>% group_by(tipo_solicitante = toupper(tipo_solicitante)) %>% summarise(cantidad = n()) %>% arrange(-cantidad)
cantXTipoSolicitante

#2.2.2 Promedio de tiempo de respuesta en funcion a tipo de solicitante, ordenado de mayor a menor
promedioXtipoSolicitante <- acceso_info_publica %>% filter(!is.na(tiempo_de_respuesta)) %>% group_by(solicitante = toupper(tipo_solicitante)) %>% 
    summarise(promedio = mean(tiempo_de_respuesta)) %>% arrange(-promedio)
promedioXtipoSolicitante

#2.2.3 Tipo de solicitante con menor tiempo de respuesta
menorTiempoTipoSolicitante <- promedioXtipoSolicitante %>% filter(promedio == min(promedio))
menorTiempoTipoSolicitante

#2.2.4 Tipo de solicitante con mayor tiempo de respuesta
mayorTiempoTipoSolicitante <- promedioXtipoSolicitante %>% filter(promedio == max(promedio))
mayorTiempoTipoSolicitante



#2.3 Dependencia de ministerio
#2.3.1 Cantidad de registros por dependencia de ministerio, ordenado de mayor a menor
cantXDependenciaMinisterio <- acceso_info_publica %>% group_by(dependencia_ministerio) %>% summarise(cantidad = n()) %>% 
    filter(dependencia_ministerio != 'NA') %>% arrange(-cantidad)
cantXDependenciaMinisterio

#2.3.2 Promedio de tiempo de respuesta en funcion a dependencia de ministerio, ordenado de mayor a menor
promedioXDependenciaMinisterio <- acceso_info_publica %>% group_by(dependencia_ministerio) %>% summarise(promedio = mean(tiempo_de_respuesta)) %>%
  filter(promedio != 'NA') %>% arrange(-promedio)
promedioXDependenciaMinisterio

#2.3.3 Dependencia de ministerio con menor tiempo de respuesta
menorTiempoDependenciaMinisterio <- promedioXDependenciaMinisterio %>% filter(promedio == min(promedio))
menorTiempoDependenciaMinisterio

#2.3.4 Dependencia de ministerio con mayor tiempo de respuesta
mayorTiempoDependenciaMinisterio <- promedioXDependenciaMinisterio %>% filter(promedio == max(promedio))
mayorTiempoDependenciaMinisterio



#2.4 Ministerio
#2.4.1 Cantidad de registros por ministerio, ordenado de mayor a menor
cantXMinisterio <- acceso_info_publica %>% group_by(ministerio) %>% filter(ministerio != 'NA', tiempo_de_respuesta != 'NA') %>% 
  summarise(cantidad = n()) %>% arrange(-cantidad)
cantXMinisterio

#2.4.2 Promedio de tiempo de respuesta en funcion a ministerio, ordenado de mayor a menor
promedioXMinisterio <- acceso_info_publica %>% group_by(ministerio) %>% filter(tiempo_de_respuesta != 'NA', ministerio != 'NA') %>% 
  summarise(promedio = mean(tiempo_de_respuesta)) %>% arrange(-promedio)
promedioXMinisterio

#2.4.3 Ministerio con menor tiempo de respuesta
menorTiempoMinisterio <- promedioXMinisterio %>% filter(promedio == min(promedio))
menorTiempoMinisterio

#2.4.4 Ministerio con mayor tiempo de respuesta
mayorTiempoMinisterio <- promedioXMinisterio %>% filter(promedio == max(promedio))
mayorTiempoMinisterio



#2.5 Calidad respuesta
#2.5.1 Cantidad de registros por calidad de respuesta, ordenado de mayor a menor
cantXCalidadRespuesta <- acceso_info_publica %>% group_by(calidad_respuesta) %>% summarise(cantidad = n()) %>% 
    filter(calidad_respuesta != 'NA') %>% arrange(-cantidad)
cantXCalidadRespuesta

#2.5.2 Promedio de tiempo de respuesta en funcion a la calidad de respuesta, ordenado de mayor a menor
promedioXCalidad <- acceso_info_publica %>% group_by(calidad_respuesta) %>% filter(tiempo_de_respuesta != 'NA', ministerio != 'NA') %>% 
  summarise(promedio = mean(tiempo_de_respuesta)) %>% arrange(-promedio)
promedioXCalidad

#2.5.3 Calidad de respuesta con menor tiempo de respuesta
menorTiempoCalidad <- promedioXCalidadRespuesta %>% filter(promedio == min(promedio))
menorTiempoCalidad

#2.5.4 Calidad de respuesta con mayor tiempo de respuesta
mayorTiempoCalidad <- promedioXCalidadRespuesta %>% filter(promedio == max(promedio))
mayorTiempoCalidad



#2.6 Categoria tema
#2.6.1 Cantidad de registros por categoria tema, ordenado de mayor a menor
cantXCategoriaTema <- acceso_info_publica %>% group_by(categoria_tema) %>% summarise(cantidad = n()) %>% 
  filter(categoria_tema != 'NA') %>% arrange(-cantidad)
cantXCategoriaTema

#2.6.2 Promedio de tiempo de respuesta en funcion a la categoria tema, ordenado de mayor a menor
promedioXCategoriaTema <- acceso_info_publica %>% group_by(categoria_tema) %>% filter(tiempo_de_respuesta != 'NA', ministerio != 'NA') %>% 
  summarise(promedio = mean(tiempo_de_respuesta)) %>% arrange(-promedio)
promedioXCategoriaTema

#2.6.3 Categoria tema con menor tiempo de respuesta
menorTiempoCategoriaTema <- promedioXCategoriaTema %>% filter(promedio == min(promedio))
menorTiempoCalidad

#2.6.4 Categoria tema con mayor tiempo de respuesta
mayorTiempoCategoriaTema <- promedioXCategoriaTema %>% filter(promedio == max(promedio))
mayorTiempoCategoriaTema


#2.7 Categoria tema especifico
#2.7.1 Cantidad de registros por categoria tema especifico ordenado de mayor a menor
cantXCatTemaEspecifico <- acceso_info_publica %>% group_by(categoria_tema_especifico) %>% summarise(cantidad = n()) %>% 
  filter(categoria_tema_especifico != 'NA') %>% arrange(-cantidad)
cantXCatTemaEspecifico

#2.7.2 Promedio de tiempo de respuesta en funcion a categoria tema especifico, ordenado de mayor a menor
promedioXCatTemaEspecifico <- acceso_info_publica %>% group_by(categoria_tema_especifico) %>% filter(tiempo_de_respuesta != 'NA', ministerio != 'NA') %>% 
  summarise(promedio = mean(tiempo_de_respuesta)) %>% arrange(-promedio)
promedioXCatTemaEspecifico

#2.7.3 Categoria tema especifico con menor tiempo de respuesta
menorTiempoCatTemaEspecifico<- promedioXCatTemaEspecifico %>% filter(promedio == min(promedio))
menorTiempoCatTemaEspecifico

#2.7.4 Categoria tema especifico con mayor tiempo de respuesta
mayorTiempoCatTemaEspecifico<- promedioXCatTemaEspecifico %>% filter(promedio == max(promedio))
mayorTiempoCatTemaEspecifico



#2.8 Pedido con mayor tiempo de respuesta en total
View(acceso_info_publica)
nNAS <- acceso_info_publica %>% filter(!is.na(tiempo_de_respuesta))
maximo <- nNAS %>% filter(tiempo_de_respuesta == max(nNAS$tiempo_de_respuesta))
maximo$contenido_respuesta
maximo$tema

#3-------- Cumplimiento a termino
#3.0 Cantidad total que requirieron prorroga
propProrroga <- nrow(acceso_info_publica %>% filter(prorroga == 'SI')) / nrow(acceso_info_publica)
propProrroga

#3.1 Requirieron prorroga por dependencia
prorrogaXDependencia <- acceso_info_publica %>% group_by(dependencia) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionDependencia <- merge(x = cantXDependencia, y = prorrogaXDependencia, by='dependencia')
propCProrrogaDependencia <- combinacionDependencia %>% group_by(dependencia) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaDependencia

#3.2 Requirieron prorroga por tipo de solicitante
prorrogaXTipoSolicitante <- acceso_info_publica %>% group_by(tipo_solicitante) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionTipoSolicitante <- merge(x = cantXTipoSolicitante, y = prorrogaXTipoSolicitante, by='tipo_solicitante')
propCProrrogaTipoSolicitante <- combinacionTipoSolicitante %>% group_by(tipo_solicitante) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal = cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaTipoSolicitante

#3.3 Requirieron prorroga por dependencia de ministerio
prorrogaXDependenciaMinisterio<- acceso_info_publica %>% group_by(dependencia_ministerio) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionDependenciaMinisterio <- merge(x = cantXDependenciaMinisterio, y = prorrogaXDependenciaMinisterio, by='dependencia_ministerio')
propCProrrogaDependenciaMinisterio <- combinacionDependenciaMinisterio %>% group_by(dependencia_ministerio) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal= cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaDependenciaMinisterio

#3.4 Requirieron prorroga por miniterio
prorrogaXMinisterio<- acceso_info_publica %>% group_by(ministerio) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionMinisterio <- merge(x = cantXMinisterio, y = prorrogaXMinisterio, by='ministerio')
propCProrrogaMinisterio <- combinacionMinisterio %>% group_by(ministerio) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal= cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaMinisterio

#3.5 Requirieron prorroga por calidad respuesta
prorrogaXCalidadRespuesta<- acceso_info_publica %>% group_by(calidad_respuesta) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionCalidadRespuesta <- merge(x = cantXCalidadRespuesta, y = prorrogaXCalidadRespuesta, by='calidad_respuesta')
propCProrrogaCalidadRespuesta <- combinacionCalidadRespuesta %>% group_by(calidad_respuesta) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal= cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaCalidadRespuesta

#3.6 Requirieron prorroga por categoria tema
prorrogaXCategoriaTema <- acceso_info_publica %>% group_by(categoria_tema) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionCategoriaTema <- merge(x = cantXCategoriaTema, y = prorrogaXCategoriaTema, by='categoria_tema')
propCProrrogaCategoriaTema <- combinacionCategoriaTema %>% group_by(categoria_tema) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal= cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaCategoriaTema

#3.7 Requirieron prorroga por calidad respuesta especifico
prorrogaXCategoriaTemaEspecifico <- acceso_info_publica %>% group_by(categoria_tema_especifico) %>% filter(prorroga=='SI') %>% summarise(cantidad = n())
combinacionCategoriaTemaEspecifico <- merge(x = cantXCatTemaEspecifico, y = prorrogaXCategoriaTemaEspecifico, by='categoria_tema_especifico')
propCProrrogaCategoriaTemaEspecifico <- combinacionCategoriaTemaEspecifico %>% group_by(categoria_tema_especifico) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal= cantidad.x) %>% arrange(-incumplimiento)
propCProrrogaCategoriaTemaEspecifico

#4-------- Completitud

#4.1 Completitud por dependencia
completitudXDependencia <- acceso_info_publica %>% group_by(dependencia) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudDependencia <- merge(x = cantXDependencia, y = completitudXDependencia, by='dependencia')
propCCompletitudDependencia <- completitudDependencia %>% group_by(dependencia) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudDependencia

#4.2 Completitud por tipo de solicitante
completitudXTipoSolicitante <- acceso_info_publica %>% group_by(tipo_solicitante) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudTipoSolicitante <- merge(x = cantXTipoSolicitante, y = completitudXTipoSolicitante, by='tipo_solicitante')
propCCompletitudTipoSolicitante<- completitudTipoSolicitante %>% group_by(tipo_solicitante) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudTipoSolicitante

#4.3 Completitud por dependencia de ministerio
completitudXDependenciaMinisterio <- acceso_info_publica %>% group_by(dependencia_ministerio) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudDependenciaMinisterio <- merge(x = cantXDependenciaMinisterio, y = completitudXDependenciaMinisterio, by='dependencia_ministerio')
propCCompletitudDependenciaMinisterio<- completitudDependenciaMinisterio %>% group_by(dependencia_ministerio) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudDependenciaMinisterio

#4.4 Completitud por miniterio
completitudXMinisterio <- acceso_info_publica %>% group_by(ministerio) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudMinisterio <- merge(x = cantXMinisterio, y = completitudXMinisterio, by='ministerio')
propCCompletitudMinisterio<- completitudMinisterio %>% group_by(ministerio) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudMinisterio

#4.5 Completitud por cateogira tema
completitudXCategoriaTema<- acceso_info_publica %>% group_by(categoria_tema) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudCategoriaTema <- merge(x = cantXCategoriaTema, y = completitudXCategoriaTema, by='categoria_tema')
propCCompletitudCategoriaTema<- completitudCategoriaTema %>% group_by(categoria_tema) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudCategoriaTema


#4.6 Completitud por cateogira tema especifico
completitudXCategoriaTemaEspecifico<- acceso_info_publica %>% group_by(categoria_tema_especifico) %>% filter(calidad_respuesta=='PARCIAL' | calidad_respuesta=='DENEGATORIA INFUNDADA') %>% summarise(cantidad = n())
completitudCategoriaTemaEspecifico <- merge(x = cantXCatTemaEspecifico, y = completitudXCategoriaTemaEspecifico, by='categoria_tema_especifico')
propCCompletitudCategoriaTemaEspecifico<- completitudCategoriaTemaEspecifico %>% group_by(categoria_tema_especifico) %>% summarise(incumplimiento = cantidad.y / cantidad.x, cantTotal=cantidad.x) %>% arrange(-incumplimiento)
propCCompletitudCategoriaTemaEspecifico
