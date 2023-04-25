rm(list = ls())
library(tidyverse)
set.seed(400)
perros <- 
  data.frame(
    id = 1:25,
    peso = round(rnorm(n = 25, mean = 2000, sd = 250), 1),
    alto  = round(rnorm(n = 25, mean = 30, sd = 10), 1), 
    Raza = sample(x = c("akita", "beagle", "collie"), size = 25, 
                  replace = TRUE),
    Vac = sample(x = c("alfa", "beta"), size = 25, replace = TRUE)
  )

View(perros)
colnames(perros)

  perros %>% 
    mutate(Raza = paste("raza", Raza, sep = "_"))

# Ahora creamos una columna llamada valor_raza, que llenamos con unos. 
# De este modo indicaremos la presencia de esta variable al convertir la columna raza
# en varias dummy. Hacemos lo mismo con la columna vac.
  
  perros %>% 
    mutate(Raza = paste("raza", Raza, sep = "_"),
           valor_raza = 1,
           Vac = paste("vac", Vac, sep = "_"),
           valor_vac = 1
    )
  
#  Usamos la función spread para convertir las columnas raza y vac en múltiples columnas. 
#  Esta función hace una transposición, convirtiendo datos altos a datos anchos.
  
# spread nos pide dos argumentos, ???key y value. `key es el nombre de la columna que 
# se usará para nombrar a las nuevas columnas, value es el valor que estas tendrán.
  
  
    
  perros %>% 
    mutate(Raza = paste("raza", Raza, sep = "_"),
           valor_raza = 1,
           Vac = paste("vac", Vac, sep = "_"),
           valor_vac = 1
    ) %>% 
    spread(key = Raza, value = valor_raza) %>% 
    spread(key = Vac, value = valor_vac)
  
## Nuestras variables están casi listas. Tenemos que cambiar los NA por 0. 
## Debemos dar un argumento adicional a spread, fill = 0.  
  
  perros %>% 
    mutate(Raza = paste("raza", Raza, sep = "_"),
           valor_raza = 1,
           Vac = paste("vac", Vac, sep = "_"),
           valor_vac = 1
    ) %>% 
    spread(key = Raza, value = valor_raza, fill = 0) %>% 
    spread(key = Vac, value = valor_vac, fill = 0)
  
  #####
  # Usando fastDummies
  #####
  
  library(fastDummies)

  dummy_cols(perros,  select_columns = c("Raza", "Vac"))  

# Esta función nos devuelve un data frame que conserva las variables originales, 
# así que tenemos que usar select para quitarlas, si así lo deseamos.   
  
Perros=  dummy_cols(perros,  select_columns = c("Raza", "Vac")) %>% 
    select(-c("Raza", "Vac"))

View(Perros)


    