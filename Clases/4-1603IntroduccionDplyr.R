# Opcion 1
dplyr::case_when(iris$Sepal.Width>4~'Grande',
                 iris$Sepal.Width<3.5~'Medio',
                 T~'Chico')

# Opcion 2
library(dplyr)
a = case_when(iris$Sepal.Width~'Grande',
                 iris$Sepal.Width<3.5~'Medio',
               T~'Chico')
# Crea una columna nueva que clasifique en "chico", "mediano" y "grande"
# a los anchos del sepalo de la base de datos iris, luego filtra a los chicos
# y trae el promedio del ancho del sepalo por especie

iris %>% mutate(
  nombre= case_when(
    Sepal.Width<3 ~ 'Chico',
    Sepal.Width<3.5 ~ 'Mediano',
    T~'Grande'
  )
) %>% filter(nombre=='Chico') %>%
  group_by(Species) %>%
  summarise(Sepal.Width=mean(Sepal.Width))

# Muestra Estratificada 
iris %>%
  group_by(Species) %>% sample_n(3)
# Sumarizacion de grupos
iris %>%
  group_by(Species) %>% summarise(mean(Petal.Length))

a = c(1000, 3, 45,6,6,3,23,5,5)
a[-1]

rep(c(2,3,4,6),0)

convertir_pesos = function(x){
  dolares = x/400
  return(dolares)
}
convertir_pesos(1000)

df = data.frame(salarios_pesos = rnorm(1000, 150000)) %>% as_tibble() 
df %>%
  mutate(salarios_dolares = convertir_pesos(salarios_pesos))

# Armar un loop para todos los numeros fibonacci haste el 1000
nums = c(0,1)
options(scipen = 999)
for(i in 3:100){
  nums[i] = nums[i-1] + nums[i-2]
  nums = append(nums, nums[i])
}
nums
