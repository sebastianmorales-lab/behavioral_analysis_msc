#levanto las bibliotecas
library(tidyverse)

#levanto los datos completos y los asigno al objeto datosCompletos
datosCompletos <- readRDS("./input/datosCompletos2.rds")

#Defino el objeto datos Limpios aplicando filtros sobre los datos completos,
# elimino los sugetos que no me sirven
# Cargar dplyr
library(dplyr)

# Paso 1: Seleccionar las columnas numéricas y convertirlas a carácter

datosCompletos <- datosCompletos %>%
  mutate(
    descartar.ch = as.character(descartar),
    game_type.ch = as.character(game_type),
    rival.n = as.numeric(rival),
    pay_competitive.n = as.numeric(pay_competitive),
    game_type.f = as.factor(game_type),
    rival.f = as.factor(rival),
    pay_competitive.f = as.factor(pay_competitive)
  )

# Paso 2: Filtrar filas según las condiciones necesarias
datosLimpios <- datosCompletos %>%
 filter(game_type.ch != "") %>%
 filter(descartar.ch != 1)


# Revisar la estructura final para verificar la creación de columnas .f
str(datosLimpios)

table(is.na(datosLimpios$game_type))
table(is.na(datosLimpios$descartar))

# library(dplyr)
# 
# datosCompletos %>%
#   group_by(code) %>%  # reemplaza "code" si tu variable de ID se llama distinto
#   summarize(
#     n_total = n(),
#     n_vacios = sum(is.na(game_type.ch) | game_type.ch == "")
#   ) %>%
#   filter(n_total == n_vacios)


#Quitar datos completos para quedarme solo con datos limpios.
rm(datosCompletos)

#Guardo los datosive/Expansion/maestria_task/proyecto_final_MAESTRIA_R/input/datosLimpios.rds")
save(datosLimpios, file = "input/datosLimpios2.RData")
saveRDS(datosLimpios, file = "input/datosLimpios2.rds")

unique(datosLimpios$code)
print("Finalizado sin problemas")
