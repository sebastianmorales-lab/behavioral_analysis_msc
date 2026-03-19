#levanto la biblioteca
library(tidyverse)
library(dplyr)

# cargar el entorno de ejecucion con el paso anterior
load("./input/emocionales.RData")

#uno los data frame df_com (comportamentales) y df_psico (psicologicos)
# con right_join, el criterio es que se trate del mismo sujeto, tenga el mismo code / codigo
#uso el argumento by para indicar por que columna tiene que unir las data frame.

df = right_join(x = df_com, y = df_psico, by =c("code"="codigo"))
df2=  right_join(x = df, y = df_emo, by =c("code"="codigo"))

#Nota: tambien se puede utilizar full_join para unir todas las columnas de ambos dataframe
#pero utilizo raight_join para que se incluyan todos los datos de los participantes que hicieron
# la tarea matchedos con los que completaron los cuestionarios psicologicos,
# no me interesa incluir a los que completaron los cuestionarios pero no realizaron la tarea expermental

#guardo los datos unidos en un solo dataframe
saveRDS(df2,file = "input/datosCompletos2.rds")
save(df2,file = "input/datosCompletos2.RData")

print("Finalizado sin problemas")

