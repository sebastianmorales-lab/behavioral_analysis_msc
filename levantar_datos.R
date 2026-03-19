#cargo la biblioteca
library(tidyverse)

#fijo el directorio de trabajo en el directorio del proyecto. Hay que cambiar el directorio dependiendo
#de como donde se guarda el proyecto.
setwd("/media/usuario/seagate/maestria_task/proyecto_final_MAESTRIA_R")
 

#Leo los archivos csv de los diferentes participantes incluidos en un directorio,
#los pega como filas (formato largo) y posteriormente con mutate convierto
#el codigo y el numero de aplicante en caracter para todos los participantes, uso across
#para poder aplicar la tranformacion en varias columnas a la vez. 
#Convierto las columnas de 'code' y 'nro_aplicante' en caracteres.
df_com = list.files('./rawdata',pattern = "*.csv",full.names = T) %>% 
map_df(.,function (x) return(read_csv(x) %>% 
                               mutate(across(c(code,nro_aplicante),as.character))))

#hago summary para ver cuales son los valores y las filas que contiene NA's
summary(df_com)

# guardo los datos comportamentales obtenidos de los logfile de la tarea como datos de R
save.image(file = './input/comportamentales.RData')


#Levanto los datos, contiene los datos psicologicos que el participante completa en los formularios web
#contiene encabezados y el formato es UTF-8, por eso los argumentos. 
#con mutate convierto el codigo en caracter.
df_psico = read.csv('./psicologicos/psicologicos.csv' , 
                    header = T ,
                    encoding = "UTF-8") %>% mutate(codigo = as.character(codigo)) 

#Hago summary para ver donde contene datos NA's 
summary(df_psico)

#guardo los datos psicologicos como RData
save.image(file = './input/psicologicos.RData')

#Levanto los datos, contiene los datos emocionales que el participante completa en los formularios web
#contiene encabezados y el formato es UTF-8, por eso los argumentos. 
#con mutate convierto el codigo en caracter.
df_emo = read.csv('./emocionales/emocionales.csv' , 
                    header = T ,
                    encoding = "UTF-8") %>% mutate(codigo = as.character(codigo)) 

#Hago summary para ver donde contene datos NA's 
summary(df_emo)

#guardo los datos emocionales como RData
save.image(file = './input/emocionales.RData')





   
