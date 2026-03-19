#Script con los diferentes paso para procesar los datos, 
#el analisis de los resultados se presenta en informe.qmd

#1. se define el directorio de trabajo, como el directorio del proyecto
setwd("media//Expansion/maestria-task/proyecto_final_MAESTRIA_R")
#ATENCIóN: es importante cambiarlo dependendo donde se guarde el proyecto. 

#2. Se levantan los datos comportamentales y psicologicos para esto se llama al scrip levantar_datos.R
source("scripts/levantar_datos.R")

#3. Unir los data frame de datos comportamentales df_com y datos psicologicos df_psico
# guarda los datos completos como datosCompletos.rds
source("scripts/unir_datos.R")

#3. Hacer una limpieza de los datos
source("scripts/limpiar_datos.R")

#4. Visualizar los datos
source("scripts/visualizar_datos.R")

#5. Modelado de los tiempos de reaccion mediante un Modelo Lineal con Efectos Mixtos
source("scripts/modelado.R")

#6 MLG pago y rival
rmarkdown::render("./scripts/modelo_GLM.Rmd", output_format = 'pdf_document',output_file = "Toma de decisiones")
#7. Analisis outcomes
rmarkdown::render("./scripts/analisis_outcomes.Rmd", output_format = 'pdf_document', output_file = "Outcomes")
#8. Analisis emocionales cojugador
source("scritps/estadisticaEmocionesCojugador.R")
# ---- Extras ---- 
#9 Analisis trialxtrial levantar betas
source("scripts/levanto_betas_decision_2.R")
#10 estadistica betas
source("script/estadistica.R")


# ---- Analisis toma de decisiones con PCA ----
#11 Analisis de PCA
source("script/pca.R")
#12 interacciones con cuestionarios y pca
source("script/interacciones.r")
#13 Heatmap de las interacciones entre cuestionarios
source("script/heatmap.r")
