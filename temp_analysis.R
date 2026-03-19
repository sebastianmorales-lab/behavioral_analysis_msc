# Cargar librerías
library(dplyr)
library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(plotrix)

# Cargar datos
emocionales = read.csv('./emocionales/emocionales.csv')
emocionales$codigo = as.factor(emocionales$codigo)
psicologicos = read.csv('./psicologicos/psicologicos.csv')
cuestionarios = right_join(x=emocionales, y=psicologicos, by='codigo')

# Crear carpeta 'figuras' si no existe
if (!dir.exists("figuras")) {
  dir.create("figuras")
}

# Función de análisis emocional generalizada
analyse_emotion <- function(name, col_ref, variable_psicologica) {
  # Determinar columnas de interés basadas en el parámetro col_ref
  col_var_desde <- which(colnames(cuestionarios) == col_ref)
  col_var_hasta <- col_var_desde + 3
  
  # Subset del dataframe con las columnas específicas
  var_wide <- cuestionarios[, c(
    which(colnames(cuestionarios) == "codigo"),
    col_var_desde:col_var_hasta
  )]
  
  # Eliminar participantes con datos incompletos
  var_wide <- var_wide[complete.cases(var_wide), ]
  
  # Renombrar columnas para trabajar con una estructura estándar
  colnames(var_wide) <- c("codigo", "var_bb", "var_bm", "var_mb", "var_mm")
  
  # Transformar a formato largo
  emocion <- melt(var_wide, id = c("codigo"))
  emocion$value <- as.numeric(emocion$value)
  
  # Unir con la variable psicológica y limpiar los datos
  emocion <- merge(emocion, cuestionarios[, c("codigo", variable_psicologica, "sexo","edad")], by = "codigo")
  emocion[[variable_psicologica]] <- as.numeric(as.character(emocion[[variable_psicologica]]))
  emocion$edad <- as.numeric(emocion$edad)
  emocion$sexo <- as.factor(emocion$sexo)
  emocion <- emocion[complete.cases(emocion[[variable_psicologica]], emocion$value), ]
  
  # Resumir datos para la gráfica
  datosGrafico <- ddply(emocion, c("variable"), summarize,
                        media = mean(value, na.rm = TRUE),
                        sd = std.error(value, na.rm = TRUE))
  
  # Crear la gráfica de barras
  graficoOutcome <- ggplot(data = datosGrafico, mapping = aes(x = variable, y = media)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(mapping = aes(x = variable, ymin = pmax(media - sd, 0), ymax = media + sd),
                  position = "dodge") +
    scale_x_discrete(labels = c("bb", "bm", "mb", "mm")) +
    ylab("Media Reportada") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 18),
          strip.text = element_text(family = 'Anton', face = 'bold', size = 24, hjust = 0)) +
    scale_y_continuous(limits = c(-1, 8)) +
    ggtitle(name)
  
  # Guardar la gráfica de barras
  nombre_archivo_barra <- paste0("figuras/", name, "_", variable_psicologica, "_barras.png")
  png(filename = nombre_archivo_barra, width = 900, height = 700)
  print(graficoOutcome)
  dev.off()

  # Gráfico de dispersión 
  graficoDispersion <- ggplot(data = emocion, aes(x = !!sym(variable_psicologica), y = value, color = variable)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, size = 1, alpha = 0.5, se = FALSE) +
    scale_color_manual(values = c("blue", "pink", "green", "orange", "red")) +
    ylab("Valor Emoción") +
    theme_minimal() +
    ggtitle(paste("Gráfico de Dispersión de ",name, "con", variable_psicologica, " como variable psicológica"))
  
  # Guardar la gráfica de dispersión
  nombre_archivo_disp <- paste0("figuras/", name, "_", variable_psicologica, "_dispersion.png")
  png(filename = nombre_archivo_disp, width = 900, height = 700)
  print(graficoDispersion)
  dev.off()

  # Crear factores para resultados de participante y cojugador
  emocion$resultadoParticipante_bool <- factor(str_detect(emocion$variable, "var_b."))
  emocion$resultadoParticipante <- as.factor(ifelse(emocion$resultadoParticipante_bool == TRUE, 'b', 'm'))
  
  emocion$resultadoCojugador_bool <- factor(str_detect(emocion$variable, "var_.b"))
  emocion$resultadoCojugador <- as.factor(ifelse(emocion$resultadoCojugador_bool == TRUE, 'b', 'm'))
  
  # Ajustar modelo lineal mixto
  formula <- as.formula(paste("value ~ resultadoCojugador * resultadoParticipante *", variable_psicologica, "+ sexo + edad + (1 | codigo)"))
  modelo <- lmer(formula, data = emocion)
  anovaOutcome <- Anova(modelo, type = 3, test.statistic = "F")
  modelo.ref.grid <- ref_grid(modelo)
  print(summary(modelo))
  
  # Resultado Anova y Gráficos
  print("#### Resultado Anova ####")
  print(anovaOutcome)
  
  # Gráficos del modelo y emmeans
  plot(modelo)
  print(emmeans(modelo.ref.grid, ~resultadoParticipante))
  print(emmeans(modelo.ref.grid, ~resultadoCojugador))
  print(emmeans(modelo.ref.grid, pairwise ~ resultadoParticipante * resultadoCojugador))

  # Calcular y comparar pendientes (emtrends)
  print('##### EMTRENDS #####')
  pends <- emtrends(modelo, ~resultadoParticipante * resultadoCojugador, var = variable_psicologica)
  print(pends)
  print(contrast(pends, method = 'pairwise', adjust = 'holm'))

  return(modelo.ref.grid)
}

# print("Analisis para felicidad")
# felicidad   <- analyse_emotion("felicidad", "Felicidad_bb", "BECK_SESION")
# felicidad   <- analyse_emotion("felicidad", "Felicidad_bb", "LSAS_Total")
# felicidad   <- analyse_emotion("felicidad", "Felicidad_bb", "STAI_ESTADO")
# felicidad   <- analyse_emotion("felicidad", "Felicidad_bb", "STAI_RASGO")
# felicidad   <- analyse_emotion("felicidad", "Felicidad_bb", "PCA")

# print("Analisis para Culpa")
# culpa       <- analyse_emotion("culpa", "Culpa_bb", "BECK_SESION")
# culpa       <- analyse_emotion("culpa", "Culpa_bb", "LSAS_Total")
# culpa       <- analyse_emotion("culpa", "Culpa_bb", "STAI_ESTADO")
# culpa       <- analyse_emotion("culpa", "Culpa_bb", "STAI_RASGO")
# culpa       <- analyse_emotion("culpa", "Culpa_bb", "PCA")

# print("Analisis para Decepcion")
# decepcion   <- analyse_emotion("decepcion", "Decepcion_bb", "BECK_SESION")
# decepcion   <- analyse_emotion("decepcion", "Decepcion_bb", "LSAS_Total")
# decepcion   <- analyse_emotion("decepcion", "Decepcion_bb", "STAI_ESTADO")
# decepcion   <- analyse_emotion("decepcion", "Decepcion_bb", "STAI_RASGO")
# decepcion   <- analyse_emotion("decepcion", "Decepcion_bb", "PCA")

# print("Analisis para Enojo")
# enojo      <- analyse_emotion("enojo", "Enojo_bb", "BECK_SESION")
# enojo      <- analyse_emotion("enojo", "Enojo_bb", "LSAS_Total")
# enojo      <- analyse_emotion("enojo", "Enojo_bb", "STAI_ESTADO")
# enojo      <- analyse_emotion("enojo", "Enojo_bb", "STAI_RASGO")
# enojo      <- analyse_emotion("enojo",  "Enojo_bb", "PCA")

# print("Analisis para Tristeza")
# tristeza    <- analyse_emotion("tristeza",  "Tristeza_bb", "BECK_SESION")
# tristeza    <- analyse_emotion("tristeza",  "Tristeza_bb", "LSAS_Total")
# tristeza    <- analyse_emotion("tristeza",  "Tristeza_bb", "STAI_ESTADO")
# tristeza    <- analyse_emotion("tristeza",  "Tristeza_bb", "STAI_RASGO")
# tristeza    <- analyse_emotion("tristeza",  "Tristeza_bb", "PCA")

# print("Analisis para Alivio")
# alivio      <- analyse_emotion("alivio",  "Alivio_bb", "BECK_SESION")
# alivio      <- analyse_emotion("alivio",  "Alivio_bb", "LSAS_Total")
# alivio      <- analyse_emotion("alivio",  "Alivio_bb", "STAI_ESTADO")
# alivio      <- analyse_emotion("alivio",  "Alivio_bb", "STAI_RASGO")
# alivio      <- analyse_emotion("alivio",  "Alivio_bb", "PCA")

# print("Analisis para Vergüenza")
# verguenza   <- analyse_emotion("vergüenza",   "Vergüenza_bb", "BECK_SESION")
# verguenza   <- analyse_emotion("vergüenza",   "Vergüenza_bb", "LSAS_Total")
# verguenza   <- analyse_emotion("vergüenza",   "Vergüenza_bb", "STAI_ESTADO")
# verguenza   <- analyse_emotion("vergüenza",   "Vergüenza_bb", "STAI_RASGO")
# verguenza   <- analyse_emotion("vergüenza",   "Vergüenza_bb", "PCA")

print("Analisis para Nerviosismo")
nerviosismo <- analyse_emotion("nerviosismo", "Nerviosismo_bb", "BECK_SESION")
nerviosismo <- analyse_emotion("nerviosismo", "Nerviosismo_bb", "LSAS_Total")
nerviosismo <- analyse_emotion("nerviosismo", "Nerviosismo_bb", "STAI_ESTADO")
nerviosismo <- analyse_emotion("nerviosismo", "Nerviosismo_bb", "STAI_RASGO")
nerviosismo <- analyse_emotion("nerviosismo", "Nerviosismo_bb", "PCA") 