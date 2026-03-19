#Con este script analizo las respuestas emocionales frente a jugar con cada una de las categorias.
library(reshape2)
library(stringr)
library(ez)
library(lme4)
library(lmerTest)
library(carData)
library(car)
library(emmeans)
library(ggplot2)
library(lmerTest)
library(cowplot)
library(ggpubr)
library(plotrix)
library(dplyr)
library(plyr)
library(knitr)
library(rmarkdown)


#----LEVANTO DATOS----
emocionales = read.csv("./emocionales/emocionales.csv")
emocionales$codigo=as.factor(emocionales$codigo)
psicologicos = read.csv("./psicologicos/psicologicos.csv")
cuestionarios = right_join(x=emocionales, y= psicologicos, by= "codigo")
cuestionarios <- cuestionarios %>% filter(cuestionarios$descartar==0)

#----FUNCION ANALISIS EMOCION bar plot----
analyse_emotion_estrellas <- function(name, col_ref) {
  col_emocion_desde = which(colnames(cuestionarios)==col_ref);
  col_emocion_hasta = col_emocion_desde + 3;
  col_beck_web = which(colnames(cuestionarios) == "Beck_web")

  emocion_wide = cuestionarios[,c(
    which(colnames(cuestionarios)=="codigo"),
    col_emocion_desde:col_emocion_hasta)];
  #Asegúrate de que la columna Beck_web existe en cuestionarios
  col_beck_web <- which(colnames(cuestionarios) == "Beck_web")
  if (length(col_beck_web) > 0) {
    emocion_wide <- cbind(emocion_wide, cuestionarios[, col_beck_web])
  } else {
    stop("La columna Beck_web no existe en el conjunto de datos cuestionarios.")
  }

  #If there is a N/A in one cell, remove participant in this analysis
  emocion_wide <- emocion_wide[complete.cases(emocion_wide),];

  colnames(emocion_wide) <- c("codigo", "una_estrella", "dos_estrellas", "tres_estrellas", "cuatro_estrellas", "Beck_web")
  emocion=melt(emocion_wide,id=c("codigo"))
  emocion$value = as.numeric(emocion$value)
  print(head(emocion))

  datosGrafico = ddply(emocion,c("variable"),summarize,media=mean(value,na.rm=T),sd = std.error(value,na.rm=T))
  graficoOutcome = ggplot(data = datosGrafico, mapping = aes(x = variable, y = media))
  graficoOutcome=graficoOutcome + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(mapping = aes(x=variable,ymin=pmax(media-sd,0),ymax=media+sd),position = "dodge") +
 scale_x_discrete(labels=c("1 Estrella","2 Estrellas","3 Estrellas","4 Estrellas")) + ylab("Media Emocion Reportada") +   theme(axis.title.x=element_blank()) + scale_y_continuous(limits = c(-1,8)) + xlab("Number of stars of the rival")+ggtitle(name)


  plot(graficoOutcome)

  #anovaOutcome = ezANOVA(data=emocion, dv = value, wid = codigo,within = c("resultadoParticipante","resultadoCojugador"),between = clinico, type = 3, return_aov=TRUE)
  modelo <- lmer(data = emocion, value ~ variable + Beck_web + (1|codigo))
  #print("###### SUMMARY DE MODELO ######")
  #print(summary(modelo))
  anovaOutcome=Anova(modelo,type=3,test.statistic = "F")
  modelo.ref.grid = ref_grid(modelo)
  print("###### RESULTADO DE ANOVA ######")
  print(name)
  print(anovaOutcome)
  print("###### EMMEAN ESTRELLAS ######")
  pairwise.t.test(emocion$value,emocion$variable,p.adjust.method = "bonf",paired = TRUE)
  print(emmeans(modelo.ref.grid,pairwise~variable))
  return(list(modelo.ref.grid, plot(graficoOutcome)))
}

#----FUNCION ANALISIS EMOCION scater plot y barplot----
### Probando efecto del Beck web ###
colores_lineas <- c("green", "yellow", "orange", "red")  # Colores para 1,2,3,4 respectivamente
etiquetas_lineas <- c("1 estrella","2 estrellas","3 estrellas","4 estrellas")

analyse_emotion_estrellas <- function(name, col_ref) {
  col_emocion_desde <- which(colnames(cuestionarios) == col_ref)
  col_emocion_hasta <- col_emocion_desde + 3
  
  emocion_wide <- cuestionarios[, c(
    which(colnames(cuestionarios) == "codigo"),
    col_emocion_desde:col_emocion_hasta
  )]
  
  # If there is a N/A in one cell, remove participant in this analysis
  emocion_wide <- emocion_wide[complete.cases(emocion_wide),]
  
  colnames(emocion_wide) <- c("codigo", "1estrella","2estrellas","3estrellas","4estrellas")
  emocion = melt(emocion_wide, id = c("codigo"))
  emocion$value <- as.numeric(emocion$value)
  
  # Merge Beck_web from cuestionarios to emocion based on codigo
  emocion <- merge(emocion, cuestionarios[, c("codigo", "Beck_web", "sexo")], by = "codigo")
  # Definir los colores para las líneas de tendencia
  colores <- c("green", "yellow", "orange", "red")
  # Convertir la variable 'sexo' a factor para usarla como linetype
  emocion$sexo <- as.factor(emocion$sexo)
  
  #codigo para el grafico de barras
  datosGrafico <- ddply(emocion, "variable", summarize, media = mean(value, na.rm = TRUE), sd = std.error(value, na.rm = TRUE))
  graficoBarra <- ggplot(data = datosGrafico, aes(x = variable, y = media)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(mapping = aes(x = variable, ymin = pmax(media - sd, 0), ymax = media + sd), position = "dodge") +
    scale_x_discrete(labels = c("1 Estrella", "2 Estrellas", "3 Estrellas", "4 Estrellas")) +
    ylab("Media Emocion Reportada") +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(limits = c(-1, 8)) +
    xlab("Number of stars of the rival") +
    ggtitle(name)
  
  plot(graficoBarra)
  # Código gráfico de dispersión con líneas de tendencia separadas por sexo y por variable
  graficoDispersion <- ggplot(data = emocion, aes(x = Beck_web, y = value, color = variable, )) + #linetype = sexo
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, size = 1, alpha = 0.5, se = FALSE) +
    scale_color_manual(values = c("blue", "pink", "green", "orange", "red")) +  # Colores para las variables
    scale_linetype_manual(values = c("solid", "dashed")) +  # Tipo de línea para sexo (M y F)
    ylab("Valor Emoción") +
    theme_minimal() +  # Cambiar tema si lo deseas
    ggtitle("Gráfico de Dispersión con Líneas de Tendencia Separadas por Sexo y Variable")
  
  # Mostrar el gráfico corregido
  print(graficoDispersion)
 # Fit lineal model with random efect 
  modelo <- lmer(data = emocion, value ~ variable * Beck_web + (1|codigo))
  anovaOutcome <- Anova(modelo, type = 3, test.statistic = "F")
  modelo.ref.grid <- ref_grid(modelo)
  
  print("###### RESULTADO DE ANOVA ######")
  print(name)
  print(anovaOutcome)
  
  print("###### EMMEAN ESTRELLAS ######")
  pairwise.t.test(emocion$value, emocion$variable, p.adjust.method = "bonf", paired = TRUE)
  print(emmeans(modelo.ref.grid, pairwise ~ variable))
  
  return(list(modelo.ref.grid, plot(graficoOutcome)))
}

# ----FUNCION ANALISIS EMOCION SCATTER PLOT Y BARPLOT GENERALIZADA----


colores_lineas <- c("green", "yellow", "orange", "red")  # Colores para 1,2,3,4 respectivamente
etiquetas_lineas <- c("1", "2", "3", "4")

analyse_emotion_estrellas <- function(name, col_ref, variable_psicologica) {
  # Verificación de datos requeridos
  if (!exists("cuestionarios", inherits = TRUE)) {
    stop("El objeto 'cuestionarios' no está disponible. Ejecuta la sección de carga de datos antes de llamar a la función.")
  }
  if (!(col_ref %in% colnames(cuestionarios))) {
    stop(paste0("La columna '", col_ref, "' no existe en 'cuestionarios'."))
  }
  if (!all(c("codigo", variable_psicologica, "sexo", "edad") %in% colnames(cuestionarios))) {
    stop("Faltan columnas requeridas en 'cuestionarios' (se esperan 'codigo', variable_psicologica, 'sexo', 'edad').")
  }
  
  col_emocion_desde <- which(colnames(cuestionarios) == col_ref)
  col_emocion_hasta <- col_emocion_desde + 3
  
  emocion_wide <- cuestionarios[, c(
    which(colnames(cuestionarios) == "codigo"),
    col_emocion_desde:col_emocion_hasta
  )]
  
  emocion_wide <- emocion_wide[complete.cases(emocion_wide), ]
  
  colnames(emocion_wide) <- c("codigo", "1estrella", "2estrellas", "3estrellas", "4estrellas")
  emocion <- melt(emocion_wide, id = c("codigo"))
  # Renombrar la columna de factor para coherencia con la función en inglés
  colnames(emocion)[colnames(emocion) == "variable"] <- "Rival"
  # Reetiquetar niveles para que en ejes/leyenda aparezca 1,2,3,4
  emocion$Rival <- factor(
    emocion$Rival,
    levels = c("1estrella", "2estrellas", "3estrellas", "4estrellas"),
    labels = c("1", "2", "3", "4")
  )
  emocion$value <- as.numeric(emocion$value)
  
  emocion <- merge(emocion, cuestionarios[, c("codigo", variable_psicologica, "sexo", "edad")], by = "codigo")
  emocion$sexo <- as.factor(emocion$sexo)
  emocion$edad <- as.numeric(as.character(emocion$edad))
  emocion[[variable_psicologica]] <- as.numeric(as.character(emocion[[variable_psicologica]]))
  emocion <- emocion[complete.cases(emocion[[variable_psicologica]], emocion$value), ]
  
  # Paleta manual adecuada para impresos/publicaciones (alta diferenciación y contraste)
  paleta_rival <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
  
  # Títulos y etiquetas solicitadas
  titulo_grafico <- paste0(name, " frente a la posibilidad de competir según la categoría del rival")
  etiqueta_x_scatter <- if (identical(variable_psicologica, "PCA")) "Puntuación A–D" else variable_psicologica
  
  # Datos para gráfico de barras
  datosGrafico <- ddply(emocion, "Rival", summarize, media = mean(value, na.rm = TRUE), sd = std.error(value, na.rm = TRUE))
  
  graficoBarra <- ggplot(data = datosGrafico, aes(x = `Rival`, y = media)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#6baed6") +
    geom_errorbar(aes(ymin = pmax(media - sd, 0), ymax = media + sd), width = 0.2, linewidth = 0.6) +
    xlab("Categoría del rival") +
    ylab("Nivel de emoción") +
    scale_y_continuous(limits = c(-1, 8)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
  
  print(graficoBarra)
  
  # Gráfico de dispersión
  graficoDispersion <- ggplot(data = emocion, aes(x = !!sym(variable_psicologica), y = value, color = `Rival`)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 1, alpha = 0.5, se = FALSE) +
    scale_color_manual(values = paleta_rival) +
    ylab("Nivel de emoción") +
    xlab(etiqueta_x_scatter) +
    labs(color = "Rival") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
  
  print(graficoDispersion)
  
  # Modelo mixto
  formula <- as.formula(paste("value ~ `Rival` *", variable_psicologica, "+ sexo + edad + (1 | codigo)"))
  modelo <- lmer(formula, data = emocion)
  anovaOutcome <- Anova(modelo, type = 3, test.statistic = "F")
  modelo.ref.grid <- ref_grid(modelo)
  
  # Figura combinada (barplot + scatter) lado a lado
  figuraCombinada <- cowplot::plot_grid(
    graficoBarra, graficoDispersion,
    ncol = 2, align = "h", rel_widths = c(1, 1)
  )
  print(figuraCombinada)
  
  print("###### RESULTADO DE ANOVA ######")
  print(name)
  print(anovaOutcome)
  
  print("###### EMMEAN RIVAL ######")
  pairwise.t.test(emocion$value, emocion$`Rival`, p.adjust.method = "bonf", paired = TRUE)
  print(emmeans(modelo.ref.grid, pairwise ~ `Rival`))
  
  print("##### EMTRENDS #####")
  pends = emtrends(modelo,~`Rival`,var=variable_psicologica)
  print(pends)
  print(contrast(pends,method="pairwise",adjust = "holm"))
  
  return(invisible(list(
    modelo.ref.grid = modelo.ref.grid,
    graficoDispersion = graficoDispersion,
    graficoBarra = graficoBarra,
    figuraCombinada = figuraCombinada
  )))
}

### Funcion analisis emocion con graficos en ingles ----
analyse_emotion_estrellas <- function(name, col_ref, variable_psicologica) {
  # Traducciones de emociones
  traducciones <- c(
    "ira" = "Anger",
    "miedo" = "Fear",
    "tristeza" = "Sadness",
    "alegría" = "Joy",
    "felicidad" = "Happiness",
    "alivio" = "Relief",
    "verguenza" = "Embarrassment",
    "vergüenza" = "Embarrassment",
    "asco" = "Disgust",
    "sorpresa" = "Surprise",
    "nerviosismo" = "Nervousness"
  )
  
  name_lower <- tolower(name)
  titulo_ingles <- if (name_lower %in% names(traducciones)) traducciones[[name_lower]] else name
  
  # Extraer columnas de emoción
  col_emocion_desde <- which(colnames(cuestionarios) == col_ref)
  col_emocion_hasta <- col_emocion_desde + 3
  
  emocion_wide <- cuestionarios[, c(
    which(colnames(cuestionarios) == "codigo"),
    col_emocion_desde:col_emocion_hasta
  )]
  
  emocion_wide <- emocion_wide[complete.cases(emocion_wide), ]
  
  colnames(emocion_wide) <- c("codigo", "1", "2", "3", "4")
  
  # Derretir los datos
  emocion <- melt(emocion_wide, id = c("codigo"))
  colnames(emocion)[colnames(emocion) == "variable"] <- "Rival"
  
  emocion$value <- as.numeric(emocion$value)
  
  # Unir con otras variables
  emocion <- merge(emocion, cuestionarios[, c("codigo", variable_psicologica, "sexo", "edad")], by = "codigo")
  emocion$sexo <- as.factor(emocion$sexo)
  emocion$edad <- as.numeric(as.character(emocion$edad))
  emocion[[variable_psicologica]] <- as.numeric(as.character(emocion[[variable_psicologica]]))
  emocion <- emocion[complete.cases(emocion[[variable_psicologica]], emocion$value), ]
  
  # Datos para gráfico de barras
  datosGrafico <- ddply(emocion, "Rival", summarize,
                        media = mean(value, na.rm = TRUE),
                        sd = std.error(value, na.rm = TRUE))
  
  graficoBarra <- ggplot(data = datosGrafico, aes(x = `Rival`, y = media)) +
    geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
    geom_errorbar(aes(ymin = pmax(media - sd, 0), ymax = media + sd), width = 0.2) +
    xlab("Rival Category") +
    ylab("Emotion Level") +
    scale_y_continuous(limits = c(-1, 8)) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
  
  print(graficoBarra)
  
  # Gráfico de dispersión
  graficoDispersion <- ggplot(data = emocion, aes(x = !!sym(variable_psicologica), y = value, color = `Rival`)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, linewidth = 1, alpha = 0.5, se = FALSE) +
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")) +
    ylab("Emotion Level") +
    xlab(if (identical(variable_psicologica, "PCA")) "A–D score" else variable_psicologica) +
    labs(color = "Rival") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
  
  print(graficoDispersion)
  
  # Modelo mixto
  formula <- as.formula(paste("value ~ `Rival` *", variable_psicologica, "+ sexo + edad + (1 | codigo)"))
  modelo <- lmer(formula, data = emocion)
  anovaOutcome <- Anova(modelo, type = 3, test.statistic = "F")
  modelo.ref.grid <- ref_grid(modelo)
  
  # Figura combinada (barplot + scatter) lado a lado
  figuraCombinada <- cowplot::plot_grid(
    graficoBarra, graficoDispersion,
    ncol = 2, align = "h", rel_widths = c(1, 1)
  )
  print(figuraCombinada)
  
  print("###### ANOVA RESULTS ######")
  print(titulo_ingles)
  print(anovaOutcome)
  
  print("###### EMMEAN RIVAL ######")
  pairwise.t.test(emocion$value, emocion$`Rival`, p.adjust.method = "bonf", paired = TRUE)
  print(emmeans(modelo.ref.grid, pairwise ~ `Rival`))
  
  print("##### EMTRENDS #####")
  pends <- emtrends(modelo, ~`Rival`, var = variable_psicologica)
  print(pends)
  print(contrast(pends, method = "pairwise", adjust = "holm"))
  
  return(invisible(list(
    modelo.ref.grid = modelo.ref.grid,
    graficoDispersion = graficoDispersion,
    graficoBarra = graficoBarra,
    figuraCombinada = figuraCombinada
  )))
}



# Ejemplo de uso de la función para analizar otra variable psicológica:
#analyse_emotion_estrellas("nombre de la variable", "columna_inicial", "variable_psicologica")


# ---- FUNCION ANALISIS EMOCION POR SUJETO----
analyse_emotion_stars_subject <- function(name, col_ref) {
  col_emocion_desde = which(colnames(cuestionarios)==col_ref);
  col_emocion_hasta = col_emocion_desde + 3;
  
  emocion_wide = cuestionarios[,c(
    which(colnames(cuestionarios)=="codigo"),
    col_emocion_desde:col_emocion_hasta)];
  
  #If there is a N/A in one cell, remove participant in this analysis
  emocion_wide <- emocion_wide[complete.cases(emocion_wide),];
  
  colnames(emocion_wide)<-c("codigo","una_estrella","dos_estrellas","tres_estrellas","cuatro_estrellas")
  emocion=melt(emocion_wide,id=c("codigo"))
  emocion$value = as.numeric(emocion$value)
  
  # Plot emotions
  datosGrafico = ddply(emocion,c("codigo","variable"),summarize,media=mean(value,na.rm=T),sd = std.error(value,na.rm=T))
  graficoOutcome = ggplot(data = datosGrafico, mapping = aes(x = variable, y = media, fill=variable))
  graficoOutcome=graficoOutcome + geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(mapping = aes(x=variable,ymin=pmax(media-sd,0),ymax=media+sd),position = "dodge") +
    scale_x_discrete(labels=element_blank())+
    scale_fill_discrete(labels=c("1 estrella","2 estrellas","3 estrellas","4 estrellas")) + 
    ylab("Media Reporte Emocional") +   
    theme(axis.title.x=element_blank()) + 
    scale_y_continuous(limits = c(-1,8)) + 
    xlab("rival category")+
    ggtitle(name)+
    facet_wrap(~codigo)
  
  plot(graficoOutcome)
}

# Elijo la emocion que quiero analizar ----
#BECK
felicidad   <- analyse_emotion_estrellas("felicidad",   "Felicidad_1estrella", "BECK_SESION")
alivio    <- analyse_emotion_estrellas("alivio",    "Alivio_1estrella", "BECK_SESION")
verguenza   <- analyse_emotion_estrellas("verguenza",   "Vergüenza_1estrella" , "BECK_SESION")
nerviosismo       <- analyse_emotion_estrellas("nerviosismo",       "Nerviosismo_1estrella", "BECK_SESION")
#LSAS
felicidad   <- analyse_emotion_estrellas("felicidad",   "Felicidad_1estrella", "LSAS_Total")
alivio    <- analyse_emotion_estrellas("alivio",    "Alivio_1estrella", "LSAS_Total")
verguenza   <- analyse_emotion_estrellas("verguenza",   "Vergüenza_1estrella" , "LSAS_Total")
nerviosismo       <- analyse_emotion_estrellas("nerviosismo",       "Nerviosismo_1estrella", "LSAS_Total")
#STAI_ESTADO
felicidad   <- analyse_emotion_estrellas("felicidad",   "Felicidad_1estrella", "STAI_ESTADO")
alivio    <- analyse_emotion_estrellas("alivio",    "Alivio_1estrella", "STAI_ESTADO")
verguenza   <- analyse_emotion_estrellas("verguenza",   "Vergüenza_1estrella" , "STAI_ESTADO")
nerviosismo       <- analyse_emotion_estrellas("nerviosismo",       "Nerviosismo_1estrella", "STAI_ESTADO")
#STAI_RASGO
felicidad   <- analyse_emotion_estrellas("felicidad",   "Felicidad_1estrella", "STAI_RASGO")
alivio    <- analyse_emotion_estrellas("alivio",    "Alivio_1estrella", "STAI_RASGO")
verguenza   <- analyse_emotion_estrellas("verguenza",   "Vergüenza_1estrella" , "STAI_RASGO")
nerviosismo       <- analyse_emotion_estrellas("nerviosismo",       "Nerviosismo_1estrella", "STAI_RASGO")
#PCA
felicidad   <- analyse_emotion_estrellas("felicidad",   "Felicidad_1estrella", "PCA")
alivio    <- analyse_emotion_estrellas("alivio",    "Alivio_1estrella", "PCA")
verguenza   <- analyse_emotion_estrellas("verguenza",   "Vergüenza_1estrella" , "PCA")
nerviosismo       <- analyse_emotion_estrellas("nerviosismo",       "Nerviosismo_1estrella", "PCA")

#Elijo la emocion que quiero graficar, facetado por sujeto ----
felicidad   <- analyse_emotion_stars_subject("felicidad",   "Felicidad_1estrella")
alivio    <- analyse_emotion_stars_subject("alivio",    "Alivio_1estrella")
vergüenza   <- analyse_emotion_stars_subject("verguenza",   "Verguenza_1estrella")
nerviosismo       <- analyse_emotion_stars_subject("nerviosismo",       "Nerviosismo_1estrella")

dev.off()

