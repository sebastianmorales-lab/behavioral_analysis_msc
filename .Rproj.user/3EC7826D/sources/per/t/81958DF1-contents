# Cargar las librerías necesarias

library(corrplot)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

# --- 1. Preparar los datos: Seleccionar y limpiar las columnas de interés ---
df_2<- read.csv('./data/df_2.csv') 
psico = read.csv('./psicologicos/psicologicos.csv')

data <- muestra %>%
  left_join(
    df_2 %>% select(codigo, setdiff(names(df_2), names(muestra))),
    by = "codigo"
  )


# Columnas de interés para la correlación
columnas_interes_cuestionarios <- c(
                                    "codigo", "BECK_SESION", "LSAS_Total", 
                                    "STAI_ESTADO", "STAI_RASGO", "BAS", "BIS",
                                    "FNE", "CBAS","ACIPS","MCOI_LIC",
                                    "MCOI_HCO","MCOI_ADCA","MCOI_SDCO" ,         
                                    "PANAS_POSITIVO", "PANAS_NEGATIVO","RRS",                
                                    "SELF_CRITICAL","SCS","CERQ", "STIP"
                                    )

# Filtrar el dataframe para incluir solo estas columnas
# y asegurar que sean numéricas (ya lo son según tu str(), pero es buena práctica)
# Agrupamos por 'code' y tomamos el primer valor de cada cuestionario
# para tener una fila por participante, ya que estas son variables de cuestionario
# que no cambian en cada trial.
data_cuestionarios_seleccionados <- data %>%
  select(all_of(columnas_interes_cuestionarios)) %>%
  group_by(codigo) %>%
  # summarise(
  #   BECK_SESION = first(BECK_SESION),
  #   LSAS_Total = first(LSAS_Total),
  #   STAI_ESTADO = first(STAI_ESTADO),
  #   STAI_RASGO = first(STAI_RASGO)
  # ) %>%
  ungroup() %>%
  # Excluir la columna 'code' para el cálculo de correlación
  select(-codigo)

# Opcional: Revisar la estructura y las primeras filas de tus datos preparados
# str(data_cuestionarios_seleccionados)
# head(data_cuestionarios_seleccionados)

# Manejar valores NA: Eliminar filas con valores faltantes para estas columnas
data_cuestionarios_limpios <- na.omit(data_cuestionarios_seleccionados)

# Verificar si hay NAs después de la limpieza
if (sum(is.na(data_cuestionarios_limpios)) > 0) {
  warning("¡Todavía hay NAs en tus datos después de na.omit! Revisa tu preparación.")
}


# --- 2. Calcular la matriz de correlaciones ---
# Usamos el método de Pearson, que es el más común para este tipo de datos.
# 'use = "pairwise.complete.obs"' maneja automáticamente los NA calculando
# la correlación para cada par de variables con los datos disponibles para ese par.
matriz_correlaciones_cuestionarios <- cor(data_cuestionarios_limpios, use = "pairwise.complete.obs")

# Opcional: Imprimir la matriz de correlaciones para ver los valores
print("Matriz de Correlaciones (Pearson):")
print(round(matriz_correlaciones_cuestionarios, 2))

# Calcular p-valores para cada correlación existente
calcular_pvalores <- function(datos) {
  vars <- colnames(datos)
  n <- length(vars)
  p_mat <- matrix(NA, n, n, dimnames = list(vars, vars))
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      test <- cor.test(datos[[i]], datos[[j]], method = "pearson")
      p_mat[i, j] <- p_mat[j, i] <- test$p.value
    }
  }
  diag(p_mat) <- 0
  return(p_mat)
}

p_valores_cuestionarios <- calcular_pvalores(data_cuestionarios_limpios)

# Función para etiquetar con asteriscos
asteriscos <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

matriz_significancia <- asteriscos(p_valores_cuestionarios)
matriz_significancia

# Redondeamos la matriz de correlaciones y la convertimos a character
cor_redondeada <- round(matriz_correlaciones_cuestionarios, 2)
cor_redondeada <- apply(cor_redondeada, 2, as.character)

# Convertimos la matriz de significancia a character
matriz_significancia <- apply(matriz_significancia, 2, as.character)

# Aseguramos que las filas y columnas tengan los mismos nombres
rownames(cor_redondeada) <- colnames(cor_redondeada)
rownames(matriz_significancia) <- colnames(cor_redondeada)

# Combinamos correlaciones y asteriscos
matriz_combinada <- matrix(
  paste0(cor_redondeada, matriz_significancia),
  nrow = nrow(cor_redondeada),
  ncol = ncol(cor_redondeada),
  dimnames = list(colnames(cor_redondeada), colnames(cor_redondeada))
)

# Mostrar la matriz simétrica con nombres coincidentes
matriz_combinada


# --- 3. Crear el Heatmap de Correlaciones ---

library(pheatmap)

# Matriz de correlaciones numérica
cor_mat <- cor(data_cuestionarios_limpios, use = "pairwise.complete.obs")

# Generar heatmap
pheatmap(cor_mat,
         color = colorRampPalette(c("blue", "white", "red"))(50), # colores: azul-negativo, blanco-cero, rojo-positivo
         cluster_rows = FALSE,   # mantener el orden original
         cluster_cols = FALSE,
         display_numbers = TRUE, # mostrar los valores de correlación
         number_format = "%.2f", # dos decimales
         fontsize_number = 10,
         main = "Heatmap de Correlaciones entre Cuestionarios"
)
# --- 4 Calcular el p value global para todas las correlaciones ---
library(psych)

# df_cuestionarios: tus datos originales
# Test de Bartlett para la matriz de correlaciones
cortest.bartlett(cor(data_cuestionarios_limpios), n = nrow(data_cuestionarios_limpios))


#--- 5 Correlacion entre edad, sexo y PCA ---

datos <- psico %>% select(sexo,edad,PCA)

cor.test(datos$edad, datos$PCA)
cor.test(datos$sexo, datos$PCA)
cor.test(datos$sexo, datos$edad)

ggplot(datos, aes(x = edad, y = PCA)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relación entre edad y PCA",
    x = "Edad",
    y = "PCA"
  ) +
  theme_minimal()

cor(datos$edad, datos$PCA, use = "complete.obs")

ggplot(datos, aes(x = edad, y = PCA, color = sexo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre edad y PCA por sexo",
    x = "Edad",
    y = "PCA",
    color = "Sexo"
  ) +
  theme_minimal()

datos$sexo <- factor(datos$sexo, levels = c(1, 2, 3))
datos$sexo <- factor(datos$sexo,
                     levels = c(1, 2, 3),
                     labels = c("Sexo 1", "Sexo 2", "Sexo 3"))
library(ggplot2)

ggplot(datos, aes(x = edad, y = PCA, color = sexo)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre edad y PCA por sexo",
    x = "Edad",
    y = "PCA",
    color = "Sexo"
  ) +
  theme_minimal()

ggplot(datos, aes(x = sexo, y = PCA, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribución de PCA por sexo",
    x = "Sexo",
    y = "PCA"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

