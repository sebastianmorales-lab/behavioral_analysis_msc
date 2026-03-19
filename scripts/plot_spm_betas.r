# Visualizar betas de SPM vs PCA
# Lee cada archivo input/intensities_*.csv, une con PCA y grafica
# NO re-analiza, solo visualiza lo que SPM ya encontró

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# Cargar datos psicológicos
df_psico <- read_csv("psicologicos/psicologicos.csv",
                     show_col_types = FALSE)

# Archivos y salida
input_dir <- "input"
files <- list.files(input_dir, pattern = "^intensities_.*\\.csv$",
                    full.names = TRUE)
out_dir <- "figuras/beta_pca"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Paleta para gráficos
paleta_named <- c(BB = "#1b9e77", BM = "#d95f02", 
                  MB = "#7570b3", MM = "#e7298a",
                  BB_grt_BM = "#66a61e", MB_grt_MM = "#e6ab02")

# Procesar cada archivo
for (f in files) {
  # Extraer nombre de ROI
  roi_name <- basename(f) %>%
    str_remove("^intensities_") %>%
    str_remove("\\.csv$")
  
  message(paste0("\n=== Procesando ROI: ", roi_name, " ==="))
  
  # Leer archivo
  df_int <- read.csv(f, dec =',')
  
 
  # Unir con PCA
  df_plot <- df_int %>%
    left_join(df_psico %>%
                select(codigo, sexo, edad,PCA), by = "codigo") %>%
    mutate(sexo = as.factor(sexo))
  
  # # Graficar
  # p <- ggplot(df_plot, aes(x = PCA, y = mean_intensity, color = sexo)) +
  #   geom_point(size = 2, alpha = 0.7) +
  #   geom_smooth(method = "lm", se = TRUE) +
  #   scale_color_manual(values = paleta_named) +
  #   labs(
  #     title = paste0("ROI: ", roi_name),
  #     x = "PCA",
  #     y = "Intensidad Beta Media (SPM)"
  #   ) +
  #   theme_minimal() +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  #     axis.title = element_text(size = 14),
  #     legend.title = element_blank(),
  #     legend.position = "top"
  #   )
  # Identificar dinámicamente las columnas 2, 3 y 4
  cols_intensidad <- names(df_plot)[2:4]
  
  p <- ggplot(df_plot, aes(x = PCA)) +
    
    # Líneas de regresión para las tres columnas
    lapply(cols_intensidad, function(col) {
      geom_smooth(
        aes(y = .data[[col]], color = col),
        method = "lm", se = FALSE
      )
    }) +
    
    # Puntos para las tres columnas
    lapply(cols_intensidad, function(col) {
      geom_point(
        aes(y = .data[[col]], color = col),
        alpha = 0.7
      )
    }) +
    
    labs(
      title = paste0("ROI: ", roi_name),
      x = "PCA",
      y = "Intensidad Beta",
      color = "Condición"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "top"
    )
  
  
  # Guardar gráfico
  out_file <- file.path(out_dir, paste0("beta_vs_pca_", roi_name, ".png"))
  ggsave(out_file, plot = p, width = 8, height = 6)
  message(paste0("Gráfico guardado en: ", out_file))
  
  # Limpiar entorno para siguiente iteración
  rm(df_int, df_plot, p)
}
  
 

  
 
