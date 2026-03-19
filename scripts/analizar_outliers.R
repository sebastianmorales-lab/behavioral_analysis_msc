# Análisis de outliers en intensidades beta
# Detecta casos con |z-score| > 2 desvíos en mean_intensity

library(dplyr)
library(readr)
library(stringr)

# Cargar datos psicologicos para contexto
df_psico <- read_csv("psicologicos/psicologicos.csv", show_col_types = FALSE)

# Directorio de resultados
out_dir <- "figuras/beta_pca"
input_dir <- "input"

# Leer lista de intensidades guardada
intensities_rds <- file.path(out_dir, "intensities_list.rds")
if (!file.exists(intensities_rds)) {
  cat("Error: No se encontró intensities_list.rds. Ejecuta primero grafico_beta_pca_clean.R\n")
  quit(status = 1)
}

intensities_list <- readRDS(intensities_rds)

# Procesar cada ROI para detectar outliers
outliers_all <- list()

for (list_name in names(intensities_list)) {
  df_int <- intensities_list[[list_name]]

  # Extraer nombre ROI
  roi_name <- str_remove(list_name, "^intensities_")

  # Normalizar códigos (para referencia, pero PCA ya está en df_int)
  df_int_clean <- df_int %>%
    mutate(
      codigo = tolower(as.character(participant)) %>%
        str_replace_all("\\s+", "")
    ) %>%
    filter(!is.na(mean_intensity) & !is.na(PCA))

  if (nrow(df_int_clean) == 0) {
    message(paste0("Sin datos válidos en ROI ", roi_name))
    next
  }

  # Calcular z-scores por ROI
  df_int_clean <- df_int_clean %>%
    mutate(
      z_score = scale(mean_intensity)[, 1],
      is_outlier = abs(z_score) > 2
    )

  # Extraer outliers
  outliers_roi <- df_int_clean %>%
    filter(is_outlier) %>%
    select(all_of(intersect(c("codigo", "contrast", "mean_intensity", 
                              "std_intensity", "PCA", "z_score"), 
                            names(.)))) %>%
    mutate(
      roi = roi_name,
      outlier_type = ifelse(z_score > 0, "high", "low")
    ) %>%
    arrange(desc(abs(z_score)))

  if (nrow(outliers_roi) > 0) {
    outliers_all[[roi_name]] <- outliers_roi
    cat(sprintf("\n=== ROI: %s ===\n", roi_name))
    cat(sprintf("Outliers detectados: %d\n", nrow(outliers_roi)))
    print(outliers_roi %>%
          select(codigo, contrast, mean_intensity, z_score, outlier_type))
  }
}

# Consolidar todos los outliers
if (length(outliers_all) > 0) {
  outliers_consolidated <- bind_rows(outliers_all)

  # Guardar reporte
  outliers_file <- file.path(out_dir, "outliers_analisis.csv")
  write_csv(outliers_consolidated, outliers_file)

  cat(sprintf("\n\nReporte de outliers guardado en: %s\n", outliers_file))
  cat(sprintf("Total de outliers encontrados: %d\n", nrow(outliers_consolidated)))

  # Resumen por tipo
  summary_by_type <- outliers_consolidated %>%
    count(outlier_type) %>%
    rename(count = n)
  cat("\nResumen por tipo:\n")
  print(summary_by_type)

} else {
  cat("No se encontraron outliers (|z-score| > 2) en los ROIs.\n")
}
