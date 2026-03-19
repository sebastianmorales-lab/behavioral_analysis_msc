# Script diagnóstico: verificar diferencias entre contrastes por ROI
# Compara estadísticas descriptivas y significancia estadística

library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(stringr)

# Cargar datos psicologicos
df_psico <- read_csv("psicologicos/psicologicos.csv", show_col_types = FALSE)

# Archivos de intensidades
input_dir <- "input"
files <- list.files(input_dir, pattern = "^intensities_.*\\.csv$", full.names = TRUE)

# Crear carpeta de salida
out_dir <- "figuras/beta_pca"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== DIAGNOSTICO: DIFERENCIAS ENTRE CONTRASTES ===\n\n")

for (f in files) {
  roi_name <- basename(f) %>%
    str_remove("^intensities_") %>%
    str_remove("\\.csv$")
  
  # Leer archivo
  df_int <- read_csv(f, show_col_types = FALSE)
  
  # Normalizar IDs
  df_int <- df_int %>%
    mutate(
      codigo = tolower(as.character(participant)) %>%
        str_replace_all("\\s+", "")
    )
  
  # Unir con PCA
  df_join <- left_join(df_int, df_psico %>% select(codigo, PCA), by = "codigo")
  
  # Preparar datos
  df_model <- df_join %>%
    filter(!is.na(mean_intensity) & !is.na(PCA)) %>%
    mutate(contrast = as.factor(contrast))
  
  cat("════════════════════════════════════════\n")
  cat("ROI:", roi_name, "\n")
  cat("════════════════════════════════════════\n\n")
  
  # 1. Estadísticas descriptivas por contraste
  cat("1. ESTADÍSTICAS DESCRIPTIVAS POR CONTRASTE:\n")
  stats <- df_model %>%
    group_by(contrast) %>%
    summarise(
      N = n(),
      Media = mean(mean_intensity, na.rm = TRUE),
      SD = sd(mean_intensity, na.rm = TRUE),
      Min = min(mean_intensity, na.rm = TRUE),
      Max = max(mean_intensity, na.rm = TRUE),
      Rango = Max - Min,
      .groups = "drop"
    )
  print(stats)
  cat("\n")
  
  # 2. Correlación entre PCA e intensidad por contraste
  cat("2. CORRELACION ENTRE PCA Y ACTIVACIÓN POR CONTRASTE:\n")
  for (ctr in unique(df_model$contrast)) {
    sub <- df_model %>% filter(contrast == ctr)
    if (nrow(sub) >= 3) {
      cor_val <- cor(sub$PCA, sub$mean_intensity, use = "complete.obs")
      cat(sprintf("  %s (n=%d): r = %.4f\n", ctr, nrow(sub), cor_val))
    }
  }
  cat("\n")
  
  # 3. Ajuste de modelos lineales simples por contraste
  cat("3. REGRESIONES LINEALES (mean_intensity ~ PCA) POR CONTRASTE:\n")
  for (ctr in unique(df_model$contrast)) {
    sub <- df_model %>% filter(contrast == ctr)
    if (nrow(sub) >= 3) {
      lm_fit <- lm(mean_intensity ~ PCA, data = sub)
      coef_lm <- coef(lm_fit)
      pval <- summary(lm_fit)$coefficients["PCA", "Pr(>|t|)"]
      cat(sprintf("  %s (n=%d): intercept=%.4f, slope=%.4f, p-value=%.4f\n", 
                  ctr, nrow(sub), coef_lm[1], coef_lm["PCA"], pval))
    }
  }
  cat("\n")
  
  # 4. ANOVA: efecto de contraste
  cat("4. ANOVA: ¿Hay diferencias significativas entre contrastes?\n")
  if (length(unique(df_model$contrast)) > 1) {
    aov_result <- aov(mean_intensity ~ contrast, data = df_model)
    aov_pval <- summary(aov_result)[[1]][1, "Pr(>F)"]
    cat(sprintf("  F-test p-value = %.6f %s\n", 
                aov_pval, 
                if (aov_pval < 0.05) "✓ DIFERENCIAS SIGNIFICATIVAS" else "✗ Sin diferencias"))
  }
  cat("\n")
  
  # 5. Modelo mixto con interacción
  cat("5. MODELO MIXTO: mean_intensity ~ PCA * contrast + (1|codigo)\n")
  df_model_mixed <- df_model %>% mutate(contrast = as.factor(contrast))
  model <- tryCatch(
    lmer(mean_intensity ~ PCA * contrast + (1 | codigo), data = df_model_mixed),
    error = function(e) NULL
  )
  
  if (!is.null(model)) {
    print(summary(model))
    cat("\n")
    
    # Extraer coeficientes de la interacción
    coef_model <- fixef(model)
    cat("COEFICIENTES DE INTERES:\n")
    for (i in seq_along(coef_model)) {
      cat(sprintf("  %s = %.6f\n", names(coef_model)[i], coef_model[i]))
    }
  } else {
    cat("  ⚠ No se pudo ajustar el modelo\n")
  }
  
  cat("\n\n")
}

cat("════════════════════════════════════════\n")
cat("Fin del diagnóstico\n")
cat("════════════════════════════════════════\n")
