# Visualizar betas de SPM vs PCA con análisis de pendientes
# Lee cada archivo input/intensities_*.csv, une con PCA y genera:
# 1. Gráficos de MB y MM por separado
# 2. Análisis estadístico de diferencias en pendientes e interceptos

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(broom)
library(tidyr)

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
                  MB = "#7570b3", MM = "#e7298a")

# DataFrame para almacenar todos los análisis
analisis_global <- list()

# Procesar cada archivo
for (f in files) {
  # Extraer nombre de ROI
  roi_name <- basename(f) %>%
    str_remove("^intensities_") %>%
    str_remove("\\.csv$")
  
  cat("\n", strrep("=", 60), "\n")
  cat("ROI:", roi_name, "\n")
  cat(strrep("=", 60), "\n\n")
  
  # Leer archivo
  df_int <- read_csv(f, show_col_types = FALSE)
  
  # Normalizar IDs
  df_int <- df_int %>%
    mutate(
      codigo = tolower(as.character(participant)) %>%
        str_replace_all("\\s+", "")
    )
  
  # Unir con PCA
  df_plot <- df_int %>%
    left_join(df_psico %>% select(codigo, PCA), by = "codigo") %>%
    filter(!is.na(mean_intensity) & !is.na(PCA)) %>%
    mutate(contrast = as.factor(contrast))
  
  # Validar datos
  if (nrow(df_plot) < 3) {
    cat("  ⚠ Pocas observaciones, saltando...\n")
    next
  }
  
  # Identificar contrastes disponibles
  available_contrasts <- unique(df_plot$contrast)
  cat("Contrastes disponibles:", paste(available_contrasts, collapse = ", "), "\n\n")
  
  # ============================================================
  # ANÁLISIS ESTADÍSTICO POR PAR DE CONTRASTES
  # ============================================================
  
  pares <- list(
    list(c1 = "BB", c2 = "BM", nombre = "BB vs BM"),
    list(c1 = "MB", c2 = "MM", nombre = "MB vs MM")
  )
  
  for (par in pares) {
    c1 <- par$c1
    c2 <- par$c2
    nombre_par <- par$nombre
    
    # Filtrar datos para este par
    df_par <- df_plot %>% filter(contrast %in% c(c1, c2))
    
    if (nrow(df_par) == 0) next
    if (!all(c(c1, c2) %in% unique(df_par$contrast))) next
    
    cat("---", nombre_par, "---\n")
    
    # 1. REGRESIONES SEPARADAS
    modelos_sep <- df_par %>%
      group_by(contrast) %>%
      do(modelo = lm(mean_intensity ~ PCA, data = .)) %>%
      mutate(coef = list(tidy(modelo)))
    
    pendientes <- modelos_sep %>%
      unnest(coef) %>%
      filter(term == "PCA") %>%
      select(contrast, estimate, std.error, p.value)
    
    interceptos <- modelos_sep %>%
      unnest(coef) %>%
      filter(term == "(Intercept)") %>%
      select(contrast, estimate)
    
    cat("\nPENDIENTES (beta ~ PCA):\n")
    print(as.data.frame(pendientes), row.names = FALSE)
    
    cat("\nINTERCEPTOS:\n")
    print(as.data.frame(interceptos), row.names = FALSE)
    
    # Calcular diferencias
    pend_c1 <- pendientes$estimate[pendientes$contrast == c1]
    pend_c2 <- pendientes$estimate[pendientes$contrast == c2]
    diff_pendiente <- pend_c1 - pend_c2
    
    int_c1 <- interceptos$estimate[interceptos$contrast == c1]
    int_c2 <- interceptos$estimate[interceptos$contrast == c2]
    diff_intercepto <- int_c1 - int_c2
    
    cat(sprintf("\nDIFERENCIA DE PENDIENTES (%s - %s): %.6f\n", c1, c2, diff_pendiente))
    cat(sprintf("DIFERENCIA DE INTERCEPTOS (%s - %s): %.6f\n", c1, c2, diff_intercepto))
    
    # 2. ANÁLISIS DE LA DIFERENCIA (c1 - c2) vs PCA
    df_diff <- df_par %>%
      select(codigo, PCA, contrast, mean_intensity) %>%
      pivot_wider(names_from = contrast, values_from = mean_intensity) %>%
      filter(!is.na(!!sym(c1)) & !is.na(!!sym(c2))) %>%
      mutate(diferencia = !!sym(c1) - !!sym(c2))
    
    if (nrow(df_diff) >= 3) {
      lm_diff <- lm(diferencia ~ PCA, data = df_diff)
      coef_diff <- tidy(lm_diff)
      
      cat("\nREGRESIÓN DE LA DIFERENCIA:\n")
      cat(sprintf("  diferencia(%s - %s) ~ PCA\n", c1, c2))
      print(as.data.frame(coef_diff), row.names = FALSE)
      
      # Correlación
      cor_result <- cor.test(df_diff$PCA, df_diff$diferencia)
      cat(sprintf("\nCorrelación PCA vs diferencia: r = %.4f, p = %.4f\n", 
                  cor_result$estimate, cor_result$p.value))
    }
    
    # Guardar análisis
    analisis_global[[paste0(roi_name, "_", nombre_par)]] <- list(
      roi = roi_name,
      par = nombre_par,
      pendientes = pendientes,
      interceptos = interceptos,
      diff_pendiente = diff_pendiente,
      diff_intercepto = diff_intercepto
    )
    
    # ============================================================
    # GRÁFICO 1: CONTRASTES SEPARADOS
    # ============================================================
    
    p1 <- ggplot(df_par, aes(x = PCA, y = mean_intensity, color = contrast)) +
      geom_point(size = 2.5, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
      scale_color_manual(values = paleta_named) +
      labs(
        title = paste0(roi_name, ": ", nombre_par),
        subtitle = sprintf("Δpendiente = %.4f | Δintercepto = %.4f", 
                          diff_pendiente, diff_intercepto),
        x = "Puntuación síntomas A-D (PCA)",
        y = "Beta (activación BOLD)",
        color = "Contraste"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        legend.position = "bottom"
      )
    
    plot_file1 <- file.path(out_dir, 
                           paste0("beta_pca_", roi_name, "_", c1, "_", c2, ".png"))
    ggsave(filename = plot_file1, plot = p1, width = 8, height = 6, dpi = 300)
    
    # ============================================================
    # GRÁFICO 2: DIFERENCIA (c1 - c2) vs PCA
    # ============================================================
    
    if (nrow(df_diff) >= 3) {
      p2 <- ggplot(df_diff, aes(x = PCA, y = diferencia)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
        geom_point(size = 3, alpha = 0.7, color = paleta_named[c1]) +
        geom_smooth(method = "lm", se = TRUE, color = paleta_named[c1], 
                   fill = paleta_named[c1], alpha = 0.2, linewidth = 1.2) +
        labs(
          title = paste0(roi_name, ": ", c1, " > ", c2, " vs síntomas A-D"),
          subtitle = sprintf("r = %.3f, p = %.4f", 
                            cor_result$estimate, cor_result$p.value),
          x = "Puntuación síntomas A-D (PCA)",
          y = sprintf("Diferencia de activación (%s - %s)", c1, c2),
          caption = "Línea punteada = sin diferencia entre contrastes"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
          panel.grid.major = element_line(color = "grey90")
        )
      
      plot_file2 <- file.path(out_dir, 
                             paste0("diferencia_", roi_name, "_", c1, "_vs_", c2, ".png"))
      ggsave(filename = plot_file2, plot = p2, width = 8, height = 6, dpi = 300)
    }
    
    cat("\n")
  }
}

# ============================================================
# RESUMEN GLOBAL
# ============================================================

cat("\n", strrep("=", 60), "\n")
cat("RESUMEN GLOBAL DE ANÁLISIS\n")
cat(strrep("=", 60), "\n\n")

# Crear tabla resumen
resumen_df <- bind_rows(lapply(analisis_global, function(x) {
  data.frame(
    ROI = x$roi,
    Par = x$par,
    Diff_Pendiente = x$diff_pendiente,
    Diff_Intercepto = x$diff_intercepto
  )
}))

if (nrow(resumen_df) > 0) {
  print(resumen_df)
  
  # Guardar resumen
  write_csv(resumen_df, file.path(out_dir, "resumen_analisis_pendientes.csv"))
  cat("\n✓ Resumen guardado en: resumen_analisis_pendientes.csv\n")
}

cat("\n✓ Proceso completado\n")
cat("✓ Gráficos y análisis guardados en:", out_dir, "\n")
