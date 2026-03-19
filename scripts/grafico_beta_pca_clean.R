# Generar gráfico de betas vs PCA
# Lee cada archivo input/intensities_*.csv, une con PCA de psicologicos,
# ajusta modelo mixto mean_intensity ~ PCA + contrast (1|codigo) y guarda figura/resumen

library(ggplot2)
library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(stringr)
library(emmeans)

# Cargar datos psicologicos
df_psico <- read_csv("psicologicos/psicologicos.csv",
                     show_col_types = FALSE)

# Archivos y salida
input_dir <- "input"
files <- list.files(input_dir, pattern = "^intensities_.*\\.csv$",
                    full.names = TRUE)
out_dir <- "figuras/beta_pca"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Paleta para gráficos
paleta <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
paleta_named <- c(BB = paleta[1], BM = paleta[2], MB = paleta[3], MM = paleta[4])


# Listas para almacenar resultados
intensities_list <- list()
models_list <- list()
coefficients_list <- list()

# Procesar cada archivo
for (f in files) {
  # Extraer nombre de ROI
  roi_name <- basename(f) %>%
    str_remove("^intensities_") %>%
    str_remove("\\.csv$")

  # Leer archivo
  df_int <- read_csv(f, show_col_types = FALSE)

  # Normalizar IDs: participant -> codigo (minúsculas, sin espacios)
  # Nuevo formato: SUJETO_XXXXXXX
  df_int <- df_int %>%
    mutate(
      codigo = tolower(as.character(participant)) %>%
        str_replace_all("\\s+", "")
    )

  # Unir con df_psico para obtener PCA
  df_join <- left_join(
    df_int,
    df_psico %>% select(codigo, PCA),
    by = "codigo"
  )

  # Guardar en lista
  list_name <- paste0("intensities_", roi_name)
  intensities_list[[list_name]] <- df_join

  # Preparar datos para modelo (remover NAs)
  df_model <- df_join %>%
    filter(!is.na(mean_intensity) & !is.na(PCA))

  # Validar cantidad de observaciones
  if (nrow(df_model) < 3) {
    message(paste("Pocas observaciones para", roi_name,
                  "(", nrow(df_model), ")"))
    next
  }

  # Ajustar modelo lineal mixto considerando que los contrasts son distintos
  # Incluimos interacción para obtener pendientes (betas) por contraste
  df_model <- df_model %>% mutate(contrast = as.factor(contrast))
  # Nuevo formato: contrast ya contiene BB, BM, MB, MM directamente
  df_model <- df_model %>%
    mutate(contrast_code = as.factor(as.character(contrast)))

  model <- tryCatch(
    lmer(mean_intensity ~ PCA * contrast + (1 | codigo),
         data = df_model),
    error = function(e) NULL
  )

  if (!is.null(model)) {
    models_list[[list_name]] <- model

    # Guardar resumen
    summary_file <- file.path(out_dir,
                              paste0("model_summary_", roi_name,
                                     ".txt"))
    writeLines(capture.output(summary(model)),
               con = summary_file)

    # Extraer pendientes (betas) de PCA por contraste usando emmeans
    em_trends <- tryCatch(
      emtrends(model, var = "PCA", specs = "contrast"),
      error = function(e) NULL
    )

    if (!is.null(em_trends)) {
      tr_df <- as.data.frame(em_trends)
      # identificar columna de especificación (por ejemplo 'contrast')
      possible_spec <- setdiff(names(tr_df), c("SE", "df", "t.ratio", "p.value"))
      # elegir la primera columna no numérica como spec, si existe
      spec_col <- possible_spec[which.max(sapply(tr_df[possible_spec], function(x) !is.numeric(x)))]
      if (is.na(spec_col) || is.null(spec_col)) {
        # fallback: tomar la primera columna que no sea una de las estadísticas
        spec_col <- possible_spec[1]
      }

      # detectar la columna que contiene la tendencia (trend)
      stat_cols <- c(spec_col, "SE", "df", "t.ratio", "p.value")
      trend_col <- setdiff(names(tr_df), stat_cols)[1]
      if (is.null(trend_col) || trend_col == "") {
        # si no encontramos una columna de trend, intentar buscar por patrón
        trend_col <- grep("PCA|trend|slope", names(tr_df), value = TRUE)[1]
      }

      # Normalizar columnas y renombrar a un formato estándar
      if (!is.null(trend_col) && trend_col %in% names(tr_df)) {
        tr_df_standard <- tr_df %>%
          rename(!!spec_col := all_of(spec_col)) %>%
          rename(trend = all_of(trend_col))
      } else {
        tr_df_standard <- tr_df
      }

      # Asegurar que las columnas esperadas existen antes de seleccionar
      available_cols <- names(tr_df_standard)
      cols_to_select <- intersect(c(spec_col, "trend", "SE", "df", "t.ratio", "p.value"), available_cols)

      tr_df_processed <- tr_df_standard %>%
        rename(contrast = all_of(spec_col)) %>%
        mutate(
          roi = roi_name,
          contrast_label = as.character(contrast)
        ) %>%
        select(all_of(intersect(c("roi", "contrast", "contrast_label", "trend", "SE", "df", "t.ratio", "p.value"), names(.))))

      coefficients_list[[list_name]] <- tr_df_processed
      # También guardar por ROI
      coef_file_roi <- file.path(out_dir,
                                 paste0("coeficientes_", roi_name, ".csv"))
      write_csv(tr_df_processed, coef_file_roi)
    }

    # Crear gráfico comparando pares de contrastes
    # Los contrastes ya contienen BB, BM, MB, MM directamente
    # Identificar qué contrastes están presentes en este ROI
    available_contrasts <- unique(df_model$contrast_code)
    message(paste0("ROI: ", roi_name, " — contrastes disponibles: ",
                   paste(available_contrasts, collapse = ", ")))
    
    df_model_pairs <- df_model %>%
      mutate(
        contrast_label = as.factor(as.character(contrast_code)),
        pair = case_when(
          contrast_code %in% c("BB", "BM") ~ "BB vs BM",
          contrast_code %in% c("MB", "MM") ~ "MB vs MM",
          TRUE ~ "other"
        )
      )
    
    # Determinar qué pares están disponibles
    available_pairs <- unique(df_model_pairs$pair[df_model_pairs$pair != "other"])
    message(paste0("  → pares disponibles: ", paste(available_pairs, collapse = ", ")))
    
    # Si no hay pares completos, permitir visualización de contrastes individuales
    if (length(available_pairs) == 0) {
      message(paste0("  ⚠ ROI sin pares BB-BM o MB-MM, mostrando todos los contrastes"))
      df_model_pairs <- df_model %>%
        mutate(
          contrast_label = as.factor(as.character(contrast_code)),
          pair = "todos"
        )
    } else {
      df_model_pairs <- df_model_pairs %>%
        filter(pair != "other")
    }
    
    # Depuración: mostrar conteo por contraste/par en este ROI
    cnt <- df_model_pairs %>% count(pair, contrast_label)
    message(paste0("  Counts por par/contraste:"))
    print(cnt)
    message("  Ejemplo filas (PCA, contrast_label, mean_intensity):")
    print(head(df_model_pairs %>% select(PCA, contrast_label, mean_intensity, std_intensity), 8))

    # Ajustar un lm por contraste (por pares) y construir predicciones con IC
    pred_list_lm <- list()
    ctr_levels <- unique(df_model_pairs$contrast_code)
    
    for (ctr in ctr_levels) {
      sub <- df_model_pairs %>% filter(contrast_code == ctr)
      # Necesitamos suficientes puntos para ajustar lm
      if (nrow(sub) < 2) {
        message(paste0("    Contraste ", ctr, ": solo ", nrow(sub), " obs, saltando LM"))
        next
      }
      lm_fit <- tryCatch(lm(mean_intensity ~ PCA, data = sub), error = function(e) NULL)
      if (is.null(lm_fit)) {
        message(paste0("    Contraste ", ctr, ": error ajustando LM"))
        next
      }
      # crear rango de PCA para predicciones (solo dentro del rango observado por ese contraste)
      newx <- seq(min(sub$PCA, na.rm = TRUE), max(sub$PCA, na.rm = TRUE), length.out = 200)
      preds <- predict(lm_fit, newdata = data.frame(PCA = newx), se.fit = TRUE)
      tval <- qt(0.975, df = lm_fit$df.residual)
      pred_df_ctr <- data.frame(
        PCA = newx,
        fit = preds$fit,
        se.fit = preds$se.fit,
        lower = preds$fit - tval * preds$se.fit,
        upper = preds$fit + tval * preds$se.fit,
        contrast = ctr
      ) %>%
        mutate(
          contrast_label = as.character(contrast),
          pair = case_when(
            contrast %in% c("BB", "BM") ~ "BB vs BM",
            contrast %in% c("MB", "MM") ~ "MB vs MM",
            TRUE ~ "todos"
          )
        )
      pred_list_lm[[ctr]] <- pred_df_ctr
      message(paste0("    ✓ Predicciones ajustadas para ", ctr, " (n=", nrow(sub), ")"))
    }
    pred_df_lm <- bind_rows(pred_list_lm)
    
    # Guardar predicciones detalladas por ROI
    pred_file <- file.path(out_dir, paste0("predicciones_lm_", roi_name, ".csv"))
    if (nrow(pred_df_lm) > 0) {
      write_csv(pred_df_lm, pred_file)
      message(paste0("  → predicciones generadas para ", length(unique(pred_df_lm$contrast_label)), " contraste(s)"))
    } else {
      message(paste0("  ⚠ ADVERTENCIA: No se generaron predicciones para ", roi_name))
    }

    # Construir gráfico: puntos + barras de error + líneas y bandas por lm por contraste
    p <- ggplot(df_model_pairs,
                aes(x = PCA, y = mean_intensity,
                    color = contrast_label)) +
      geom_point(aes(group = interaction(pair, contrast_label)),
                 size = 2, alpha = 0.8,
                 position = position_dodge(width = 0.05)) +
      geom_errorbar(aes(ymin = mean_intensity - std_intensity,
                        ymax = mean_intensity + std_intensity,
                        group = interaction(pair, contrast_label)),
                    width = 0.02,
                    position = position_dodge(width = 0.05),
                    alpha = 0.7) +
      theme_light(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        strip.background = element_rect(fill = "grey95", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA)
      ) +
      scale_color_manual(values = paleta_named) +
      labs(
        title = paste0("ROI: ", roi_name),
        x = "Puntuación A-D",
        y = "Beta (activación)",
        color = "Contraste",
        fill = "Contraste"
      ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right"
      )

    # Agregar facets solo si hay múltiples pares
    if (length(unique(df_model_pairs$pair)) > 1) {
      p <- p + facet_wrap(~ pair, ncol = 2, scales = "free")
    }
    
    # Agregar líneas y bandas predichas si existen
    if (nrow(pred_df_lm) > 0) {
      p <- p +
        geom_ribbon(data = pred_df_lm, aes(x = PCA, ymin = lower, ymax = upper,
                                          fill = contrast_label, group = interaction(pair, contrast_label)),
                    alpha = 0.2, inherit.aes = FALSE) +
        geom_line(data = pred_df_lm, aes(x = PCA, y = fit, color = contrast_label,
                                         group = interaction(pair, contrast_label)), size = 1.2, inherit.aes = FALSE)
    }
    
    # Guardar gráfico
    plot_file <- file.path(out_dir,
                           paste0("beta_pca_", roi_name, ".png"))
    ggsave(filename = plot_file, plot = p, width = 10, height = 6,
           dpi = 300)
    message(paste0("  ✓ Gráfico guardado: ", basename(plot_file)))
  }
}

# Guardar listas de resultados
saveRDS(intensities_list,
        file = file.path(out_dir, "intensities_list.rds"))
saveRDS(models_list,
        file = file.path(out_dir, "models_list.rds"))

# Mensaje final
cat(sprintf("Procesados %d archivos. Salida en: %s\n",
            length(files), out_dir))
