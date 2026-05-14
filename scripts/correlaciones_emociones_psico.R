# Correlaciones: variables emocionales / expectativas / rendimiento vs PCA y BECK_SESION
# Datos: datosLimpios2 (objeto datosLimpios). Formato largo: se agrega por code (un valor por sujeto).
# Graficos: matrices con corrplot; p-valores de cada par ajustados por Holm (comparaciones multiples).

library(dplyr)
library(corrplot)

# Raiz del proyecto (funciona al ejecutar desde scripts/ o desde la raiz con Rscript)
proj_dir <- Sys.getenv("PROYECTO_MAESTRIA_R", unset = "")
if (nzchar(proj_dir)) {
  proj_dir <- normalizePath(proj_dir, winslash = "/", mustWork = TRUE)
} else {
  script_path <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(script_path)) {
    proj_dir <- normalizePath(file.path(dirname(normalizePath(script_path)), ".."), winslash = "/", mustWork = TRUE)
  } else if (basename(getwd()) == "scripts") {
    proj_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
  } else {
    proj_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}

rdata_path <- file.path(proj_dir, "input", "datosLimpios2.RData")
if (!file.exists(rdata_path)) {
  stop("No se encuentra ", rdata_path, ". Defina PROYECTO_MAESTRIA_R con la ruta absoluta al proyecto.")
}

load(rdata_path)
if (!exists("datosLimpios")) {
  stop("Tras cargar ", rdata_path, " no existe el objeto 'datosLimpios'.")
}

datos <- datosLimpios

if (!"code" %in% names(datos)) {
  stop("En datosLimpios no existe la columna 'code' necesaria para agrupar por sujeto.")
}

# --- Definicion de columnas (nombres como en datosLimpios2) ---
vars_expect_rend <- c(
  "Espectativa_propia",
  "Espectativa_otros",
  "Rendimiento_pos_propio",
  "Rendimiento_pos_otros"
)

vars_psico <- c("PCA", "BECK_SESION")

vars_emociones <- c(
  "Feliz", "Enojada", "Nerviosa", "Triste",
  "Culpable", "Avergonzada", "Decepcionada", "Aliviada"
)

todas <- unique(c(vars_expect_rend, vars_psico, vars_emociones))
faltan <- setdiff(todas, names(datos))
if (length(faltan)) {
  stop("Faltan columnas en datosLimpios: ", paste(faltan, collapse = ", "))
}

# --- Un valor por sujeto: la variable se midio una vez y se repite en cada ensayo;
#     por code tomamos el primer valor no NA (equivale al valor unico si no hay NA). ---
primer_no_na <- function(x) {
  v <- x[!is.na(x)]
  if (!length(v)) NA_real_ else v[[1L]]
}

vars_agg <- todas
datos_por_sujeto <- datos %>%
  group_by(code) %>%
  summarise(across(all_of(vars_agg), primer_no_na), .groups = "drop")

message(
  "Filas en datos largos: ", nrow(datos),
  " | Filas unicas por code (un valor por variable): ", nrow(datos_por_sujeto)
)

# --- Utilidad: correlacion cruzada con prueba (n = sujetos con par completo) ---
cross_cor_table <- function(df, cols_x, cols_y, method = "pearson") {
  out <- list()
  for (cx in cols_x) {
    for (cy in cols_y) {
      ok <- complete.cases(df[[cx]], df[[cy]])
      n <- sum(ok)
      if (n < 3L) {
        out[[length(out) + 1L]] <- tibble::tibble(
          variable_x = cx, variable_y = cy,
          r = NA_real_, p_valor = NA_real_, n = n, metodo = method
        )
        next
      }
      ct <- cor.test(df[[cx]][ok], df[[cy]][ok], method = method, exact = FALSE)
      r_est <- unname(ct$estimate)
      if (length(r_est) > 1L) r_est <- r_est["rho"]
      out[[length(out) + 1L]] <- tibble::tibble(
        variable_x = cx, variable_y = cy,
        r = as.numeric(r_est), p_valor = ct$p.value, n = n, metodo = method
      )
    }
  }
  bind_rows(out)
}

# --- Matriz de correlacion + p-valores (Holm sobre todos los pares de la matriz) ---
cor_y_pmat <- function(df, vars, method = "pearson", adjust = "holm") {
  x <- df[, vars, drop = FALSE]
  M <- cor(x, use = "pairwise.complete.obs", method = method)
  pairs <- combn(vars, 2, simplify = FALSE)
  raw_p <- vapply(pairs, function(pr) {
    a <- x[[pr[[1L]]]]
    b <- x[[pr[[2L]]]]
    ok <- complete.cases(a, b)
    if (sum(ok) < 3L) {
      return(NA_real_)
    }
    cor.test(a[ok], b[ok], method = method, exact = FALSE)$p.value
  }, numeric(1))
  adj <- raw_p
  okp <- which(!is.na(raw_p))
  if (length(okp)) {
    adj[okp] <- p.adjust(raw_p[okp], method = adjust)
  }
  p <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
  diag(p) <- 1
  for (i in seq_along(pairs)) {
    pr <- pairs[[i]]
    val <- adj[i]
    if (is.na(val)) val <- 1
    p[pr[[1L]], pr[[2L]]] <- p[pr[[2L]], pr[[1L]]] <- val
  }
  list(cor = M, p = p, method = method, adjust = adjust)
}

guardar_matriz_corr <- function(df, vars, titulo, ruta_png, width = 2200, height = 2000, res = 150) {
  cp <- cor_y_pmat(df, vars, method = "pearson", adjust = "holm")
  grDevices::png(ruta_png, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(1, 0, 3, 0) + 0.1)
  corrplot(
    cp$cor,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.85,
    cl.cex = 0.9,
    mar = c(0, 0, 2, 0),
    title = titulo,
    p.mat = cp$p,
    sig.level = 0.05,
    insig = "pch",
    pch.col = "grey40",
    pch.cex = 1.2,
    diag = FALSE,
    col = colorRampPalette(c("#4B7BB8", "white", "#C85A4A"))(200)
  )
  mtext(
    "Leyenda: la marca 'x' indica correlacion NO significativa (p ajustado Holm > 0.05). Sin 'x': significativa (p Holm <= 0.05).",
    side = 1,
    line = 2.2,
    cex = 0.95,
    adj = 0.5
  )
  invisible(ruta_png)
}

# --- Analisis 1: expectativas / rendimiento vs PCA y Beck (nivel sujeto) ---
tabla_1 <- cross_cor_table(datos_por_sujeto, vars_expect_rend, vars_psico, method = "pearson")
cat("\n=== Analisis 1 (por sujeto): Expectativas y rendimiento vs PCA y BECK_SESION (Pearson) ===\n")
print(as.data.frame(tabla_1), row.names = FALSE)

mat_1 <- cor(
  datos_por_sujeto[, c(vars_expect_rend, vars_psico)],
  use = "pairwise.complete.obs",
  method = "pearson"
)
cat("\nMatriz de correlacion por sujeto (Pearson, pairwise):\n")
print(round(mat_1, 3))

# --- Analisis 2: emociones vs PCA y Beck (nivel sujeto) ---
tabla_2 <- cross_cor_table(datos_por_sujeto, vars_emociones, vars_psico, method = "pearson")
cat("\n=== Analisis 2 (por sujeto): Emociones vs PCA y BECK_SESION (Pearson) ===\n")
print(as.data.frame(tabla_2), row.names = FALSE)

mat_2 <- cor(
  datos_por_sujeto[, c(vars_emociones, vars_psico)],
  use = "pairwise.complete.obs",
  method = "pearson"
)
cat("\nMatriz de correlacion por sujeto (Pearson, pairwise):\n")
print(round(mat_2, 3))

# --- Graficos de matrices (triangulo superior; 'x' = NO significativo con Holm a 0.05) ---
fig_dir <- file.path(proj_dir, "output", "correlaciones_figuras")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

vars_plot_1 <- c(vars_expect_rend, vars_psico)
vars_plot_2 <- c(vars_emociones, vars_psico)

ruta_fig_1 <- file.path(fig_dir, "matriz_corr_expectativa_rendimiento_vs_psico_por_sujeto.png")
ruta_fig_2 <- file.path(fig_dir, "matriz_corr_emociones_vs_psico_por_sujeto.png")

guardar_matriz_corr(
  datos_por_sujeto,
  vars_plot_1,
  titulo = "Correlaciones (Pearson, n=sujetos)\nExpectativas / rendimiento + PCA / BECK | p ajustado Holm",
  ruta_png = ruta_fig_1,
  width = 2400,
  height = 2200,
  res = 160
)
guardar_matriz_corr(
  datos_por_sujeto,
  vars_plot_2,
  titulo = "Correlaciones (Pearson, n=sujetos)\nEmociones + PCA / BECK | p ajustado Holm",
  ruta_png = ruta_fig_2,
  width = 2600,
  height = 2400,
  res = 160
)
message("\nFiguras guardadas en: ", fig_dir)
message(" - ", basename(ruta_fig_1))
message(" - ", basename(ruta_fig_2))

# --- Export CSV ---
out_dir <- file.path(proj_dir, "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(
  datos_por_sujeto,
  file.path(out_dir, "datosLimpios_un_valor_por_code.csv"),
  row.names = FALSE
)
write.csv(
  tabla_1,
  file.path(out_dir, "correlaciones_expectativa_rendimiento_vs_psico_por_sujeto.csv"),
  row.names = FALSE
)
write.csv(
  tabla_2,
  file.path(out_dir, "correlaciones_emociones_vs_psico_por_sujeto.csv"),
  row.names = FALSE
)
message("\nCSV guardados en: ", out_dir)
message(" - datosLimpios_un_valor_por_code.csv")
message(" - correlaciones_expectativa_rendimiento_vs_psico_por_sujeto.csv")
message(" - correlaciones_emociones_vs_psico_por_sujeto.csv")
