# Ejecutar desde RStudio
# 1. Ir a la carpeta del proyecto
setwd("d:/Expansion/maestria_task/proyecto_final_MAESTRIA_R")

# 2. Cargar y ejecutar el script principal
source("scripts/grafico_beta_pca_clean.R")

# 3. Ejecutar diagnóstico
cat("\n\n")
cat("✓ Gráficos generados. Ahora ejecutando diagnóstico...\n\n")
source("scripts/diagnostico_contraste.R")
