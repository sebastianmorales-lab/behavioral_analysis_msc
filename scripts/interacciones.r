library(ggplot2)
library(tidyr)
library(tidyverse)
#Grafico probabilidades de decisiones individuales por rival facetado por pago con BECK---- 

data %>%  group_by(code,rival.f,pay_competitive.f,BECK_SESION) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = BECK_SESION, y = p_ind, color = rival.f)) +
  geom_point() +
  facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje de Beck",
    y = "Probabilidad de elecciones individuales",
    title = "Líneas de tendencia por nivel de recompensa"
  )
#Grafico probabilidades de decisiones individuales por rival con LSAS----
data %>%  group_by(code,rival.f,pay_competitive.f,LSAS_Total) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = LSAS_Total, y = p_ind, color = rival.f)) +
  geom_point() +
  facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje de LSAS",
    y = "Probabilidad de elecciones individuales",
    title = "Elecciones individuales vs puntaje LSAS"
  )

###Relacion entre Beck y LSAS----


ggplot(data, aes(x = BECK_SESION, y = LSAS_Total)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Relación entre Cuestionario Beck y LSAS",
       x = "Cuestionario Beck",
       y = "Cuestionario LSAS") +
  theme_minimal()

cor.test(data$BECK_SESION, data$LSAS_Total, method = "pearson")



#Grafico probabilidades de decisiones individuales por rival con STAI RASGO----
data %>%  group_by(code,rival.f,pay_competitive.f,STAI_RASGO) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = STAI_RASGO, y = p_ind, color = rival.f)) +
  geom_point() +
  facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje de STAI rasgo",
    y = "Probabilidad de elecciones individuales",
    title = "Elecciones individuales vs puntaje STAI RASGO"
  )

#Grafico probabilidades de decisiones individuales por rival con STAI ESTADO----
data %>%  group_by(code,rival.f,pay_competitive.f,STAI_ESTADO) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = STAI_ESTADO, y = p_ind, color = rival.f)) +
  geom_point() +
  facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje de STAI estado",
    y = "Probabilidad de elecciones individuales",
    title = "Elecciones individuales vs puntaje STAI ESTADO"
  )
# ***Graficos de toma de decisiones por rival con el primer componente del PCA ***----
### Grafico probabilidades de decisiones individuales por rival con primer componente PCA----
data %>%  group_by(code,rival.f,pay_competitive.f,PCA) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = PCA, y = p_ind, color = rival.f)) +
  geom_point() +
  facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje PCA",
    y = "Probabilidad de elecciones individuales",
    title = "Elecciones individuales vs PCA"
  )
### Grafico probabilidades de decisiones competitivas por rival con primer componente PCA----
data %>%  
  group_by(code, rival.f, pay_competitive.f, PCA) %>% 
  mutate(n_trials = n(),
         n_com = sum(game_type.f %in% "competitive"),
         p_com = n_com / n_trials) %>% 
  ggplot(aes(x = PCA, y = p_com, color = rival.f)) +
  geom_point() +
  # facet_wrap(~pay_competitive)+
  geom_smooth(method = "lm", se = T) +
  labs(
    x = "Depression / Anxiety",  # Eje X corregido
    y = "Probability of Competitive Choice",  # Eje Y corregido
    title = "Competitive Choice by Rival and Depression / Anxiety Level",  # Título corregido
    color = "Rival Category"  # Leyenda en inglés
  ) +
  theme(
    axis.text = element_text(size = 16),      # Números de los ejes más grandes
    axis.title = element_text(size = 18),     # Títulos de ejes más grandes  
    plot.title = element_text(size = 20),     # Título principal más grande
    legend.title = element_text(size = 16),   # Título de leyenda más grande
    legend.text = element_text(size = 14)     # Texto de leyenda más grande
  )+theme_minimal()
### Grafico estilizado para la tesis ----

library(ggplot2)
library(dplyr)

# Preparar datos agregados por sujeto y rival
datos_plot <- data %>%  
  group_by(code, rival.f, pay_competitive.f, PCA) %>% 
  summarise(
    n_trials = n(),
    n_com = sum(game_type.f %in% "competitive"),
    p_com = n_com / n_trials,
    .groups = "drop"
  )


library(ggplot2)
library(dplyr)
library(scales)  # para la función alpha()

# Crear una paleta rainbow en tonos pastel
n_rivales <- length(unique(data$rival.f))
colores_pastel <- scales::alpha(rainbow(n_rivales, s = 0.5, v = 0.9), 0.7)

data %>%  
  group_by(code, rival.f, pay_competitive.f, PCA) %>% 
  mutate(
    n_trials = n(),
    n_com = sum(game_type.f %in% "competitive"),
    p_com = n_com / n_trials
  ) %>% 
  ggplot(aes(x = PCA, y = p_com, color = rival.f, fill = rival.f)) +
  geom_point(size = 3, alpha = 0.6) +                   # puntos suaves
  geom_smooth(method = "lm", se = TRUE, size = 1, alpha = 0.2) +    # línea con banda suavizada
  labs(
    x = "puntuacion A-D",
    y = "Probabilidad de elección competitiva",
    color = "Rival",
    fill = "Rival"
  ) +
  scale_color_manual(values = colores_pastel) +
  scale_fill_manual(values = colores_pastel) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

#--- VISUALIZAR interaccion triple----

#1. Interpretación de la interacción con PCA

# Obtener predicciones
predicciones <- ggpredict(modelo_pca3, terms = c("PCA", "rival.n", "pay_competitive.n"))
plot(predicciones)

predicciones2 <- ggpredict(modelo_pca3, terms = c("PCA", "pay_competitive.n", "rival.n"))
plot(predicciones2)

# Graficar con PCA en el eje X, colores según rival.f y paneles según pay_competitive.f

# Crear etiquetas personalizadas
labels_pago <- c(
  "1" = "RECOMPENSA = 1",
  "2" = "RECOMPENSA = 2",
  "3" = "RECOMPENSA = 3",
  "4" = "RECOMPENSA = 4"
)

# Graficar
ggplot(predicciones, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2, linetype = "dotted") +
  facet_wrap(~facet, scales = "free_y", labeller = labeller(facet = labels_pago)) +
  labs(
    x = "puntuación A-D", 
    y = "Predicción", 
    color = "Rival", 
    fill = "Rival"
  ) +
  theme_minimal()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )


# ---- *** EFECTO DE LA EDAD Y EL SEXO *** ----

cat("correlaciones entre edad y variables psicologicas\n")

cor.test(data$edad,data$BECK_SESION, method = "pearson")
cor.test(data$edad,data$LSAS_Total, method = "pearson")
cor.test(data$edad,data$STAI_ESTADO, method = "pearson")
cor.test(data$edad, data$STAI_RASGO, method= "pearson")
cor.test(data$edad, data$PCA, method = "pearson")

cat( "relacion de las variables psicologias con SEXO \n")

sexo = data$sexo
BDI = data$BECK_SESION
LSAS = data$LSAS_Total
S.EST = data$STAI_ESTADO
S.RAS = data$STAI_RASGO
PCA = data$PCA

t.test(sexo,BDI,data=data)
t.test(sexo,LSAS, data = data)
t.test(sexo,S.EST, data = data)
t.test (sexo, S.RAS, data = data)
t.test (sexo, PCA, data=data)



# Lista de variables psicológicas
variables_psico <- c("BECK_SESION", "LSAS_Total", "STAI_ESTADO", "STAI_RASGO", "PCA")

# Función auxiliar que verifica si todos los valores son iguales
valor_unico_o_na <- function(x) {
  ux <- unique(na.omit(x))
  if (length(ux) == 1) ux else NA
}

# Crear el resumen
data_resumen <- data %>%
  group_by(code) %>%
  summarise(
    edad = first(edad),
    across(all_of(variables_psico), valor_unico_o_na),
    .groups = "drop"
  )


library(ggplot2)

ggplot(data_resumen, aes(x = edad, y = BECK_SESION)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Edad vs BECK", x = "Edad", y = "Puntaje BECK") +
  theme_minimal()

graficos <- lapply(variables_psico, function(var) {
  ggplot(data_resumen, aes_string(x = "edad", y = var)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
    labs(title = paste("Edad vs", var), x = "Edad", y = paste("Puntaje", var)) +
    theme_minimal()
})




# ---- SIMULAMOS DATOS ----
# Datos simulados con el modelo
df_plot = tibble(BECK_SESION = rep(0:39,each=16),
  pay_competitive.n = rep(1:4,times=160),
  rival.n = rep(rep(1:4,each=4),40) 
)

df_plot$code = ""

df_plot$logodds = predict(modelo_beck,df_plot,re.form=NA)
df_plot$p_ind = plogis(df_plot$logodds)

df_plot$pay_competitive.f = as.factor(df_plot$pay_competitive.n)
df_plot$rival.f = as.factor(df_plot$rival.n)


#Plot con los datos predichos por el modelo
data %>%  group_by(code,rival.f,pay_competitive.f,BECK_SESION) %>% 
  mutate(n_trials = n(),
         n_ind = sum(game_type.f %in% "individual"),
         p_ind = n_ind / n_trials,) %>% 
  ggplot(aes(x = BECK_SESION, y = p_ind, color = rival.f)) +
  geom_point(alpha=.1) +
  facet_wrap(~pay_competitive)+
  #ggplot(aes(x = BECK_SESION, y = pI, color = rival.n),data=) +
  geom_line(data=df_plot,size=2) +
  facet_wrap(~pay_competitive.n)+
  #geom_smooth(method = "lm", se = F) +  # Añadir color a las líneas de tendencia
  labs(
    x = "Puntaje de Beck",
    y = "Cantidad de elecciones individuales",
    title = "Líneas de tendencia por nivel de recompensa"
  )+
  theme_classic()



