# Armar un analisis de PCA con los cuestionarios psicologicos

# Levanto las bibliotecas
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(geepack)
library(gee)
library(lme4)
library(lmerTest)
library(car)
library(lsmeans)
library(emmeans)
library(effectsize)
library(corrplot)
library(ggeffects)
library(corrplot)
library(RColorBrewer)

#Levanto los datos psicologicos
# setwd("/media/usuario/Seagate Portable Drive/Expansion/maestria_task/proyecto_final_MAESTRIA_R")
load("/media/usuario/Seagate Portable Drive/Expansion/maestria_task/proyecto_final_MAESTRIA_R/input/datosLimpios2.RData")
data <- datosLimpios
rm(datosLimpios)
psico = read.csv('./psicologicos/psicologicos.csv')
psico <- psico %>% filter(second_level==0)


# psico <- psico %>% select(Beck_web,LSAS_Total,
#                 BAS,BIS,FNE,
#                 CBAS_comp_social ,CBAS_comp_no_social ,CBAS_cog_social, CBAS_cog_no_social, CBAS,
#                 ACIPS, MCOI_LIC, MCOI_HCO,
#                 MCOI_ADCA, MCOI_SDCO,
#                 PANAS_POSITIVO, PANAS_NEGATIVO,
#                 RRS, SELF_CRITICAL, SCS, CERQ ,STIP,
#                 BECK_SESION, STAI_ESTADO, STAI_RASGO)
# 
#   print(head(psico))

str(data)


# ---- MODELO CON CUESTIONARIOS ----
data_0 =data %>% 
  mutate(rival_0 = rival.n -1,
         pay_competitive_0 = pay_competitive.n-1)
#----modelo con beck----
modelo_beck = glmer(game_type.f~scale(rival.n)*scale(BECK_SESION)*scale(pay_competitive.n)+sexo+edad+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
# modelo_beck_0 = glmer(game_type.f~rival_0*scale(BECK_SESION)*pay_competitive_0+(1|code),data = data_0,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
# summary(modelo_beck_0)
summary(modelo_beck)
Anova(modelo_beck,type=3)

# #calculo las pendientes 
# pend_beck = emtrends(modelo_beck,~rival.f, var="BECK_SESION")
# contrast(pend_beck, method="pairwise",adjust = "holm")                



#interpretando los coeficientes
coef_mod0 = fixef(modelo_beck_0)

# intercepto: recom =1, rival =1, beck media
coef_p1_r1_b0 = fixef(modelo_beck_0)[1]

# intercepto: recom =1, rival =1, beck +2SD
coef_p1_r1_b2 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3]


# intercepto: recom =4, rival =1, beck media
coef_p4_r1_b0 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3] + 3*fixef(modelo_beck_0)[4]
fixef(modelo_beck_0)[1] + 3*fixef(modelo_beck_0)[4]

# intercepto: recom =4, rival =1, beck +2SD
coef_p4_r1_b2 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3] + 3*fixef(modelo_beck_0)[4] + 6*fixef(modelo_beck_0)[6]


# efecto recomp vs  becl para riv=1
int_rec_beck = (coef_p4_r1_b0 - coef_p1_r1_b0) - (coef_p4_r1_b2 - coef_p1_r1_b2)



# intercepto: recom =1, rival =4, beck media
coef_p1_r1_b0 = fixef(modelo_beck_0)[1] + 

# intercepto: recom =1, rival =1, beck +2SD
coef_p1_r1_b2 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3]


# intercepto: recom =4, rival =1, beck media
coef_p4_r1_b0 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3] + 3*fixef(modelo_beck_0)[4]
fixef(modelo_beck_0)[1] + 3*fixef(modelo_beck_0)[4]

# intercepto: recom =4, rival =1, beck +2SD
coef_p4_r1_b2 = fixef(modelo_beck_0)[1] + 2*fixef(modelo_beck_0)[3] + 3*fixef(modelo_beck_0)[4]


# efecto recomp vs  becl para riv=1
int_rec_beck = (coef_p4_r1_b0 - coef_p1_r1_b0) - (coef_p4_r1_b2 - coef_p1_r1_b2)




#---- modelo con LSAS ----
modelo_lsas = glmer(game_type.f~scale(rival.n)*scale(LSAS_Total)*scale(pay_competitive.n)+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_lsas)
Anova(modelo_lsas,type=3)

#----modelo con STAI ESTADO----
modelo_stai_est = glmer(game_type.f~scale(rival.n)*scale(STAI_ESTADO)*scale(pay_competitive.n)+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_stai_est )
Anova(modelo_stai_est, type=3)


#----modelo STAI RASGO----
modelo_stai_ras = glmer(game_type.f~scale(rival.n)*scale(STAI_RASGO)*scale(pay_competitive.n)+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_stai_ras)
Anova(modelo_stai_ras, type=3)




# ---- Graficos cuestionarios  ----

### *** CON BECK ---------

# Convertir las variables relevantes a los tipos de datos adecuados
data <- data %>%
  mutate(puntaje_beck = scale(BECK_SESION), tipo_de_juego = as.factor(game_type), rival = as.factor(rival), pay_competitive = as.factor(pay_competitive))

# Filtrar los datos solo para los juegos individuales
datos_competir <- data %>%
  filter(tipo_de_juego == "competitive")

# Calcular la cantidad de elecciones individuales por sujeto y rival
datos_rival <- datos_competir %>%
  group_by(rival,pay_competitive, code, puntaje_beck) %>%
  dplyr::summarise(cantidad_elecciones = n())

# Crear el scatterplot con líneas de tendencia por rival y coloridas
# Verificar que 'rival' es un factor
datos_rival$rival <- as.factor(datos_rival$rival)

# Asegurarse de que 'pay_competitive' es un factor
datos_rival$pay_competitive <- as.factor(datos_rival$pay_competitive)

# Crear el scatterplot con líneas de tendencia por rival y coloridas
ggplot(datos_rival, aes(x = puntaje_beck, y = cantidad_elecciones, color = rival)) +
  geom_point() +
  #facet_grid(. ~ pay_competitive) + # si quiero separado por nivel de recompensa descomentar esta linea
  geom_smooth(method = "lm", se = TRUE, aes(color = rival)) +  # Nadir color a las líneas de tendencia
  labs(
    x = "Puntaje de Beck",
    y = "Cantidad de elecciones competitivas",
    title = "Líneas de tendencia por rival"
  )

### *** CON LSAS ----

# Convertir las variables relevantes a los tipos de datos adecuados
data <- data %>%
  mutate(puntaje_LSAS = scale(LSAS_Total), tipo_de_juego = as.factor(game_type), rival = as.factor(rival), pay_competitive = as.factor(pay_competitive))

# Filtrar los datos solo para los juegos individuales
datos_competir <- data %>%
  filter(tipo_de_juego == "competitive")

# Calcular la cantidad de elecciones individuales por sujeto y rival
datos_rival <- datos_competir %>%
  group_by(rival,pay_competitive, code, puntaje_LSAS) %>%
  dplyr::summarise(cantidad_elecciones = n())

# Crear el scatterplot con líneas de tendencia por rival y coloridas
# Verificar que 'rival' es un factor
datos_rival$rival <- as.factor(datos_rival$rival)

# Asegurarse de que 'pay_competitive' es un factor
datos_rival$pay_competitive <- as.factor(datos_rival$pay_competitive)

# Crear el scatterplot con líneas de tendencia por rival y coloridas
ggplot(datos_rival, aes(x = puntaje_LSAS, y = cantidad_elecciones, color = rival)) +
  geom_point() +
  #facet_grid(. ~ pay_competitive) + # si quiero separado por nivel de recompensa descomentar esta linea
  geom_smooth(method = "lm", se = TRUE, aes(color = rival)) +  # Nadir color a las líneas de tendencia
  labs(
    x = "Puntaje de LSAS",
    y = "Cantidad de elecciones competitivas",
    title = "Líneas de tendencia por rival"
  )

### *** CON STAI ----

# Convertir las variables relevantes a los tipos de datos adecuados
data <- data %>%
  mutate(puntaje_STAI_ESTADO = scale(STAI_ESTADO), tipo_de_juego = as.factor(game_type), rival = as.factor(rival), pay_competitive = as.factor(pay_competitive))

# Filtrar los datos solo para los juegos individuales
datos_competir <- data %>%
  filter(tipo_de_juego == "competitive")

# Calcular la cantidad de elecciones individuales por sujeto y rival
datos_rival <- datos_competir %>%
  group_by(rival,pay_competitive, code, puntaje_STAI_ESTADO) %>%
  dplyr::summarise(cantidad_elecciones = n())

# Crear el scatterplot con líneas de tendencia por rival y coloridas
# Verificar que 'rival' es un factor
datos_rival$rival <- as.factor(datos_rival$rival)

# Asegurarse de que 'pay_competitive' es un factor
datos_rival$pay_competitive <- as.factor(datos_rival$pay_competitive)

# Crear el scatterplot con líneas de tendencia por rival y coloridas
ggplot(datos_rival, aes(x = puntaje_STAI_ESTADO, y = cantidad_elecciones, color = rival)) +
  geom_point() +
  #facet_grid(. ~ pay_competitive) + # si quiero separado por nivel de recompensa descomentar esta linea
  geom_smooth(method = "lm", se = TRUE, aes(color = rival)) +  # Nadir color a las líneas de tendencia
  labs(
    x = "Puntaje de STAI ESTADO",
    y = "Cantidad de elecciones competitivas",
    title = "Líneas de tendencia por rival"
  )

# Convertir las variables relevantes a los tipos de datos adecuados
data <- data %>%
  mutate(puntaje_STAI_RASGO = scale(STAI_RASGO), tipo_de_juego = as.factor(game_type), rival = as.factor(rival), pay_competitive = as.factor(pay_competitive))

# Filtrar los datos solo para los juegos individuales
datos_competir <- data %>%
  filter(tipo_de_juego == "competitive")

# Calcular la cantidad de elecciones individuales por sujeto y rival
datos_rival <- datos_competir %>%
  group_by(rival,pay_competitive, code, puntaje_STAI_RASGO) %>%
  dplyr::summarise(cantidad_elecciones = n())

# Crear el scatterplot con líneas de tendencia por rival y coloridas
# Verificar que 'rival' es un factor
datos_rival$rival <- as.factor(datos_rival$rival)

# Asegurarse de que 'pay_competitive' es un factor
datos_rival$pay_competitive <- as.factor(datos_rival$pay_competitive)

# Crear el scatterplot con líneas de tendencia por rival y coloridas
ggplot(datos_rival, aes(x = puntaje_STAI_RASGO, y = cantidad_elecciones, color = rival)) +
  geom_point() +
  #facet_grid(. ~ pay_competitive) + # si quiero separado por nivel de recompensa descomentar esta linea
  geom_smooth(method = "lm", se = TRUE, aes(color = rival)) +  # Nadir color a las líneas de tendencia
  labs(
    x = "Puntaje de STAI RASGO",
    y = "Cantidad de elecciones competitivas",
    title = "Líneas de tendencia por rival"
  )

# MODELO CON PCA ----
modelo_pca <- glmer(game_type.f~rival.n*PCAscore*pay_competitive.n+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_pca)

Anova(modelo_pca,type = 3)

modelo_pca2 <- glmer(game_type.f~rival.n*PCAsimple*pay_competitive.n+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_pca2)

Anova(modelo_pca2,type = 3)
# modelo PCA3 todos como numericos
modelo_pca3 <- glmer(game_type.f~scale(rival.n)*PCA*scale(pay_competitive.n)+edad+sexo+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_pca3)

Anova(modelo_pca3,type = 3)

#Modelo PCA4 rival y pago como factor
modelo_pca4 <- glmer(game_type.f~rival.f*PCA*pay_competitive.f+edad+sexo+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_pca4)

#Modelo PCA5 rival como factor y pago numerico
modelo_pca5 <- glmer(game_type.f~rival.f*PCA*scale(pay_competitive.n)+edad+sexo+(1|code),data = data,family = binomial,control=glmerControl(optCtrl=list(maxfun = 100000),optimizer = "bobyqa"))
summary(modelo_pca5)

Anova(modelo_pca4, type=3)

anova(modelo_pca3,modelo_pca4)
# --- VISUALIZAR efecto principal del rival ----
library(emmeans)

# Rival como numérico (ya está escalado en tu modelo, por eso hay que indicarle los valores reales)
# Si rival.n va de 1 a 4:
emmeans(modelo_pca3, 
        specs = ~ scale(rival.n), 
        at = list(rival.n = 1:4), 
        type = "response")
library(ggeffects)

# Predicciones de rival en valores 1 a 4
pred <- ggpredict(modelo_pca3, terms = "rival.n [1:4]")
print(pred)

# Gráfico con curva logística y puntos de predicción
plot(pred)



