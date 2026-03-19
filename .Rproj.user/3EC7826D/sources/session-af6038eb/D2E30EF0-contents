#Modelodo de los tiempos de reaccion mediante LMM

#Cargo las bbliotecas
library(graphics)
library(tidyverse)
library(ggplot2)
library(geepack)
library(gee)
library(lme4)
library(lmerTest)
library(car)
library(carData)
library(emmeans)
library(effectsize)
library(plyr)
library(dplyr)

#Levanto los datos procesados
datosLimpios <- readRDS("input/datosLimpios.rds")
#selecciono las posibles columnas de interes
datos = datosLimpios %>% select(code,sexo,edad,n_educativo,
                                game_type.f,rival,pay_competitive,pay_competitive.f,
                                rival.f,
                                BECK_SESION,LSAS_Anxiety,LSAS_Fear,LSAS_Total, social_time)

nrow(datosLimpios)

# Calcular la media y desviación estándar de social_time
mean_time <- mean(datos$social_time, na.rm = TRUE)
sd_time <- sd(datos$social_time, na.rm = TRUE)

datos %>% filter(social_time >.4) %>% ggplot(aes(x=social_time,fill=rival.f))+
  geom_histogram()


datos %>% ggplot(aes(x=log(social_time),fill=rival.f))+
  geom_histogram()


#----MODELO CON BECK ----
# Ajusto un modelo lineal para los tiempos de reaccion

lm.tiempo_reacc_cat.vs.game_type=
  lmer (formula = log(social_time) ~ game_type.f*BECK_SESION*rival.f*pay_competitive.f +(1|code),
        na.action = na.omit, data=datos)

summary (lm.tiempo_reacc_cat.vs.game_type)
#print(lm.tiempo_reacc_cat.vs.game_type, correlation=TRUE)

#Grafico con todas las variables
datos %>%
  ggplot(aes(x = BECK_SESION, y = social_time)) +
  geom_point(alpha = 0.2, color = "gray") +  # Puntos en gris tenue
  geom_smooth(aes(color = factor(rival.f)), method = "lm", size = 1, se = FALSE) +  # Líneas más delgadas
  facet_grid(pay_competitive.f ~ game_type.f) +  # Facetas en filas y columnas
  scale_color_brewer(palette = "Dark2") +  # Paleta de colores para las líneas
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(size = 14),  # Texto de las facetas más claro
    axis.text.y = element_blank(),  # Eliminar valores en el eje Y
    axis.ticks.y = element_blank(),  # Eliminar marcas del eje Y
    axis.text.x = element_text(size = 12),  # Texto del eje X más claro
    legend.position = "bottom",  # Leyenda abajo,
    axis.title.y = element_text(size = 14, margin = margin(r = 10))
  ) +
  labs(
    title = "Relación entre BECK y tiempo de reacción",
    x = "puntaje de Beck",
    y = "nivel de recompensa",
    color = "Rival"
  )





# Realzo un anova sobre el modelo para obtener valores de F
anova_interaccion = anova(lm.tiempo_reacc_cat.vs.game_type,type = 3)
print(anova_interaccion)

#calculo grilla de referencia
emm_options(pbkrtest.limit = 18000, lmerTest.limit = 18000)
lm.tiempo_reacc_cat.vs.game_type.rg = ref_grid(lm.tiempo_reacc_cat.vs.game_type)

#calculo medias marginales para variable
lm.tiempo_reacc_cat.vs.game_type.emm.game_type = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "game_type.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.pay = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "pay_competitive.f")
lm.tiempo_reacc_cat.vs.game_type.emm.payrival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,c("pay_competitive.f", "rival.f","game_type.f"))
lm.tiempo_reacc_cat.vs.game_type.emm.pay_rival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,"pay_competitive.f", by="rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,by="pay_competitive.f", "rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay_gametype = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,by="pay_competitive.f", c("rival.f","game_type.f"))

summary(lm.tiempo_reacc_cat.vs.game_type.emm.rival)
summary(lm.tiempo_reacc_cat.vs.game_type.emm.pay)


#contraste 
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.game_type, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival, "pairwise", adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.payrival, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.pay_rival, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay_gametype, "pairwise",adjust = "holm")


#comparar pendientes
pends_beck_sesion = emtrends(lm.tiempo_reacc_cat.vs.game_type,~game_type.f,var="BECK_SESION")
contrast(pends_beck_sesion,method="pairwise",adjust = "holm")

pends_inter = emtrends(lm.tiempo_reacc_cat.vs.game_type,~c(game_type.f,rival.f),var="BECK_SESION")
contrast(pends_inter, method ="pairwise",adjust="holm")
# ---- Visualizar interacciones ----

# Efecto del tipo de juego
datosTR = ddply(datos,c("game_type.f"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = game_type.f, y = mediaTR, fill = game_type.f)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción por tipo de juego"
  ) + scale_x_discrete("opción de juego",labels=c("Competir", "Individual"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))

# Efecto del rival
datosTR = ddply(datos,c("game_type.f","rival.f"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = game_type.f, y = mediaTR, fill = game_type.f)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción por tipo de juego"
  ) + scale_x_discrete("opción de juego",labels=c("Competir", "Individual"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))+
  facet_grid(~rival.f)

# interaccion tipo de juego y beck
datos %>%
  ggplot(aes(x = BECK_SESION, y = social_time, color = game_type.f)) +
  geom_smooth(method = "lm") 
  #facet_grid(~ game_type.f)  # Distribuye por filas (pay_competitive.f) y columnas (game_type.f)

#interaccion rival y pago
datosTR = ddply(datos,c("rival.f","pay_competitive.f"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = rival.f, y = mediaTR, fill = pay_competitive.f)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción rival"
  ) + scale_x_discrete("rival",labels=c("1","2","3","4"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))

#Visualizar datos de intección BECK_SESION:game_type:rival
datos %>%
  ggplot(aes(x = BECK_SESION, y = social_time, color = rival.f)) +
  geom_smooth(method = "lm") +
  facet_grid(~ game_type.f)  # Distribuye por filas (pay_competitive.f) y columnas (game_type.f)

#Visualizar datos de intección game_type:rival_pay_competitive
#interaccion rival y pago
datosTR = ddply(datos,c("rival.f","pay_competitive.f","game_type.f"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = rival.f, y = mediaTR, fill = pay_competitive.f)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción"
  ) + scale_x_discrete("rival",labels=c("1","2","3","4"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))+
  facet_grid(~game_type.f)


#----MODELO CON LSAS ----
# Ajusto un modelo lineal para los tiempos de reaccion

lm.tiempo_reacc_cat.vs.game_type=
  lmer (formula = log(social_time) ~ game_type.f*LSAS_Total*rival.f*pay_competitive.f +(1|code),
        na.action = na.omit, data=datos)

summary (lm.tiempo_reacc_cat.vs.game_type)
#print(lm.tiempo_reacc_cat.vs.game_type, correlation=TRUE)

#Grafico con todas las variables
datos %>%
  ggplot(aes(x = LSAS_Total, y = social_time)) +
  geom_point(alpha = 0.2, color = "gray") +  # Puntos en gris tenue
  geom_smooth(aes(color = factor(rival.f)), method = "lm", size = 1, se = FALSE) +  # Líneas más delgadas
  facet_grid(pay_competitive.f ~ game_type.f) +  # Facetas en filas y columnas
  scale_color_brewer(palette = "Dark2") +  # Paleta de colores para las líneas
  theme_minimal(base_size = 16) +
  theme(
    strip.text = element_text(size = 14),  # Texto de las facetas más claro
    axis.text.y = element_blank(),  # Eliminar valores en el eje Y
    axis.ticks.y = element_blank(),  # Eliminar marcas del eje Y
    axis.text.x = element_text(size = 12),  # Texto del eje X más claro
    legend.position = "bottom",  # Leyenda abajo,
    axis.title.y = element_text(size = 14, margin = margin(r = 10))
  ) +
  labs(
    title = "Relación entre LSAS y tiempo de reacción",
    x = "puntaje de LSAS",
    y = "nivel de recompensa",
    color = "Rival"
  )


# Realzo un anova sobre el modelo para obtener valores de F
anova_interaccion = anova(lm.tiempo_reacc_cat.vs.game_type,type = 3)
print(anova_interaccion)

#calculo grilla de referencia
emm_options(pbkrtest.limit = 18000, lmerTest.limit = 18000)
lm.tiempo_reacc_cat.vs.game_type.rg = ref_grid(lm.tiempo_reacc_cat.vs.game_type)

#calculo medias marginales para variable
lm.tiempo_reacc_cat.vs.game_type.emm.game_type = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "game_type.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.pay = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg, "pay_competitive.f")
lm.tiempo_reacc_cat.vs.game_type.emm.payrival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,c("pay_competitive.f", "rival.f","game_type.f"))
lm.tiempo_reacc_cat.vs.game_type.emm.pay_rival = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,"pay_competitive.f", by="rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,by="pay_competitive.f", "rival.f")
lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay_gametype = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,by="pay_competitive.f", c("rival.f","game_type.f"))

summary(lm.tiempo_reacc_cat.vs.game_type.emm.rival)
summary(lm.tiempo_reacc_cat.vs.game_type.emm.pay)


#contraste 
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.game_type, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival, "pairwise", adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.payrival, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.pay_rival, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay, "pairwise",adjust = "holm")
contrast(lm.tiempo_reacc_cat.vs.game_type.emm.rival_pay_gametype, "pairwise",adjust = "holm")


#comparar pendientes
pends_lsas = emtrends(lm.tiempo_reacc_cat.vs.game_type,~game_type.f,var="LSAS_Total")
contrast(pends_beck_sesion,method="pairwise",adjust = "holm")

pends_inter <- emtrends(lm.tiempo_reacc_cat.vs.game_type, 
                        ~ pay_competitive.f * game_type.f, 
                        var = "LSAS_Total", 
                        by = "game_type.f")
pend_inter_rp <- emtrends(lm.tiempo_reacc_cat.vs.game_type,
                            ~ rival.f*pay_competitive.f,
                          var="LSAS_Total",
                          by = "pay_competitive.f")

# Contrastamos las tendencias marginales con método de Holm
contrast(pends_inter, method = "pairwise", adjust = "holm")
contrast(pend_inter_rp, method="pairwise", adjust = "holm")

# Estimación de los efectos marginales
emm_inter <- emmeans(lm.tiempo_reacc_cat.vs.game_type, 
                     ~ pay_competitive.f * rival.f | LSAS_Total)
summary(emm_inter)



#----Visualizar interacciones ----
#Visualizar datos de intección LSAS:rival:pago
datos %>%
  ggplot(aes(x = LSAS_Total, y = social_time, color = rival.f)) +
  geom_smooth(method = "lm") +
  facet_grid(~ pay_competitive.f)  # Distribuye por columnas (pay_competitive.f)

#Visualizo datos de interacción tipo de juego:LSAS:pago
datos %>%
  ggplot(aes(x = LSAS_Total, y = social_time, color = pay_competitive.f)) +
  geom_smooth(method = "lm") +
  facet_grid(~ game_type.f)  #Distribuido por columnas (game_type)

#----MODELO LSAS Anxiety----
# Ajusto un modelo lineal para los tiempos de reaccion

lm.tiempo_reacc_cat.vs.game_type=
  lmer (formula = social_time ~ game_type*LSAS_Anxiety*rival.f*pay_competitive.f +(1|code),
        na.action = na.omit, data=datos)

summary (lm.tiempo_reacc_cat.vs.game_type)
print(lm.tiempo_reacc_cat.vs.game_type, correlation=TRUE)
# Realzo un anova sobre el modelo para obtener valores de F
anova_interaccion = anova(lm.tiempo_reacc_cat.vs.game_type,type = 3)
print(anova_interaccion)
#Calculo una grilla de referencia, al tener muchas observaciones agregue el argumento emm_options(pbkrtest.limit=4345)
emm_options(pbkrtest.limit = 15856)
lm.tiempo_reacc_cat.vs.game_type.rg = ref_grid(lm.tiempo_reacc_cat.vs.game_type)

#Hago el desglose para los niveles del tipo de juego, calculo medias marginales
lm.tiempo_reacc_cat.vs.game_type.emm = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,"game_type", "LSAS_Anxiety")

#Hago los contrastes 
contrast(lm.tiempo_reacc_cat.vs.game_type.emm,"pairwise")

#----MODELO CON LSAS FEAR----
# Ajusto un modelo lineal para los tiempos de reaccion

lm.tiempo_reacc_cat.vs.game_type=
  lmer (formula = social_time ~ game_type *LSAS_Fear*rival+pay_competitive + (1|code),
        na.action = na.omit, data=datos)

summary (lm.tiempo_reacc_cat.vs.game_type)
#print(lm.tiempo_reacc_cat.vs.game_type, correlation=TRUE)
# Realzo un anova sobre el modelo para obtener valores de F
anova_interaccion = anova(lm.tiempo_reacc_cat.vs.game_type,type = 3)
print(anova_interaccion)
#Calculo una grilla de referencia, al tener muchas observaciones agregue el argumento emm_options(pbkrtest.limit=4345)
lm.tiempo_reacc_cat.vs.game_type.rg = ref_grid(lm.tiempo_reacc_cat.vs.game_type,emm_options(pbkrtest.limit=4719))

#Hago el desglose para los niveles del tipo de juego, calculo medias marginales
lm.tiempo_reacc_cat.vs.game_type.emm = emmeans(lm.tiempo_reacc_cat.vs.game_type.rg,"game_type", "LSAS_Fear")

#Hago los contrastes 
contrast(lm.tiempo_reacc_cat.vs.game_type.emm,"pairwise")

