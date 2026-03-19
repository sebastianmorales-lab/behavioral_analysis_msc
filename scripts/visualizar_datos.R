#cargo las bibliotecas

library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
#Leo los datos ya Limpios
datos <- readRDS("./input/datosLimpios2.rds")
# levanto el entorno con los datagrames
load("./input/datosLimpios2.RData")

# ---- Descritivos de la muestra ----

#levanto los datos y filtro los que se incluyen en el analisis de fMRI
df <- read.csv("psicologicos/psicologicos.csv", header = T, encoding = "UTF-8") %>% 
  mutate(codigo = as.character(codigo)) %>% filter(second_level == 0)
#contar cuantos participantes son mujeres
table(df$sexo) #son 89 mujeres y 28 hombres y 1 otros
# calcular promedio de edad y desvio estandar
M = mean(df$edad, na.rm = T) #promedio de edad es 23.2 años
SD = sd(df$edad, na.rm = T) #desvio estandar de edad es 3.7 años


#Participantes segun Beck_web agrupados segun sexo y edad
por_sexo <- datos %>% group_by(code, sexo,Beck_web) %>% 
  dplyr::summarise(n=n()) %>% 
  pivot_wider(names_from = sexo, values_from = n)
write.csv(por_sexo, here::here("tablas/por_sexo.csv"))

por_edad <- datos %>% group_by(code, edad,Beck_web) %>% 
  dplyr::summarise(n=n()) %>% 
  pivot_wider(names_from = edad, values_from = n)
write.csv(por_edad, here::here("tablas/por_edad.csv"))
#----TABLAS -----
#tabla de sexos
table(df_psico$sexo)
prop.table(table(df_psico$sexo))
#tabla de edades
table(df$edad)
prop.table(datos$edad)
#calculo cuantas veces elige cada tipo de juego (game_type) por sujeto
cantidades <- datos %>% group_by(code,game_type) %>% 
  dplyr::summarise(n=n())%>%
  pivot_wider(names_from = game_type,values_from = n)  
write.csv(cantidades, here::here("tablas/cantidades.csv"))

#calculo cuantas veces se eligio cada opcion de juego (competir o individual) y cual porcentaje
# de las veces representa
table(datos$game_type)
prop.table(table(datos$game_type))

#cantidad de veces que a los participantes se le presentaron las distintas categorias de rival
table(datos$rival)
prop.table(table(datos$rival))

#cantidad de veces que los participantes eligieron competir con las categorias de rival. 
por_rivales = datos %>% filter(game_type=="competitive") %>% group_by(code)
table(por_rivales$rival)
prop.table(table(por_rivales$rival))

#cantidad de veces que a los participantes se le presentaron los niveles de recompensa
table(datos$pay_competitive)
prop.table(table(datos$pay_competitive))

#cantidad de veces que los participantes eligieron competir segun niveles de recompensa 
por_recompensa = datos %>% filter(game_type=="competitive") %>% group_by(code)
table(por_rivales$pay_competitive)
prop.table(table(por_rivales$pay_competitive))

#----BOXPLOT ----
## boxplot veces que los sujetos eligen competir segur rival
library(ggplot2)
library(dplyr)
datos %>% as.data.frame(datos) %>% 
  group_by(code, game_type, rival.f) %>% 
  dplyr::summarise(n = n()) %>% 
  ggplot(aes(x = rival.f, y = n)) + 
  geom_boxplot()
  
## boxplot veces que los sujetos eligen competir segur recompensa
datos %>%
  filter(game_type=="competitive") %>%
  group_by(code,game_type,pay_competitive) %>% 
  dplyr::summarize(n=n()) %>% ggplot(aes(x= pay_competitive, y=n)) +
  geom_boxplot()
# boxplot Beck-web segun sexo
df_psico %>% select(codigo,sexo,Beck_web) %>% 
  group_by(sexo) %>% 
  ggplot(aes(x = sexo, y = Beck_web, group = sexo))+
  geom_boxplot()
# boxplot BECK_SESION segun sexo
df_psico %>% select(codigo,sexo,BECK_SESION) %>% 
  group_by(sexo) %>% 
  ggplot(aes(x = sexo, y = BECK_SESION, group = sexo))+
  geom_boxplot()

# Histograma con densidad edad
ggplot(datos, aes(x = edad)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
# Histograma con densidad sexo
ggplot(datos, aes(x = sexo)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
# Histograma con densidad beck web
ggplot(datos, aes(x = Beck_web)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
# Histograma con densidad beck sesion
ggplot(datos, aes(x = BECK_SESION)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
# Histograma con densidad lsas
ggplot(datos, aes(x = LSAS_Total)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)
#grafica edad vs Beck
ggplot(datos, aes(x = edad, y = Beck_web))+
  geom_col(position = position_dodge())
#grafica codigo vs Beck color = sexo
library(hrbrthemes)

ggplot(datos, aes(x=code, y=Beck_web, color=sexo)) + 
  geom_point(size=6) +
  theme_ipsum()
#grafica edad vs Beck sesion
ggplot(datos, aes(x = edad, y = BECK_SESION))+
  geom_col(position = position_dodge())

#grafica codigo vs Beck sesion color = sexo
library(hrbrthemes)

ggplot(datos, aes(x=code, y=BECK_SESION, color=sexo)) + 
  geom_point(size=6) +
  theme_ipsum()

#grafica codigo vs LSAS_Total, color = sexo
library(hrbrthemes)

ggplot(datos, aes(x=code, y=LSAS_Total, color=sexo)) + 
  geom_point(size=6) +
  theme_ipsum()
#----CORRELACIONES Y REGRESIONES ----
#Relacion Beck_web con sexo
modelo.beck_web.vs.sexo = lm(formula = Beck_web ~ sexo, 
                             data = df_psico)
summary(modelo.beck_web.vs.sexo)

#Relacion BECK_SESION con sexo
modelo.BECK_SESION.vs.sexo = lm(formula = BECK_SESION ~ sexo, 
                                data = df_psico)
summary(modelo.BECK_SESION.vs.sexo)

#grafico edad vs Beck_web
datos %>% 
  ggplot(aes(x=edad, y=Beck_web))+
  geom_point() +
  geom_smooth(method = "lm")
#grafico edad vs BECK_SESION
datos %>% 
  ggplot(aes(x=edad, y=BECK_SESION))+
  geom_point() +
  geom_smooth(method = "lm")
#Corelation test
cor.BECK_SESION.vs.edad =cor.test(datos$edad, datos$BECK_SESION,
         alternative = c("two.sided"),
         method = c("spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)
print(cor.BECK_SESION.vs.edad)
#Corelation test
cor.Beck_web.vs.edad =cor.test(datos$edad, datos$Beck_web,
         alternative = c("two.sided"),
         method = c("spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)
print(cor.Beck_web.vs.edad)

# cor.test(datos$edad, datos$Beck_web,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
# cor.test(datos$sexo, datos$Beck_web,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
# cor.test(datos$sexo,datos$BECK_SESION,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
# cor.test(datos$edad,datos$BECK_SESION,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
# cor.test(datos$sexo,datos$LSAS_Total,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
# cor.test(datos$edad,datos$LSAS_Total,
#          alternative = c("two.sided", "less", "greater"),
#          method = c("pearson", "kendall", "spearman"),
#          exact = NULL, conf.level = 0.95, continuity = FALSE)
#
# datos agrupados segun tipo de juego (approach o avoidance) ----
A1 <- datos %>%
  group_by(code,game_type,) %>% 
  dplyr::summarise(n=n()) 
#Nota:calcula usando summarse la cantdad de cada opcion de juego, para eso agrupo por game_type
A1 %>% 
  group_by(game_type) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n()
  ) %>% ggplot(aes( x= game_type, y = ((mean*100)/128))) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = ((mean*100)/128) - sd, ymax = ((mean*100)/128) + sd),
    position = position_dodge2(padding = 0.1)
  ) +
  labs(
    x= 'tipo de juego',
    y= 'porcentaje de respuestas',
    title = " "
  )
### Grafico estilizado ----
A1 %>% 
  group_by(game_type) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n()
  ) %>% 
  # Cambiar las etiquetas del eje x
  mutate(game_type_label = case_when(
    game_type == "competitive" ~ "Competitivas",
    game_type == "individual" ~ "Individuales",
    TRUE ~ as.character(game_type)
  ),
  percentage = (mean * 100)/128,
  print(percentage)
  ) %>%
  ggplot(aes(x = game_type_label, y = (percentage))) +
  geom_col(
    position = position_dodge(),
    fill = c("#3498db", "#e74c3c"),  # Azul y rojo vibrantes
    alpha = 0.8,
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = (percentage) - sd, ymax = (percentage) + sd),
    position = position_dodge2(padding = 0.1),
    width = 0.3,
    color = "#2c3e50",
    size = 0.8
  ) +
  # Tema moderno y colorido
  theme_minimal() +
  theme(
    # Fondo del panel
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    
    # Grillas
    panel.grid.major = element_line(color = "#e9ecef", size = 0.5),
    panel.grid.minor = element_blank(),
    
    # Títulos y etiquetas
    plot.title = element_text(
      size = 16, 
      face = "bold", 
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    axis.title = element_text(
      size = 12, 
      face = "bold", 
      color = "#34495e"
    ),
    axis.text = element_text(
      size = 11, 
      color = "#5d6d7e"
    ),
    axis.text.x = element_text(
      face = "bold",
      color = "#2c3e50"
    ),
    
    # Márgenes
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    x = 'Tipo de juego',
    y = 'Porcentaje de respuestas (%)',
    title = "Distribución por tipo de juego"
  ) +
  # Escala del eje y más limpia
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = function(x) paste0(round(x, 1), "%")
  )
#Nota uso la funcion ggplot para generar el grafico, defino la geometra en columnas
# y agrego las barras de desvio estandar con geom_errorbar
df_wide <- tidyr::pivot_wider(A1, names_from = game_type, values_from = n, values_fill = 0)

# Prueba t pareada
t_test_result <- t.test(df_wide$competitive, df_wide$individual, paired = TRUE)
print(t_test_result)

# Prueba de Wilcoxon
wilcox_test_result <- wilcox.test(df_wide$competitive, df_wide$individual, paired = TRUE)
print(wilcox_test_result)


#guardo la los datos en un archivo csv en la carpeta tablas.
write.csv(A1,here::here("tablas/cantidades_elecciones.csv"))

pdf("mi_plot.pdf") #abro un pdf para guardar mi plot

#datos agrupados segun tipo de juego (approach o avoidance) y puntaje en el BDI-II
A2 <- datos %>%
  group_by(code,BECK_SESION,game_type) %>% 
  dplyr::summarise(n=n()) %>% 
  filter(game_type=="competitive")
#Nota:calcula usando summarse la cantidad de cada opción de juego, para eso agrupo por game_type y por Beck (BDI-II)

A2 %>% mutate(BECK_SESION = as.factor(BECK_SESION)) %>% 
  group_by(code,BECK_SESION) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n()
  ) %>% ggplot(aes( x= BECK_SESION, y = ((mean*100)/128))) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = ((mean*100)/128) - sd, ymax = ((mean*100)/128) + sd),
    position = position_dodge2(padding = 0.1)
  ) +
  labs(
    x= 'puntaje de Beck sesion',
    y= 'porcentaje de respuestas',
    title = " "
  )

dev.off() #cierro el pdf

pdf("mi_plot_TR.pdf") #abro un pdf para guardar mi plot
#datos agrupados segun tipo de juego (approach o avoidance)
A3 <- datos %>%
  group_by(game_type,BECK_SESION,code) %>% 
  dplyr::summarise(n=n()) 
#Nota:calcula usando summarise la cantdad de cada opcion de juego, para eso agrupo por game_type

A3 %>% mutate(BECK_SESION = as.factor(BECK_SESION)) %>% 
  group_by(game_type, BECK_SESION,) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n()
  ) %>% ggplot(aes( x= BECK_SESION, y = ((mean*100)/128), fill = game_type)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = ((mean*100)/128) - sd, ymax = ((mean*100)/128) + sd),
    position = position_dodge2(padding = 0.1)
  ) +
  labs(
    x= 'puntaje de Beck sesion',
    y= 'porcentaje de respuestas',
    title = " "
  )

dev.off() #cierro el pdf

#Vemos que pasa con las decisones y la ansiedad social (medida LSAS)

A4 <- datos %>%
  group_by(game_type, LSAS_Total, code) %>% 
  dplyr::summarise(n = n(), .groups = 'drop')  # Agregar .groups = 'drop'

A4 %>% 
  mutate(LSAS_Total = as.factor(LSAS_Total)) %>% 
  group_by(game_type, LSAS_Total) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n(), .groups = 'drop'
  ) %>% 
  ggplot(aes(x = LSAS_Total, y = ((mean * 100) / 128), fill = game_type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ((mean * 100) / 128) - sd, ymax = ((mean * 100) / 128) + sd),
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = 'puntaje de LSAS Total',
    y = 'porcentaje de respuestas',
    title = " "
  )
A5 <- datos %>%
  group_by(game_type, LSAS_Anxiety, code) %>% 
  dplyr::summarise(n = n(), .groups = 'drop')  # Agregar .groups = 'drop'


A5 %>% 
  mutate(LSAS_Anxiety = as.factor(LSAS_Anxiety)) %>% 
  group_by(game_type, LSAS_Anxiety) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n(), .groups = 'drop'
  ) %>% 
  ggplot(aes(x = LSAS_Anxiety, y = ((mean * 100) / 128), fill = game_type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ((mean * 100) / 128) - sd, ymax = ((mean * 100) / 128) + sd),
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = 'puntaje de LSAS Anxiety',
    y = 'porcentaje de respuestas',
    title = " "
  )

A6 <- datos %>%
  group_by(game_type, LSAS_Fear, code) %>% 
  dplyr::summarise(n = n(), .groups = 'drop')  # Agregar .groups = 'drop'


A6 %>% 
  mutate(LSAS_Fear = as.factor(LSAS_Fear)) %>% 
  group_by(game_type, LSAS_Fear) %>%
  dplyr::summarise(
    mean = mean(n),
    sd = sd(n),
    n = n(), .groups = 'drop'
  ) %>% 
  ggplot(aes(x = LSAS_Fear, y = ((mean * 100) / 128), fill = game_type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = ((mean * 100) / 128) - sd, ymax = ((mean * 100) / 128) + sd),
    position = position_dodge(width = 0.9)
  ) +
  labs(
    x = 'puntaje de LSAS Fear',
    y = 'porcentaje de respuestas',
    title = " "
  )

#Genera el grafico para los tiempos de reaccion
datosTR = ddply(datos,c("game_type","BECK_SESION"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = game_type, y = mediaTR, fill = game_type)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción por tipo de juego y puntaje del BDI-II"
  ) + scale_x_discrete("opción de juego",labels=c("C", "I"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))+
  facet_grid( ~BECK_SESION)
  
datosTR = ddply(datos,c("game_type","LSAS_Total"),summarise, mediaTR =mean(social_time, na.rm = T), sdTR = sd(social_time, na.rm = T))
ggplot(data=datosTR, aes(x = game_type, y = mediaTR, fill = game_type)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(
    aes(ymin = mediaTR - sdTR, ymax = mediaTR + sdTR), 
    position = position_dodge2(padding = 0.5)
  ) +
  labs(
    x= 'opción de juego',
    y= 'Media del tiempo de reacción',
    title = "Tiempos de reacción por tipo de juego y puntaje del LSAS_Total"
  ) + scale_x_discrete("opción de juego",labels=c("C", "I"))+
  theme ( axis.text.x = element_text(size = 8),
          strip.text=element_text(family='Anton', face='bold', size=8, hjust=0))+
  facet_grid( ~LSAS_Total)


print("Finalizado sin problemas")
