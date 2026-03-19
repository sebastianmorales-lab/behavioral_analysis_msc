library(geepack)
library(gee)
library(lme4)
library(gee)
library(lmerTest)
library(car)
library(lsmeans)
library(stringr)
library(plyr)
library(ggplot2)
library(dplyr)
library(zoo)
library(ggpubr)
library(reshape2)
library(ez)
library(cowplot)
library(magick)

todos_juntos = read.csv("/media/cibpsi/Portable Drive/datos_tesis_maestria/files_team_task/spm12_batch/extraccion_betas/extraccion/x_trial_decision/full_table.csv")

todos_juntos$cat_elegida = as.factor(todos_juntos$cat_elegida)
todos_juntos$condicion = paste0(todos_juntos$cat_asignada1,todos_juntos$cat_asignada2)
todos_juntos$clinico = as.factor(todos_juntos$clinico)
todos_juntos$cod_part=as.factor(todos_juntos$cod_part)
todos_juntos$condicion = as.factor(todos_juntos$condicion)
todos_juntos$resultado_participante_lag = lag(todos_juntos$resultado_participante)
todos_juntos$resultado_jugador_lag = lag(todos_juntos$resultado_jugador)

todos_juntos=todos_juntos[,-which(colnames(todos_juntos)=="trial")[1]]


todos_juntos <- todos_juntos %>%
  mutate(conflict = recode(condicion,
                           "21" = "lc",
                           "31" = "lc",
                           "32" = "lc",
                           "41" = "mc",
                           "42" = "mc",
                           "51" = "mc",
                           "52" = "mc",
                           "43" = "hc",
                           "53" = "hc",
                           "54" = "hc"),
         clinico = recode(clinico,
                          "0" = "Control",
                          "1" = "MD-SA")
  )


lm_betas=
  lmer(mean_0_26_32~conflict*clinico+(1|cod_part),
       na.action = na.omit, data=todos_juntos[todos_juntos$cat_elegida=="downward",],
  )
Anova(lm_betas)
grilla = ref.grid(lm_betas)
emmeans(grilla,pairwise~conflict)

conflict_plot = ggplot(todos_juntos, aes(x = conflict, y = mean_0_26_32)) +
  geom_point() +
  labs(x = "categoria", y = "betas") 
conflict_plot

conflict_plot_data = ddply(todos_juntos,c("conflict","cat_elegida","clinico"),summarize,media=mean(mean_0_26_32))
conflict_plot_data = conflict_plot_data[!conflict_plot_data$cat_elegida=="None",]

conflict_barplot = ggplot(conflict_plot_data,aes(x=cat_elegida,y=media,fill=conflict))

conflict_barplot + geom_bar(stat="identity",position="dodge") + facet_wrap(~clinico)

ddply(conflict_plot_data,"conflict",summarize,diff=upward-downward)

help(reshape)
