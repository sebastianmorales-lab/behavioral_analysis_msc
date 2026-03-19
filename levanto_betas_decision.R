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
library(tidyverse)

no_interest = 4
base_dir = "/media/cibpsi/Portable Drive/datos_tesis_maestria/files_team_task/spm12_batch/extraccion_betas/"

betas = read.csv(paste0(base_dir,"extraccion/x_trial_decision/extraccion.csv"))
betas$codigo = substr(betas$codigo,14,16)
betas$nro_con = substr(betas$contrast,6,8)
betas$cod_con = paste0(betas$codigo,betas$nro_con)

source(paste0(base_dir,"analisis/x_trial_decision/levanto_comportamentales.R"))

todos_juntos <- todos_juntos[as.factor(todos_juntos$cod_part) %in% as.factor(betas$codigo),]

todos_juntos$cod_part = formatC(todos_juntos$cod_part, width = 3, format = "d", flag = "0")

todos_juntos$conds = todos_juntos$trial

sesion2 = todos_juntos$nro_sesion == "2"
sesion3 = todos_juntos$nro_sesion == "3"
sesion4 = todos_juntos$nro_sesion == "4"

todos_juntos$conds[sesion2] = todos_juntos$conds[sesion2] + (no_interest*1)
todos_juntos$conds[sesion3] = todos_juntos$conds[sesion3] + (no_interest*2)
todos_juntos$conds[sesion4] = todos_juntos$conds[sesion4] + (no_interest*3)

todos_juntos$conds = formatC(todos_juntos$conds, width = 3, format = "d", flag = "0")
todos_juntos$conds = paste0(todos_juntos$cod_part,todos_juntos$conds)

nro_decisiones = ddply(todos_juntos,c("cod_part","nro_sesion"),summarize,n=n())

allDec = c()

for (i in levels(as.factor(nro_decisiones$cod_part))) {
  
  decXsesion = nro_decisiones[nro_decisiones$cod_part == i,]
  
  startDecSes1 = 1
  
  endDecSes1   = decXsesion$n[1]
  startDecSes2 = endDecSes1 + no_interest + 1
  
  endDecSes2   = startDecSes2 + decXsesion$n[2] -1
  startDecSes3 = endDecSes2 + no_interest + 1
  
  endDecSes3   = startDecSes3 + decXsesion$n[3] -1
  startDecSes4 = endDecSes3 + no_interest + 1
  
  endDecSes4   = startDecSes4 + decXsesion$n[4] -1
  
  decConds = c(startDecSes1:endDecSes1,startDecSes2:endDecSes2,
               
               startDecSes3:endDecSes3,startDecSes4:endDecSes4)
  
  decConds = formatC(decConds, width = 3, format = "d", flag = "0")
  
  partDec = paste0(i,decConds)
  
  allDec = c(allDec,partDec)
  
}

betas <- betas[as.factor(betas$cod_con) %in% as.factor(allDec),]

ordered_betas = arrange(betas,betas$cod_con)
ordered_todos_juntos = arrange(todos_juntos,todos_juntos$conds)

sum(ordered_betas$cod_con == ordered_todos_juntos$conds)

final_table = (merge(ordered_todos_juntos,ordered_betas,by.x="conds",by.y="cod_con"))
setwd("/media/cibpsi/Portable Drive/datos_tesis_maestria/files_team_task/spm12_batch/extraccion_betas/extraccion/x_trial_decision")
write.csv(final_table,"full_table")
