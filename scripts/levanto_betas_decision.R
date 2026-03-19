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
base_dir = "./input" #carpeta dentro del proyecto que tiene los datos que entro

betas = read.csv(paste0(base_dir,"/analysis_table_0_26_32.csv")) #cambiar nombre por el archivo que contiene los beta
betas$codigo = substr(betas$codigo,0,16)
betas$nro_con = substr(betas$contrast,6,8)
betas$cod_con = paste0(betas$codigo,betas$nro_con)

todos_juntos<- readRDS("input/datosCompletos.rds") #aca levanto mis datos comportamentales sin Limpiar
betas['code'] = apply(betas['codigo'], 2, tolower)
todos_juntos<- readRDS("input/datosCompletos.rds")
output = left_join(betas, todos_juntos, by="code",relationship = "many-to-many" )
#uno el data frame de los betas con el todos_juntos en el output

#todos_juntos$conds = todos_juntos$trial no se que hace esta linea ??

#separo en sesiones
sesion2 = output$nro_bloque == "2"
sesion3 = output$nro_bloque == "3"
sesion4 = output$nro_bloque == "4"

output$conds = output$nro_trial_global #aća va el tiral global o el trial por sesion??

output$conds[sesion2] = output$conds[sesion2] + (no_interest*1)
output$conds[sesion3] = output$conds[sesion3] + (no_interest*2)
output$conds[sesion4] = output$conds[sesion4] + (no_interest*3)
#hasta aca esta adaptado----- 
# todos_juntos$conds = formatC(todos_juntos$conds, width = 3, format = "d", flag = "0")
# todos_juntos$conds = paste0(todos_juntos$cod_part,todos_juntos$conds)

nro_decisiones = ddply(todos_juntos,c("cod_part","nro_sesion"),summarize,n=n()) #fijarse si esto anda

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
  
  allDec = c(allDec,partDec) #el numero de beta que me interesa
  
}

betas <- betas[as.factor(betas$cod_con) %in% as.factor(allDec),]

ordered_betas = arrange(betas,betas$cod_con)
ordered_todos_juntos = arrange(todos_juntos,todos_juntos$conds)

sum(ordered_betas$cod_con == ordered_todos_juntos$conds)

final_table = (merge(ordered_todos_juntos,ordered_betas,by.x="conds",by.y="cod_con"))
setwd("/media/cibpsi/Portable Drive/datos_tesis_maestria/files_team_task/spm12_batch/extraccion_betas/extraccion/x_trial_decision")
write.csv(final_table,"full_table")
