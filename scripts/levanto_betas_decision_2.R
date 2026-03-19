# El objetivo de este script es unir la planilla de betas con la planilla comportamental.
# Para eso, se va a crear una columna que este formada por el codigo del sujeto pegado
# con el numero de trial (por ej, "sujeto_1", "ronda_1" pasaria a llamarse "sujeto_1ronda_1").
# Esto se va a hacer tanto para la planilla de betas como para la comportamental. Luego
# se van a unir ambas planillas usando esta variable en comun.

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
betas['code'] = apply(betas['codigo'], 2, tolower)
betas$cod_con = paste0(betas$code,betas$nro_con)

todos_juntos<- readRDS("input/datosCompletos.rds") #aca levanto mis datos comportamentales sin Limpiar
#output = left_join(betas, todos_juntos, by="code",relationship = "many-to-many" )
#uno el data frame de los betas con el todos_juntos en el output

#todos_juntos$conds = todos_juntos$trial no se que hace esta linea ??

todos_juntos$conds = todos_juntos$nro_trial_global #aća va el tiral global o el trial por sesion??

todos_juntos = todos_juntos[!is.na(todos_juntos$conds),]

#separo en sesiones
sesion2 = todos_juntos$nro_bloque == "2"
sesion3 = todos_juntos$nro_bloque == "3"
sesion4 = todos_juntos$nro_bloque == "4"

# La variable conds es igual a trial. En estas tres lineas le sumo el numero de
# regresores de interes que tengo, porque en el archivo de betas luego de que 
# termina una sesion los contrastes siguen con los contrastes de los outcomes.
# Es decir el contraste 33 en betas corresponde al contraste de bb, mientras
# que en el archivo comportamental el trial 33 corresponde a una ronda
todos_juntos$conds[sesion2] = todos_juntos$conds[sesion2] + (no_interest*1)
todos_juntos$conds[sesion3] = todos_juntos$conds[sesion3] + (no_interest*2)
todos_juntos$conds[sesion4] = todos_juntos$conds[sesion4] + (no_interest*3)
#hasta aca esta adaptado----- 
 todos_juntos$conds = formatC(todos_juntos$conds, width = 3, format = "d", flag = "0")
 todos_juntos$conds = paste0(todos_juntos$code,todos_juntos$conds)

nro_decisiones = ddply(todos_juntos,c("code","nro_bloque"),summarize,n=n()) #fijarse si esto anda

allDec = c()

# No puedo directamente agarrar todos los contrastes que estan en betas, ya que hay
# algunos contrastes que corresponden a outcomes. Por lo tanto, en este for, voy a 
# agarrar el numero de trials que hubo en una sesion, luego salterame la cantidad
# de regresores de interes que tengo, y agarrar los trials de la siguiente sesion.
# Esto para cada sujeto, de manera loopeada. Los regresores de movimiento no cuentan
# como regresores de interes, ya que no se pusieron como contraste en el script de matlab.
for (i in levels(as.factor(nro_decisiones$code))) {
  
  decXsesion = nro_decisiones[nro_decisiones$code == i,]
  
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
  
  allDec = c(allDec,partDec) #tiene los numeros de betas que corresponden a contrastes de trials, y no a outcomes
  
}

betas <- betas[as.factor(betas$cod_con) %in% as.factor(allDec),] #

ordered_betas = arrange(betas,betas$cod_con)
ordered_todos_juntos = arrange(todos_juntos,todos_juntos$conds)

#sum(ordered_betas$cod_con == ordered_todos_juntos$conds)

final_table = (merge(ordered_todos_juntos,ordered_betas,by.x="conds",by.y="cod_con"))
setwd("./input")
write.csv(final_table,"full_table.csv")
