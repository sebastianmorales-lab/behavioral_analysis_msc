archivos=list.files("/media/cibpsi/Portable Drive/datos_tesis_maestria/fmri_team_task_data/comportamental/team2Colapsado_modelo_con_stimulus_duration_y_question_no_interest",recursive=T,full.names = T)

comportamentales=grep("team2trivia", archivos)

comportamentales = archivos[comportamentales]

todos_juntos = data.frame()

for (name in comportamentales) {
  
  logfile = read.csv(name,na.strings = c("[]","a","d"))
  
  todos_juntos = rbind(todos_juntos,logfile)
  
}

todos_juntos$cod_part = formatC(todos_juntos$cod_part, width = 3, format = "d", flag = "0")

cuestionarios = read.csv("/media/cibpsi/Portable Drive/datos_tesis_maestria/fmri_team_task_data/subject_key_data.csv")
cuestionarios$codigo = substr(cuestionarios$codigo,14,16)

todos_juntos = merge(todos_juntos,cuestionarios[,c("codigo","clinico")], by.x = "cod_part", by.y = "codigo")
