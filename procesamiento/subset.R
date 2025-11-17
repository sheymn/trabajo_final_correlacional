library(dplyr)
library(haven)

rm(list = ls()) # para limpiar el entorno de trabajo

casen2022 <- read_sav("input/data-orig/Base de datos Casen 2022 SPSS_18 marzo 2024.sav") # llamar desde carpeta localcasen2022_subset <- casen2022 %>% 

casen2022_subset <- casen2022 %>%  
  select(y1, y2_dias, y2_hrs,sexo, edad, e6a_asiste, e6a_no_asiste, o1, o10, 
         o12, o14, o15, o16, o19, o18, o25, o26a, o26b, o26c, o26d,)  # seleccionar variables y dar un nombre al subset (puede ser cualquier nombre)

casen2022_subset <- casen2022_subset %>% 
  rename("salario" = y1, "trabajo_hrs_pac" = y2_hrs, "trabajo_dias" = y2_dias, 
         "nivel_edu_asist" = e6a_asiste, "nivel_edu" = e6a_no_asiste, "ocupado" = o1, 
         "trabajo_hrs" = o10, "tipo_trabajo" = o12, "trabajo_como" = o15, 
         "regist_sii" = o16, "contrato" = o19, "contrato_tipo" = o18, 
         "tamaño_empresa" = o25, "pertenece_sindicato" = o26a, "pertenece_asoc_func" = o26b,
         "pertenece_gremio" = o26c, "pertenece_colegio_prof" = o26d) #renombrar 

rm(list = c('casen2022')) # quitar del environment para liberar espacio en la memoria
casen2022 <- casen2022_subset
rm(list = c('casen2022_subset')) # quitar del environment para liberar espacio en la memoria

save(casen2022, 
     file = "input/data-proc/casen2022.Rdata") #guardar objeto
