pacman::p_load(tidyverse, # Manipulacion datos
               sjPlot, # Graficos y tablas
               sjmisc, # Descriptivos
               corrplot, # Correlaciones
               psych, # Test estadísticos
               kableExtra, # Tablas
               rempsyc,
               broom,
               sjstats,
               gginference,
               haven,
               ggplot,
               ggpubr,
               gtsummary)


options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

casen2022 <- read_sav("input/Base de datos Casen 2022 SPSS_18 marzo 2024.sav") # llamar desde carpeta localcasen2022_subset <- casen2022 %>% 

casen2022 <- read_dta("input/data-orig/ene-2024-11-ond.dta") # llamar desde carpeta localcasen2022_subset <- casen2022 %>% 

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
     file = "input/data/casen2022.Rdata") #guardar objeto

load("input/data/casen2022.Rdata")

casen2022 <- casen2022 %>%
  mutate(across(everything(), ~ na_if(., -88)))

casen2022 <- casen2022 %>%
  mutate(across(everything(), ~ na_if(., -99)))


sjPlot::sjt.xtab(var.row = casen2022$tamaño_empresa, 
                 var.col = casen2022$pertenece_sindicato, 
                 show.summary = F, 
                 emph.total = T, 
                 show.row.prc = T, # porcentaje fila
                 show.col.prc = T # porcentaje columna
)

sjmisc::frq(casen2022$tamaño_empresa)
sjmisc::frq(casen2022$pertenece_sindicato)

casen2022 <- casen2022 %>%
  mutate(pertenece_asoc = as.numeric(if_any(c(pertenece_sindicato, pertenece_asoc_func, 
                                 pertenece_gremio, pertenece_colegio_prof), ~ . == 1)))

### Descriptivos ----

descriptivos <- describe(casen2022,)

descriptivos <- summarytools::descr(casen2022, transpose = TRUE)

tab_df(descriptivos, show.rownames = TRUE)

colSums(is.na(casen2022))

summarytools::dfSummary(casen2022, style = descriptivos)

# Bivariada ----

cor.test(casen2022$pertenece_sindicato, casen2022$tamaño_empresa, use = "complete.obs")

cor.test(casen2022$salario, casen2022$tamaño_empresa, use = "complete.obs")

t.test(casen2022$pertenece_asoc, casen2022$salario)

ggplot(data=casen2022, aes(x=pertenece_sindicato, y=tamaño_empresa)) +
  geom_bar(stat="identity")

ggplot(data=casen2022, aes(x=pertenece_asoc_func, y=salario)) +
  geom_point(stat="identity")

test_sal_sindi <- t.test(casen2022$tamaño_empresa ~ casen2022$pertenece_sindicato, 
                         alternative = "greater",
                         conf.level = 0.95)
test_sal_sindi
gginference::ggttest(test_sal_sindi)