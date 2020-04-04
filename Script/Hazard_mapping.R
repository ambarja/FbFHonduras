# 1. Insumos --------------------------------------------------------------
library(tidyverse)
library(sf)
library(extrafont)
library(viridis)


# 2. Lectura de datos  ----------------------------------------------------
read_csv('../Datos/Hazard_dataset.csv') %>%
  separate(col = Fecha,
           into = c('Año','Mes','Dia'),
           sep = '-') %>% 
  select(Evento,
         Año,
         Departamento,
         Municipio,
         Damnificados,
         Afectados,
         Observaciones) %>% 
  mutate(Damnificados = if_else(is.na(Damnificados),0,Damnificados),
         Afectados = if_else(is.na(Afectados),0,Afectados))-> tabla


# 3. Barras según el N° de afectados --------------------------------------
tabla %>% 
  select(Evento,Departamento,Damnificados,Afectados) %>% 
  group_by(Evento,Departamento) %>% 
  summarise(Total = sum(Afectados)) %>% 
  arrange(desc(Total)) %>% 
  ggplot(aes(x = reorder(Evento,Total),y = Total)) + 
  geom_bar(stat = 'identity') + 
  coord_flip()


