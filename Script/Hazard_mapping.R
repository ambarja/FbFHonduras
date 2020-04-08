# 1. Insumos --------------------------------------------------------------
library(tidyverse)
library(sf)
library(extrafont)
library(viridis)
library(tmap)

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


# Remover columnas
rmcol <- c("PLAGUE","EPIDEMIC","FROST","FIRE", "STRONGWIND",
           "FORESTFIRE","POLLUTION","SURGE" ,
           "STRUCTURE","ELECTRICSTORM","EXPLOSION","EARTHQUAKE","FOG",
           "OTHER","ACCIDENT","SPATE","AVALANCHE","TORNADO","HAILSTORM",
           "Tormenta Tropical","STORM","LIQUEFACTION","BIOLOGICAL","PANIC","HURRICANE",
           "SEDIMENTATION","TSUNAMI","LEAK","ALLUVION","HEATWAVE","ERUPTION")


# Nueva tabla de datos
tabla %>% 
  select(Departamento,Evento,Afectados,Damnificados) %>% 
  filter(!Evento %in% rmcol) -> newdata

newdata <- newdata %>% 
  mutate(Evento = recode(Evento,'FLOOD'='Inundaciones')) %>% 
  mutate(Evento = recode(Evento,'LANDSLIDE'='Deslizamientos')) %>% 
  mutate(Evento = recode(Evento,'RAIN'='Lluvias')) %>% 
  mutate(Evento = recode(Evento,'DROUGHT'='Sequías'))


# 3. Barras según el N° de afectados --------------------------------------
newdata %>% 
  select(Evento,Departamento,Damnificados,Afectados) %>% 
  group_by(Evento) %>% 
  summarise(Total = sum(Afectados)) %>% 
  arrange(desc(Total)) %>% 
  ggplot(aes(x = reorder(Evento,Total),y = Total)) + 
  geom_bar(stat = 'identity') + 
  coord_flip()


# nuevo gráfico 
newdata %>% 
  group_by(Evento) %>%
  summarise(Total = sum(Afectados)) %>% 
  ggplot(aes(x = reorder(Evento,Total), 
             y = Total,
             fill = Total)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  labs(y = 'Número de afectados',
       x= '') + 
  scale_fill_viridis() +
  theme_light() + 
  theme(legend.text = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.text.y = element_text(family  = 'DejaVu Sans Mono',
                                   face    = 'bold',
                                   color   = 'black'),
        axis.text.x = element_text(family  = 'DejaVu Sans Mono', 
                                   face    = 'bold', 
                                   color   = 'black'),
        axis.title.x = element_text(family = 'DejaVu Sans Mono', 
                                    face   = 'bold', 
                                    color  = 'black'), 
        legend.position = 'none') +
  labs(caption = 'Fuente: Elaboración propia con datos recolectados de DesInventar project')

ggsave(filename = '../Graficos/EventosxAfectados_v2.png',
       plot = last_plot(),width = 12,height = 6)



# 4. Join -----------------------------------------------------------------
newdata %>%
  filter(Evento == 'Inundaciones') %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) -> tabla

shp <- read_sf('../Datos/SHP/Departamentos.shp') 
shp$DEPTO <- toupper(shp$DEPTO)
anti_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))
tabla %>% 
  mutate(Departamento = recode(Departamento,'FR. MORAZAN'='FRANCISCO MORAZAN',
                               'STA BARBARA'='SANTA BARBARA')) -> tabla
anti_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))
join <- left_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))


newdata %>%
  filter(Evento == 'Sequías') %>% 
  group_by(Departamento) %>% 
  summarise(total = sum(Afectados)) -> tabla

shp <- read_sf('../Datos/SHP/Departamentos.shp') 
shp$DEPTO <- toupper(shp$DEPTO)
anti_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))
tabla %>% 
  mutate(Departamento = recode(Departamento,'FR. MORAZAN'='FRANCISCO MORAZAN',
                               'STA BARBARA'='SANTA BARBARA')) -> tabla
anti_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))
join <- left_join(x = shp,y = tabla, by = c('DEPTO'='Departamento'))

# 5. Mapa de inundación ---------------------------------------------------

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(0.7,0)) +
  tm_fill(col = 'total',style = 'quantile',palette = 'viridis',
          title = 'Afectados por \n Inundaciones') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.04,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'DEPTO',size = 0.6,fontface = 'bold')-> mapa

tmap_save(tm = mapa,
          filename = '../Mapas/Inundaciones_Departamento.png',
          width = 11,height = 8)

# 6. Mapas de sequías -----------------------------------------------------

join %>% 
  mutate(total = if_else(is.na(total),0,total)) -> join

join %>% 
  tm_shape() + 
  tm_layout(frame = F, legend.position = c(0.7,0)) +
  tm_fill(col = 'total',style = 'jenks',palette = 'viridis',
          title = 'Afectados por \n sequías') + 
  tm_grid(n.x = 5, n.y = 5,lwd = 0.2) + 
  tm_scale_bar(position = c(0.04,0),width = 0.2,lwd = 0.2) + 
  tm_compass(position = c(0,0.8)) +
  tm_text(text = 'DEPTO',size = 0.6,fontface = 'bold')-> mapa

tmap_save(tm = mapa,
          filename = '../Mapas/sequías_Departamento.png',
          width = 11,height = 8)
