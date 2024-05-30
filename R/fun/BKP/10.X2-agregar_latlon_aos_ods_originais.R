library('tidyverse')
library('tidylog')
library('sf')
library('mapview')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_viario_osm   <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)


# ------------------------------------------------------------------------------
# Juntar latlons dos centróides aos qgis_id de origem e destino
# ------------------------------------------------------------------------------

# Abrir arquivo resultante da etapa anterior, com qgis_ids de origem e destino
open_file <- sprintf('%s/01_origens_e_destinos_viagens_consideradas.csv', pasta_orig_vs_mod)
ods_vgs   <- read_delim(open_file, delim = ';', col_types = cols(.default = "c"))
head(ods_vgs)

# Abrir shape de linhas do viário do OpenStreetMap com osm_id, qgis_id e length_m
viario_osm <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_viario_osm)
viario_osm <- read_sf(viario_osm) %>% select(qgis_id)
head(viario_osm)

# Isolar somente qgis_ids presentes nas bases de viagem
viario_osm <- viario_osm %>% filter(qgis_id %in% ods_vgs$qgis_id | qgis_id %in% ods_vgs$qgis_id_to)
# mapview(viario_osm)

# Gerar centróides dos trechos de viário e transformar em WGS84
viario_osm <- viario_osm %>% st_centroid() %>% st_transform(4326)
head(viario_osm)

# Puxar colunas de latlon para cada centróide
# https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
# viario_osm %>% cbind(viario_osm, st_coordinates(viario_osm))
viario_osm <- 
  viario_osm %>% 
  mutate(lon = st_coordinates(geom)[,1],
         lat = st_coordinates(geom)[,2]) %>% 
  st_drop_geometry()


# Juntar coordenadas dos centróides de cada qgis_id à origem e destino
ods_vgs <- 
  ods_vgs %>% 
  # Renomear colunas para facilitar saber a qual cada coordenada se refere
  rename(qgis_id.x = qgis_id,
         qgis_id.y = qgis_id_to) %>% 
  left_join(viario_osm, by = c('qgis_id.x' = 'qgis_id')) %>% 
  left_join(viario_osm, by = c('qgis_id.y' = 'qgis_id'))

# Para garantir, temos algum NA?
colSums(is.na(ods_vgs))


# Gravar resultados
out_file <- sprintf('%s/02_origens_e_destinos_com_latlon.csv', pasta_orig_vs_mod)
write_delim(ods_vgs, out_file, delim = ';')
