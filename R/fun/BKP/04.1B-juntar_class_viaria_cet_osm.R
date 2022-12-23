# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados <- "../../yellow_dados"
pasta_tmp <- sprintf("%s/TMP_APAGAR", pasta_dados)

# Arquivo com os vértices dos viários OSM e classificação viária da CET/Geosampa
# associados por nearest neighbor a 15 metros
open_file <- sprintf('%s/Vertices CET-OSM Nearest Neighbor.gpkg', pasta_tmp)
class_viaria <- read_sf(open_file)
# head(class_viaria)
# 
# names(class_viaria)

# Queremos apenas as três colunas principais, para agrupar depois
class_viaria <- 
  class_viaria %>% 
  st_drop_geometry() %>% 
  filter(n == 1) %>% 
  select(qgis_id, cvc_dctipo, distance)
# head(class_viaria)


# class_viaria %>% group_by(qgis_id, cvc_dctipo) %>% summarise(dist = sum(distance)) %>% head(20)

# Agrupar por qgis_id e cvc_dctipo e filtrar o qgis_id que tem a soma maior de
# extensão. Ficar só com as colunas de qgis_id e cvc_dctipo
class_viaria <- 
  class_viaria %>% 
  group_by(qgis_id, cvc_dctipo) %>% 
  summarise(dist = sum(distance)) %>% 
  filter(dist == max(dist)) %>% 
  select(-dist) %>% 
  mutate(cvc_dctipo = tolower(cvc_dctipo))

# out_file <- sprintf('%s/OSM_class_viaria_por_QGIS_ID.csv', pasta_tmp)
# write_delim(class_viaria, out_file, delim = ';')


# Abrir viário do OSM para fazer associação entre qgis_id e classificação viária
open_file2 <- sprintf('%s/Cleaned - Viario OSM com altimetrias.gpkg', pasta_tmp)
viario_OSM <- read_sf(open_file2)
# head(viario_OSM)

# Executar a associação entre qgis_id e classificação viária
viario_OSM <- viario_OSM %>% left_join(class_viaria, by = 'qgis_id')

# Exportar como .gpkg
out_file <- sprintf('%s/OSM_class_viaria_por_QGIS_ID.gpkg', pasta_tmp)
st_write(viario_OSM, out_file, driver = 'GPKG', append = FALSE)
