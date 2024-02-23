# ------------------------------------------------------------------------------
# Isolar pontos de início e fim de viário para associar a semáforos
# ------------------------------------------------------------------------------

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
# pasta_viario_osm   <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_atrib_viario <- sprintf("%s/04_atributos_viario", pasta_dados)

# # Abrir shape de linhas do viário do OpenStreetMap com osm_id, qgis_id e length_m
# viario_osm <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_viario_osm)
# viario_osm <- read_sf(viario_osm)
# 
# # Do shape de viário, queremos só os ids e a extensão dos arcos
# viario_osm <- viario_osm %>% st_drop_geometry() %>% select(osm_id, qgis_id, length_m)
# head(viario_osm)

# Abrir shape de pontos do viário do OpenStreetMap com valoz Z na coluna elev_mdt
elevacao_viario <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped.gpkg', pasta_elevacao)
elevacao_viario <- read_sf(elevacao_viario)

# Abrir shape de semáforos exportados do OSM
semaforos_osm <- sprintf('%s/listagem_osm_semaforos.gpkg', pasta_atrib_viario)
semaforos_osm <- read_sf(semaforos_osm)



# ------------------------------------------------------------------------------
# Tratamento das bases
# ------------------------------------------------------------------------------

# Na base de elevação, simplificar dataframe e criar um id único para cada ponto
elevacao_viario <- 
  elevacao_viario %>% select(osm_id, qgis_id, distance) %>% 
  add_column(id_ponto = 1:nrow(elevacao_viario), .before = 'osm_id')

head(elevacao_viario)


# Criar um dataframe dos pontos de elevação sem a geometria
elevacao_viario_df <- elevacao_viario %>% st_drop_geometry()
head(elevacao_viario_df)


# Na base de semáforos, remover semáforos de pedestres e piscantes
semaforos_osm <- semaforos_osm %>% filter(blinker == 'não' & crossing == 'não' & pedestrian == 'não')

head(semaforos_osm)



# ------------------------------------------------------------------------------
# Semáforos no início/fim do trecho (com relação ao sentido do traçado da linha)
# ------------------------------------------------------------------------------

# Para cada trecho do viário (qgis_id), achar primeiro e último pontos. Este 
# valor é dado pela coluna "distance", gerada ao criar os pontos a cada 2m -
# ao fazer isso, o QGIS considerou como início da linha o início do traçado,
# seguindo em direção para onde a linha apontava. Esse sentido é o mesmo do 
# fluxo de veículos para vias de sentido único no OSM
pontos_inicio_fim <- 
  elevacao_viario %>% 
  st_drop_geometry() %>% 
  group_by(qgis_id) %>% 
  summarise(p1_dist = min(distance),
            p2_dist = max(distance))


# Achar ids de pontos relativos ao início dos trechos
pontos_inicio <- 
  pontos_inicio_fim %>% 
  select(qgis_id, p1_dist) %>% 
  left_join(elevacao_viario_df, by = c('qgis_id', 'p1_dist' = 'distance')) %>% 
  select(id_ponto)

# Isolar somente pontos relativos ao início do traçado das linhas de cada trecho
pontos_viario_inicio <- elevacao_viario %>% filter(id_ponto %in% pontos_inicio$id_ponto)
head(pontos_viario_inicio)

# Gravar resultados
out_file1 <- sprintf('%s/tmp_pontos_viario_inicio.gpkg', pasta_atrib_viario)
st_write(pontos_viario_inicio, out_file1, driver = 'GPKG', append = FALSE)


# Achar ids de pontos relativos ao início dos trechos
pontos_fim <- 
  pontos_inicio_fim %>% 
  select(qgis_id, p2_dist) %>% 
  left_join(elevacao_viario_df, by = c('qgis_id', 'p2_dist' = 'distance')) %>% 
  select(id_ponto)

# Isolar somente pontos relativos ao final do traçado das linhas de cada trecho
pontos_viario_fim <- elevacao_viario %>% filter(id_ponto %in% pontos_fim$id_ponto)
head(pontos_viario_fim)

# Gravar resultados
out_file2 <- sprintf('%s/tmp_pontos_viario_fim.gpkg', pasta_atrib_viario)
st_write(pontos_viario_fim, out_file2, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Buffer de 2 m em volta dos pontos de semáforos
# ------------------------------------------------------------------------------

# Criar um buffer de 2 metros à volta dos pontos de semáforo, para ser usado
# no st_intersects depois
buffer_semaforos <-
  semaforos_osm %>%
  # Transformar em SIRGAS para distância ser calculada em metros
  st_transform(31983) %>%
  st_buffer(dist = 2) %>%
  # Transformar de volta em WGS84
  st_transform(4326)

# mapview(semaforos_osm) + mapview(buffer_semaforos)


# Gravar resultados
out_file3 <- sprintf('%s/tmp_buffer_2m_semaforos.gpkg', pasta_atrib_viario)
st_write(buffer_semaforos, out_file3, driver = 'GPKG', append = FALSE)
