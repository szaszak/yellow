# Inserir como parte da função obter_dados_edges() no script valhalla_map_matching.R



# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_base         <- sprintf("%s/05_testes_viagens_20181112-20181117", pasta_dados)
pasta_csv2         <- sprintf("%s/viagens_processadas_csv2", pasta_base)


resultados <- sprintf('%s/000144_00.csv', pasta_csv2)
resultados <- read_delim(open_file, delim = ';', col_types = 'cidddddddicddccciiiddcc')
resultados <- resultados %>% select(edges.way_id, edges.length, matched_points.lat, matched_points.lon, isovalor_var)
resultados <- resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326)
resultados %>% mapview()

osm_ids <- resultados %>% select(edges.way_id) %>% distinct()

altimetrias <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z_por_quadra_com_aclividades.gpkg', pasta_elevacao)
altimetrias <- read_sf(altimetrias)
altimetrias <- altimetrias %>% filter(qgis_id != qgis_id_2)
altimetrias <- altimetrias %>% st_transform(4326)
head(altimetrias)


# Selecionar somente linhas que possuem os mesmos osm_id da rota - queremos os
# qgis_id dessas linhas
altimetrias_sel <- 
  altimetrias %>% 
  filter(osm_id %in% osm_ids$edges.way_id) %>% 
  st_cast('POINT')

# Agora queremos os pontos mais próximos
alt_sel_ids <- 
  altimetrias_sel %>% 
  select(osm_id, qgis_id) %>% 
  slice(st_nearest_feature(resultados, altimetrias_sel)) %>% 
  st_drop_geometry() %>% 
  select(qgis_id)

# # Para quais qgis_id o ciclista está se deslocando?
# id_to <- 
#   alt_sel_ids %>% 
#   distinct() %>% 
#   # Puxar o próximo qgis_id da linha para uma columa - na última linha, em que
#   # é NA, vai virar um id igual ao da origem
#   mutate(qgis_id_2 = shift(qgis_id, type = 'lead'),
#          qgis_id_2 = case_when(is.na(qgis_id_2) ~ qgis_id,
#                                TRUE ~ qgis_id_2))
#   
# 
# alt_sel_ids <- alt_sel_ids %>% left_join(id_to, by = 'qgis_id')
# 
# 
# resultados <- resultados %>% cbind(alt_sel_ids)
# resultados %>% tail()

resultados <- resultados %>% cbind(alt_sel_ids)
 
altimetrias_sel <-
  altimetrias_sel %>%
  st_drop_geometry() %>%
  select(qgis_id, length_m) %>%
  distinct()
# 
# 
# resultados %>% left_join(altimetrias_sel, by = c('qgis_id', 'qgis_id_2')) %>% st_drop_geometry()
# 
# 
# altimetrias %>% filter(qgis_id == '154890' & qgis_id_2 == '218830')
# 
# 
# 
# 
# 
# 
# resultados <- sprintf('%s/000144_00.csv', pasta_csv2)
# resultados <- read_delim(open_file, delim = ';', col_types = 'cidddddddicddccciiiddcc')
# resultados <- resultados %>% select(edges.way_id, edges.length, isovalor_var)

alt_sel_ids %>% distinct()

resultados_final <- 
  resultados %>% 
  st_drop_geometry() %>% 
  group_by(edges.way_id, edges.length) %>% 
  summarise(alt_var = sum(isovalor_var)) %>%
  mutate(
    # Calcular elev_grad (m): Δy / Δx
    # https://www.calculator.net/slope-calculator.html
    elev_grad = alt_var / edges.length,
    # Calcular ângulo de inclinação (θ): arctan(Δy / Δx) - como a função
    # atan() traz o resultado em radianos, é preciso converter para graus
    # https://r-lang.com/how-to-convert-radians-to-degrees-in-r/
    elev_ang_inc = atan(elev_grad) * 180 / pi)

resultados_final
sum(resultados_final$edges.length)
sum(resultados_final$length_m)

resultados <- resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326)
resultados %>% mapview()