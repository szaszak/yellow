# Inserir como parte da função obter_dados_edges() no script valhalla_map_matching.R

# carregar bibliotecas
source('fun/setup.R')
source('fun/st_dbscan.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
# pasta_base         <- sprintf("%s/05_testes_viagens_20181112-20181117", pasta_dados)
pasta_base         <- sprintf("%s/06_testes_viagens_pre_modelo", pasta_dados)
pasta_csv2         <- sprintf("%s/viagens_processadas_csv2", pasta_base)

altimetrias <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z_por_quadra_com_aclividades.gpkg', pasta_elevacao)
altimetrias <- read_sf(altimetrias)
altimetrias <- altimetrias %>% filter(qgis_id != qgis_id_2)
altimetrias <- altimetrias %>% st_transform(4326)
head(altimetrias)

# resultados <- sprintf('%s/000144_00.csv', pasta_csv2)
# resultados <- sprintf('%s/157340_00.csv', pasta_csv2)
resultados <- sprintf('%s/027463_02.csv', pasta_csv2)
resultados <- read_delim(resultados, delim = ';', col_types = 'cidddddddicddccciiiddcc')
resultados <- 
  resultados %>% 
  select(edges.way_id, edges.length, matched_points.lat, matched_points.lon, 
         isovalor_var, osm_cycletype, area_restrita) %>% 
  # Precisaremos de uma coluna com a sequência dos pontos para reordenar o
  # dataframe após a informação de altimetrias
  add_column(seq_order = 1:nrow(.), .before = 'edges.way_id') %>% 
  st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326)

resultados %>% mapview(zcol = 'edges.way_id')
resultados %>% head(20)

# -----------------------------------------------------------------------------
# Calcular bearing - direção para onde os pontos estão se movendo
# -----------------------------------------------------------------------------

# # Calcular ângulo de direção (bearing) com geosphere::bearing()
# # https://rspatial.org/raster/sphere/3-direction.html
# # Este método resulta no mesmo bearing do que o st_geod_azimuth()
# resultados %>%
#   mutate(lat_to = shift(matched_points.lat, type = 'lead'),
#          lon_to = shift(matched_points.lon, type = 'lead'),
#          this = bearing(cbind(matched_points.lat, matched_points.lon), cbind(lat_to, lon_to)),
#          that = bearing(cbind(matched_points.lon, matched_points.lat), cbind(lon_to, lat_to))) %>% 
#   select(-c(edges.length, isovalor_var, elev_grad)) %>% 
#   st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326) %>% 
#   mutate(those = c(lwgeom::st_geod_azimuth(geometry),
#                    units::set_units(NA, "radians")),
#          those = units::set_units(those, "degrees")) %>% 
#   st_drop_geometry()


# Calcular "bearing", que seria a direção angular para onde os pontos estão
# se movendo
# https://stackoverflow.com/questions/72276233/calculate-the-angle-between-two-sf-points-in-r-to-calculate-the-direction-of-roa
# https://www.rdocumentation.org/packages/lwgeom/versions/0.2-8/topics/st_geod_azimuth
# https://www.rdocumentation.org/packages/geosphere/versions/1.5-14/topics/bearing
resultados <- 
  resultados %>%
  mutate(bearing = c(lwgeom::st_geod_azimuth(geometry),
                     units::set_units(NA, "radians")),
         bearing = units::set_units(bearing, "degrees"),
         .before = 'geometry') 

# resultados %>% mapview(zcol = 'edges.way_id')
resultados %>% head(20)


# -----------------------------------------------------------------------------
# Juntar gradiente de aclividade/declividade
# -----------------------------------------------------------------------------

# Da base de altimetrias completa, selecionar somente linhas que possuem os 
# mesmos osm_id da rota contida na base de resultados. Isolar ajuda o 
# processamento seguinte a rodar mais rápido. Vamos querer os qgis_id dessas linhas
sel_osm_ids <- resultados %>% distinct(edges.way_id)

altimetrias_sel <- 
  altimetrias %>% 
  select(osm_id, qgis_id, length_m, elev_grad, geometry = geom) %>% 
  filter(osm_id %in% sel_osm_ids$edges.way_id) %>% 
  st_cast('POINT')

altimetrias_sel %>% mapview(zcol = 'qgis_id')

tmp_df <- data.frame()
for (id in sel_osm_ids$edges.way_id) {
  # Filtrar somente linhas de resultados e altimetrias com o mesmo osm_id
  tmp_resultados  <- resultados %>% filter(edges.way_id == id)
  tmp_altimetrias <- altimetrias_sel %>% filter(osm_id == id)
  
  # Puxar os dados do ponto de altimetria mais próximo (do mesmo osm_id)
  tmp_out <- 
    tmp_altimetrias %>% 
    slice(st_nearest_feature(tmp_resultados, tmp_altimetrias)) %>% 
    st_drop_geometry() %>% 
    select(qgis_id, length_m, elev_grad) %>% 
    # Adicionar a coluna de sq_order, que vai permitir reordenar o dataframe
    add_column(seq_order = tmp_resultados$seq_order)
  
  tmp_df <- tmp_df %>% rbind(tmp_out)
}

resultados <- resultados %>% left_join(tmp_df, by = 'seq_order')
resultados %>% mapview(zcol = 'qgis_id')

# # Isolar qgis_id junto com os dados de extensão do trecho no QGIS (length_m) e
# # o gradiente. Notar que o valor do gradiente aqui se refere a um dos sentidos,
# # sendo preciso reavaliá-lo depois
# alt_sel_ids <- 
#   altimetrias_sel %>% 
#   # Puxar ponto de altimetria mais próximo (pode ter osm_id diferente - pontos
#   # próximos a esquinas podem pegar o ponto de altimetria do trecho seguinte. 
#   # Isso não deve afetar muito o resultado, pois de qualquer forma o trajeto 
#   # seguiria para aquele ponto alguns segundos depois
#   slice(st_nearest_feature(resultados, altimetrias_sel)) %>% 
#   st_drop_geometry() %>% 
#   select(qgis_id, length_m, elev_grad)
# 
# # # Para quais qgis_id o ciclista está se deslocando?
# # id_to <- 
# #   alt_sel_ids %>% 
# #   distinct() %>% 
# #   # Puxar o próximo qgis_id da linha para uma columa - na última linha, em que
# #   # é NA, vai virar um id igual ao da origem
# #   mutate(qgis_id_2 = shift(qgis_id, type = 'lead'),
# #          qgis_id_2 = case_when(is.na(qgis_id_2) ~ qgis_id,
# #                                TRUE ~ qgis_id_2))
# #   
# # 
# # alt_sel_ids <- alt_sel_ids %>% left_join(id_to, by = 'qgis_id')
# # resultados <- resultados %>% cbind(alt_sel_ids)
# # resultados %>% tail()
# 
# 
# # Juntar os dois dataframes, que possuem o mesmo número de linhas
# resultados <- resultados %>% cbind(alt_sel_ids)
# resultados %>% mapview(zcol = 'qgis_id')


# -----------------------------------------------------------------------------
# Revisar extensão do arco
# -----------------------------------------------------------------------------

# Por enquanto, temos duas extensões do arco: uma vinda do edges.length e outra
# vinda do length_m. A primeira em teoria se refere à extensão percorrida no 
# arco de um determinado osm_id e calculada com base no map matching - mas esta
# extensão pode estar subestimada em casos em que o map matching traçou rotas
# estranhas. A segunda se refere à extensão do arco no QGIS, mas é o arco todo
# e não somente o trecho percorrido. Vamos fazer uma coluna de extensão revista
# que seja o menor valor dos dois
resultados <- 
  resultados %>% 
  # Caso o valor seja "igual", length_m é preferível por não estar arredondado
  mutate(length_m_rev = ifelse(edges.length < length_m, edges.length, length_m))


# -----------------------------------------------------------------------------
# Consertar o sinal do gradiente
# -----------------------------------------------------------------------------

# O gradiente inserido até o momento ignora o sentido da via - ele foi definido
# apenas como o ponto de altimetria mais próximo ao ponto GPS resultante do
# processo de map matching. Este ponto poderia ser positivo ou negativo de forma
# arbitrária. Precisamos endereçar isso

resultados %>% st_drop_geometry() %>% tail(20)
resultados %>% mapview(zcol = 'seq_order')

# Usar o algoritmo do st_dbscan para reconhecer os clusters - especificamente,
# queremos linhas que tenham o mesmo osm_id e qgis_id e estejam a uma distância
# de seq_order máxima de 1 entre um e outro. Isso porque se um mesmo bloco de
# osm_id e qgis_id se repetem (ver viagem 027463_002 como exemplo), a nova
# passagem por um trecho repetido pertence a outro cluster
clusters <- st_dbscan(x    = resultados$edges.way_id,
                      y    = resultados$qgis_id, 
                      time = resultados$seq_order, 
                      eps1 = 1,
                      eps2 = 1, # distância em seq_order tem de ser 1, se não
                      minpts = 1, # cluster pode ter 1 ponto, sem problemas
                      dry  = TRUE) %>% 
  as.data.frame()

# Inserir coluna de clusters de elevação
resultados <- resultados %>% add_column(elev_grad_cluster = clusters$cluster, 
                                        .before = 'elev_grad')



# Agrupar trechos conforme seus clusters e somar as variações de isovalor. O que
# queremos é saber se a variação será positiva ou negativa, para então aplicar
# este sinal à coluna de elev_grad
declividades_revisadas <- 
  resultados %>% 
  st_drop_geometry() %>% 
  group_by(elev_grad_cluster, edges.way_id, qgis_id, elev_grad) %>% 
  summarise(alt_var_trecho = sum(isovalor_var)) %>%
  mutate(elev_grad2 = ifelse(alt_var_trecho >= 0, abs(elev_grad), abs(elev_grad) * -1))

declividades_revisadas <- declividades_revisadas %>% select(-elev_grad)


resultados <- 
  resultados %>% 
  left_join(declividades_revisadas, by = c('edges.way_id', 'qgis_id', 'elev_grad_cluster'))

resultados %>% mapview(zcol = 'elev_grad2')
