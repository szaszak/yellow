# ------------------------------------------------------------------------------
# Tratamento de declividades no viário - elev min, max por qgis_id e sentido
# ------------------------------------------------------------------------------

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_viario_osm   <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_atrib_viario <- sprintf("%s/04_atributos_viario", pasta_dados)

# Abrir shape de linhas do viário do OpenStreetMap com osm_id, qgis_id e length_m
viario_osm <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_viario_osm)
viario_osm <- read_sf(viario_osm)

# Do shape de viário, queremos só os ids e a extensão dos arcos
viario_osm <- viario_osm %>% st_drop_geometry() %>% select(osm_id, qgis_id, length_m)
head(viario_osm)


# Abrir shape de pontos do viário do OpenStreetMap com valoz Z na coluna elev_mdt
elevacao_viario <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped.gpkg', pasta_elevacao)
elevacao_viario <- read_sf(elevacao_viario)
head(elevacao_viario)


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


# Simplificar dataframe - pegar a altimetria por relação com a ordem dos pontos
relacao_altimetrias <- elevacao_viario %>% st_drop_geometry() %>% select(qgis_id, distance, elev_mdt)
# head(relacao_altimetrias)


# Juntar as altimetrias aos pontos iniciais e finais
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  left_join(relacao_altimetrias, by = c('qgis_id', 'p1_dist' = 'distance')) %>% 
  rename(elev_mdt_p1 = elev_mdt) %>% 
  left_join(relacao_altimetrias, by = c('qgis_id', 'p2_dist' = 'distance')) %>% 
  rename(elev_mdt_p2 = elev_mdt)

# Marcar altimetrias máximas e mínimas
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  mutate(elev_min = ifelse(elev_mdt_p2 <  elev_mdt_p1, elev_mdt_p2, elev_mdt_p1),
         elev_max = ifelse(elev_mdt_p2 >= elev_mdt_p1, elev_mdt_p2, elev_mdt_p1))


# Calcular gradiente e ângulo de inclinação
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  # Agregar extensões dos arcos (length_m)
  left_join(viario_osm, by = 'qgis_id') %>% 
  mutate(elev_var = elev_max - elev_min,
         # Calcular elev_grad (m): Δy / Δx
         # https://www.calculator.net/slope-calculator.html
         elev_grad_abs = elev_var / length_m * 100,
         # Calcular ângulo de inclinação (θ): arctan(Δy / Δx) - como a função
         # atan() traz o resultado em radianos, é preciso converter para graus
         # https://r-lang.com/how-to-convert-radians-to-degrees-in-r/
         elev_ang_inc = atan(elev_grad_abs) * 180 / pi)


# Revisar sinal de gradiente de elevação de acordo com a via - é subida ou descida?
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  # Se altimetria ao final da linha for maior do que no começo, via é subida e
  # gradiente é igual a elev_grad_abs. Se não, é descida e sinal é alterado
  mutate(elev_grad = ifelse(elev_mdt_p2 >= elev_mdt_p1, elev_grad_abs, elev_grad_abs * -1))


# Selecionar e reordenar colunas para exportar
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  select(osm_id, qgis_id, length_m, p1_dist, p2_dist, elev_mdt_p1, elev_mdt_p2,
         elev_min, elev_max, elev_var, elev_grad_abs, elev_grad, elev_ang_inc)

head(pontos_inicio_fim)

# Exportar resultados
out_file <- sprintf('%s/A1_listagem_elevacao_por_trecho_viario.csv', pasta_atrib_viario)
write_delim(pontos_inicio_fim, out_file, delim = ';')
