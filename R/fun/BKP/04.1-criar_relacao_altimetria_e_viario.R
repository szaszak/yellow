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

# Inserir ordem dos pontos, para pegar primeiro e último por trecho
elevacao_viario <- elevacao_viario %>% add_column(fid = 1:nrow(.), .before = 'elev_mdt')
# head(elevacao_viario)


# Simplificar dataframe - pegar a altimetria por relação com a ordem dos pontos
relacao_altimetrias <- elevacao_viario %>% st_drop_geometry() %>% select(fid, elev_mdt)
# head(relacao_altimetrias)

# Para cada trecho do viário (qgis_id), achar primeiro e último pontos
pontos_inicio_fim <- 
  elevacao_viario %>% 
  st_drop_geometry() %>% 
  group_by(qgis_id) %>% 
  summarise(p1 = min(fid),
            p2 = max(fid))


# # Guardar relação dos fids com os pontos iniciais e finais em um dataframe longer
# pontos_longer <- 
#   pontos_inicio_fim %>% 
#   pivot_longer(cols = c(p1, p2),
#                 names_to = 'tipo_ponto',
#                values_to = 'fid')

# Juntar as altimetrias aos pontos iniciais e finais
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  left_join(relacao_altimetrias, by = c('p1' = 'fid')) %>% 
  left_join(relacao_altimetrias, by = c('p2' = 'fid'))

# Marcar altimetrias máximas e mínimas
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  mutate(elev_min = ifelse(elev_mdt.x < elev_mdt.y, elev_mdt.x, elev_mdt.y),
         elev_max = ifelse(elev_mdt.x >= elev_mdt.y, elev_mdt.x, elev_mdt.y))


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


# Selecionar e reordenar colunas para exportar
pontos_inicio_fim <- 
  pontos_inicio_fim %>% 
  select(osm_id, qgis_id, length_m, elev_min, elev_max, elev_var, elev_grad_abs, elev_ang_inc)


# Exportar resultados
out_file <- sprintf('%s/A1_listagem_elevacao_por_trecho_viario.csv', pasta_atrib_viario)
write_delim(pontos_inicio_fim, out_file, delim = ';')
