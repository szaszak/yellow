# TODO - Reconhecer paradas, tanto no meio quanto ao início/fim
# OK - Pensar se não há outra forma da calcular a extensão percorrida em via - 
# vai ser reajustando a distância nos edges pela distância do shape da rota toda;
# pode funcionar pelos legs da viagem
# OK - Incorporar número de esquinas (virou cruzamentos semaforizados)
# OK - Pegar o polyline do matchings.geometry - shape é um pouco melhor, mas pode 
# não obedecer vário nas ligações entre pontos que tiveram rota diferente da 
# esperada pelo map matching
# 
# matchings %>% select(matchings.legs) %>% unnest_wider(matchings.legs, names_sep = "_") %>% unnest(cols = c(matchings.legs_via_waypoints, matchings.legs_steps), keep_empty = TRUE
# 
# vias_sp %>% filter(st_is_within_distance(matchings_latlong2, vias_sp, dist = 2, sparse = FALSE)) %>% mapview()
# vias_sp %>% filter(st_intersects(matchings_latlong2, vias_sp, sparse = FALSE)) %>% mapview()
# vias_sp %>% filter(st_crosses(matchings_latlong2, vias_sp, sparse = FALSE)) %>% mapview()
# # st_boundary() extracts the endpoints
# vias_na_rota <- vias_sp %>% filter(st_is_within_distance(matchings_latlong2, vias_sp, dist = 2, sparse = FALSE)) 
# esquinas_proximas <- vias_na_rota %>% st_boundary()
# # esquinas_na_rota <- esquinas_proximas %>% st_join(y = esquinas_proximas) %>% filter(name.x != name.y)
# esquinas_na_rota <- esquinas_proximas %>% st_join(y = vias_sp) %>% filter(name.x != name.y)
# esquinas <- esquinas_na_rota %>% select(osm_id.x, name.x, osm_id.y, name.y, geom)
# esquinas <- esquinas %>% st_buffer(3) %>% st_cast('POLYGON')
# 
# esquinas %>% filter(st_intersects(matchings_latlong2, esquinas, sparse = FALSE)) %>% mapview() + mapview(matchings_latlong2)

# carregar bibliotecas
source('fun/setup.R')
source('fun/valhalla_map_matching.R')
library('geosphere')

# Estrutura de pastas
pasta_dados      <- "../yellow_dados"
pasta_viagens_sp <- sprintf("%s/01_viagens_em_sp", pasta_dados)
pasta_osm_sp     <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao   <- sprintf("%s/03_curva_intermediaria_sp", pasta_dados)
pasta_semaforos  <- sprintf("%s/04_semaforos_sp", pasta_dados)
pasta_20_viagens <- sprintf("%s/05_testes_20_viagens", pasta_dados)
dir.create(pasta_20_viagens, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------
# Abrir dados de atributos relacionados aos OSM ids
# ----------------------------------------------------------
# Abrir listagem de vias com infraestrutura cicloviária
vias_ciclo <- sprintf('%s/listagem_vias_infra_cicloviaria.csv', pasta_osm_sp)
vias_ciclo <- read_delim(vias_ciclo, delim = ';', col_types = cols(.default = "c"))
vias_ciclo <- vias_ciclo %>% select(osm_id, osm_cycletype = tipo_2018)

# Abrir listagem de vias em áreas com alguma restrição - parques, principalmente
vias_restritas <- sprintf('%s/listagem_vias_em_areas_restritas.csv', pasta_osm_sp)
vias_restritas <- read_delim(vias_restritas, delim = ';', col_types = cols(.default = "c"))

# Abrir listagem de vias em áreas com alguma restrição - parques, principalmente
pontos_elevacao <- sprintf('%s/geosampa_pontos_de_elevacao_no_viario.gpkg', pasta_elevacao)
pontos_elevacao <- read_sf(pontos_elevacao)

# Abrir esquinas com semáforos - buffers de 25m a partir do shape da CET
semaforos <- sprintf('%s/semaforos_buffer25_sp_dissolved.gpkg', pasta_semaforos)
semaforos <- read_sf(semaforos)


# ----------------------------------------------------------
# Viagens em SP - aplicar filtros
# ----------------------------------------------------------

# # Abrir a base de viagens da Yellow em SP
# open_file <- sprintf("%s/sp_viagens_yellow.rds", pasta_viagens_sp)
# viagens <- read_rds(open_file) %>% select(-n_points)

# ----------------------------------------------------------
# Criar seleção de 20 viagens de teste
# ----------------------------------------------------------

# # Abrir a base de viagens da Yellow em SP
# open_file <- sprintf('%s/sp_viagens_yellow.rds', pasta_viagens_sp)
# yellow_sp <- read_rds(open_file)
# 
# # Selecionar viagens para testes do Valhalla
# viagens_teste <- c('000643', '035917', '039193', '098269', '180975', 
#                    '187252', '209982', '212701', '234740', '237075', 
#                    '243795', '247325', '297794', '303925', '311194', 
#                    '315373', '353180', '389934', '394614', '425829')
# 
# viagens <- yellow_sp %>% filter(trip_id %in% viagens_teste) %>% rename(lat = lats, lon = longs)
# 
# # Gravar arquivo
# out_file <- sprintf("%s/sp_20_viagens_teste.rds", pasta_20_viagens)
# write_rds(viagens, out_file, compress = 'gz')


# Abrir a base de viagens da Yellow em SP
open_file2 <- sprintf('%s/sp_20_viagens_teste.rds', pasta_20_viagens)
viagens <- read_rds(open_file2) %>% select(-n_points)


# ----------------------------------------------------------
# Aplicar filtros - outliers, número mínimo de pontos etc
# ----------------------------------------------------------

# Retirar pontos que possuem as mesmas coordenadas latlong
viagens <- viagens %>% distinct(trip_id, lat, lon, .keep_all = TRUE)


# Retirar viagens com tempos menores do que o tempo mínimo (sem segundos)
tempo_minimo <- 30

# Selecionar viagens que estão abaixo do tempo mínimo
viagens_tempos <- 
  viagens %>% 
  # Isolar colunas de interesse
  select(trip_id, timestamps) %>% 
  group_by(trip_id) %>% 
  # Calcular tempo total da viagem
  summarise(trip_time = max(timestamps) - min(timestamps)) %>% 
  # Filtrar viagens abaixo do tempo mínimo e isolar coluna de trip_id
  filter(trip_time <= tempo_minimo) %>% 
  select(trip_id)

# Retirar viagens abaixo do tempo mínimo
viagens <- viagens %>% filter(trip_id %nin% viagens_tempos$trip_id)
rm(viagens_tempos)

  
# # Atualizar coluna de n_points
# n_poins_table <- 
#   viagens %>% 
#   group_by(trip_id) %>% 
#   mutate(n_points = n(), 
#          trip_time = max(timestamps) - min(timestamps),
#          .after = 'trip_id')
  

# ----------------------------------------------------------
# Testes com 20 viagens
# ----------------------------------------------------------

# '000643' - [estac] - pontos estacionários na Av. Pedroso de Morais
# '035917' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '039193' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '098269' - [rotaL] - rota longa Av. Berrini + Rua Flórida
# '180975' - [rotaC] - rota curta Av. Pedroso de Morais rumo ao metrô F. Lima
# '187252' - [rotaL] - rota longa e complexa  passando pela Ponte Laguna
# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '212701' - [rotaC] - rota curta Rua Elvira Ferraz + Rua Prof Atilio Innocenti
# '234740' - [ibira] - percurso dentro do Ibirapuera, aparente bom geocode
# '237075' - [rotaL] - rota longa saindo do Shop. JK e entrando no Ibirapuera
# '243795' - [ibira] - percurso curto dentro do Ibirapuera, bom geocode
# '247325' - [rotaL] - rota longa Av. Berrini + Faria Lima até o metrô
# '297794' - [ativ]  - rota longa Fradique + Pedroso + Cunha Gago + Pinheiros, com paradas longas
# '303925' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim
# '311194' - [inter] - rota longa Ibirapuera + Honduras + Estados Unidos, com interrupção de sinal
# '315373' - [estac] - pontos estacionários na Rua Pirajussara, perto da Vital Brasil
# '353180' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim
# '389934' - [rotaC] - rota curta Av. Hélio Pellegrino
# '394614' - [rotaL] - rota longa saindo do Itaim e entrando no Ibirapuera
# '425829' - [rotaL] - rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers



# Detecta se há quebras na viagem maiores do que o tempo máximo definido e, se 
# há, retorna o maior trecho. Viagens com mais de uma quebra são descartadas
detectar_quebras_em_viagens <- function(trip_df, max_break_time = 90, min_trip_time = tempo_minimo){
  
  # trip_df <- df_tmp; max_break_time <- 90; min_trip_time <- tempo_minimo
  
  # Houve quebra de viagens? Uma quebra é definida pela perda de sinal por mais
  # do que 90s, ou mais do que 18 sinais consecutivos. Uma viagem a 12 km/h, ou
  # 3,333 m/s percorreria 100m nesse intervalo de tempo, o que pode prejudicar
  # o map matching. No artigo dos técnicos da Microsoft, 90s é o último intervalo
  # de tempo do segundo bloco a partir do qual o map matching perde eficiência
  n_quebras <- trip_df %>% mutate(index = 1:nrow(trip_df)) %>% filter(time_s > max_break_time)
  
  if (nrow(n_quebras) == 0) {
    message('\nSem quebras de viagens detectadas, continuando...')
    
    return(trip_df)
    
  } else if (nrow(n_quebras) == 1) {
    message('\nDetectada uma quebra, dividindo as viagens e pegando a maior...')
    
    # Qual o tamanho das viagens se divididas?
    size1 <- n_quebras$index - 1
    size2 <- nrow(trip_df) - n_quebras$index
    
    # Pegar somente a maior viagem
    if (size1 > size2) {
      trip_df <- trip_df %>% slice(1:size1)
    } else {
      trip_df <- trip_df %>% slice((n_quebras$index + 1):nrow(trip_df))
    }
    
    return(trip_df)
    
  } else if (nrow(n_quebras) == 2) {
    message('\nDetectada duas quebras, dividindo as viagens e pegando a maior...')
    
    # Qual o tamanho das viagens se divididas?
    size1 <- n_quebras$index[[1]] - 1
    size2 <- (n_quebras$index[[2]] - 1) - (n_quebras$index[[1]] + 1)
    size3 <- nrow(trip_df) - n_quebras$index[[2]]
    
    # Detectar a maior viagem e retornar
    if (size1 > size2 & size1 > size3) { # primeira parte é maior
      trip_df <- trip_df %>% slice(1:n_quebras$index[[1]] - 1)
    } else if (size2 > size1 & size2 > size3) { # segunda parte é maior
      trip_df <- trip_df %>% slice((n_quebras$index[[1]] + 1):(n_quebras$index[[2]] - 1))
    } else { # terceira parte é maior
      trip_df <- trip_df %>% slice((n_quebras$index[[2]] + 1):nrow(trip_df))
    }
    
    return(trip_df)
    
    
  } else if (nrow(n_quebras) == 1) {
    
  } else { # n_quebras > 1
    stop('\nDetectadas mais de duas quebras, ignorando viagem...')
  }
  
}



# Calcular distância percorrida, velocidade média e velocidade máxima
gerar_resumo_viagem <- function(df_stat){
  trip_duration_s <- max(df_stat$timestamps) - min(df_stat$timestamps)
  trip_distance_m <- round(sum(df_stat$dist_m), 2)
  avg_speed <- round(trip_distance_m / trip_duration_s * 3.6, 1)
  max_speed <- round(max(df_tmp$speed_kph), 1)
  print(paste('[ORIG] distância: ', trip_distance_m, 'm',
              'tempo: ', trip_duration_s, 'seg', 
              'velocidade: ', avg_speed, 'km/h',
              'velocidade_max: ', max_speed, 'km/h'))  
}



# Retirar outliers extremos, segundo proposto por Favero e Belfiore (2021)
retirar_outliers_extremos <- function(df_out){
  # Alterado da implementação em:
  # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102
  
  # Isolar coluna de interesse - velocidade
  x <- df_out$speed_kph
  # Pegar o primeiro e quarto quantis 
  qnt <- quantile(x, probs = c(0.25, 0.75))
  # Outliers extremos estão a 3 * IQR
  H <- 3 * IQR(x) 
  # Retirar outliers do dataframe
  df_out <- df_out %>% filter(!(speed_kph < (qnt[1] - H) | speed_kph > (qnt[2] + H)))
  
  # Isolar coluna de interesse - aceleração
  y <- df_out$accel_ms2
  # Pegar o primeiro e quarto quantis 
  qnt <- quantile(y, probs = c(0.25, 0.75))
  # Outliers extremos estão a 3 * IQR
  H <- 3 * IQR(y) 
  # Retirar outliers do dataframe
  df_out <- df_out %>% filter(!(accel_ms2 < (qnt[1] - H) | accel_ms2 > (qnt[2] + H)))
}

# ----------------------------------------------------------
# Tratamento / filtros
# ----------------------------------------------------------

# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '297794' - [ativ]  - rota longa Fradique + Pedroso + Cunha Gago + Pinheiros, com paradas longas

# '098269' - [rotaL] - rota longa Av. Berrini + Rua Flórida
# '187252' - [rotaL] - rota longa e complexa  passando pela Ponte Laguna
# '237075' - [rotaL] - rota longa saindo do Shop. JK e entrando no Ibirapuera
# '247325' - [rotaL] - rota longa Av. Berrini + Faria Lima até o metrô
# '394614' - [rotaL] - rota longa saindo do Itaim e entrando no Ibirapuera
# '425829' - [rotaL] - rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers
# 
# '180975' - [rotaC] - rota curta Av. Pedroso de Morais rumo ao metrô F. Lima
# '212701' - [rotaC] - rota curta Rua Elvira Ferraz + Rua Prof Atilio Innocenti
# '389934' - [rotaC] - rota curta Av. Hélio Pellegrino
# 
# '303925' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim
# '353180' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim

# '000643' - [estac] - pontos estacionários na Av. Pedroso de Morais
# '315373' - [estac] - pontos estacionários na Rua Pirajussara, perto da Vital Brasil

# '035917' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '039193' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '234740' - [ibira] - percurso dentro do Ibirapuera, aparente bom geocode
# '243795' - [ibira] - percurso curto dentro do Ibirapuera, bom geocode

sel_trip <- '394614'

df_tmp <- viagens %>% filter(trip_id == {{sel_trip}})
df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()

# Criar coluna com o cálculo de tempo entre um ponto e outro
df_tmp <- df_tmp %>% mutate(time_s = c(diff(timestamps), 0))


# ----------------------------------------------------------
# Detectar quebra de viagens - se houver, pegar a maior
# ----------------------------------------------------------
df_tmp <- detectar_quebras_em_viagens(df_tmp)
df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()


# Criar colunas de velocidade, distância e aceleração
df_tmp <- 
  df_tmp %>%
  # Criar coluna com o cálculo de tempo entre um ponto e outro
  mutate(# time_s = timestamps - lag(timestamps), # cálculo na linha posterior
         # time_s = c(diff(timestamps), 0), # cálculo na mesma linha
         # Fazer o mesmo para os latlongs - para isso é preciso
         # criar novas colunas que trazem os pontos de latlong
         # da linha de baixo para a mesma linha...
         # https://stackoverflow.com/questions/50007783/calculating-geographic-distance-between-two-rows-in-data-table
         lat_to = shift(lat, type = 'lead'),
         lon_to = shift(lon, type = 'lead'),
         # ... para poder calcular a distância entre eles...
         dist_m = distVincentyEllipsoid(cbind(lon, lat), cbind(lon_to, lat_to)),
         # ... e a velocidade no trecho
         # speed_mps = dist_m / time_s,
         speed_kph = dist_m / time_s * 3.6,
         accel_ms2 = (dist_m / time_s) / time_s) %>%
  # Descartar colunas temporárias com latlongs do ponto seguinte
  # select(-c(lat_to, lon_to)) %>% 
  # Converter a coluna de timestamps para dia-mes-ano-hora
  add_column(time = as.POSIXlt(.$timestamps, origin = '1970-01-01'), .before = 'timestamps') %>% 
  # Substituir NAs por zeros em colunas específicas
  mutate(time_s = replace_na(time_s, 0),
         dist_m = replace_na(dist_m, 0),
         speed_kph = replace_na(speed_kph, 0),
         accel_ms2 = replace_na(accel_ms2, 0))


# ----------------------------------------------------------
# Retirar outliers extremos de velocidade e aceleração
# ----------------------------------------------------------
# Retirar outliers extremos
df_tmp <- retirar_outliers_extremos(df_tmp)

gerar_resumo_viagem(df_tmp)
df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()


# ----------------------------------------------------------
# Entrando no Valhalla
# ----------------------------------------------------------

source('fun/valhalla_map_matching.R')


# ----------------------------------------------------------
# trace_route() - interessa a distância percorrida
# ----------------------------------------------------------
# df_rota <- viagens %>% filter(trip_id == {{trip_id}})
# df_rota %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()

# Traçar rota com trace_route, retorna o response do POST
r_tr <- trace_route_valhalla(df_tmp, 'pedestrian') # pedestrian, bicycle

# Pegar resposta do POST e transformar em lista com o parse_json()
response_text_tr <- parse_json(r_tr, simplifyVector = FALSE)

# str(response_text_tr)

# Do trace_route(), pegar a seção de matchings, que contém
# os dados de distância percorrida por trecho
matchings <- route_matchings(response_text_tr, trip_id = sel_trip)

# Retorna a distância percorrida em metros para a rota toda
route_distance  <- matchings_route_distance(matchings)

# Pegar tempo total da rota, calculado pela diferença entre o último
# ponto GPS e o primeiro, em segundos
route_time <- matchings_route_duration(matchings)

# Calcular a velocidade média: distância sobre tempo, em km/h
route_speed_kph <- matchings_route_speed(route_distance, route_time)


route_distance; route_time; route_speed_kph
gerar_resumo_viagem(df_tmp)


matchings_polyline <- matchings$route_polyline

# Pegando o shape das as pernas e juntando depois (fica sem ligações)
matchings_latlong <- lapply(matchings_polyline, polyline_to_latlong, sel_trip)
matchings_latlong <- lapply(matchings_latlong, polyline_latlong_to_linestring, sel_trip) 
matchings_latlong <- matchings_latlong %>% rbindlist()
# matchings_latlong %>% st_as_sf() %>% mapview()
dist1 <- matchings_latlong %>% st_as_sf() %>% st_union() %>% st_length()

# Juntando os latlongs antes de transformar em shape (ligações ficam diretas,
# sem necessariamente obedecer viário)
matchings_latlong2 <- lapply(matchings_polyline, polyline_to_latlong, sel_trip) %>% rbindlist()
matchings_latlong2 <- polyline_latlong_to_linestring(matchings_latlong2, sel_trip)
# matchings_latlong2 %>% mapview()
dist2 <- matchings_latlong2 %>% st_length()

# Comparando as extensões - qual delas é considerada para o route_shape?
# route_distance; dist1; dist2

# pernas <- matchings_route_steps(matchings)
# matchings_route_geometry(matchings) %>% head()


# resp_tp <- response_text_tr['tracepoints']
# df_tp <- 
#   as.data.frame(fromJSON(toJSON(resp_tp)))




# ----------------------------------------------------------
# trace_attributes() - interessa o shape completo, dados dos
# edges (id, nome, extensão) e matched_points
# ----------------------------------------------------------
source('fun/valhalla_map_matching.R')
# Traçar rota com trace_attributes, pegar resposta e transformar em lista
r_at <- trace_attributes_valhalla(df_tmp, 'pedestrian') # bicycle, pedestrian

# Pegar resposta do POST e transformar em lista com o parse_json()
response_text_at <- parse_json(r_at, simplifyVector = FALSE)


# Pegar dados OSM: edges.way_id é o id que pode ser encontrado no OSM e seria
# o equivalende a um id do logradouro; edges.id é um id específico daquele
# edge e que, aparentemente, depende do versão do mapa que se está usando
trip_edges <- attributes_edges(response_text_at)

# Pegar dados dos pontos resultantes do map matching
trip_points <- attributes_matched_points(response_text_at)

# Associar matched_points com os dados dos edges
trip_points <- trip_points %>% left_join(trip_edges, by = c("matched_points.edge_index" = "edges.edge_index"))

# A quantidade de pontos que resultante do map matching deve ser a mesma dos
# pontos originais - se for, juntar os dois dataframes
if (nrow(df_tmp) == nrow(trip_points)) {
  df_tmp <- df_tmp %>% cbind(trip_points)
}

# Porém, há pontos que não conseguiram ser associados a um edge no map matching -
# esses pontos possuem duas características: (a) ou o matched_points.type == 
# 'unmatched'ou (b) ou o matched_points.type == 'interpolated' mas o 
# matched_points.edge_index vai ter um valor absurdamente alto. É preciso 
# retirar esses pontos. Como a coluna de índice em trip_edges foi criada a partir
# do 1, podemos excluir tudo o que for acima de nrow(trip_edges):
df_tmp <- df_tmp %>% filter(matched_points.type != 'unmatched' & matched_points.edge_index < nrow(trip_edges))
df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()


# Inserir dados de elevação - OSM
df_tmp <- get_elevation_data(df_tmp)

# Inserir dados de elevação - Geosampa
# Guardar ids de viário que estão relacionados à rota
viarios_matched <- df_tmp %>% select(edges.way_id) %>% distinct()

# Pegar somente pontos de elecação que estão relacionados à rota
elevacao_proxima <- pontos_elevacao %>% filter(pontos_elevacao$osm_id %in% viarios_matched$edges.way_id)
# elevacao_proxima %>% mapview()

# Criar dataframe simplificado para puxar dados de elevação do geosampa
rota_matched <- 
  df_tmp %>% 
  select(osm_id = edges.way_id,
         lat = matched_points.lat,
         lon = matched_points.lon) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# Na base de elevacao_proxima, pegar o index dos pontos que estão mais próximos
# aos pontos em rota_matched
idx_emp <- st_nearest_feature(rota_matched, elevacao_proxima)
length(idx_emp)

# Usar os índices para selecionar os pontos em elevacao_proxima
elevacao_proxima <- elevacao_proxima %>% slice(idx_emp) %>% st_drop_geometry() %>% select(ISOVALOR)
nrow(elevacao_proxima)

if (nrow(df_tmp) == nrow(elevacao_proxima)) { df_tmp <- cbind(df_tmp, elevacao_proxima)}

df_tmp <- df_tmp %>% mutate(isovalor_var = c(diff(ISOVALOR), 0))

df_tmp %>% select(elev_var) %>% filter(elev_var < 0) %>% sum()
df_tmp %>% select(elev_var) %>% filter(elev_var > 0) %>% sum()
df_tmp %>% select(isovalor_var) %>% filter(isovalor_var < 0) %>% sum()
df_tmp %>% select(isovalor_var) %>% filter(isovalor_var > 0) %>% sum()


# Inserir quantidade de cruzamentos com semáforos
semaforos_na_rota <- semaforos %>% filter(st_intersects(matchings_latlong2, semaforos, sparse = FALSE))
mapview(semaforos_na_rota) + mapview(matchings_latlong2)
qtd_semaforos <- nrow(semaforos_na_rota)



# distância - melhor valor parece ser o de route_distance  <- matchings_route_distance(matchings)
# tempo - melhor valor ainda é o dos timestamps originais
# 

# # Quantos edges tem na rota?
# n_edges <- length(response_text_at['edges'][[1]])
# edges <- attributes_edges(response_text_at)


# Juntar dados de trechos com infra cicloviária e dentro de áreas restritas
df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview() 
df_tmp <- 
  df_tmp %>% 
  # select(1, 9, 18, 20) %>% 
  # Juntar dados relativos a vias com infraestrutura cicloviária
  left_join(vias_ciclo, by = c('edges.way_id' = 'osm_id')) %>% 
  # Juntar dados se trecho está dentro de áreas restritas, tais como parques
  mutate(area_restrita = case_when(!edges.way_id %in% vias_restritas$osm_id ~ FALSE,
                                   TRUE ~ TRUE))



# ----------------------------------------------------------
# Gerar shape resumitivo da viagem toda
# ----------------------------------------------------------

# Gerar dados resumidos para shape com rota toda
trip_duration_s <- max(df_tmp$timestamps) - min(df_tmp$timestamps)
trip_distance_m <- round(sum(df_tmp$dist_m), 2)
avg_speed <- round(trip_distance_m / trip_duration_s * 3.6, 1)
max_speed <- round(max(df_tmp$speed_kph), 1)


# Calcular variações positivas e negativas de elevação
elev_osm_pos <- df_tmp %>% select(elev_var) %>% filter(elev_var > 0) %>% sum()
elev_osm_neg <- df_tmp %>% select(elev_var) %>% filter(elev_var < 0) %>% sum()


# # Calcular trechos que passam por viários com e sem estruturas cicloviárias
# group_infra_ciclo    <- df_tmp %>% group_by(osm_cycletype) %>% summarise(dist = sum(edges.length))
# trecho_em_vias       <- subset(group_infra_ciclo, is.na(osm_cycletype))$dist
# trecho_em_ciclovia   <- subset(group_infra_ciclo, osm_cycletype == 'ciclovia')$dist
# trecho_em_ciclofaixa <- subset(group_infra_ciclo, osm_cycletype == 'ciclofaixa')$dist
# trecho_em_expressa   <- subset(group_infra_ciclo, osm_cycletype == 'expressa')$dist
# # Se trecho não inclui vias expressas, o resultado anterior ficou em 'numeric(0)';
# # esta linha atualiza este valor para zero. O mesmo vale para os demais tipos
# if (length(trecho_em_vias) == 0)       { trecho_em_vias = 0 }
# if (length(trecho_em_ciclovia) == 0)   { trecho_em_ciclovia = 0 }
# if (length(trecho_em_ciclofaixa) == 0) { trecho_em_ciclofaixa = 0 }
# if (length(trecho_em_expressa) == 0)   { trecho_em_expressa = 0 }
# 
# 
# # Calcular trechos que passam por viários dentro e fora de áreas restritas
# group_areas_restritas     <- df_tmp %>% group_by(area_restrita) %>% summarise(dist = sum(edges.length))
# trecho_em_areas_restritas <- subset(group_areas_restritas, area_restrita == TRUE)$dist
# trecho_em_vias_comuns     <- subset(group_areas_restritas, area_restrita == FALSE)$dist
# # Se trecho não inclui vias expressas, o resultado anterior ficou em 'numeric(0)';
# # esta linha atualiza este valor para zero. O mesmo vale para os demais tipos
# if (length(trecho_em_areas_restritas) == 0) { trecho_em_areas_restritas = 0 }
# if (length(trecho_em_vias_comuns) == 0)     { trecho_em_vias_comuns = 0 }



# Distância com base nos edges
calculos_edges <- df_tmp %>% select(edges.way_id, edges.names, osm_cycletype, area_restrita, edges.length)

# Para calcular as distâncias, temos que pegar os edges.way_id na ordem, com suas
# respectivas extensões (um mesmo id pode ter uma extensão diferente na linha
# seguinte) e somar os valores. Não podemos usar o distinct() aqui, se não
# perderemos trajetos que foram e voltaram passando por trechos iguais
calculos_edges <- 
  calculos_edges %>% 
  # Vamos puxar os dados de edges.way_id e edges.length da linha de baixo
  # para novas colunas
  mutate(next_id = shift(edges.way_id, type = 'lead'), 
         next_dist = shift(edges.length, type = 'lead')) %>% 
  # O filtro aqui é: se a linha de baixo tiver um id diferente da atual ou
  # tiver uma extensão diferente da atual, manter essa linha - isso faz com
  # que linhas que se repetem (ex. foram usadas na ida e na volta) continuem
  # no dataframe para a soma das extensões
  filter(edges.way_id != next_id | next_dist != edges.length)



extensao_edges <- sum(calculos_edges$edges.length)
avgspeed_kph_edges <- round(extensao_edges / trip_duration_s * 3.6, 1)

group_edges <- calculos_edges %>% group_by(osm_cycletype) %>% summarise(dist = sum(edges.length))
trecho_em_vias       <- subset(group_edges, is.na(osm_cycletype))$dist
trecho_em_ciclovia   <- subset(group_edges, osm_cycletype == 'ciclovia')$dist
trecho_em_ciclofaixa <- subset(group_edges, osm_cycletype == 'ciclofaixa')$dist
trecho_em_expressa   <- subset(group_edges, osm_cycletype == 'expressa')$dist
# Se trecho não inclui vias expressas, o resultado anterior ficou em 'numeric(0)';
# esta linha atualiza este valor para zero. O mesmo vale para os demais tipos
if (length(trecho_em_vias) == 0)       { trecho_em_vias = 0 }
if (length(trecho_em_ciclovia) == 0)   { trecho_em_ciclovia = 0 }
if (length(trecho_em_ciclofaixa) == 0) { trecho_em_ciclofaixa = 0 }
if (length(trecho_em_expressa) == 0)   { trecho_em_expressa = 0 }

# Calcular trechos que passam por viários dentro e fora de áreas restritas
group_edges_parques     <- calculos_edges %>% group_by(area_restrita) %>% summarise(dist = sum(edges.length))
trecho_em_areas_restritas <- subset(group_edges_parques, area_restrita == TRUE)$dist
trecho_em_vias_comuns     <- subset(group_edges_parques, area_restrita == FALSE)$dist
# Se trecho não inclui vias expressas, o resultado anterior ficou em 'numeric(0)';
# esta linha atualiza este valor para zero. O mesmo vale para os demais tipos
if (length(trecho_em_areas_restritas) == 0) { trecho_em_areas_restritas = 0 }
if (length(trecho_em_vias_comuns) == 0)     { trecho_em_vias_comuns = 0 }


# group_edges %>% mutate(dist_tot = route_distance,
#                        dist_prop = dist / dist_tot,
#                        dist_calc = dist * dist_prop)
# 
# dist_edges <- sum(calculos_edges$edges.length)

# Calcular variações positivas e negativas de elevação com base no geosampa
elev_geo_pos <- df_tmp %>% select(isovalor_var) %>% filter(isovalor_var > 0) %>% sum()
elev_geo_neg <- df_tmp %>% select(isovalor_var) %>% filter(isovalor_var < 0) %>% sum()







# Do resultado do trace_attributes(), selecionar seção de shape
attributes_shape <- response_text_at['shape']

# Converter shape para sf com linestring
sf_out <- polyline_to_latlong(trip_id = sel_trip, attributes_shape)
sf_out <- polyline_latlong_to_linestring(sf_out, trip_id = sel_trip)

# Juntar dados de distância, tempo e velocidade ao shape e retornar
sf_out <- sf_out %>% mutate(ext_m_valh_total      = route_distance,  # usar como distância
                            tempo_s_valh          = route_time,      # não usar
                            velmedia_kph_valh     = route_speed_kph, # não usar
                            ext_m_gps             = trip_distance_m, # não usar, só para ter como referência
                            tempo_s_gps           = trip_duration_s, # usar como medida de tempo
                            velmedia_kph_gps      = avg_speed,       # não usar
                            velmaxima_kph_gps     = max_speed,       # não sei se vamos usar, registrando por enquanto
                            ext_m_edges           = extensao_edges,  # usar como segunda distância
                            velmedia_kph_edges    = avgspeed_kph_edges, # usar como segundo cálculo de velocidade média
                            ext_m_valh_viario     = trecho_em_vias,  # usar, calculado com base em extensao_edges
                            ext_m_valh_ciclovia   = trecho_em_ciclovia,        # usar, calculado com base em extensao_edges
                            ext_m_valh_ciclofaixa = trecho_em_ciclofaixa,      # usar, calculado com base em extensao_edges
                            ext_m_valh_expressa   = trecho_em_expressa,        # usar, calculado com base em extensao_edges
                            ext_m_valh_areas_rest = trecho_em_areas_restritas, # usar, calculado com base em extensao_edges,
                            ext_m_valh_areas_nrest = trecho_em_vias_comuns,    # usar, calculado com base em extensao_edges,
                            velmedia_kph_valh_gps = ext_m_valh_total / tempo_s_gps * 3.6,
                            var_elev_pos_osm_m    = elev_osm_pos,
                            var_elev_neg_osm_m    = elev_osm_neg,
                            var_elev_pos_geos_m   = elev_geo_pos,
                            var_elev_neg_geos_m   = elev_geo_neg,
                            ext_m_shape_matchings = dist2,
                            velmedia_kph_matchings = dist2 / tempo_s_gps * 3.6,
                            semaforos             = qtd_semaforos,
                            .after = 'trip_id')

sf_out %>% mapview()
# sf_out %>% st_drop_geometry() %>% select(tempo = tempo_s_gps, 
#                                          dist_shape1 = ext_m_valh_total,
#                                          vel_media1 = velmedia_kph_valh_gps,
#                                          dist_shape2 = ext_m_shape_matchings,
#                                          vel_media2 = velmedia_kph_matchings,
#                                          dist_edges = ext_m_edges,
#                                          ext_viario = ext_m_valh_viario,
#                                          ext_ciclovia = ext_m_valh_ciclovia, 
#                                          ext_ciclofaixa = ext_m_valh_ciclofaixa, 
#                                          ext_expressa = ext_m_valh_expressa, 
#                                          ext_parques = ext_m_valh_areas_rest,
#                                          ext_fora_parques = ext_m_valh_areas_nrest,
#                                          elev_osm_pos = var_elev_pos_osm_m,
#                                          elev_osm_neg = var_elev_neg_osm_m,
#                                          elev_geos_pos = var_elev_pos_geos_m, 
#                                          elev_geos_neg = var_elev_neg_geos_m,
#                                          semaforos)

sf_out <- sf_out %>% st_drop_geometry() %>% select(tempo = tempo_s_gps, 
                                                   dist  = ext_m_shape_matchings,
                                                   veloc = velmedia_kph_matchings,
                                                   ext_edges = ext_m_edges,
                                                   ext_cvia = ext_m_valh_ciclovia, 
                                                   ext_cfxa = ext_m_valh_ciclofaixa, 
                                                   ext_expr = ext_m_valh_expressa, 
                                                   ext_parque = ext_m_valh_areas_rest,
                                                   semaforos,
                                                   elev_pos = var_elev_pos_geos_m, 
                                                   elev_neg = var_elev_neg_geos_m)

sf_out$dist/sf_out$ext_edges * sf_out$ext_cvia
sf_out$dist/sf_out$ext_edges * sf_out$ext_parque

# Comparando as extensões - qual delas é considerada para o route_shape?
route_distance; dist1; dist2; sf_out %>% st_length()


# matchings_latlong %>% st_as_sf() %>% mapview()
matchings_latlong2 %>% mapview()




# ----------------------------------------------------------
# Outliers
# ----------------------------------------------------------

# '425829' - [rotaL] - rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers
# sel_trip <- '209982'
# df_tmp <- viagens %>% filter(trip_id == {{sel_trip}})
# df_tmp %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
# test <- get_route_data(df_tmp, trip_id = sel_trip, dist_thrs = 300)
# test
# test %>% mapview()


# ----------------------------------------------------------
# Paradas curtas/longas
# ----------------------------------------------------------

# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# do 91 ao 100
# do 167 ao 196
# do 236 ao 310
# do ponto 378 até o final
# aqui, a pessoa está passeando no parque, retirando as paradas, velocidade média é de 4,52 km/h
# A observar:
# - outliers ao final estão fazendo a rota ficar enorme, pois ela volta para o parque;
# - início da rota no parque não reconhece o caminho e faz um trajeto irreal;
# - há algumas paradas no meio do caminho e ao final da viagem

# '297794' - [ativ]  - rota longa Fradique + Pedroso + Cunha Gago + Pinheiros, com paradas longas
# A observar:
# - pequeno outlier na Av. Sagres bem no início da rota está fazendo com que ela comece na avenida, não no parque
# - esta rota pega o túnel Ayrton Senna - modifiquei o acesso a pedestres no túnel no OSM,
# precisamos ver se com isso a rota muda


# ----------------------------------------------------------
# Interrupção de sinal - OK
# ----------------------------------------------------------

# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '311194' - [inter] - rota longa Ibirapuera + Honduras + Estados Unidos, com interrupção de sinal


# ----------------------------------------------------------
# Villa Lobos - muito ruído - OK, está caindo junto com interrupção de sinal
# ----------------------------------------------------------

# '303925' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim
# '353180' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim


# ----------------------------------------------------------
# Pontos estacionários - o primeiro tem 30.8 seg e passa em quebra de viagens e outliers
# ----------------------------------------------------------

# '000643' - [estac] - pontos estacionários na Av. Pedroso de Morais
# '315373' - [estac] - pontos estacionários na Rua Pirajussara, perto da Vital Brasil


# ----------------------------------------------------------
# Rotas curtas
# ----------------------------------------------------------

# '180975' - [rotaC] - rota curta Av. Pedroso de Morais rumo ao metrô F. Lima
# '212701' - [rotaC] - rota curta Rua Elvira Ferraz + Rua Prof Atilio Innocenti
# '389934' - [rotaC] - rota curta Av. Hélio Pellegrino


# ----------------------------------------------------------
# Rotas longas
# ----------------------------------------------------------

# '098269' - [rotaL] - rota longa Av. Berrini + Rua Flórida
# '187252' - [rotaL] - rota longa e complexa  passando pela Ponte Laguna
# '237075' - [rotaL] - rota longa saindo do Shop. JK e entrando no Ibirapuera
# '247325' - [rotaL] - rota longa Av. Berrini + Faria Lima até o metrô
# '394614' - [rotaL] - rota longa saindo do Itaim e entrando no Ibirapuera
# '425829' - [rotaL] - rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers
# 

# ----------------------------------------------------------
# Ibirapuera - completamente dentro do parque
# ----------------------------------------------------------


# '035917' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '039193' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '234740' - [ibira] - percurso dentro do Ibirapuera, aparente bom geocode
# '243795' - [ibira] - percurso curto dentro do Ibirapuera, bom geocode