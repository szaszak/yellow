# --------------------------------------------------------------
# Funções para map-matching (Valhalla) - Trace Route
# --------------------------------------------------------------

# Fazer trace_route com o Valhalla, retorna o response do POST
trace_route_valhalla <- function(input_df, active_mode = 'pedestrian') {
  # Esta parte do código foi inspirada por:
  # https://towardsdatascience.com/map-matching-done-right-using-valhallas-meili-f635ebd17053
  # Valhalla Meili API Documentation
  # https://github.com/valhalla/valhalla-docs/blob/master/turn-by-turn/api-reference.md

  # Preparing the request to Valhalla's Meili
  meili_coordinates <- toJSON(input_df)
  meili_head <- '{"shape":'

  if (active_mode == 'pedestrian') {

    # Testes usando o costing = pedestrian parecem melhores pois conseguem
    # pegar caminhos dentro de parques e contra-mãos, o que o de bycicle não
    # consegue. Porém, não usam ciclovia, então isso deve ser levado em
    # consideração posteriormente. Pode-se argumentar que a precisão do GPS
    # da Yellow talvez não seja suficiente para cravar quando a pessoa
    # está de fato ou não nas ciclovias
    meili_tail <- ', "shape_match": "map_snap", "costing": "pedestrian", "format": "osrm", \
                         "costing_options": {"pedestrian": {"walking_speed": 12, "walkway_factor": 0.9}}, \
                         "trace_options":{"turn_penalty_factor": 500, "search_radius": 50}}' # "search_radius": 20,
  } else if (active_mode == 'bicycle') {
    meili_tail <- ', "shape_match": "map_snap", "costing": "bicycle", "format": "osrm", \
                         "costing_options": {"bicycle": {"bicycle_type": "City", "use_roads": 0.2}}, \
                         "trace_options":{"turn_penalty_factor": 500, "search_radius": 50}}' # "search_radius": 20,
  }    

  meili_request_body <- paste(meili_head, meili_coordinates, meili_tail)

  # Sending a request to Valhalla's Meili
  url <- "http://localhost:8002/trace_route"
  # headers = {'Content-type': 'application/json'}
  # data = str(meili_request_body)
  r <- POST(url, body = meili_request_body)

  # http_status(r)
  # class(r) # response
  # http_status(r)$category # 'Success'
  # http_status(r)$message # 'Success: (200) OK'
  return(r)
  
}


## Retorna df com dados de interesse da seção 'tracepoints' (resultado do trace_route)
#route_tracepoints2 <- function(response_text, trip_id) {
#  # response_text vem de:
#  # # Pegar resposta do POST e transformar em lista com o parse_json()
#  # # response_text_tr <- parse_json(r_at, simplifyVector = FALSE)
#  
#  # Da lista de resposta, selecionar somente a seção de tracepoints
#  resp_tp <- response_text['tracepoints']
#  
#  # Transformar a seção tracepoints em dataframe
#  df_tp <- 
#    # Transformar a seção tracepoints em JSON e o JSON em dataframe
#    as.data.frame(fromJSON(toJSON(resp_tp))) %>% 
#    # Isolar as colunas de interesse name, distance e location
#    select(tracepoints.name, tracepoints.distance, tracepoints.location) %>% 
#    # A coluna de location deve ser dividida (unnested) em long, lat
#    unnest_wider(tracepoints.location, names_sep = "_")  %>% 
#    # Renomear tudo para tirar o 'tracepoints' dos nomes de coluna e
#    # deixar claras as colunas de long e lat
#    rename(name = tracepoints.name,
#           distance = tracepoints.distance,
#           lon = tracepoints.location_1,
#           lat = tracepoints.location_2) %>%
#    # Converter colunas name e distance para character e double, respectivamente;
#    # vamos manter as linhas que não tiveram match e que ficaram como NA para
#    # poder associar ao dataframe original depois
#    unnest(cols = c(name, distance), keep_empty = TRUE) %>% 
#    # Inserir coluna com o trip_id da rota
#    add_column(tripid = trip_id, .before = 'name') #%>% 
#  # Converter para sf - isso não vai ser feito agora, pois precisamos de
#  # um dataframe resultante com o mesmo número de linhas para associar
#  # ao original e manter os dados de tempo e ordem dos pontos
#  # st_as_sf(coords = c('lon', 'lat'), crs = 4326)
#  
#  return(df_tp)
#}


# Retorna df com dados de interesse da seção 'tracepoints' (resultado do trace_route)
route_tracepoints <- function(response_text, trip_id) {
  # Selecionar tracepoints, que é uma lista, e transformá-la em dataframe
  resp_tp <- response_text$tracepoints %>% as.data.frame()
  
  # Lista de colunas dos tracepoints
  # "alternatives_count", "distance", "location", "matchings_index",
  # "name", "waypoint_index"
  
  # Simplificar e preparar dataframe para retornar
  df_tp <- 
    resp_tp %>% 
    # Isolar as colunas de interesse name, distance e location
    select(name, distance, location) %>% 
    # A coluna de location deve ser dividida (unnested) em long, lat
    unnest_wider(location, names_sep = "_") %>% 
    # Renomear colunas de long e lat
    rename(lon = location_1,
           lat = location_2) %>%
    # Inserir coluna com o trip_id da rota
    add_column(tripid = trip_id, .before = 'name')
  
  return(df_tp)
}


## Retorna df com dados de interesse da seção 'matchings' (resultado do trace_route)
#route_matchings2 <- function(response_text, trip_id){
#  # response_text vem de:
#  # # Pegar resposta do POST e transformar em lista com o parse_json()
#  # # response_text_tr <- parse_json(r_at, simplifyVector = FALSE)

#  # Da lista, selecionar somente a seção de matchings, que contém
#  # o polyline da rota utilizada
#  resp_m <- response_text['matchings']
#    # Transformar a seção matchings em dataframe
#  df_rm <- 
#    as.data.frame(fromJSON(toJSON(resp_m))) %>% 
#    # Transformar a coluna com a rota em character e isolá-la
#    mutate(route_polyline = as.character(matchings.geometry)) %>% 
#    #         select(route_polyline) %>% 
#    # Inserir coluna com o trip_id da rota
#    add_column(tripid = trip_id, .before = 'route_polyline')

#  return(df_rm)
#}


# Retorna df com dados de interesse da seção 'matchings' (resultado do trace_route)
route_matchings <- function(response_text, trip_id){
  # Selecionar matchings, que é uma lista, e transformá-la em dataframe
  resp_m <- response_text$matchings %>% as.data.frame()
  
  # Lista de colunas dos matchings
  # "confidence", "distance", "duration", "geometry", "legs" (lista), "weight", 
  # "weight_name"
  
  # Simplificar e preparar dataframe para retornar
  df_rm <- 
    resp_m %>% 
    # Isolar coluna com a geometria
    select(route_polyline = geometry) %>%
    # Inserir coluna com o trip_id da rota
    add_column(tripid = trip_id, .before = 'route_polyline')
  
  return(df_rm)
}


## Retorna a distância percorrida em metros - esta distância
## é menos precisa do que a calculada a partir de seção geometry do JSON
#matchings_route_distance <- function(matchings_df, dist_col = 'matchings.distance'){
#  #    if (nrow(matchings_df) == 1){
#  #        distance <- matchings_df %>% select({{dist_col}}) %>% pull() %>% unlist()
#  #        return(distance)
#  #    }    
#  distance <- matchings_df %>% select({{dist_col}}) %>% unlist() %>% sum()
#  return(distance)
#}


## Retorna o tempo da rota em segundos - este tempo é calculado pelo Valhalla e
## não vamos usar. Vamos nos focar nos tempos registrados pelos pontos GPS
#matchings_route_duration <- function(matchings_df, duration_col = 'matchings.duration'){
#  #    if (nrow(matchings_df) == 1){
#  #        distance <- matchings_df %>% select({{dist_col}}) %>% pull() %>% unlist()
#  #        return(distance)
#  #    }    
#  duration <- matchings_df %>% select({{duration_col}}) %>% unlist() %>% sum()
#  return(duration)
#}


# # Número de pernas (matching.legs) das rotas
# nrow(matchings)

# Retorna df com nome, extensão e polyline dos steps da rota
matchings_route_steps <- function(matchings_df){
  df_rs <- matchings_df %>% 
    # Descompactar pernas da rota
    unnest(cols = c(matchings.legs)) %>% 
    # Isolar os diferentes passos da rota e descompactá-los
    select(steps) %>% 
    unnest_wider(steps)  %>% 
    # Descompactar as colunas de interesse
    unnest(cols = c(name, duration, distance, geometry)) %>% 
    # Simplificar o dataframe
    select(name, duration, distance, geometry) %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(distance = as.double(distance),
           duration = as.double(duration)) %>% 
    # Retirar trechos (legs)) sem distância ou duração
    filter(duration > 0 & distance > 0)
  
  return(df_rs)
}


## Retorna o polyline (geometry) da rota toda
#matchings_route_geometry <- function(matchings_df, geom_col = 'matchings.geometry'){
#  if (nrow(matchings_df) == 1) {
#    geometry <- matchings_df %>% select({{geom_col}}) %>% pull() %>% unlist()
#    return(geometry)
#  }    
#}


# Retorna a velocidade em km/h
matchings_route_speed <- function(dist, time){
  return(round(dist / time * 3.6, 2))
}


# # Onde checar os polylines?
# # https://valhalla.github.io/demos/polyline/
# resp1[['matchings']][[1]][['legs']][[1]][['steps']][[1]][['geometry']]
# 'xaz_l@xacaxAO`B_A`DQjABr@'
# resp1[[1]][[1]]$legs[[1]]$steps[[1]]$geometry
# 'xaz_l@xacaxAO`B_A`DQjABr@'
# resp1[['matchings']][[1]][['legs']][[1]][['steps']][[2]][['geometry']]
# 'z~y_l@|mcaxACs@PkA~@aDNaB@}@]WwGkB'
# resp1[[1]][[1]]$legs[[1]]$steps[[2]]$geometry
# 'z~y_l@|mcaxACs@PkA~@aDNaB@}@]WwGkB'


# Transforma linha de polyline para dataframe com latlongs
polyline_to_latlong <- function(polyline, trip_id){
  this <- as.data.frame(decode(as.character(polyline)))
  this <- this %>% 
    # Formatação dos pontos está fora do lugar: 
    # de -235.641 para -23.5641 - ajeitar
    mutate(trip_id  = trip_id,
           lat = str_replace(lat, '\\.', ''),
           lon = str_replace(lon, '\\.', ''),
           lat = as.double(str_replace(lat, '-23', '-23.')),
           lon = as.double(str_replace(lon, '-46', '-46.')))
  
  return(this)
}


# Transforma polyline em sf_linestring, retorna sf
polyline_latlong_to_linestring <- function(df, trip_id){
  this <- df %>% 
    # Transformar em sf
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    # Transformar pontos em linha - ver possíveis erros em
    # https://github.com/r-spatial/sf/issues/321
    # # Modo 1 - Com st_coordinates, retorna matriz
    # Retrieve coordinates in matrix form 
    # st_coordinates() %>%
    # st_linestring()
    # Modo 2 - Com summarize, retorna sf
    # Aqui, o summarize pode ser qualquer coisa, o 
    # importante é o 'do_union=FALSE'
    group_by(trip_id) %>% 
    summarize(m = n(), do_union = FALSE) %>% 
    select(-m) %>% 
    st_cast("LINESTRING")
  
  return(this)
}


# Transforma conjunto de polylines da rota em um shape único de linha
create_route_shape <- function(polylines_df, trip_id) {
  # # Pegando o shape das as pernas e juntando depois (fica sem ligações)
  # shape_rota <- lapply(polylines_df, polyline_to_latlong, trip_id)
  # shape_rota <- lapply(shape_rota, polyline_latlong_to_linestring, trip_id) 
  # shape_rota <- shape_rota %>% rbindlist()

  # Calcular a distância total da rota - para isso, transformar os polylines em 
  # latlong e juntar esses latlongs ANTES de transformar tudo em shape (as ligações 
  # entre pequenas interrupções são traçadas como linhas retas,o que pode não 
  # obedecer o viário, mas após comparar as medidas das rotas no QGIS, este é o 
  # cálculo de distância mais preciso gerado pelo Valhalla)

  # Transformar cada polyline em latlong e juntar todos em um só dataframe
  df_latlongs <- lapply(polylines_df, polyline_to_latlong, trip_id) 
  df_latlongs <- df_latlongs %>% rbindlist()
  
  # Transformar dataframe em um shape de LINESTRING
  sf_out <- polyline_latlong_to_linestring(df_latlongs, trip_id)
  
  return(sf_out)
}


# Pega uma lista de dataframes com polilynes e retorna um sf único com as linestrings
polyline_latlong_multi_to_linestring <- function(trip_id, df, geom_col = 'geometry'){
  this <- decode(as.character(df[[geom_col]]))
  
  this <- this %>%   
    # Inserir coluna de id nos dataframes
    map(mutate, id = sel_trip,
        lat = str_replace(lat, '\\.', ''),
        lon = str_replace(lon, '\\.', ''),
        lat = as.double(str_replace(lat, '-23', '-23.')),
        lon = as.double(str_replace(lon, '-46', '-46.'))) %>% 
    # Reformatar colunas de latlong
    map(polyline_latlong_to_linestring) %>% 
    bind_rows()
  
  return(this)
}



# --------------------------------------------------------------
# Funções para map-matching (Valhalla) - Trace Attributes
# --------------------------------------------------------------

# Fazer trace_attributes com o Valhalla, retorna o response do POST
trace_attributes_valhalla <- function(input_df, active_mode = 'pedestrian'){
  
  # Preparing the request to Valhalla's Meili
  meili_coordinates <- toJSON(input_df)
  meili_head <- '{"shape":'
  
  if (active_mode == 'pedestrian') {
    # Testes usando o costing = pedestrian parecem melhores pois conseguem
    # pegar caminhos dentro de parques e contra-mãos, o que o de bycicle não 
    # consegue. Porém, não usam ciclovia, então isso deve ser levado em 
    # consideração posteriormente. Pode-se argumentar que a precisão do GPS
    # da Yellow talvez não seja suficiente para cravar quando a pessoa
    # está de fato ou não nas ciclovias
    #         meili_tail <- ', "shape_match": "map_snap", "costing": "pedestrian", "format": "osrm", \
    #                          "costing_options": {"pedestrian": {"walkway_factor": "0.2"}}, \
    #                          "trace_options":{"turn_penalty_factor": 500}, \
    #                          "filters": {"attributes": ["edge.names", "edge.length", "edge.id", "edge.way_id", 
    #                                                     "shape"], \
    #                                     "action":"include"}}' # "shape"
    meili_tail <- ', "shape_match": "map_snap", "costing": "pedestrian", "format": "osrm", \
                         "costing_options": {"pedestrian": {"walking_speed": 12, "walkway_factor": 0.9}}, \
                         "trace_options":{"turn_penalty_factor": 500, "search_radius": 50}}' # "search_radius": 20,
  } else if (active_mode == 'bicycle') {
    meili_tail <- ', "shape_match": "map_snap", "costing": "bicycle", "format": "osrm", \
                         "costing_options": {"bicycle": {"bicycle_type": "City", "use_roads": 0.2}}, \
                         "trace_options":{"turn_penalty_factor": 500, "search_radius": 50}}' # "search_radius": 20,
  }

  # meili_request_body
  meili_request_body <- paste(meili_head, meili_coordinates, meili_tail)
  
  # Sending a request to Valhalla's Meili
  url = "http://localhost:8002/trace_attributes"
  # headers = {'Content-type': 'application/json'}
  # data = str(meili_request_body)
  r <- POST(url, body = meili_request_body)
  
  return(r)
}


## Retorna df com dados de interesse da seção 'edges' (resultado do trace_attributes)
#attributes_edges_v1 <- function(response_text){
#  # response_text vem de:
#  # # Pegar resposta do POST e transformar em lista com o parse_json()
#    # response_text_at <- parse_json(r_at, simplifyVector = FALSE)

#  # Da lista, selecionar somente a seção de edges
#  resp_ed <- response_text['edges']
#  
#  # Transformar a seção em dataframe
#  df_ae <- 
#    # Transformar a seção edges em JSON e o JSON em dataframe
#    as.data.frame(fromJSON(toJSON(resp_ed), flatten = TRUE)) %>% 
#    # Isolar as colunas de interesse
#    select(edges.way_id, edges.id,
#           edges.names, edges.length, 
#           # edges.begin_shape_index, edges.end_shape_index, 
#           # este só funciona se a viagem tiver uma só perna (leg):
#           # edges.internal_intersection 
#           # não é sempre que tem as colunas de grade:
#           # edges.max_upward_grade, edge.max_downward_grade
#           edges.mean_elevation) %>% 
#    # Criar coluna de índice dos edges - ao que parece (ver
#    # https://uonfu.com/q/valhalla/valhalla/2659/737368741),
#    # os índices são a ordem dos edges, começando por zero:
#    mutate(edges.edge_index = 1:nrow(.) - 1, .before = 'edges.way_id',
#           # Substituir elevações inexistentes por NA, de acordo com 
#           # https://github.com/valhalla/valhalla-docs/blob/master/map-matching/api-reference.md
#           edges.mean_elevation = as.double(str_replace(edges.mean_elevation, '32768', 'NA')),
#           # Transformar edges.length em metros (padrão é quilômetros)
#           edges.length = as.double(edges.length) * 1000) %>%
#  mutate(across(where(is.list), as.character))
#  
#  return(df_ae)
#}


# Retorna df com dados de interesse da seção 'edges' (resultado do trace_attributes)
attributes_edges <- function(response_text){
  
  # Selecionar a seção de edges, que é uma lista, e transformá-la em dataframe
  resp_ed <- response_text$edges %>% as.data.frame()
  
  # Lista de colunas dos edges
  # "begin_heading", "begin_shape_index", "bicycle_network", "cycle_lane", 
  # "density", "drive_on_right", "end_heading", "end_node" (lista), "end_shape_index", 
  # "id", "lane_count", "length", "max_downward_grade", "max_upward_grade", 
  # "mean_elevation", "names", "pedestrian_type", "road_class", "sac_scale", 
  # "shoulder", "speed", "surface", "travel_mode", "traversability", "unpaved", 
  # "use", "way_id", "weighted_grade"
  
  # Pode ser que a seção de edges não tenha uma coluna de nomes (provavelmente,
  # porque seriam todos NULL) - se não tiver, criá-la
  if (!'names' %in% names(resp_ed)) { resp_ed$names <- NA }
  
  # Simplificar e preparar dataframe para retornar
  df_ae <- 
    resp_ed %>% 
    # Isolar as colunas de interesse
    select(edges.way_id = way_id, 
           edges.id     = id,
           edges.names  = names,
           edges.length = length,
           edges.mean_elevation = mean_elevation,
           edges.begin_heading = begin_heading) %>% 
    # Criar coluna de índice dos edges - ao que parece (ver
    # https://uonfu.com/q/valhalla/valhalla/2659/737368741),
    # os índices são a ordem dos edges, começando por zero:
    mutate(edges.edge_index = 1:nrow(.) - 1, .before = 'edges.way_id',
           # Substituir elevações inexistentes por NA, de acordo com 
           # https://github.com/valhalla/valhalla-docs/blob/master/map-matching/api-reference.md
           edges.mean_elevation = as.double(str_replace(edges.mean_elevation, '32768', 'NA')),
           # Transformar edges.length em metros (padrão é quilômetros)
           edges.length = as.double(edges.length) * 1000) %>%
    mutate(across(where(is.list), as.character))
  
  return(df_ae)
}


## Retorna df com dados de interesse da seção 'matched_points' (resultado do trace_attributes)
#attributes_matched_points2 <- function(response_text){
#  # response_text vem de:
#  # # Pegar resposta do POST e transformar em lista com o parse_json()
#    # response_text_at <- parse_json(r_at, simplifyVector = FALSE)
#  # Da lista, selecionar somente a seção de matched_points
#  resp_ed <- response_text['matched_points']
#  
#  # Transformar a seção matched_points em dataframe
#  df_mp <- 
#    # Transformar a seção tracepoints em JSON e o JSON em dataframe
#    as.data.frame(fromJSON(toJSON(resp_ed), flatten = TRUE)) %>%

#  # Transformar todas as colunas em character
#  mutate(across(where(is.list), as.character)) %>%
#  select(matched_points.edge_index, matched_points.type, ,
#         # matched_points.distance_from_trace_point, 
#         # matched_points.distance_along_edge,
#         matched_points.lat, matched_points.lon) %>%
#  # Atribuir valor 0 para linhas iniciais que não deram match -
#  # checar se sempre os valores NULL estarão nas linhas iniciais,
#  # ou seja, se sempre poderão ser associados ao edge_index de 0
#  mutate(matched_points.edge_index = case_when(matched_points.edge_index == 'NULL' ~ '0',
#                                               TRUE ~ matched_points.edge_index)) %>% 
#  # Transformar coluna em double para left_join() com df de attributes_edges()
#  mutate(matched_points.edge_index = as.double(matched_points.edge_index)) 
#  
#  return(df_mp)
#}


# Retorna df com dados de interesse da seção 'matched_points' (resultado do trace_attributes)
attributes_matched_points <- function(response_text){
  # Selecionar seção matched_points, que é uma lista; transformá-la em dataframe
  resp_ed <- response_text$matched_points %>% as.data.frame()
  
  # Lista de colunas dos matched_points
  # "distance_along_edge", "distance_from_trace_point", "edge_index",
  # "lat", "lon", "type"
  
  # Simplificar e preparar dataframe para retornar
  df_mp <- 
    resp_ed %>%
    # Atribuir valor 0 para linhas iniciais que não deram match -
    # checar se sempre os valores NULL estarão nas linhas iniciais,
    # ou seja, se sempre poderão ser associados ao edge_index de 0
    mutate(edge_index = as.character(edge_index),
           edge_index = case_when(edge_index == 'NULL' ~ '0',
                                  TRUE ~ edge_index),
           edge_index = as.numeric(edge_index)) %>%
    # Isolar as colunas de interesse
    select(matched_points.edge_index = edge_index,
           matched_points.type       = type,
           matched_points.lat        = lat,
           matched_points.lon        = lon)
    
  
  return(df_mp)
}


# Limpa pontos que não conseguiram ser associados a um edge no map matching
clean_matched_points <- function(trip_df, n_trip_edges) {
  # Há pontos que não conseguiram ser associados a um edge no map matching -
  # esses pontos possuem duas características: (a) ou o matched_points.type == 
  # 'unmatched'ou (b) ou o matched_points.type == 'interpolated' mas o 
  # matched_points.edge_index vai ter um valor absurdamente alto. É preciso 
  # retirar esses pontos. Como a coluna de índice em trip_edges foi criada a partir
  # do 1, podemos excluir tudo o que for acima de nrow(trip_edges):
  
  # Retirar pontos problemáticos
  trip_df <- trip_df %>% filter(matched_points.type != 'unmatched' & 
                                matched_points.edge_index < n_trip_edges)
  
  return(trip_df)
}


# --------------------------------------------------------------
# Função geral para pegar dados resumitivos da rota
# --------------------------------------------------------------

## Retorna um sf da rota com a distância percorrida (m), tempo total de viagem (s),
## velocidade (km/h) e geometry resultante do map matching com o valhalla
#get_route_data <- function(df_rota, trip_id, dist_thrs = 300){
#  
#  # trip_id <- '209982'
#  # df_rota <- viagens %>% filter(trip_id == {{trip_id}})
#  # df_rota %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
#  
#  # Traçar rota com trace_route, retorna o response do POST
#  r_tr <- trace_route_valhalla(df_rota, 'pedestrian') # pedestrian, bicycle
#  
#  # Se rota foi traçada, continuar; se não, avisar
#  if (class(r_tr) == 'response') {
#    # Pegar resposta do POST e transformar em lista com o parse_json()
#    response_text_tr <- parse_json(r_tr, simplifyVector = FALSE)

#    # Do trace_route(), pegar a seção de matchings, que contém
#    # os dados de distância percorrida por trecho
#    matchings <- route_matchings(response_text_tr, trip_id)
#    # Retorna a distância percorrida em metros para a rota toda
#    route_distance  <- matchings_route_distance(matchings)

#    # Só continuar cálculos se rota for maior do que limite mínimo
#    if (route_distance >= dist_thrs) {

#      # Pegar tempo total da rota, calculado pela diferença entre o último
#      # ponto GPS e o primeiro, em segundos
#      route_time <- get_elapsed_time(df_rota)

#      # Calcular a velocidade média: distância sobre tempo, em km/h
#      route_speed_kph <- matchings_route_speed(route_distance, route_time)

#      # Traçar rota com trace_attributes, pegar resposta e transformar em lista
#      r_at <- trace_attributes_valhalla(df_rota, 'pedestrian') # bicycle, pedestrian

#      # Se rota foi traçada, continuar; se não, avisar
#      if (class(r_at) == 'response') {
#        # Pegar resposta do POST e transformar em lista com o parse_json()
#        response_text_at <- parse_json(r_at, simplifyVector = FALSE)

#        # # Quantos edges tem na rota?
#        # n_edges <- length(response_text_at['edges'][[1]])
#        # edges <- attributes_edges(response_text_at)

#        # Do resultado do trace_attributes(), selecionar seção de shape
#        attributes_shape <- response_text_at['shape']

#        # Converter shape para sf com linestring
#        sf_out <- polyline_to_latlong(trip_id, attributes_shape)
#        sf_out <- polyline_latlong_to_linestring(sf_out)

#        # Juntar dados de distância, tempo e velocidade ao shape e retornar
#        sf_out <- sf_out %>% mutate(distance  = route_distance,
#                                    time      = route_time,
#                                    speed_kph = route_speed_kph,
#                                    .after = 'trip_id')


#        return(sf_out)
#      } else { # class(r_at) == 'character' & str_detect(r_tr, 'Error')
#        print(r_at)
#    }

#  } else { # class(r_tr) == 'character' & str_detect(r_tr, 'Error')
#    print(r_tr)
#  }

#  }
#}


# Calcula distâncias com base nos edges (infra cicloviária, áreas restritas)
obter_dados_edges <- function(trip_df) {
  # Selecionar colunas de interesse
  calculos_edges <- trip_df %>% select(edges.way_id, edges.id, osm_cycletype, area_restrita, edges.length)
  
  # Para calcular as distâncias, temos que pegar os edges.way_id na ordem em que
  # estão, com suas respectivas extensões (um mesmo id pode ter uma extensão 
  # diferente na linha seguinte) e somar os valores. Não podemos usar o distinct() 
  # aqui, se não perderemos trajetos que foram e voltaram passando por trechos iguais
  calculos_edges <- 
    calculos_edges %>% 
    # Vamos puxar os dados de edges.way_id e edges.length da linha de baixo
    # para novas colunas
    mutate(next_way_id  = shift(edges.way_id, type = 'lead'),
           next_edge_id = shift(edges.id, type = 'lead'), 
           next_dist    = shift(edges.length, type = 'lead')) %>% 
    # O filtro aqui é: se a linha de baixo tiver um id diferente da atual ou
    # tiver uma extensão diferente da atual, manter essa linha - isso faz com
    # que linhas que se repetem (ex. foram usadas na ida e na volta) continuem
    # no dataframe para a soma das extensões
    filter(edges.way_id != next_way_id | edges.id != next_edge_id | next_dist != edges.length)
  
  # Calcular extensão total dos edges (sem repetição dos edges)
  extensao_edges <- sum(calculos_edges$edges.length)
  
  
  # Infra cicloviária
  
  # Somar as extensões dos trechos com infraestrutura cicloviária
  group_edges <- calculos_edges %>% group_by(osm_cycletype) %>% summarise(dist = sum(edges.length))
  
  # Substituir NA, que se refere ao trecho em viário
  group_edges <- group_edges %>% mutate(osm_cycletype = replace_na(osm_cycletype, 'sem_infra_ciclo'))
  
  # Guardar os dados calculados
  df_infra_ciclo <- 
    # Criar dataframe vazio para garantir que todos os cenários existirão
    data.frame(osm_cycletype = c('ciclovia', 'ciclofaixa', 'expressa', 'sem_infra_ciclo')) %>% 
    # Juntá-lo aos dados calculados
    left_join(group_edges, by = 'osm_cycletype') %>% 
    mutate(dist = replace_na(dist, 0))
  
  
  # Em áreas restritas (parques, USP, outras)
  
  # Somar as extensões dos trechos em áreas restritas
  group_edges_parques <- calculos_edges %>% group_by(area_restrita) %>% summarise(dist = sum(edges.length))
  
  # Substituir TRUE e FALSE, que se referem a estar ou não em áreas restritas
  group_edges_parques <- 
    group_edges_parques %>% 
    mutate(area_restrita = case_when(area_restrita == FALSE ~ 'vias_comuns',
                                     area_restrita == TRUE ~ 'vias_restritas'))

  # Guardar os dados calculados
  df_parques <- 
    # Criar dataframe vazio para garantir que todos os cenários existirão
    data.frame(area_restrita = c('vias_comuns', 'vias_restritas')) %>% 
    # Juntar dados e substituir NAs por zeros
    left_join(group_edges_parques, by = 'area_restrita') %>% 
    mutate(dist = replace_na(dist, 0))
  
  # Juntar todos os resultados em um dataframe único - transpor os dataframes
  # intermediários e juntar as colunas
  out_df <- 
    cbind(t(df_parques), t(df_infra_ciclo)) %>% 
    as.data.frame() %>% 
    # Pegar os nomes das colunas da primeira linha e derrubá-la em seguida
    setNames(slice(., 1)) %>% 
    slice(2:nrow(.)) %>% 
  # Inserir dado com a extensão total dos edges
    add_column(dist_edges = extensao_edges, .before = 'vias_comuns')
  
  # Retirar a exibição dos nomes de linhas do dataframe (coluna de index)
  # https://stackoverflow.com/questions/24428051/removing-display-of-row-names-from-data-frame
  rownames(out_df) <- NULL
  
  return(out_df)

}


# --------------------------------------------------------------
# Funções Valhalla - Dados de altimetria
# --------------------------------------------------------------

# Calcula e retorna variação de altimetria de um df com base nos latlongs
# Visualizar elevação no mapa: https://onthegomap.com/#/create
get_elevation_data <- function(df_elev) {

  # URL base para dados de altimetria
  elevation_url <- "localhost:8002/height"

  # As coordenadas latlong virão das colunas lat e lon do df que chama a função
  elevation_coordinates <- toJSON(df_elev)

  # Argumentos aqui não podem ter espaço após a vírgula; "range":true retorna 
  # não somente a elevação, mas a distância acumulada
  elevation_head <- '{"range":true,"height_precision":2,"shape":'
  elevation_tail <- '}'

  # Formar a URL completa para query de elevação
  elevation_request_body <- paste(elevation_head, elevation_coordinates, elevation_tail)

  # Fazer a query e retornar 
  r <- POST(elevation_url, body = elevation_request_body)

  if (class(r) == 'response') {
    parsed <- parse_json(r, simplifyVector = FALSE)
    df_elev <- 
      df_elev %>% 
      mutate(dist_elev = parsed$range_height) %>% 
      unnest_wider(dist_elev) %>% 
      rename(elev_dist = ...1,
             elev_value = ...2) %>% 
      mutate(elev_var = c(diff(elev_value), 0))

  } else {
    print(r)
  }

  return(df_elev)

}


# --------------------------------------------------------------
# Funções principais map matching - juntando ambos os processos
# --------------------------------------------------------------

# Fazer map matching com o trace_route() e retornar lista com distância da 
# rota calculada e shape da rota
trace_route_valhalla_main <- function(trecho, sel_trip, active_mode = 'pedestrian'){
  
  # ----------------------------------------------------------
  # Map matching Valhalla - trace_route()
  # ----------------------------------------------------------
  # Traçar rota com trace_route, retorna o response do POST
  r_tr <- trace_route_valhalla(trecho, active_mode = 'pedestrian') # pedestrian, bicycle
  
  # Se roteamento for bem sucedido, calcular distância do trecho em metros
  if (http_status(r_tr)$message == 'Success: (200) OK') {
    
    # Pegar a resposta do POST e usar str_c() - string collapse para transformar
    # os vetores em uma única string, que por sua vez será transformada em um
    # dataframe - estes elementos estarão facilmente acessíveis a partir dai
    # https://www.qiushiyan.dev/post/json-column-r/
    response_text_tr <- 
      # Ignorar aviso 'argument is not an atomic vector; coercing'
      suppressWarnings(str_c(r_tr, collapse = ", ")) %>% 
      str_c("[", ., "]") %>% 
      fromJSON() %>% 
      as.data.frame()
    
    
    # Do trace_route(), pegar a seção de matchings, que contém
    # os dados de distância percorrida por trecho
    matchings <- route_matchings(response_text_tr, sel_trip)
    
    # Pegar os polylines de todas as pernas (legs) da trecho
    matchings_polyline <- matchings$route_polyline
    
    # Criar um shapefile único para a rota toda
    shape_rota <- create_route_shape(matchings_polyline, sel_trip)
    # shape_rota %>% mapview()
    
    # Calcular a distância total da trecho - ver observações sobre o cálculo na
    # função create_route_shape()
    dist_rota <- shape_rota %>% st_length() %>% as.double()
    
  } else {
    # Se roteamento não foi bem sucedido, shape e dist_rota serão nulos
    shape_rota <- dist_rota <- NA
    
  }
  
  # Retorna a distância da rota calculada pelo trace_route()
  return(list(dist_rota, shape_rota))
  
}


# Fazer o map matching com o trace_attributes() e retornar lista com código de
# processamento e trecho com atributos resultantes
trace_attributes_valhalla_main <- function(trecho, sel_trip, active_mode = 'pedestrian'){
  # ----------------------------------------------------------
  # Map matching Valhalla - trace_attributes()
  # Interessam os dados dos edges (id, nome, extensão) e matched_points
  # ----------------------------------------------------------
  
  # Traçar rota com trace_attributes, pegar resposta e transformar em lista
  r_at <- trace_attributes_valhalla(trecho, active_mode = 'pedestrian') # bicycle, pedestrian
  
  # Se roteamento for bem sucedido, continuar
  if (http_status(r_at)$message == 'Success: (200) OK') {
    
    # Pegar a resposta do POST e usar str_c() - string collapse para transformar
    # os vetores em uma única string, que por sua vez será transformada em um
    # dataframe - estes elementos estarão facilmente acessíveis a partir dai
    # https://www.qiushiyan.dev/post/json-column-r/
    response_text_at <- 
      # Ignorar aviso 'argument is not an atomic vector; coercing'
      suppressWarnings(str_c(r_at, collapse = ", ")) %>% 
      str_c("[", ., "]") %>% 
      fromJSON() %>% 
      as.data.frame()
    
    
    # Pegar dados OSM: edges.way_id é o id que pode ser encontrado no OSM e seria
    # o equivalende a um id do logradouro; edges.id é um id específico daquele
    # edge e que, aparentemente, depende do versão do mapa que se está usando
    trip_edges <- attributes_edges(response_text_at)
    
    # Pegar dados dos pontos resultantes do map matching
    trip_points <- attributes_matched_points(response_text_at)
    
    # Associar matched_points com os dados dos edges
    trip_points <- 
      trip_points %>% 
      left_join(trip_edges, by = c("matched_points.edge_index" = "edges.edge_index")) %>% 
      # Transformar way_id em character para left_joins futuros
      mutate(edges.way_id = as.character(edges.way_id))
    
    # A quantidade de pontos que resultante do map matching deve ser a mesma dos
    # pontos originais - juntar os dois dataframes
    if (nrow(trecho) == nrow(trip_points)) {
      # Se sim, associar as duas bases de dados
      trecho <- trecho %>% cbind(trip_points)
      
      # Limpar pontos que não conseguiram ser associados a um edge no map matching
      trecho <- clean_matched_points(trecho, n_trip_edges = nrow(trip_edges))
      # trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
      
      # # Do resultado do trace_attributes(), selecionar seção de shape
      # attributes_shape <- response_text_at['shape']
      
      # Marcar map matching como bem sucedido e retornar trecho com dados
      cp_j <- '0'
      return(list(cp_j, trecho))
      
    } else {
      # Retornar erro no map matching: número de pontos resultantes não bate com
      # a quantidade de pontos original
      cp_j <- '2'; trecho <- NA
      return(list(cp_j, trecho))
    }
    
  } else {
    # Se segundo map matching não pôde ser completado, abortar continuação
    cp_j <- '1'; trecho <- NA
    return(list(cp_j, trecho))
  }
  
}


# Executar os map matchings com trace_route() e trace_attributes() e retornar 
# lista com código de processamento e rota com atributos resultantes
run_map_matching <- function(trecho, sel_trip, tempo_viagem, dist_min_viagem, active_mode,
                             pontos_elevacao, vias_ciclo, vias_restritas, semaforos) {
  # trecho <- viagem
  
  # Faz o trace_route() da rota. Este processo é o que resulta na distância mais
  # próxima à calculada entre os pontos no QGIS e 
  trace_route_results <- trace_route_valhalla_main(trecho, sel_trip, active_mode)
  dist_rota  <- trace_route_results[[1]]
  shape_rota <- trace_route_results[[2]]
  
  # Se distância foi calculada e for maior que a mínima, continuar para o map 
  # matching completo
  if (!is.na(dist_rota) & dist_rota >= dist_min_viagem) {
    
    # Rodar segundo map matching com trace_attributes()
    trace_attributes_results <- trace_attributes_valhalla_main(trecho, sel_trip, active_mode)
    
    # Atualizar códigos de processamento referentes aos map matchings
    cp_i <- '0'; cp_j <- trace_attributes_results[[1]]
    cp_map_m <- sprintf('%s%s', cp_i, cp_j)
    
    # Trecho com atributos é o segundo item da lista
    trecho <- trace_attributes_results[[2]]
    
    # Se trace_attributes() foi bem sucedido, inserir demais atributos da rota
    if (cp_j == '0') {
      # ----------------------------------------------------------
      # Inserir dados de elevação
      # ----------------------------------------------------------
      # Inserir dados de elevação - Geosampa
      trecho <- get_elevation(trecho, pontos_elevacao)
      

       # Checar se a inserção dos dados de elevação foi bem sucedida; se não, descartar
       if ('ISOVALOR' %in% names(trecho)) {
       
          # Calcular variações positivas e negativas de elevação com base no geosampa
          elev_var     <- trecho %>% select(isovalor_var)
          elev_geo_pos <- elev_var %>% filter(isovalor_var > 0) %>% sum()
          elev_geo_neg <- elev_var %>% filter(isovalor_var < 0) %>% sum()
          
          # ----------------------------------------------------------
          # Inserir dados de infracicloviária e áreas restritas
          # ----------------------------------------------------------
          # Juntar dados relativos a vias com infraestrutura cicloviária
          trecho <- trecho %>% left_join(vias_ciclo, by = c('edges.way_id' = 'osm_id'))
          
          # Juntar dados se trecho está dentro de áreas restritas, tais como parques
          trecho <- trecho %>% mutate(area_restrita = 
                                        case_when(!edges.way_id %in% vias_restritas$osm_id ~ FALSE,
                                                  TRUE ~ TRUE))
          
          # ----------------------------------------------------------
          # Quantidade de semátofos
          # ----------------------------------------------------------
          # Obter a quantidade de cruzamentos com semáforos ao longo da rota
          qtd_semaforos <- semaforos %>% filter(st_intersects(shape_rota, semaforos, sparse = FALSE))
          qtd_semaforos <- nrow(qtd_semaforos)
          
          # ----------------------------------------------------------
          # Cálculos de distâncias via edges - infra cicloviária, parques
          # ----------------------------------------------------------
          # Obter distâncias com base nos edges (infra cicloviária, áreas restritas)
          dados_edges <- obter_dados_edges(trecho)
          
          # ----------------------------------------------------------
          # Gerar shape resumitivo da viagem toda
          # ----------------------------------------------------------
          # Inserir dados da distribuição dos trechos dos edges no shape de rota
          shape_rota <- shape_rota %>% cbind(dados_edges)
          
          # Inserir dados
          shape_rota <- shape_rota %>% mutate(tempo = tempo_viagem,
                                              dist  = dist_rota,
                                              veloc = dist_rota / tempo_viagem * 3.6,
                                              semaforos = qtd_semaforos,
                                              elev_pos = elev_geo_pos, 
                                              elev_neg = elev_geo_neg,
                                              .before = 'dist_edges')

       } else {
         # Se dataframe 'viagem' não tem coluna 'ISOVALOR', registrar erro
         cp_j <- '3'; trecho <- shape_rota <- NA
       }

    } else {
      # Se trace_attributes() não foi bem sucedido, não há shape e trecho pode ser nulo
      trecho <- shape_rota <- NA
    }

  } else if (is.na(dist_rota)) {
    # Se o primeiro map matching não pôde ser completado, abortar continuação
    cp_i <- '2'; cp_j <- 'X'; trecho <- shape_rota <- NA

  } else {
    # Se trecho for curto demais, marcar com código '2' para ser descartado
    cp_i <- '1'; cp_j <- 'X'; trecho <- shape_rota <- NA

  }
  
  # Montar código de processamento do map matching e retornar com trecho e shape
  cp_map_m <- sprintf('%s%s', cp_i, cp_j)
  
  
  return(list(cp_map_m, trecho, shape_rota))
  
}



# ----------------------------------------------------------
  # Inserir dados de elevação
  # ----------------------------------------------------------
  
  # # Inserir dados de elevação vindos do OSM
  # if (usar_elev_osm == TRUE) {
  #   # Inserir dados de elevação - OSM
  #   viagem <- get_elevation_data(viagem)  
  # }



# --------------------------------------------------------------
# Dados de altimetria - Geosampa
# --------------------------------------------------------------

# Insere dados de elevação do Geosampa para os matched points da rota
get_elevation <- function(trip_df, elev_df){
  # Guardar ids de viário que estão relacionados à rota
  viarios_matched <- trip_df %>% select(edges.way_id) %>% distinct()
  
  # Pegar somente pontos de elevação que estão relacionados à rota
  elevacao_proxima <- elev_df %>% filter(elev_df$osm_id %in% viarios_matched$edges.way_id)
  
  # Criar dataframe simplificado para puxar dados de elevação do geosampa
  rota_matched <- 
    trip_df %>% 
    select(osm_id = edges.way_id,
           lat = matched_points.lat,
           lon = matched_points.lon) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326)
  
  # Na base de elevacao_proxima, pegar o index dos pontos que estão mais próximos
  # aos pontos em rota_matched
  idx_emp <- st_nearest_feature(rota_matched, elevacao_proxima)
  
  # Usar os índices para selecionar os pontos em elevacao_proxima
  elevacao_proxima <- elevacao_proxima %>% slice(idx_emp) %>% st_drop_geometry() %>% select(ISOVALOR) %>% as.data.frame()
  
  # Juntar os dados de elevação à viagem
  if (nrow(trip_df) == nrow(elevacao_proxima)) { 
    trip_df <- cbind(trip_df, elevacao_proxima)
    
    # Inserir coluna com variação de elecação entre pontos
    trip_df <- trip_df %>% mutate(isovalor_var = c(diff(ISOVALOR), 0))
  }
  
  
  return(trip_df)
}



# ----------------------------------------------------------
# # Processar trecho, com map matching e pegando elevação
# ----------------------------------------------------------
processar_trecho <- function(viagem, sel_trip, cp_a, cp_viagem, qtd_quebras, qtd_iteracoes,
                             tempo_min_viagem, qtd_min_pontos, dist_min_viagem, 
                             active_mode = 'pedestrian',
                             pontos_elevacao, vias_ciclo, vias_restritas, semaforos,
                             pasta_viagens_gpkg, pasta_viagens_pngs, 
                             pasta_viagens_csv1, pasta_viagens_csv2) {
  
  # Viagem possui duração, quantidade de pontos e frequência de sinal mínimas?
  tempo_viagem <- get_elapsed_time(viagem)
  cp_trecho <- validar_trecho(viagem, tempo_viagem, tempo_min_viagem, qtd_min_pontos, 'time_s')
  
  # Se sim, cp_trecho == '000'
  if (cp_trecho == '000') {
    # Fazer map matching e registrar resultados
    map_matching_results <- run_map_matching(viagem, sel_trip, tempo_viagem, 
                                             dist_min_viagem, active_mode,
                                             pontos_elevacao, vias_ciclo, 
                                             vias_restritas, semaforos)
    
    # Finalizar código de processamento, com resultado do map matching
    cp_mm <- map_matching_results[[1]]
    cod_proc <- sprintf('%s%s%s', cp_viagem, cp_trecho, cp_mm)
    
    # Se map matching foi bem sucedido
    if (cp_mm == '00') {
      # Registrar shape da rota
      viagem <- map_matching_results[[2]]
      shape_rota <- map_matching_results[[3]]
      # mapview(shape_rota)
      
      # Fazer ajustes e inserir dados finais para exportar
      shape_rota <- 
        shape_rota %>%
        # Inserir código de processamento na viagem, quantidade de quebras de sinal
        # detectadas, quantidade de iterações para remoção de outliers extremos e
        # proporção de pontos do trecho dentro de um centróide de x metros de raio
        mutate(cod_proc        = cod_proc, 
               qtd_quebras     = qtd_quebras, 
               qtd_it_outliers = qtd_iteracoes,
               prop_centr_100 = detectar_proporcao_centroide(viagem, tam_raio = 100),
               prop_centr_150 = detectar_proporcao_centroide(viagem, tam_raio = 150),
               .after = 'trip_id') %>% 
        # Incluir dados de início e final da viagem
        mutate(vg_inicio  = as.POSIXlt(min(viagem$timestamps), origin = '1970-01-01'),
               vg_termino = as.POSIXlt(max(viagem$timestamps), origin = '1970-01-01'),
               .before = 'geometry')
      
      # Salvar resultados nos formatos .gpkg, .csv e .png
      salvar_resultados_map_matching(viagem, sel_trip, cp_a, shape_rota, 
                                     pasta_viagens_gpkg, pasta_viagens_pngs,
                                     pasta_viagens_csv1, pasta_viagens_csv2) 
      
      # Registrar processamento da viagem no log
      write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
      
    } else {
      # Map matching foi mal sucedido - Registrar processamento da viagem no log
      write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
      
    }
    
  } else {
    # Trecho não foi validado - Registrar processamento da viagem no log
    cp_mm <- 'XX'
    cod_proc <- sprintf('%s%s%s', cp_viagem, cp_trecho, cp_mm)
    write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
    
  }
  
}
