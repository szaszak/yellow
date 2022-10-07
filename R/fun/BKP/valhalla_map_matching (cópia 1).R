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
  # meili_request_body

  # Sending a request to Valhalla's Meili
  url <- "http://localhost:8002/trace_route"
  # headers = {'Content-type': 'application/json'}
  # data = str(meili_request_body)
  r <- POST(url, body = meili_request_body)
  # r
  # http_status(r)
  # class(r) # response
  # http_status(r)$category # 'Success'
  # http_status(r)$message # 'Success: (200) OK'
  
  if (http_status(r)$message == 'Success: (200) OK') {

    return(r)

  } else {

    return(sprintf('Error: %s', http_status(r)$message))
  }
}


# # Traçar rota com trace_route, pegar resposta e transformar em lista
# r <- trace_route_valhalla(df_tmp, 'pedestrian') # pedestrian, bicycle
# response_text <- parse_json(r, simplifyVector = FALSE)

# Retorna df com dados de interesse da seção 'tracepoints' (resultado do trace_route)
route_tracepoints <- function(response_text, trip_id) {
  
  # Da lista de resposta, selecionar somente a seção de tracepoints
  resp_tp <- response_text['tracepoints']
  
  # Transformar a seção tracepoints em dataframe
  df_tp <- 
    # Transformar a seção tracepoints em JSON e o JSON em dataframe
    as.data.frame(fromJSON(toJSON(resp_tp))) %>% 
    # Isolar as colunas de interesse name, distance e location
    select(tracepoints.name, tracepoints.distance, tracepoints.location) %>% 
    # A coluna de location deve ser dividida (unnested) em long, lat
    unnest_wider(tracepoints.location, names_sep = "_")  %>% 
    # Renomear tudo para tirar o 'tracepoints' dos nomes de coluna e
    # deixar claras as colunas de long e lat
    rename(name = tracepoints.name,
           distance = tracepoints.distance,
           lon = tracepoints.location_1,
           lat = tracepoints.location_2) %>%
    # Converter colunas name e distance para character e double, respectivamente;
    # vamos manter as linhas que não tiveram match e que ficaram como NA para
    # poder associar ao dataframe original depois
    unnest(cols = c(name, distance), keep_empty = TRUE) %>% 
    # Inserir coluna com o trip_id da rota
    add_column(tripid = trip_id, .before = 'name') #%>% 
  # Converter para sf - isso não vai ser feito agora, pois precisamos de
  # um dataframe resultante com o mesmo número de linhas para associar
  # ao original e manter os dados de tempo e ordem dos pontos
  # st_as_sf(coords = c('lon', 'lat'), crs = 4326)
  
  return(df_tp)
}


# Retorna df com dados de interesse da seção 'matchings' (resultado do trace_route)
route_matchings <- function(response_text, trip_id){

  # Da lista, selecionar somente a seção de matchings, que contém
  # o polyline da rota utilizada
  resp_m <- response_text['matchings']
    # Transformar a seção matchings em dataframe
  df_rm <- 
    as.data.frame(fromJSON(toJSON(resp_m))) %>% 
    # Transformar a coluna com a rota em character e isolá-la
    mutate(route_polyline = as.character(matchings.geometry)) %>% 
    #         select(route_polyline) %>% 
    # Inserir coluna com o trip_id da rota
    add_column(tripid = trip_id, .before = 'route_polyline')

  return(df_rm)
}


# Retorna a distância percorrida em metros
matchings_route_distance <- function(matchings_df, dist_col = 'matchings.distance'){
  #    if (nrow(matchings_df) == 1){
  #        distance <- matchings_df %>% select({{dist_col}}) %>% pull() %>% unlist()
  #        return(distance)
  #    }    
  distance <- matchings_df %>% select({{dist_col}}) %>% unlist() %>% sum()
  return(distance)
}


# Retorna o tempo da rota em segundos
matchings_route_duration <- function(matchings_df, duration_col = 'matchings.duration'){
  #    if (nrow(matchings_df) == 1){
  #        distance <- matchings_df %>% select({{dist_col}}) %>% pull() %>% unlist()
  #        return(distance)
  #    }    
  duration <- matchings_df %>% select({{duration_col}}) %>% unlist() %>% sum()
  return(duration)
}


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


# Transformar polyline em sf_linestring, retorna sf
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
trace_attributes_valhalla <- function(input_df, active_mode){
  
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
  # r
  # http_status(r)
  # class(r) # response
  # http_status(r)$category # 'Success'
  # http_status(r)$message # 'Success: (200) OK'
  
  if (http_status(r)$message == 'Success: (200) OK') {
    return(r)
  } else {
    return(sprintf('Error: %s', http_status(r)$message))
  }
}


# Retorna df com dados de interesse da seção 'edges' (resultado do trace_attributes)
attributes_edges <- function(response_text){
  # Da lista, selecionar somente a seção de edges
  resp_ed <- response_text['edges']
  
  # Transformar a seção em dataframe
  df_ae <- 
    # Transformar a seção edges em JSON e o JSON em dataframe
    as.data.frame(fromJSON(toJSON(resp_ed), flatten = TRUE)) %>% 
    # Isolar as colunas de interesse
    select(edges.way_id, edges.id,
           edges.names, edges.length, 
           # edges.begin_shape_index, edges.end_shape_index, 
           # este só funciona se a viagem tiver uma só perna (leg):
           # edges.internal_intersection 
           # não é sempre que tem as colunas de grade:
           # edges.max_upward_grade, edge.max_downward_grade
           edges.mean_elevation) %>% 
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


# Retorna df com dados de interesse da seção 'matched_points' (resultado do trace_attributes)
attributes_matched_points <- function(response_text){
  # Da lista, selecionar somente a seção de matched_points
  resp_ed <- response_text['matched_points']
  
  # Transformar a seção matched_points em dataframe
  df_mp <- 
    # Transformar a seção tracepoints em JSON e o JSON em dataframe
    as.data.frame(fromJSON(toJSON(resp_ed), flatten = TRUE)) %>%

  # -----------------------------------------------
  # REVER ESTE TRECHO
  # -----------------------------------------------
  # Transformar todas as colunas em character
  mutate(across(where(is.list), as.character)) %>%
  select(matched_points.edge_index, matched_points.type, ,
         matched_points.distance_from_trace_point, 
         matched_points.distance_along_edge,
         matched_points.lat, matched_points.lon) %>%
  # Atribuir valor 0 para linhas iniciais que não deram match -
  # checar se sempre os valores NULL estarão nas linhas iniciais,
  # ou seja, se sempre poderão ser associados ao edge_index de 0
  mutate(matched_points.edge_index = case_when(matched_points.edge_index == 'NULL' ~ '0',
                                               TRUE ~ matched_points.edge_index)) %>% 
  # Transformar coluna em double para left_join() com df de attributes_edges()
  mutate(matched_points.edge_index = as.double(matched_points.edge_index)) 
  
  return(df_mp)
}


# --------------------------------------------------------------
# Função geral para pegar dados resumitivos da rota
# --------------------------------------------------------------

# Retorna um sf da rota com a distância percorrida (m), tempo total de viagem (s),
# velocidade (km/h) e geometry resultante do map matching com o valhalla
get_route_data <- function(df_rota, trip_id, dist_thrs = 300){
  
  # trip_id <- '209982'
  # df_rota <- viagens %>% filter(trip_id == {{trip_id}})
  # df_rota %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Traçar rota com trace_route, retorna o response do POST
  r_tr <- trace_route_valhalla(df_rota, 'pedestrian') # pedestrian, bicycle
  
  # Se rota foi traçada, continuar; se não, avisar
  if (class(r_tr) == 'response') {
    # Pegar resposta do POST e transformar em lista com o parse_json()
    response_text_tr <- parse_json(r_tr, simplifyVector = FALSE)

    # Do trace_route(), pegar a seção de matchings, que contém
    # os dados de distância percorrida por trecho
    matchings <- route_matchings(response_text_tr, trip_id)
    # Retorna a distância percorrida em metros para a rota toda
    route_distance  <- matchings_route_distance(matchings)

    # Só continuar cálculos se rota for maior do que limite mínimo
    if (route_distance >= dist_thrs) {

      # Pegar tempo total da rota, calculado pela diferença entre o último
      # ponto GPS e o primeiro, em segundos
      route_time <- get_elapsed_time(df_rota)

      # Calcular a velocidade média: distância sobre tempo, em km/h
      route_speed_kph <- matchings_route_speed(route_distance, route_time)

      # Traçar rota com trace_attributes, pegar resposta e transformar em lista
      r_at <- trace_attributes_valhalla(df_rota, 'pedestrian') # bicycle, pedestrian

      # Se rota foi traçada, continuar; se não, avisar
      if (class(r_at) == 'response') {
        # Pegar resposta do POST e transformar em lista com o parse_json()
        response_text_at <- parse_json(r_at, simplifyVector = FALSE)

        # # Quantos edges tem na rota?
        # n_edges <- length(response_text_at['edges'][[1]])
        # edges <- attributes_edges(response_text_at)

        # Do resultado do trace_attributes(), selecionar seção de shape
        attributes_shape <- response_text_at['shape']

        # Converter shape para sf com linestring
        sf_out <- polyline_to_latlong(trip_id, attributes_shape)
        sf_out <- polyline_latlong_to_linestring(sf_out)

        # Juntar dados de distância, tempo e velocidade ao shape e retornar
        sf_out <- sf_out %>% mutate(distance  = route_distance,
                                    time      = route_time,
                                    speed_kph = route_speed_kph,
                                    .after = 'trip_id')


        return(sf_out)
      } else { # class(r_at) == 'character' & str_detect(r_tr, 'Error')
        print(r_at)
    }

  } else { # class(r_tr) == 'character' & str_detect(r_tr, 'Error')
    print(r_tr)
  }

  }
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
