# carregar bibliotecas
source('fun/setup.R')
library('httr')
library('jsonlite')

# https://github.com/graphhopper/graphhopper/blob/master/docs/web/api-doc.md
# https://docs.graphhopper.com/#tag/Routing-API
# https://towardsdatascience.com/automating-running-routes-in-r-1e97eb4ed716

# https://graphhopper.com/maps/?profile=hike&layer=Omniscale&custom_model=%7B%22distance_influence%22%3A100%2C%22speed%22%3A%5B%7B%22if%22%3A%22road_class+%3D%3D+TRACK+%7C%7C+road_environment+%3D%3D+FERRY+%7C%7C+surface+%3D%3D+DIRT%22%2C%22limit_to%22%3A%2210%22%7D%5D%2C%22priority%22%3A%5B%7B%22if%22%3A%22road_environment+%3D%3D+TUNNEL+%7C%7C+toll+%3D%3D+ALL%22%2C%22multiply_by%22%3A%220.5%22%7D%2C%7B%22if%22%3A%22max_weight+%3C+3+%7C%7C+max_height+%3C+2.5%22%2C%22multiply_by%22%3A%220.0%22%7D%5D%7D
# http://localhost:8989/maps/?point=-23.54973%2C-46.706605&point=-23.558229%2C-46.696941&profile=bike&layer=OpenStreetMap


# Transforma linha de polyline para dataframe com latlongs
polyline_to_latlong <- function(polyline, trip_id = '000'){
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
polyline_latlong_to_linestring <- function(df, trip_id = '000'){
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

# ------------------------------------------------------------------------------
# Infos gerais da query (configuração do GraphHopper)
# ------------------------------------------------------------------------------

lala <- GET('http://localhost:8989/info')
http_status(lala)$message

response_text <- 
  # Ignorar aviso 'argument is not an atomic vector; coercing'
  suppressWarnings(str_c(lala, collapse = ", ")) %>% 
  str_c("[", ., "]") %>% 
  fromJSON() %>% 
  as.data.frame()

response_text


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

start_point <- c(-23.55329,-46.702914)
end_point   <- c(-23.555298,-46.700009)


start_point <- c(-23.54973,-46.706605)
end_point   <- c(-23.558229,-46.696941)

# Criação da URL para GET
url <- paste('http://localhost:8989/route/?point=', start_point[1], '%2C', start_point[2],
             '&point=', end_point[1], '%2C', end_point[2], '&profile=bike&instructions=false&calc_points=true&details=average_speed',
             sep = '')

lala <- GET(url)

# Mensagem tem que ser "Success: (200) OK"
http_status(lala)$message

# Resposta da query, já colapsada e transformada em dataframe
response_text <- 
  # Ignorar aviso 'argument is not an atomic vector; coercing'
  suppressWarnings(str_c(lala, collapse = ", ")) %>% 
  str_c("[", ., "]") %>% 
  fromJSON() %>% 
  as.data.frame()

# Nos interessa a coluna de 'paths', como um novo dataframe
df <- response_text$paths %>% as.data.frame()
df

# As coluna de interesse
df$distance
df$weight
df$time/1000
# Velocidade em km/h
df$distance / df$weight * 3.6

# Pontos inicial e final - beeline, em polyline
df$snapped_waypoints

# Shape da rota traçada, em polyline
df$points

# Transformar a rota resultante em latlongs e visualizar no mapview
goo <- polyline_to_latlong(df$points)
polyline_latlong_to_linestring(goo) %>% mapview()


