# Comando para rodar o GraphHopper no terminal - atenção para o PBF a ser carregado
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml

# carregar bibliotecas
source('fun/setup.R')
library('httr')
library('jsonlite')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/Multiplicidade", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_hexagonos   <- sprintf("%s/02_hexagonos", pasta_graphhopper)
pasta_gh_ttmarix  <- sprintf("%s/04_ttmatrix_rede_2019", pasta_graphhopper)
dir.create(pasta_gh_ttmarix, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Gerar latlong para as origens e destinos (centroides dos hexágonos)
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 8, com distância de ~1km entre os vértices
hex_sp <- read_rds(sprintf("%s/hex_spo_08_2019.rds", dados_originais))

# Tratar como dataframe e selecionar somente colunas de interesse
hex_sp <- hex_sp %>% st_drop_geometry() %>% select(id_hex, centroides_muni)

# Separar coluna de centroides_muni em latlon
hex_sp <-
  hex_sp %>%
  separate(centroides_muni, '[c\\(\\), )]', into = c('x', 'y', 'lon', 'z', 'lat', 'u')) %>%
  select(id_hex, lat, lon)

# hex_sp %>% filter(is.na(lat) | is.na(lon))

# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/hex_spo_res08_17vizinhos.csv", pasta_hexagonos)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))

# Juntar hexágonos de origem e destino às cordenadas latlong de seus centroides
hex_com_vizinhos <-
  hex_com_vizinhos %>%
  left_join(hex_sp, by = c('id_hex_x' = 'id_hex')) %>%
  left_join(hex_sp, by = c('id_hex_y' = 'id_hex'))

# Remover hexágonos vizinhos que estão fora do shape de São Paulo
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.y) & !is.na(lon.y))

# Limpar ambiente
rm(hex_sp)


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe
gh_route <- function(url) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  
  # Fazer a GET de roteamento no Grahphopper
  # print(url)
  gh_response <- GET(url)
  
  # Encurtar url para guardar no dataframe de resultado
  url <- 
    url %>% 
    str_replace('http:\\/\\/localhost:8989\\/route\\/\\?point=', '') %>% 
    str_replace('&point=', ';') %>%
    str_replace_all('%2C', ',') %>% 
    str_replace('&profile=bike&instructions=false&calc_points=true&details=average_speed', '')
  
  # Mensagem tem que ser "Success: (200) OK"
  if (http_status(gh_response)$message == 'Success: (200) OK') {
    
    # Resposta da query, já colapsada e transformada em dataframe
    # Remover aviso de 'No encoding supplied: defaulting to UTF-8' na linha fromJSON()
    suppressMessages(
      response_text <- 
        # Ignorar aviso 'argument is not an atomic vector; coercing'
        suppressWarnings(str_c(gh_response, collapse = ", ")) %>% 
        # Concatenar toda a string de resultados
        str_c("[", ., "]") %>% 
        # Transformar em dataframe
        fromJSON() %>% 
        as.data.frame()
    )
    
    # Nos interessa a coluna de 'paths', como um novo dataframe
    paths <- response_text$paths %>% as.data.frame()
    
    # Isolar colunas de interesse
    paths <- 
      paths %>% 
      # Calcular tempo em segundos e velocidade média
      mutate(time = time / 1000,
             speed = distance / time * 3.6) %>% 
      # Descartar colunas extras - a coluna poly é o shape da rota traçada
      select(distance, weight, time, speed, poly = points) %>% 
      mutate(url = url)
    
  } else {
    
    # Se a query no GraphHopper não deu resultados, guardar como dataframe vazio
    paths <- data.frame(distance = NA,
                        weight   = NA,
                        time     = NA,
                        speed    = NA,
                        poly     = NA,
                        url      = url
                        )
    
  }
  
  # Guardar resultados temporários
  out_file <- sprintf('%s/ttmatrix_res08.csv', pasta_gh_ttmarix)
  write_delim(paths, out_file, delim = ';', append = TRUE)
  
}


# # Criar coluna com URL para GET no GraphHopper
# hex_test <- hex_com_vizinhos %>% head(3)
# hex_test <- 
#   hex_test %>% 
#   mutate(url = paste('http://localhost:8989/route/?point=', 
#                      lat.x, '%2C', lon.x, '&point=', 
#                      lat.y, '%2C', lon.y, '&profile=bike&instructions=false&calc_points=true&details=average_speed',
#                      sep = ''))

# lala <- lapply(hex_test$url, gh_route)
# lala <- rbindlist(lala)
# lala %>% mutate(time2 = time/60)
# 
# # Transformar a rota resultante em latlongs e visualizar no mapview
# goo <- polyline_to_latlong(lala %>% slice(2) %>% select(poly))
# polyline_latlong_to_linestring(goo) %>% mapview()


# Criar coluna com URL para GET no GraphHopper
hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, '&profile=bike&instructions=false&calc_points=true&details=average_speed',
                     sep = ''))

head(hex_com_vizinhos)


# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter
detach("package:tidylog")
lapply(hex_com_vizinhos$url, gh_route)

