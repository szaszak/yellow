# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_hexagonos   <- sprintf("%s/02_hexagonos", pasta_graphhopper)
pasta_gh_ttmarix  <- sprintf("%s/04_ttmatrix_rede_2019", pasta_graphhopper)
# dir.create(pasta_gh_ttmarix, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Resultados ttmatrix - isolar somente coluna de id e guardar resultados
# ------------------------------------------------------------------------------

# Abrir resultados para ttmatrix e juntá-los em um único dataframe
resultados_ttmatrix <- list.files(pasta_gh_ttmarix, pattern = '^ttmatrix_res09_([0-9]){2}\\.csv', full.names = TRUE)

resultados_ttmatrix <- 
  lapply(X = resultados_ttmatrix, 
         FUN = read_delim, 
         delim = ';',
         col_types = 'ddddcc',
         col_names = FALSE) %>% 
  rbindlist()


# Selecionar somente a coluna de id
resultados_ttmatrix <- resultados_ttmatrix %>% select(id = X6)

# Criar um segundo id - para facilitar o processamento, vamos guardar o dataframe
# original do resultados_ttmatrix mas processar só a parte dos ids
resultados_ttmatrix <- resultados_ttmatrix %>% add_column(index_col = 1:nrow(resultados_ttmatrix))

# Limpar memória
gc(T)


# Reconstituir coluna de id: os centróides não estão batendo nas casas decimais -
# por isso, vamos restringir as colunas de lat e lon a menos caracteres
resultados_ttmatrix <- 
  resultados_ttmatrix %>% 
  # Separar id em seus componentes de latlong
  separate(id, into = c('x', 'y'), sep = ';') %>%
  separate(x, into = c('lat.x', 'lon.x'), sep = ',') %>% 
  separate(y, into = c('lat.y', 'lon.y'), sep = ',') %>% 
  # Simplificar colunas de lat e lon para conterem só 9 caracteres
  mutate(lat.x = str_sub(lat.x, 1, 9),
         lon.x = str_sub(lon.x, 1, 9),
         lat.y = str_sub(lat.y, 1, 9),
         lon.y = str_sub(lon.y, 1, 9)) %>% 
  # Reconstituir coluna de id
  mutate(id = paste(lat.x, ',', lon.x, ';', 
                    lat.y, ',', lon.y, sep = '')) %>% 
  select(-c(lat.x, lon.x, lat.y, lon.y))

# Limpar memória
gc(T)

head(resultados_ttmatrix)

# Guardar resultados - relação entre ids originais e linhas onde se localizam
out_file <- sprintf('%s/ttmatrix_res09_ids.csv', pasta_gh_ttmarix)
write_delim(resultados_ttmatrix, out_file, delim = ';')


# # Guardar resultados vazios para analisar
# resultados_vazios <-
#   resultados_ttmatrix %>%
#   select(-poly) %>% 
#   filter(is.na(distance)) %>%
#   separate(id, into = c('x', 'y'), sep = ';') %>%
#   separate(y, into = c('latitude', 'longitude'), sep = ',') %>%
#   select(-x) %>%
#   distinct()
# 
# # Guardar resultados temporários
# out_file_empty <- sprintf('%s/04_tmp_resultados_ttmatrix_vazios_res08.csv', pasta_gh_ttmarix)
# write_delim(resultados_vazios, out_file_empty, delim = ';')
# 
# # Checar um dos resultados vazios em que um dos pontos está no centro de SP -
# # este ponto está no extremo norte da Cantareira, gerando dois pontos (um com ele
# # na origem, outro com ele no destino) em que não é possível traçar uma rota# 
# # resultados_ttmatrix %>% 
# #   filter(str_detect(id, '-23.5390199310058,-46.6376369484305') & is.na(distance))
# # Já neste, há 5 resultados, todos no extremo norte tb
# # resultados_ttmatrix %>%
# #   filter(str_detect(id, '-23.4993977930825,-46.6640114840266') & is.na(distance))
# 
# # Limpar ambiente
# rm(resultados_vazios)



# [ REINICIAR R PARA RODAR O RESTANTE ]

# ------------------------------------------------------------------------------
# Grade de hexágonos com vizinhos
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))

# Tratar como dataframe e selecionar somente colunas de interesse
hex_sp <- st_centroid(hex_sp) %>% mutate(centroides = as.character(geom)) %>% st_drop_geometry()

# Separar coluna de centroides em latlon
hex_sp <-
  hex_sp %>%
  separate(centroides, '[c\\(\\), )]', into = c('x', 'y', 'lon', 'z', 'lat', 'u')) %>%
  select(id_hex, lat, lon) %>%
  # Simplificar colunas de lat e lon para conterem só 9 caracteres
  mutate(lat = str_sub(lat, 1, 9),
         lon = str_sub(lon, 1, 9))


# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/hex_spo_res09_23vizinhos.csv", pasta_hexagonos)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))

# Juntar hexágonos de origem e destino às cordenadas latlong de seus centroides
hex_com_vizinhos <-
  hex_com_vizinhos %>%
  left_join(hex_sp, by = c('id_hex_x' = 'id_hex')) %>%
  left_join(hex_sp, by = c('id_hex_y' = 'id_hex'))

# Remover hexágonos vizinhos que estão fora do shape de São Paulo
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.y) & !is.na(lon.y))
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.x) & !is.na(lon.x))


# Criar coluna de id para juntar aos resultados da ttmatrix
hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(id = paste(lat.x, ',', lon.x, ';', 
                    lat.y, ',', lon.y, sep = '')) %>% 
  select(id_hex_x, id_hex_y, id)

head(hex_com_vizinhos)


# Limpar memória
rm(hex_sp)
gc(T)


# ------------------------------------------------------------------------------
# Juntar resultados ttmatrix com grade de hexágonos
# ------------------------------------------------------------------------------

# Abrir arquivo que foi exportado com os ids do ttmatix
ids_ttmatrix <- sprintf('%s/ttmatrix_res09_ids.csv', pasta_gh_ttmarix)
ids_ttmatrix <- read_delim(ids_ttmatrix, delim = ';', col_types = 'ic')
head(ids_ttmatrix)

# # Testar se join vai dar certo
# ids_ttmatrix %>% 
#   left_join(hex_com_vizinhos, by = 'id') %>% 
#   filter(is.na(id_hex_x))

# Juntar combinação de hexágonos e vizinhos com id novo (index_col)
hex_com_vizinhos <- hex_com_vizinhos %>% left_join(ids_ttmatrix, by = 'id')

# Remover linhas para as quais não há resultados do ttmatrix
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(index_col)) %>% arrange(index_col)

head(hex_com_vizinhos)

# Limpar memória
rm(ids_ttmatrix)
gc(T)



# ------------------------------------------------------------------------------
# Juntar resultados ttmatrix com hexágonos
# ------------------------------------------------------------------------------

# Abrir resultados para ttmatrix e juntá-los em um único dataframe
resultados_ttmatrix <- list.files(pasta_gh_ttmarix, pattern = '^ttmatrix_res09_([0-9]){2}\\.csv', full.names = TRUE)

resultados_ttmatrix <- 
  lapply(X = resultados_ttmatrix, 
         FUN = read_delim, 
         delim = ';',
         col_types = 'ddddcc',
         col_names = FALSE) %>% 
  rbindlist()

# Renomear colunas
resultados_ttmatrix <- resultados_ttmatrix %>% select(distance = X1,
                                                      weight   = X2,
                                                      time     = X3,
                                                      speed    = X4
                                                      # poly     = X5,
                                                      # id       = X6
                                                      )

# Limpar memória
gc(T)


# Criar um segundo id - para facilitar o processamento, vamos guardar o dataframe
# original do resultados_ttmatrix mas processar só a parte dos ids
resultados_ttmatrix <- resultados_ttmatrix %>% add_column(index_col = 1:nrow(resultados_ttmatrix))

# head(resultados_ttmatrix)

# Juntar resultados ttmatrix às combinações de hexágonos
resultados_ttmatrix <- resultados_ttmatrix %>% left_join(hex_com_vizinhos, by = 'index_col')

# Reordenar para exportar
resultados_ttmatrix <- resultados_ttmatrix %>% select(id_hex_x, id_hex_y, distance, weight, time, speed, index_col)

head(resultados_ttmatrix)


# Gravar resultados
out_file <- sprintf('%s/ttmatrix_res09_resultados_2019.csv', pasta_gh_ttmarix)
write_delim(resultados_ttmatrix, out_file, delim = ';')


resultados_ttmatrix %>% filter(!is.na(speed)) %>% select(distance, time, speed) %>% summary()
# distance             time               speed       
# Min.   :    9.85   Min.   :    3.105   Min.   : 4.116  
# 1st Qu.: 4730.99   1st Qu.: 1580.396   1st Qu.:10.620  
# Median : 6699.31   Median : 2229.486   Median :10.806  
# Mean   : 6521.86   Mean   : 2172.090   Mean   :10.800  
# 3rd Qu.: 8277.05   3rd Qu.: 2750.213   3rd Qu.:10.994  
# Max.   :43727.05   Max.   :14698.061   Max.   :13.219 




# ------------------------------------------------------------------------------
# Avaliar rotas gigantescas
# ------------------------------------------------------------------------------

# source('fun/valhalla_map_matching.R')

# # Quais são as rotas maiores traçadas?
# resultados_ttmatrix %>% filter(distance == max(distance))
# 
# # Selecionar uma delas para análise
# poly <- resultados_ttmatrix %>% filter(distance == max(distance, na.rm = TRUE)) %>% select(poly) %>% distinct() %>% head(1)
# 
# # Tranformar o polyline em shape
# goo <- polyline_to_latlong(poly, trip_id = 'id')
# this <- polyline_latlong_to_linestring(goo)
# mapview(this)
# 
# # Gravar resultados
# pasta_exemplos <- "../../yellow_src/z_exemplos"
# out_shape      <- sprintf('%s/hexagonos8_17vizinhos_rota_maxima.gpkg', pasta_exemplos)
# st_write(this, out_shape, driver = 'GPKG', append = FALSE)
