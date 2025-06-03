# Gerar base de pares de hexágono de origens e destinos, já filtrando para o
# recorte de estudantes de ensino médio de escolas públicas nas origens e locais
# das escolas com matrículas nos destinos

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)


# ------------------------------------------------------------------------------
# Grid de hexágonos h3 resolução 9 do IPEA
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", pasta_ipea))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))
head(hex_sp)

# Puxar todos os centróides dos hexágonos do shape
centroides_hex <- hex_sp %>% 
  st_centroid() %>% 
  mutate(lon = as.character(st_coordinates(geom)[, 1]),
         lat = as.character(st_coordinates(geom)[, 2])) %>% 
  st_drop_geometry()


# ------------------------------------------------------------------------------
# Puxar e filtrar origens e destinos
# ------------------------------------------------------------------------------

# Destinos: Censo escolar com todas as escolas
destinos <- sprintf('%s/02_matriculas_censo_escolar_2023_associadas_a_hexagonos.gpkg', pasta_ttmatrix_24_28)
destinos <- read_sf(destinos)

# Escolas públicas com matrículas de ensino médio, sem restrições de atendimento
destinos <- destinos %>% 
  filter(QT_MAT_MED > 0 & CA_CATEGORIA == 'Pública' & CA_RESTRICAO == 'ESCOLA EM FUNCIONAMENTO E SEM RESTRIÇÃO DE ATENDIMENTO')

# Quantidade de matrículas para ensino médio: 364.441
sum(destinos$QT_MAT_MED)

# Puxar hexágonos onde estão essas escolas
hex_destinos <- destinos %>% st_drop_geometry() %>% select(id_hex) %>% distinct()


# Origens: estudantes de ensino médio de escolas públicas, sem limite de idade,
# que estudam em escolas dentro do território de SP
origens <- sprintf('%s/01_divisao_prop_integral_estudantes_por_zona_OD_hexagonos_short.csv', pasta_ttmatrix_24_28)
origens <- read_delim(origens, delim = ';', col_types = 'icidiiiiiii')

# Quantidade de estudantes: 337.983
sum(origens$estudantes_totais)

# Puxar hexágonos onde estão os estudantes
hex_origens <- origens %>% select(id_hex = h3_address) %>% distinct()

# Gerar um shapefile para conferência
shape_origens <- origens %>% group_by(h3_address) %>% summarise(estudantes_totais = sum(estudantes_totais))
shape_origens <- hex_sp %>% left_join(shape_origens, by = c('id_hex' = 'h3_address'))
shape_origens <- shape_origens %>% select(id_hex, estudantes_totais)
shape_origens <- shape_origens %>% mutate(estudantes_totais = ifelse(is.na(estudantes_totais), 0, estudantes_totais))
out_file <- sprintf('%s/01_shape_resumido_estudantes_por_hexagono.gpkg', pasta_ttmatrix_24_28)
st_write(shape_origens, out_file, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Pareamento de hexágonos com vizinhos
# ------------------------------------------------------------------------------

vizinhos <- sprintf('%s/00_hex_spo_res09_12vizinhos.csv', pasta_ttmatrix_24_28)
vizinhos <- read_delim(vizinhos, delim = ';', col_types = 'cc')

# Filtrar somente hexágonos com origens e destinos que serão utilizadas
vizinhos <- vizinhos %>% filter(id_hex_x %in% hex_origens$id_hex)
vizinhos <- vizinhos %>% filter(id_hex_y %in% hex_destinos$id_hex)

# Checagem 1: Origens (para checar no QGIS)
origens_check <- hex_sp %>% filter(id_hex %in% vizinhos$id_hex_x)
origens_check %>% filter(!id_hex %in% hex_origens$id_hex)
out_file <- sprintf('%s/origens_check_para_ttmatrix.gpkg', pasta_ttmatrix_24_28)
st_write(origens_check, out_file, driver = 'GPKG', append = FALSE)

# Checagem 2: Destinos (para checar no QGIS)
destinos_check <- hex_sp %>% filter(id_hex %in% vizinhos$id_hex_y)
destinos_check %>% filter(!id_hex %in% hex_destinos$id_hex)
out_file <- sprintf('%s/destinos_check_para_ttmatrix.gpkg', pasta_ttmatrix_24_28)
st_write(destinos_check, out_file, driver = 'GPKG', append = FALSE)


# Juntar coordenadas de latlon
vizinhos <- 
  vizinhos %>% 
  left_join(centroides_hex, by = c('id_hex_x' = 'id_hex')) %>% 
  left_join(centroides_hex, by = c('id_hex_y' = 'id_hex'))

# vizinhos %>% filter(!is.na(lat.y) & !is.na(lon.y))
# vizinhos %>% filter(!is.na(lat.x) & !is.na(lon.x))

# 205656 combinações
nrow(vizinhos)

# Criar coluna com URL para GET no GraphHopper - tudo o que pode ser habilitado
# nos arquivos de config-example.yml e custom models pode ser puxado aqui com
# a tag de &details=XXX. A documentação básica está neste link, mas não diz isso:
# https://github.com/graphhopper/graphhopper/blob/master/docs/web/api-doc.md
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id&details=road_class&details=bike_network&details=smoothness&details=distance'

# Criar coluna de url para routing
vizinhos <- 
  vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))

# Criar uma coluna de id
vizinhos <- vizinhos %>% 
  mutate(id = str_c(id_hex_x, id_hex_y, sep = '-'), .before = 'id_hex_x') %>% 
  select(id, url)

head(vizinhos)

# Guardar resultados - base integral
out_file <- sprintf('%s/03_base_routing_res09_12vizinhos.csv', pasta_ttmatrix_24_28)
write_delim(vizinhos, out_file, delim = ';')
