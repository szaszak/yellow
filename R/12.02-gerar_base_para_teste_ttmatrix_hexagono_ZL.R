# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)


# ------------------------------------------------------------------------------
# Agregar totais de população e oportunidades aos hexágonos
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))

# Tratar como dataframe e selecionar somente colunas de interesse
hex_sp <- st_centroid(hex_sp) %>% mutate(centroides = as.character(geom)) %>% st_drop_geometry()

# Oportunidades por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_landuse_2019_v2.csv', dados_originais)
dados_ipea <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea <- dados_ipea %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar só totais de oportunidades
dados_ipea <- dados_ipea %>% mutate(oportunidades = T001 + E001 + M001 + S001 + C001) %>% select(id_hex, oportunidades)
head(dados_ipea)

# População por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_population_2010_v2.csv', dados_originais)
dados_ipea_pop <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea_pop <- dados_ipea_pop %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar neste momento só dados totais da população
dados_ipea_pop <- dados_ipea_pop %>% select(id_hex, populacao = P001)
head(dados_ipea_pop)

# Juntar dados de oportunidades e população
hex_sp <- 
  hex_sp %>% 
  left_join(dados_ipea, by = 'id_hex') %>% 
  left_join(dados_ipea_pop, by = 'id_hex')

# Hexágonos sem oportunidade e sem população devem ser descartados
hex_sp <- hex_sp %>% filter(oportunidades > 0 & populacao > 0)


# ------------------------------------------------------------------------------
# Gerar latlong para as origens e destinos (centroides dos hexágonos)
# ------------------------------------------------------------------------------

# Separar coluna de centroides em latlon
hex_sp <-
  hex_sp %>%
  separate(centroides, '[c\\(\\), )]', into = c('x', 'y', 'lon', 'z', 'lat', 'u')) %>%
  select(id_hex, lat, lon)

# hex_sp %>% filter(is.na(lat) | is.na(lon))

# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/hex_spo_res09_26vizinhos.csv", pasta_aoprev_hex)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))

# Juntar hexágonos de origem e destino às cordenadas latlong de seus centroides
hex_com_vizinhos <-
  hex_com_vizinhos %>%
  left_join(hex_sp, by = c('id_hex_x' = 'id_hex')) %>%
  left_join(hex_sp, by = c('id_hex_y' = 'id_hex'))

# Remover hexágonos vizinhos que estão fora do shape de São Paulo
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.y) & !is.na(lon.y))
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.x) & !is.na(lon.x))
# 10911566 / 1267988 = 8.6 vezes as queries com resolução 8
nrow(hex_com_vizinhos)

# Limpar ambiente
rm(hex_sp, dados_ipea, dados_ipea_pop)


# ------------------------------------------------------------------------------
# Criar base para routing
# ------------------------------------------------------------------------------

# Para o primeiro teste, queremos apenas dois hexágonos da zona leste
# filter: removed 10,909,452 rows (>99%), 2,114 rows remaining
hex_com_vizinhos <- hex_com_vizinhos %>% filter(id_hex_x == '89a81046b2fffff' | id_hex_x == '89a81044d93ffff')
# hex_com_vizinhos %>% filter(id_hex_x == id_hex_y)
# head(hex_com_vizinhos)

# Criar coluna com URL para GET no GraphHopper - tudo o que pode ser habilitado
# nos arquivos de config-example.yml e custom models pode ser puxado aqui com
# a tag de &details=XXX. A documentação básica está neste link, mas não diz isso:
# https://github.com/graphhopper/graphhopper/blob/master/docs/web/api-doc.md
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id&details=road_class&details=bike_network&details=smoothness&details=distance'

hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))


# Criar uma coluna de id
hex_com_vizinhos <- hex_com_vizinhos %>% mutate(id = str_c(id_hex_x, id_hex_y, sep = '-'), .before = 'id_hex_x')
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
head(hex_com_vizinhos)


# Guardar resultados - base integral
out_file <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
write_delim(hex_com_vizinhos, out_file, delim = ';')
