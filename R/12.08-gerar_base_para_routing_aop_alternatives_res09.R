# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
dir.create(pasta_aoprv_alter, recursive = TRUE, showWarnings = FALSE)
# pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
# dir.create(pasta_ids_aopt_19, recursive = TRUE, showWarnings = FALSE)
# dir.create(pasta_rts_aopt_19, recursive = TRUE, showWarnings = FALSE)


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

# Criar coluna com URL para GET no GraphHopper
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id'

hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))


# Criar uma coluna de id
hex_com_vizinhos <- hex_com_vizinhos %>% mutate(id = str_c(id_hex_x, id_hex_y, sep = '-'), .before = 'id_hex_x')
head(hex_com_vizinhos)


# Guardar resultados - base integral
out_file <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
write_delim(hex_com_vizinhos, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Dividir resultados para processamento - fazer para os anos 2019 e, depois, 2028
# ------------------------------------------------------------------------------

# Dividir resultados em arquivos menores, para processamento
# out_file <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
# hex_com_vizinhos <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
# nrow(hex_com_vizinhos) # 10.911.566
head(hex_com_vizinhos)

# Definir intervalo que custe pouca memória do future
intervalo <- 100000
# Puxar valor de nrow(hex_com_vizinhos)
max_value <- nrow(hex_com_vizinhos)
# max_value/ intervalo # 110 vezes

# Abrir base hex_com_vizinhos e selecionar quantidade de linhas
# de acordo com intervalo, checar ids que já rodaram, fazer
# routing com future, limpar memória e reiniciar até terminar
counter <- 1
for (i in seq(1, max_value, intervalo)) {
  # i <- 1;

  # Se valor máximo pelo intervalo for maior que dataframe,
  # considerar tamanho do dataframe como máximo
  interval_max <- i + intervalo - 1
  if (interval_max > max_value) { interval_max <- max_value }
  print(sprintf('%i - %i', i, interval_max))

  # Puxar só linhas correspondentes ao intervalo
  out_hex <- hex_com_vizinhos %>% slice(i:interval_max)

  # Gravar arquivo temporário
  out_file <- sprintf('%s/tmp_base_para_routing_res09_26vizinhos_%s.csv', pasta_aoprv_alter, str_pad(counter, 3, pad = '0'))
  write_delim(out_hex, out_file, delim = ';')
  
  # Atualizar counter
  counter <- counter + 1

}
