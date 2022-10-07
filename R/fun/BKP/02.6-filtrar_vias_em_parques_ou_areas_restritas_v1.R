# Isola os ids e os nomes das vias que estão dentro das áreas de parque e no
# campus da USP e exporta como arquivos .csv

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../yellow_dados"
dados_originais  <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_osm_sp <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)

# Abrir shapefile com polígonos de parques
open_file1 <- sprintf('%s/20160825_WGS84_parques_municipais_geosampa_com_adicoes.gpkg', dados_originais)
parques <- read_sf(open_file1)

# Modificar camada de parques de MULTIPOLYGON para POLYGON
parques <- parques %>% st_make_valid() %>% st_cast('MULTIPOLYGON') %>% st_cast("POLYGON")

# Isolar campus da USP
campus_usp <- parques %>% filter(pq_nome == 'CAMPUS USP')
# Retirar campus da USP do shape de parques e juntar todos os shapes
parques <- parques %>% filter(pq_nome != 'CAMPUS USP') %>% st_union()


# Abrir arquivo OSM  simplificado com viário de SP
open_file2 <- sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp)
vias_sp <- read_sf(open_file2)

# Checar se projeção dos dois arquivos é a mesma
st_crs(vias_sp)$input == st_crs(parques)$input & st_crs(vias_sp)$input == st_crs(campus_usp)$input


# Simplificar base de viário de sp
vias_sp <- vias_sp %>% select(-c(highway, infra_ciclo))


# Filtrar somente linhas de viário que intersecionam com o shape de parques - vamos
# transformar o shape de linha em pontos para usar o st_intersects(), que é rápido
vias_sp <- vias_sp %>% st_cast('POINT')
vias_em_parques <- vias_sp %>% filter(st_intersects(vias_sp, parques, sparse = FALSE))
vias_na_usp     <- vias_sp %>% filter(st_intersects(vias_sp, campus_usp, sparse = FALSE))


# Checar resultados
vias_em_parques %>% mapview()
vias_na_usp %>% mapview()


# Preparar listagens para exportar
vias_em_parques <- vias_em_parques %>% st_drop_geometry() %>% distinct()
vias_na_usp <- vias_na_usp %>% st_drop_geometry() %>% distinct()


# Salvar arquivos
write_delim(vias_em_parques, sprintf('%s/listagem_vias_em_parques.csv', pasta_osm_sp), delim = ';')
write_delim(vias_na_usp,     sprintf('%s/listagem_vias_campus_usp.csv', pasta_osm_sp), delim = ';')
