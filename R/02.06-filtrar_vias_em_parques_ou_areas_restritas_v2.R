# Isola os ids e os nomes das vias que estão dentro das áreas de parque e no
# campus da USP e exporta como arquivos .csv

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
dados_originais  <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_osm_sp <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)

# Abrir shapefile com polígonos de parques
open_file1 <- sprintf('%s/20160825_WGS84_parques_municipais_geosampa_com_adicoes.gpkg', dados_originais)
areas_restritas <- read_sf(open_file1)

# Modificar camada de areas restritas de MULTIPOLYGON para POLYGON
areas_restritas <- 
  areas_restritas %>% 
  st_make_valid() %>% 
  # Garantir que formato vai estar em 'Polygon'
  st_cast('MULTIPOLYGON') %>% 
  st_cast("POLYGON") %>% 
  # Juntar todos os diferentes polígonos em um só
  st_union()

# areas_restritas %>% mapview()


# Abrir arquivo OSM  simplificado com viário de SP
open_file2 <- sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp)
vias_sp <- read_sf(open_file2)

# Simplificar base de viário de sp
vias_sp <- vias_sp %>% select(-c(highway, infra_ciclo))
# vias_sp %>% mapview()

# Checar se projeção dos dois arquivos é a mesma
st_crs(vias_sp)$input == st_crs(areas_restritas)$input


# Filtrar somente linhas de viário que intersecionam com o shape de areas_restritas - vamos
# transformar o shape de linha em pontos para usar o st_intersects(), que é rápido
vias_sp <- vias_sp %>% st_cast('POINT')
vias_restritas <- vias_sp %>% filter(st_intersects(vias_sp, areas_restritas, sparse = FALSE))


# Checar resultados
vias_restritas %>% mapview()


# Preparar listagens para exportar
vias_restritas <- vias_restritas %>% st_drop_geometry() %>% distinct()

# Salvar arquivos
write_delim(vias_restritas, sprintf('%s/listagem_vias_em_areas_restritas.csv', pasta_osm_sp), delim = ';')
