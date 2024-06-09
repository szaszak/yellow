# Simplifica o arquivo .pbf com o viário da cidade de São Paulo para que possa
# ser aberto no QGIS para a conferência das vias com infraestrutura cicloviária
# e áreas de parques. São descartadas algumas colunas e tipos de 'highway'

# carregar bibliotecas
source('fun/setup.R')

# Nome do último arquivo .pbf modificado
# 20220216_sao_paulo_edited_20221223.osm.pbf

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
pasta_valhalla      <- "../../yellow_src/valhalla_tiles_sp/pbf"
pasta_osm_sp <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
dir.create(pasta_osm_sp, recursive = TRUE, showWarnings = FALSE)


# Arquivo de viário para o município pode ser muito grande e não ler 
# inteiro com o read_sf() abaixo. Em especial, isso vai acontecer com
# São Paulo - todos os arquivos de malha viária das cidades têm menos
# de 25 MB, enquanto o de SP tem quase 120 MB. Na dúvida, vamos ler
# todos esses arquivos como .gpkg para garantir que o viário seja lido
# em sua totalidade
# viario_muni <- read_sf(map_file, layer = 'lines') # não usar

# Ler arquivos de viário como .gpkg. Sobre este tema, ver:
# https://github.com/ropensci/osmextract/issues/12
read_from_gpkg <- function(path) {
  gpkg_file <- paste0(tempfile(), ".gpkg")
  gdal_utils(
    util = "vectortranslate",
    source = path, 
    destination = gpkg_file, 
    options = c("-f", "GPKG", "lines")
  )
  res <- st_read(gpkg_file, quiet = TRUE)
  names(res)[which(names(res) == "geom")] <- "geometry"
  st_geometry(res) <- "geometry"
  res
}

# Abrir arquivo .pbf do OSM para a cidade de São Paulo
# map_file <- sprintf("%s/20220216_sao_paulo.osm.pbf", pasta_valhalla)
map_file <- list.files(pasta_valhalla, pattern = "^\\d{8}_sao_paulo_edited_\\d{8}.osm.pbf")
map_file     <- sprintf('%s/%s', pasta_valhalla, map_file)
viario_muni <- read_from_gpkg(map_file)


# ----------------------------------------------------------
# Simplificar mapa OSM para SP
# ----------------------------------------------------------

# Retirar tudo o que não é viário de veículos terrestres
viario_muni <- viario_muni %>% filter(is.na(waterway) &
                                      is.na(aerialway) &
                                      is.na(barrier) &
                                      is.na(man_made))


# Tentar marcar vias que possuem infraestrutura cicloviária - essa informação
# pode aparecer na coluna 'other_tags' ou diretamente na coluna de highway
viario_muni <- viario_muni %>% mutate(infra_ciclo = str_detect(other_tags, '"bicycle"=>"designated"|cycleway'))
viario_muni <- viario_muni %>% mutate(infra_ciclo = case_when(highway == 'cycleway' ~ TRUE,
                                                              TRUE ~ infra_ciclo))

# Simplificar a base e descartar colunas desnecessárias
viario_muni <- viario_muni %>% select(-c(waterway, aerialway, barrier, man_made, z_order, other_tags))


# viario_muni %>% st_drop_geometry() %>% select(highway) %>% distinct()
# viario_muni %>% filter(highway == 'raceway') %>% mapview()


# Retirar esses tipos de estrutura - todas foram olhadas no mapa para checagem
no_use <- c("raceway", "proposed", "construction", "elevator", "bus_stop", 
            "platform", "emergency_bay", "crossing", "services")
viario_muni_out <- viario_muni %>% filter(!highway %in% no_use)
# Retirar também viários em que highway é nulo
viario_muni_out <- viario_muni_out %>% filter(!is.na(highway))

# Ordenar por osm_id para exportar
viario_muni_out <- viario_muni_out %>% arrange(osm_id)


# Exportar a base resultante
st_write(viario_muni_out, sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp), driver = 'GPKG', append = FALSE)
