# Exporta pontos de semáforos e câmeras de fiscalizaçõ do arquivo .pbf OSM

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_atrib_viario <- sprintf("%s/04_atributos_viario", pasta_dados)

# Arquivo .pbf a ser utilizado
pasta_geral_tiles  <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/valhalla_tiles_sp"
pasta_valhalla_pbf <- sprintf("%s/pbf", pasta_geral_tiles)
osm_file_orig     <- sprintf('%s/20220216_sao_paulo_edited_20221223.osm.pbf', pasta_valhalla_pbf)


# ----------------------------------------------------------------------------
# Converter arquivo .pbf original em .opl para edições
# ----------------------------------------------------------------------------
# Nome do arquivo temporário
osm_file_tmp  <- sprintf('%s/lala.opl', pasta_atrib_viario)


# Converter arquivo .pbf para o formato .opl, que pode ser lido como texto
message('\nConvertendo arquivo .pbf em .opl com o osmium.\n')
osmium_path <- sprintf("/usr/bin/osmium")
arg_o1 <- sprintf('cat "%s"', osm_file_orig)
arg_o2 <- sprintf('--overwrite')
arg_o3 <- sprintf('-o "%s"', osm_file_tmp)
system2(command = osmium_path, args = c(arg_o1, arg_o2, arg_o3))

# Limpar ambiente
rm(arg_o1, arg_o2, arg_o3)


# ----------------------------------------------------------------------------
# Gerar arquivo osm base para edições
# ----------------------------------------------------------------------------

# Abrir arquivo .opl para edição, sem linha de header
osm <- read_delim(osm_file_tmp, delim = ' ', col_types = cols(.default = "c"), col_names = FALSE)

# Renomear colunas, para facilitar edição
osm <- osm %>% select(obj_type     = X1, 
                      # obj_version  = X2,
                      # is_visible   = X3,
                      # last_edit    = X4,
                      # time_last_edit = X5,
                      # last_user_id   = X6,
                      # last_user_name = X7,
                      tags = X8, 
                      lon  = X9, 
                      lat  = X10)

# # Criar coluna temporária de index, para merge posterior
# osm <- osm %>% add_column(index_col = 1:nrow(osm))

# # As tags com aspas duplas estão dando erro - remover as aspas
# osm <- osm %>% mutate(tags = str_replace_all(tags, '"', ''))


# ----------------------------------------------------------------------------
# Detectar cruzamentos semaforizados (marca blinkers e semáforos de pedestres)
# ----------------------------------------------------------------------------

# Selectionar somente os trechos de nodes (n), descartando vias (w - ways) e 
# relações (r) - https://osmcode.org/opl-file-format/#encoding
osm_nodes <- osm %>% filter(str_starts(obj_type, 'n'))


# Isolar localizações de faróis dentro do OSM
osm_signals <- 
  osm_nodes %>%
  # Isolar linhas com as tags de interesse
  filter(str_detect(tags, 'traffic_signals')) %>% 
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  # Converter as colunas de lon e lat para double
  mutate(lon = str_replace(lon, 'x', ''),
         lat = str_replace(lat, 'y', ''),
         lon = as.double(lon),
         lat = as.double(lat)) %>% 
  # Transformar em sf
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# mapview(osm_signals)


# Fazer marcações de relevância
osm_signals <- 
  osm_signals %>% 
  mutate(blinker    = ifelse(str_detect(tags, 'traffic_signals=blinker'), 'sim', 'não'),
         crossing   = ifelse(str_detect(tags, 'highway=crossing'), 'sim', 'não'),
         pedestrian = ifelse(str_detect(tags, 'barrier=kerb'), 'sim', 'não'))


# Salvar
out_file <- sprintf('%s/listagem_osm_semaforos.gpkg', pasta_atrib_viario)
st_write(osm_signals, out_file, driver = 'GPKG', append = FALSE)


# ----------------------------------------------------------------------------
# Detectar cameras de fiscalização
# ----------------------------------------------------------------------------

# Isolar localizações de faróis dentro do OSM
osm_cameras <- 
  osm_nodes %>%
  # Isolar linhas com as tags de interesse
  filter(str_detect(tags, 'speed_camera')) %>% 
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  # Converter as colunas de lon e lat para double
  mutate(lon = str_replace(lon, 'x', ''),
         lat = str_replace(lat, 'y', ''),
         lon = as.double(lon),
         lat = as.double(lat)) %>% 
  # Transformar em sf
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# mapview(osm_cameras)


# Salvar
out_file <- sprintf('%s/listagem_osm_cameras.gpkg', pasta_atrib_viario)
st_write(osm_cameras, out_file, driver = 'GPKG', append = FALSE)


# Remover arquivo temporário .opl
file.remove(osm_file_tmp)
