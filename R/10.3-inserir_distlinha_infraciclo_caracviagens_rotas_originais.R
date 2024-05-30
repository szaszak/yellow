library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('geosphere')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)


# ------------------------------------------------------------------------------
# Arquivos a serem utilizados
# ------------------------------------------------------------------------------

# Avaliar rotas originais para entender as grandes diferenças e detours abaixo de 1
# Puxar listagem de viagens originais (latlon virá do map matching)
pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
mm_files1 <- list.files(pasta_mm_1, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files2 <- list.files(pasta_mm_2, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files3 <- list.files(pasta_mm_3, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files <- rbind(mm_files1, mm_files2, mm_files3) %>% rename(arq = '.')
rm(mm_files1, mm_files2, mm_files3, pasta_mm_1, pasta_mm_2, pasta_mm_3)


# Puxar resultados de viagens, para características de cada uma - ex. com 
# contramão, com parques, com loop etc.
arq_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
arq_modelo <- read_delim(arq_modelo, delim = ';', col_types = "ccccdddddccddccdccdddccccccccccccc")
arq_modelo <- arq_modelo %>% select(trip_id, vg_contramao, vg_loop, vg_exper, vg_parques, dist_total) %>% distinct()
head(arq_modelo)


# Origens e destinos - rotas originais
ods_vgs <- sprintf('%s/01_origens_e_destinos_viagens_consideradas.csv', pasta_orig_vs_mod)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'ccc')
# Remover origens e destinos com o mesmo qgis_id
ods_vgs <- ods_vgs %>% filter(qgis_id != qgis_id_to)
ods_vgs <- ods_vgs %>% select(trip_id, qgis_id.x = qgis_id, qgis_id.y = qgis_id_to)


# Abrir resultados
resultados <- sprintf('%s/02_uso_de_estrutura_cicloviaria_rotas_originais.csv', pasta_orig_vs_mod)
resultados <- read_delim(resultados, delim = ';', col_types = 'cddddd')

# Remover viagens com origem e destino no mesmo qgis_id
# filter: removed 537 rows (<1%), 129,755 rows remaining
resultados <- resultados %>% filter(trip_id %in% ods_vgs$trip_id)
tail(resultados, 20)


# Juntar ao dataframe de resultados
resultados <- resultados %>% left_join(arq_modelo, by = 'trip_id')

# Calcular extensão total da rota
# resultados <- resultados %>% mutate(ext_total = via_comum + infra_ciclo, .after = 'ciclofaixa')
head(resultados)


# Calcular distância em linha reta entre os dois pontos de origem e destino
resultados <- resultados %>% left_join(ods_vgs, by = 'trip_id')

# Reordenar colunas
resultados <- resultados %>% select(trip_id, 
                                    qgis_id.x,
                                    qgis_id.y,
                                    via_comum,
                                    infra_ciclo,
                                    ciclo_expressa,
                                    ciclo_comum,
                                    ciclofaixa,
                                    vg_contramao,
                                    vg_loop,
                                    vg_exper,
                                    vg_parques,
                                    dist_total)



# ------------------------------------------------------------------------------
# Calcular distância da rota a partir da extensão da linha do shapefile
# ------------------------------------------------------------------------------

# Transforma dataframe com várias linhas de latlon em sf
df_latlong_to_sf <- function(df, trip_id, st_type = 'LINESTRING'){
  # df <- this
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
    st_cast(st_type)

  return(this)
}


# Retorna latlongs inicial e final, distância beeline entre esses dois pontos e 
# a extensão (m) do shapefile da rota original (map matching)
dados_rota_original <- function(trip_id) {
  # check_id <- sprintf('%s.csv', '319758_00')
  check_id <- sprintf('%s.csv', trip_id)
  check_trip <- mm_files %>% filter(str_detect(arq, check_id)) %>% pull()
  check_trip <- read_delim(check_trip, delim = ';', col_types = "cidddddddicddcccddd")
  check_trip <- check_trip %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
  check_trip <- check_trip %>% distinct()

  # Registrar primeiro e último pontos
  first_point <- check_trip %>% slice(1)
  last_point  <- check_trip %>% slice(nrow(check_trip))
  
  # Transformar em shapefile para medir a distância
  check_trip <- df_latlong_to_sf(check_trip, trip) %>% st_transform(31983)
  check_trip <- check_trip %>% mutate(dist = round(st_length(.), 4))
  
  
  # Agrupar tudo em um dataframe único de saída
  dados_out <- 
    left_join(first_point, last_point, by = 'trip_id') %>% 
    mutate(dist_linha = as.double(check_trip$dist), .after = 'trip_id') %>% 
    select(-trip_id)
  
  # Calcular beeline entre pontos de início e fim
  dados_out <- dados_out %>% mutate(dist_reta = distHaversine(cbind(lon.x, lat.x),
                                                              cbind(lon.y, lat.y)),
                                    .before = 'dist_linha')
  
  # Calcular fator de detour
  dados_out <- dados_out %>% mutate(detour_mm = dist_linha / dist_reta, 
                                    .after = 'dist_linha')

  
  }



# Gravar base única com todos os resultados
out_file <- sprintf('%s/03_rotas_originais_infraciclo_detour_carac_viagens.csv', pasta_orig_vs_mod)

# Se arquivo de saída existe, desconsiderar viagens que já foram roteadas
if (file.exists(out_file)) {
  this <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
  this <- this %>% select(trip_id) %>% distinct()
  
  # Remover viagens que já foram roteadas
  resultados <- resultados %>% filter(!trip_id %in% this$trip_id)
  rm(this)
  
}


# Para cada trip_id, inserir dados vindos do map matching (latlong de início e 
# fim, distâncias da linha da rota original e beeline entre os pontos de início 
# e fim)
detach("package:tidylog")
for (i in seq(1, nrow(resultados))) {
  # i <- 1
  
  # Isolar linha por linha
  line <- resultados %>% slice(i)
  # Agregar 
  line <- cbind(line, dados_rota_original(line$trip_id))
  
  
  # Guardar resultados
  if (file.exists(out_file)) {
    write_delim(line, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(line, out_file, delim = ';', append = FALSE)
  }

}


# # Ajustar resultados
# this <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
# this <- this %>% distinct()
# this <- this %>% arrange(trip_id)
# this <- this %>% select(trip_id,
#                         qgis_id.x,
#                         qgis_id.y,
#                         via_comum,
#                         infra_ciclo,
#                         ciclo_expressa,
#                         ciclo_comum,
#                         ciclofaixa,
#                         vg_contramao,
#                         vg_loop,
#                         vg_exper,
#                         vg_parques,
#                         dist_total,
#                         ext_total,
#                         dist_reta,
#                         dist_line,
#                         lon.x,
#                         lat.x,
#                         lon.y,
#                         lat.y)
# write_delim(this, out_file, delim = ';')
