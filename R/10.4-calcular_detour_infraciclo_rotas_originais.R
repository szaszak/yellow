library('tidyverse')
library('tidylog')
# library('sf')
# library('mapview')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
# pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
# pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)


# Abrir resultados
resultados <- sprintf('%s/03_rotas_originais_infraciclo_detour_carac_viagens.csv', pasta_orig_vs_mod)
resultados <- read_delim(resultados, delim = ';', col_types = 'cccdddddccccdddddddd')
head(resultados)


# # ------------------------------------------------------------------------------
# # Por que alguns detours ficam menores do que 1? - trip_id <- '319758_00'
# # ------------------------------------------------------------------------------
# 
# # Ao calcular os fatores de detour, alguns ficam menores do que 1. O motivo é
# # que as colunas de lon.x, lat.x, lon.y e lat.y se referem aos centróides dos
# # osm_id de início (primeiro) e fim (último) das viagens originais, e não aos
# # pontos de latlong iniciais e finais. Com isso, alguns desses pontos ficam
# # mais distantes do que o próprio shapefile resultante da rota original. Um
# # ótimo exemplo acontece na trip_id '319758_00'.
# 
# 
# # Abrir shapefile .pbf do mapa de SP - rede cicloviária 2019
# sp <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_osm_sp)
# sp <- read_sf(sp)
# 
# 
# # Avaliar rotas originais para entender as grandes diferenças e detours abaixo de 1
# # Puxar listagem de viagens originais (latlon virá do map matching)
# pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
# pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
# pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
# mm_files1 <- list.files(pasta_mm_1, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
# mm_files2 <- list.files(pasta_mm_2, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
# mm_files3 <- list.files(pasta_mm_3, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
# mm_files <- rbind(mm_files1, mm_files2, mm_files3) %>% rename(arq = '.')
# rm(mm_files1, mm_files2, mm_files3, pasta_mm_1, pasta_mm_2, pasta_mm_3)
# 
# 
# 
# # Transforma dataframe com várias linhas de latlon em sf
# df_latlong_to_sf <- function(df, trip_id, st_type = 'LINESTRING'){
#   # df <- this
#   this <- df %>%
#     # Transformar em sf
#     st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#     # Transformar pontos em linha - ver possíveis erros em
#     # https://github.com/r-spatial/sf/issues/321
#     # # Modo 1 - Com st_coordinates, retorna matriz
#     # Retrieve coordinates in matrix form
#     # st_coordinates() %>%
#     # st_linestring()
#     # Modo 2 - Com summarize, retorna sf
#     # Aqui, o summarize pode ser qualquer coisa, o
#     # importante é o 'do_union=FALSE'
#     group_by(trip_id) %>%
#     summarize(m = n(), do_union = FALSE) %>%
#     select(-m) %>%
#     st_cast(st_type)
# 
#   return(this)
# }
# 
# ver_rota_original <- function(trip_id) {
#   # check_id <- sprintf('%s.csv', '319758_00')
#   check_id <- sprintf('%s.csv', trip_id)
#   check_trip <- mm_files %>% filter(str_detect(arq, check_id)) %>% pull()
#   check_trip <- read_delim(check_trip, delim = ';', col_types = "cidddddddicddcccddd")
#   check_trip <- check_trip %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
#   check_trip <- check_trip %>% distinct()
#   check_trip <- df_latlong_to_sf(check_trip, trip) %>% st_transform(31983)
#   # mapview(check_trip)
# }
# 
# 
# # Puxar pontos dos centróides de início e fim da viagem
# trip <- '319758_00'
# centroide_1 <- resultados %>% filter(trip_id == trip) %>% st_as_sf(coords = c('lon.x', 'lat.x'), crs = 4326)
# centroide_2 <- resultados %>% filter(trip_id == trip) %>% st_as_sf(coords = c('lon.y', 'lat.y'), crs = 4326)
# # Pegar traçado da rota original (map matching)
# rota_original <- ver_rota_original(trip)
# # Isolar osm_ids referentes aos centróides de início e fim
# sp <- sp %>% filter(osm_id == '542357553' | osm_id == '942659849')
# 
# mapview(centroide_1, layer.name = 'Centróide 1', col.regions = 'red', legend = FALSE) + 
#   mapview(centroide_2, layer.name = 'Centróide 2', col.regions = 'red', legend = FALSE) + 
#   mapview(sp, color = 'red', lwd = 4, layer.name = 'Viário OSM', legend = FALSE) +
#   mapview(rota_original, lwd = 5, layer.name = 'trip_id')


# ------------------------------------------------------------------------------
# Ver resultados - Uso de infra cicloviária
# ------------------------------------------------------------------------------

# Soma das colunas sem e com infraciclo
resultados <- resultados %>% mutate(ext_total = via_comum + infra_ciclo, .after = 'ciclofaixa')

# Extensão em infraestrutura cicloviária geral: 37.82
round(sum(resultados$infra_ciclo) / sum(resultados$ext_total) * 100, 2)

# Extensão em infraestrutura cicloviária - somente ciclofaixas: 3.86
round(sum(resultados$ciclofaixa) / sum(resultados$ext_total) * 100, 2)


# Calcular em viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
resultados_nao_cm <- resultados %>% filter(vg_contramao == 'não')

# Extensão em infraestrutura cicloviária geral: 48.5
round(sum(resultados_nao_cm$infra_ciclo) / sum(resultados_nao_cm$ext_total) * 100, 2)

# Extensão em infraestrutura cicloviária - somente ciclofaixas: 4.01
round(sum(resultados_nao_cm$ciclofaixa) / sum(resultados_nao_cm$ext_total) * 100, 2)


# # ------------------------------------------------------------------------------
# # Calcular detour
# # ------------------------------------------------------------------------------

detour <- summary(resultados$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm = Freq)
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary    detour_mm
# 1    Min.    0.9948781
# 2 1st Qu.    1.1285749
# 3  Median    1.2709509
# 4    Mean    2.6743009
# 5 3rd Qu.    1.4549206
# 6    Max. 5063.5374310


# Isolar coluna de interesse - detour_mapmatching
x <- resultados$detour_mm
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- resultados %>% filter(!(detour_mm < (qnt[1] - H) | detour_mm > (qnt[2] + H)))
rm(x, qnt, H)
# 
detour_sem_outliers <- summary(detour_sem_outliers$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm = Freq)
detour_sem_outliers
#   summary detour_mm
# 1    Min. 0.9948781
# 2 1st Qu. 1.1198449
# 3  Median 1.2529587
# 4    Mean 1.3028204
# 5 3rd Qu. 1.4053027
# 6    Max. 2.4335233


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
resultados_nao_cm <- resultados %>% filter(vg_contramao == 'não')
nrow(resultados_nao_cm)

detour <- summary(resultados_nao_cm$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm = Freq)
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary    detour_mm
# 1    Min.    0.9948781
# 2 1st Qu.    1.0675904
# 3  Median    1.2149744
# 4    Mean    3.1197564
# 5 3rd Qu.    1.4144062
# 6    Max. 2098.5727164


# Isolar coluna de interesse - detour_mapmatching
x <- resultados_nao_cm$detour_mm
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 3,836 rows (7%), 47,733 rows remaining
detour_ncm_sem_outliers <- resultados_nao_cm %>% filter(!(detour_mm < (qnt[1] - H) | detour_mm > (qnt[2] + H)))
rm(x, qnt, H)

nrow(detour_ncm_sem_outliers)
detour_ncm_sem_outliers <- summary(detour_ncm_sem_outliers$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm = Freq)
detour_ncm_sem_outliers
#   summary detour_mm
# 1    Min. 0.9948781
# 2 1st Qu. 1.0587454
# 3  Median 1.1901764
# 4    Mean 1.2528135
# 5 3rd Qu. 1.3601293
# 6    Max. 2.4536456