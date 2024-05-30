library('tidyverse')
library('tidylog')
library('geosphere')
library('sf')
library('mapview')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_orig_modalt  <- sprintf("%s/11_rotas_modeladas_com_alternativas", pasta_dados)


# Arquivo de resultados das rotas modeladas
resultados <- sprintf('%s/03_ttmatrix_rotas_modeladas_de_viagens_originais_com_carac_viagens.csv', pasta_orig_modalt)
resultados <- read_delim(resultados, delim = ';', col_types = 'ciccddddddiiddddddcccccdcdddd')
head(resultados)

# Onde temos NAs?
# data.frame(nas = sapply(resultados, function(y) sum(is.na(y)))) %>% filter(nas > 0)
#                 nas
# ext_linhas       47

# Temos NAs no cálculo de acurácia das linhas que faziam interseção com o buffer
# das rotas modeladas. Neste caso, todos os resultados de acurácia por pontos
# são zero, o que significa que todos esses NAs são zero também. Substituir:
resultados <- resultados %>% mutate(across(everything(), ~replace_na(.x, 0)))

# Quantas viagens temos ao final? 129.754
resultados %>% select(trip_id) %>% distinct() %>% nrow()


# # ------------------------------------------------------------------------------
# # Calcular fatores de detour
# # ------------------------------------------------------------------------------
# 
# # Calcular distância em linha reta entre os dois pontos de origem e destino
# resultados <- resultados %>% mutate(dist_reta = distVincentyEllipsoid(cbind(lon.x, lat.x), 
#                                                                       cbind(lon.y, lat.y)))
# 
# # Calcular fatores de detours de viagens reais (map matching) e viagens modeladas
# resultados <- resultados %>% mutate(detour_mm = dist_total / dist_reta, # map_matching
#                                     detour_gh = distance / dist_reta)    # graphhopper rotas modeladas
# 
# 
# det1 <- summary(resultados$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm = Freq)
# det2 <- summary(resultados$detour_gh) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_gh = Freq)
# detour <- left_join(det1, det2, by = 'summary')
# rm(det1, det2)
# # ATENÇÃO: Fatores de detour menores do que 1 são porque as distâncias calculadas
# # em linha reta são a partir dos centróides dos arcos de origem e destino 
# #   summary    detour_mm   detour_gh
# # 1    Min.    0.5362966   0.1806366
# # 2 1st Qu.    1.1886083   1.2400358
# # 3  Median    1.3427033   1.4008515
# # 4    Mean    2.2263526   1.5790442
# # 5 3rd Qu.    1.5527656   1.6511801
# # 6    Max. 2619.8539019 298.9796933
# 
# 
# # ------------------------------------------------------------------------------
# # Calcular diferências de distâncias entre rotas originais e modeladas
# # ------------------------------------------------------------------------------
# 
# # Temos rotas com distâncias muito diferentes entre as "reais" (map matching) e
# # as modeladas (menores custos)
# resultados <- resultados %>% mutate(dif_dist = dist_total - distance)
# summary(resultados$dif_dist) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, dif_dist = Freq)
# #   summary   dif_dist
# # 1    Min. -7720.1087
# # 2 1st Qu.  -275.4779
# # 3  Median   -39.3709
# # 4    Mean    69.0210
# # 5 3rd Qu.   122.3207
# # 6    Max. 28449.4063
# 
# # Retirar outliers extremos, segundo proposto por Favero e Belfiore (2021)
# # Alterado da implementação em:
# # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102
# 
# # Isolar coluna de interesse - detour_mapmatching
# x <- resultados$dif_dist
# # Pegar o primeiro e quarto quantis 
# qnt <- quantile(x, probs = c(0.25, 0.75))
# # Outliers extremos estão a 3 * IQR
# H <- 3 * IQR(x) 
# # Retirar outliers do dataframe
# # filter: removed 11,602 rows (6%), 184,378 rows remaining
# resultados_difdist <- resultados %>% filter(!(dif_dist < (qnt[1] - H) | dif_dist > (qnt[2] + H)))
# rm(x, qnt, H)
# 
# summary(resultados_difdist$dif_dist) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, dif_dist = Freq)
# #   summary    dif_dist
# # 1    Min. -1468.69027
# # 2 1st Qu.  -282.98932
# # 3  Median   -51.50707
# # 4    Mean   -75.53509
# # 5 3rd Qu.    82.68216
# # 6    Max.  1315.58321
# 
# det3 <- summary(resultados_difdist$detour_mm) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mm_dd = Freq)
# det4 <- summary(resultados_difdist$detour_gh) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_gh_dd = Freq)
# detour_difdist <- left_join(det3, det4, by = 'summary')
# rm(det3, det4)
# #   summary detour_mm_dd detour_gh_dd
# # 1    Min.    0.5362966    0.8495784
# # 2 1st Qu.    1.1812702    1.2392305
# # 3  Median    1.3276608    1.3992549
# # 4    Mean    1.4511606    1.5330429
# # 5 3rd Qu.    1.5084150    1.6400727
# # 6    Max.  357.2487140   44.9247971
# 
# 
# # ------------------------------------------------------------------------------
# # Avaliar rotas com distâncias muito discrepantes e detours abaixo de 1
# # ------------------------------------------------------------------------------
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
#   check_id <- sprintf('%s.csv', trip_id)
#   check_trip <- mm_files %>% filter(str_detect(arq, check_id)) %>% pull()
#   check_trip <- read_delim(check_trip, delim = ';', col_types = "cidddddddicddcccddd")
#   check_trip <- check_trip %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
#   check_trip <- check_trip %>% distinct()
#   check_trip <- df_latlong_to_sf(check_trip, trip) %>% st_transform(31983)
#   mapview(check_trip)
# }
# 
# trip <- '102996_00'
# ver_rota_original(trip)


# ------------------------------------------------------------------------------
# Ver resultados - menor tempo
# ------------------------------------------------------------------------------

# Definir quantis para visualizar resultados
quantis <- c(0.25, 0.3333, 0.5, 0.75, 0.85, 0.95, 0.99)
quantis2 <- c(0.25, 0.3333, 0.5, 0.6666, 0.75, 0.85, 0.95, 0.99)


# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
tempo <- resultados %>% group_by(trip_id) %>% filter(time == min(time)) %>% ungroup()


# Resultado geral por pontos: 60.89161 (modelo sem ajustar LTS: 58.89442)
sum(tempo$pts_intsct) / sum(tempo$pts_viagem) * 100
# Média de acurácia por pontos: 67.66305 (modelo sem ajustar LTS: 66.4416)
mean(tempo$acuracia_pontos)
# Mediana por pontos: 75 (modelo sem ajustar LTS: 73.07692)
median(tempo$acuracia_pontos)
# Quantis por pontos
quantile(tempo$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.08 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 40.58  52.20  75.00 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(tempo$acuracia_pontos)
# Histograma
hist(tempo$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das distâncias
hist(tempo$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_tempo_pontos <- 
  data.frame(tempo_geral = quantile(tempo$acuracia_pontos, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')# %>% 
# pivot_longer(tempo_nao_cm, names_to = "perfil_rotas") %>%
# pivot_wider(id_cols = perfil_rotas, names_from = quantiles)


# Média de acurácia  por extensão de linhas: 63.76383
mean(tempo$acuracia_linhas)
# Mediana  por extensão de linhas: 69.35618
median(tempo$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#    25% 33.33%    50%    75%    85%    95%    99% 
# 37.17  48.72  69.36  93.86  98.98 100.00 100.00
# Boxplot
boxplot(tempo$acuracia_linhas)
# Histograma
hist(tempo$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_tempo_linhas <- 
  data.frame(tempo_geral = quantile(tempo$acuracia_linhas, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem loop -----------------------------------------

# Resultados para viagens sem loop
# filter: removed 776 rows (1%), 128,978 rows remaining
tempo_nao_loop <- tempo %>% filter(vg_loop == 'não')
head(tempo_nao_loop)

# Resultado geral por pontos: 61.41476 (modelo sem ajustar LTS: 59.39098)
sum(tempo_nao_loop$pts_intsct) / sum(tempo_nao_loop$pts_viagem) * 100
# Média de acurácia por pontos: 67.87872 (modelo sem ajustar LTS: 66.65127)
mean(tempo_nao_loop$acuracia_pontos)
# Mediana por pontos: 75.55556 (modelo sem ajustar LTS: 73.4375)
median(tempo_nao_loop$acuracia_pontos)
# Quantis
quantile(tempo_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 40.96  52.63  75.56 100.00 100.00 100.00 100.00
# Boxplot
boxplot(tempo_nao_loop$acuracia_pontos)
# Histograma
hist(tempo_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# Histograma das distâncias
hist(tempo_nao_loop$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_loop_pontos <- 
  data.frame(tempo_sem_loop = quantile(tempo_nao_loop$acuracia_pontos, 
                                       probs = quantis2, 
                                       na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.6334
mean(tempo_nao_loop$acuracia_linhas)
# Mediana  por extensão de linhas: 69.13994
median(tempo_nao_loop$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#    25% 33.33%    50%    75%    85%    95%    99% 
# 37.03  48.56  69.14  93.68  98.85 100.00 100.00 
# Boxplot
boxplot(tempo_nao_loop$acuracia_linhas)
# Histograma
hist(tempo_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_loop_linhas <- 
  data.frame(tempo_sem_loop = quantile(tempo_nao_loop$acuracia_linhas, 
                                       probs = quantis2, 
                                       na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
tempo_nao_cm <- tempo %>% filter(vg_contramao == 'não')
head(tempo_nao_cm)

# Resultado geral por pontos: 72.24712 (modelo sem ajustar LTS: 70.23178)
sum(tempo_nao_cm$pts_intsct) / sum(tempo_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 80.657 (modelo sem ajustar LTS: 79.39418)
mean(tempo_nao_cm$acuracia_pontos)
# Mediana por pontos: 98.6348 (modelo sem ajustar LTS: 97.5)
median(tempo_nao_cm$acuracia_pontos)
# Quantis
quantile(tempo_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 62.50  79.45  97.50 100.00 100.00 100.00 100.00 
# Modelo ajustando o LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 66.37  82.50  98.63 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(tempo_nao_cm$acuracia_pontos)
# Histograma
hist(tempo_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(tempo_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_cm_pontos <- 
  data.frame(tempo_sem_cm = quantile(tempo_nao_cm$acuracia_pontos, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 79.86773
mean(tempo_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 92.10369
median(tempo_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#    25% 33.33%    50%    75%    85%    95%    99% 
# 67.75  78.84  92.10  99.61 100.00 100.00 100.00 
# Boxplot
boxplot(tempo_nao_cm$acuracia_linhas)
# Histograma
hist(tempo_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_cm_linhas <- 
  data.frame(tempo_sem_cm = quantile(tempo_nao_cm$acuracia_linhas, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas') 


# -------------------------- Com contramão -------------------------------------

# Resultados para viagens com contramão
# filter: removed 51,568 rows (40%), 78,186 rows remaining
tempo_sim_cm <- tempo %>% filter(vg_contramao == 'sim')
head(tempo_sim_cm)

# Resultado geral por pontos: 54.68525 (modelo sem ajustar LTS: 52.69799)
sum(tempo_sim_cm$pts_intsct) / sum(tempo_sim_cm$pts_viagem) * 100
# Média de acurácia por pontos: 59.09282 (modelo sem ajustar LTS: 57.89866)
mean(tempo_sim_cm$acuracia_pontos)
# Mediana por pontos: 60 (modelo sem ajustar LTS: 58.13953)
median(tempo_sim_cm$acuracia_pontos)
# Quantis
quantile(tempo_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
# 25% 33.33%    50%    75%    85%    95%    99% 
# 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# Modelo ajustando o LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 33.33  41.86  60.00  87.10  96.25 100.00 100.00 
# Boxplot
boxplot(tempo_sim_cm$acuracia_pontos)
# Histograma
hist(tempo_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(tempo_sim_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_tempo_com_cm_pontos <- 
  data.frame(tempo_com_cm = quantile(tempo_sim_cm$acuracia_pontos, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 53.14242
mean(tempo_sim_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 53.27057
median(tempo_sim_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 27.04  35.75  53.27  78.68  89.33 100.00 100.00 
# Boxplot
boxplot(tempo_sim_cm$acuracia_linhas)
# Histograma
hist(tempo_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_tempo_com_cm_linhas <- 
  data.frame(tempo_com_cm = quantile(tempo_sim_cm$acuracia_linhas, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas') # %>% 
# pivot_longer(tempo_sim_cm, names_to = "perfil_rotas") %>%
# pivot_wider(id_cols = perfil_rotas, names_from = quantiles)



#------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------

# Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# filter: removed 81,662 rows (63%), 48,092 rows remaining
tempo_nao_cm_lp_pq <- tempo %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
head(tempo_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)

# Resultado geral por pontos: 76.90294
sum(tempo_nao_cm_lp_pq$pts_intsct) / sum(tempo_nao_cm_lp_pq$pts_viagem) * 100
# Média de acurácia por pontos: 83.33861
mean(tempo_nao_cm_lp_pq$acuracia_pontos)
# Mediana por pontos: 100
median(tempo_nao_cm_lp_pq$acuracia_pontos)
# Quantis
quantile(tempo_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo ajustando o LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 73.44  87.34 100.00 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(tempo_nao_cm_lp_pq$acuracia_pontos)
# Histograma
hist(tempo_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# Histograma das distâncias
hist(tempo_nao_cm_lp_pq$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_cm_lp_pq_pontos <- 
  data.frame(tempo_sem_cm_lp_pq = quantile(tempo_nao_cm_lp_pq$acuracia_pontos, 
                                           probs = quantis2, 
                                           na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.26486
mean(tempo_nao_cm_lp_pq$acuracia_linhas)
# Mediana  por extensão de linhas: 92.03825
median(tempo_nao_cm_lp_pq$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 68.88  79.38  92.04  99.32 100.00 100.00 100.00 
# Boxplot
boxplot(tempo_nao_cm_lp_pq$acuracia_linhas)
# Histograma
hist(tempo_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_tempo_sem_cm_lp_pq_linhas <- 
  data.frame(tempo_sem_cm_lp_pq = quantile(tempo_nao_cm_lp_pq$acuracia_linhas, 
                                           probs = quantis2, 
                                           na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


#----------------------------- Resultados finais  ------------------------------

menor_tempo_pontos %>% 
  left_join(menor_tempo_sem_loop_pontos,     by = 'quantiles_ptos') %>%
  left_join(menor_tempo_sem_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_tempo_com_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_tempo_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
#   quantiles_ptos tempo_geral tempo_sem_loop tempo_sem_cm tempo_com_cm tempo_sem_cm_lp_pq
# 1            25%    40.57971       40.96386     66.36669     33.33333           73.43750
# 2         33.33%    52.19802       52.63158     82.50000     41.86047           87.34177
# 3            50%    75.00000       75.55556     98.63480     60.00000          100.00000
# 4         66.66%    94.00000       94.11765    100.00000     78.48913          100.00000
# 5            75%   100.00000      100.00000    100.00000     87.09677          100.00000
# 6            85%   100.00000      100.00000    100.00000     96.25000          100.00000
# 7            95%   100.00000      100.00000    100.00000    100.00000          100.00000
# 8            99%   100.00000      100.00000    100.00000    100.00000          100.00000

menor_tempo_linhas %>% 
  left_join(menor_tempo_sem_loop_linhas,     by = 'quantiles_linhas') %>%
  left_join(menor_tempo_sem_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_tempo_com_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_tempo_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
#   quantiles_linhas tempo_geral tempo_sem_loop tempo_sem_cm tempo_com_cm tempo_sem_cm_lp_pq
# 1              25%    37.16614       37.03287     67.75137     27.03954           68.87950
# 2           33.33%    48.72080       48.56424     78.84185     35.74535           79.37975
# 3              50%    69.35618       69.13994     92.10369     53.27057           92.03825
# 4           66.66%    87.10410       86.88825     97.74499     70.27222           97.54054
# 5              75%    93.85557       93.67638     99.60741     78.67937           99.31612
# 6              85%    98.97951       98.84766    100.00000     89.33308          100.00000
# 7              95%   100.00000      100.00000    100.00000    100.00000          100.00000
# 8              99%   100.00000      100.00000    100.00000    100.00000          100.00000



# ------------------------------------------------------------------------------
# Ver resultados - menor distância
# ------------------------------------------------------------------------------

# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
dist <- resultados %>% group_by(trip_id) %>% filter(distance == min(distance)) %>% ungroup()


# Resultado geral por pontos: 60.14606
sum(dist$pts_intsct) / sum(dist$pts_viagem) * 100
# Média de acurácia por pontos: 67.22149
mean(dist$acuracia_pontos)
# Mediana por pontos: 74.35897
median(dist$acuracia_pontos)
# Quantis por pontos
quantile(dist$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.08 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 39.73  51.33  74.36 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(dist$acuracia_pontos)
# Histograma
hist(dist$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das distâncias
hist(dist$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_dist_pontos <- 
  data.frame(dist_geral = quantile(dist$acuracia_pontos, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.34116
mean(dist$acuracia_linhas)
# Mediana  por extensão de linhas: 68.60054
median(dist$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 36.39  47.87  68.60  93.74  98.98 100.00 100.00
# Boxplot
boxplot(dist$acuracia_linhas)
# Histograma
hist(dist$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_dist_linhas <- 
  data.frame(dist_geral = quantile(dist$acuracia_linhas, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem loop -----------------------------------------

# Resultados para viagens sem loop
# filter: removed 776 rows (1%), 128,978 rows remaining
dist_nao_loop <- dist %>% filter(vg_loop == 'não')
head(dist_nao_loop)

# Resultado geral por pontos: 60.65751
sum(dist_nao_loop$pts_intsct) / sum(dist_nao_loop$pts_viagem) * 100
# Média de acurácia por pontos: 67.43494
mean(dist_nao_loop$acuracia_pontos)
# Mediana por pontos: 74.80157
median(dist_nao_loop$acuracia_pontos)
# Quantis
quantile(dist_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 40.00  51.72  74.80 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(dist_nao_loop$acuracia_pontos)
# Histograma
hist(dist_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# Histograma das distâncias
hist(dist_nao_loop$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_loop_pontos <- 
  data.frame(dist_sem_loop = quantile(dist_nao_loop$acuracia_pontos, 
                                       probs = quantis2, 
                                       na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.20913
mean(dist_nao_loop$acuracia_linhas)
# Mediana  por extensão de linhas: 68.37213
median(dist_nao_loop$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 36.25  47.70  68.37  93.55  98.84 100.00 100.00 
# Boxplot
boxplot(dist_nao_loop$acuracia_linhas)
# Histograma
hist(dist_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist_nao_loop$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_loop_linhas <- 
  data.frame(dist_sem_loop = quantile(dist_nao_loop$acuracia_linhas, 
                                       probs = quantis2, 
                                       na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
dist_nao_cm <- dist %>% filter(vg_contramao == 'não')
head(dist_nao_cm)

# Resultado geral por pontos: 71.39719
sum(dist_nao_cm$pts_intsct) / sum(dist_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 80.17678
mean(dist_nao_cm$acuracia_pontos)
# Mediana por pontos: 98.36732
median(dist_nao_cm$acuracia_pontos)
# Quantis
quantile(dist_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 62.50  79.45  97.50 100.00 100.00 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 64.80  81.40  98.37 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(dist_nao_cm$acuracia_pontos)
# Histograma
hist(dist_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(dist_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_cm_pontos <- 
  data.frame(dist_sem_cm = quantile(dist_nao_cm$acuracia_pontos, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 79.42633
mean(dist_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 91.95001
median(dist_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 66.61  78.13  91.95  99.63 100.00 100.00 100.00
# Boxplot
boxplot(dist_nao_cm$acuracia_linhas)
# Histograma
hist(dist_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_cm_linhas <- 
  data.frame(dist_sem_cm = quantile(dist_nao_cm$acuracia_linhas, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Com contramão -------------------------------------

# Resultados para viagens com contramão
# filter: removed 51,568 rows (40%), 78,186 rows remaining
dist_sim_cm <- dist %>% filter(vg_contramao == 'sim')
head(dist_sim_cm)

# Resultado geral por pontos: 53.99676
sum(dist_sim_cm$pts_intsct) / sum(dist_sim_cm$pts_viagem) * 100
# Média de acurácia por pontos: 58.67675
mean(dist_sim_cm$acuracia_pontos)
# Mediana por pontos: 59.32203
median(dist_sim_cm$acuracia_pontos)
# Quantis
quantile(dist_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
# 25% 33.33%    50%    75%    85%    95%    99% 
# 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 32.61  41.18  59.32  86.81  96.10 100.00 100.00 
# Boxplot
boxplot(dist_sim_cm$acuracia_pontos)
# Histograma
hist(dist_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(dist_sim_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_dist_com_cm_pontos <- 
  data.frame(dist_com_cm = quantile(dist_sim_cm$acuracia_pontos, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 52.73209
mean(dist_sim_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 52.62296
median(dist_sim_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 26.58  35.21  52.62  78.25  89.11 100.00 100.00 
# Boxplot
boxplot(dist_sim_cm$acuracia_linhas)
# Histograma
hist(dist_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist_sim_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_dist_com_cm_linhas <- 
  data.frame(dist_com_cm = quantile(dist_sim_cm$acuracia_linhas, 
                                     probs = quantis2, 
                                     na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas') # %>% 
# pivot_longer(dist_sim_cm, names_to = "perfil_rotas") %>%
# pivot_wider(id_cols = perfil_rotas, names_from = quantiles)



#------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------

# Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# filter: removed 81,681 rows (63%), 48,073 rows remaining
dist_nao_cm_lp_pq <- dist %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
head(dist_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)

# Resultado geral por pontos: 75.94904
sum(dist_nao_cm_lp_pq$pts_intsct) / sum(dist_nao_cm_lp_pq$pts_viagem) * 100
# Média de acurácia por pontos: 82.81114
mean(dist_nao_cm_lp_pq$acuracia_pontos)
# Mediana por pontos: 100
median(dist_nao_cm_lp_pq$acuracia_pontos)
# Quantis
quantile(dist_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 71.88  86.36 100.00 100.00 100.00 100.00 100.00
# Boxplot
boxplot(dist_nao_cm_lp_pq$acuracia_pontos)
# Histograma
hist(dist_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# Histograma das distâncias
hist(dist_nao_cm_lp_pq$dist_total, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_cm_lp_pq_pontos <- 
  data.frame(dist_sem_cm_lp_pq = quantile(dist_nao_cm_lp_pq$acuracia_pontos, 
                                           probs = quantis2, 
                                           na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 79.76859
mean(dist_nao_cm_lp_pq$acuracia_linhas)
# Mediana  por extensão de linhas: 91.88069
median(dist_nao_cm_lp_pq$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.68  78.63  91.88  99.33 100.00 100.00 100.00 
# Boxplot
boxplot(dist_nao_cm_lp_pq$acuracia_linhas)
# Histograma
hist(dist_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_dist_sem_cm_lp_pq_linhas <- 
  data.frame(dist_sem_cm_lp_pq = quantile(dist_nao_cm_lp_pq$acuracia_linhas, 
                                           probs = quantis2, 
                                           na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


#----------------------------- Resultados finais  ------------------------------

menor_dist_pontos %>% 
  left_join(menor_dist_sem_loop_pontos,     by = 'quantiles_ptos') %>%
  left_join(menor_dist_sem_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_dist_com_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_dist_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
#   quantiles_ptos dist_geral dist_sem_loop dist_sem_cm dist_com_cm dist_sem_cm_lp_pq
# 1            25%   39.72603      40.00000    64.80000    32.60870          71.87500
# 2         33.33%   51.32743      51.72414    81.39535    41.17647          86.36364
# 3            50%   74.35897      74.80157    98.36732    59.32203         100.00000
# 4         66.66%   93.75000      93.91304   100.00000    78.04878         100.00000
# 5            75%  100.00000     100.00000   100.00000    86.81319         100.00000
# 6            85%  100.00000     100.00000   100.00000    96.10390         100.00000
# 7            95%  100.00000     100.00000   100.00000   100.00000         100.00000
# 8            99%  100.00000     100.00000   100.00000   100.00000         100.00000

menor_dist_linhas %>% 
  left_join(menor_dist_sem_loop_linhas,     by = 'quantiles_linhas') %>%
  left_join(menor_dist_sem_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_dist_com_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_dist_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
#   quantiles_linhas dist_geral dist_sem_loop dist_sem_cm dist_com_cm dist_sem_cm_lp_pq
# 1              25%   36.38620      36.25173    66.61286    26.58158          67.68214
# 2           33.33%   47.86979      47.70430    78.13253    35.20755          78.63329
# 3              50%   68.60054      68.37213    91.95001    52.62296          91.88069
# 4           66.66%   86.79114      86.54402    97.75353    69.71199          97.53787
# 5              75%   93.74398      93.54910    99.62973    78.24867          99.33078
# 6              85%   98.97735      98.84237   100.00000    89.10774         100.00000
# 7              95%  100.00000     100.00000   100.00000   100.00000         100.00000
# 8              99%  100.00000     100.00000   100.00000   100.00000         100.00000



# ------------------------------------------------------------------------------
# Ver resultados - maior uso de infra cicloviária
# ------------------------------------------------------------------------------

# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
infraciclo <- resultados %>% group_by(trip_id) %>% filter(infra_ciclo == max(infra_ciclo)) %>% ungroup()
nrow(infraciclo)
# Pelo visto, há resultados em que a extensão de uso de ciclovias é o mesmo em 
# mais de uma alternativa. Pegar então a de menor custo dentro desses resultados
infraciclo <- infraciclo %>% group_by(trip_id) %>% filter(weight == min(weight)) %>% ungroup()
nrow(infraciclo)


# Resultado geral por pontos: 61.57158
sum(infraciclo$pts_intsct) / sum(infraciclo$pts_viagem) * 100
# Média de acurácia por pontos: 68.06905
mean(infraciclo$acuracia_pontos)
# Mediana por pontos: 75.86207
median(infraciclo$acuracia_pontos)
# Quantis por pontos
quantile(infraciclo$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.08 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 41.51  53.06  75.86 100.00 100.00 100.00 100.00
# Boxplot
boxplot(infraciclo$acuracia_pontos)
# Histograma
hist(infraciclo$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das infracicloâncias
hist(infraciclo$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_pontos <- 
  data.frame(infraciclo_geral = quantile(infraciclo$acuracia_pontos, 
                                   probs = quantis2, 
                                   na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.91268
mean(infraciclo$acuracia_linhas)
# Mediana  por extensão de linhas: 69.69617
median(infraciclo$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.52  49.22  69.70  93.79  98.92 100.00 100.00
# Boxplot
boxplot(infraciclo$acuracia_linhas)
# Histograma
hist(infraciclo$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das infracicloâncias
hist(infraciclo$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_linhas <- 
  data.frame(infraciclo_geral = quantile(infraciclo$acuracia_linhas, 
                                   probs = quantis2, 
                                   na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem loop -----------------------------------------

# Resultados para viagens sem loop
# ilter: removed 776 rows (1%), 128,978 rows remaining
infraciclo_nao_loop <- infraciclo %>% filter(vg_loop == 'não')
head(infraciclo_nao_loop)

# Resultado geral por pontos: 62.09994
sum(infraciclo_nao_loop$pts_intsct) / sum(infraciclo_nao_loop$pts_viagem) * 100
# Média de acurácia por pontos: 68.28531
mean(infraciclo_nao_loop$acuracia_pontos)
# Mediana por pontos: 76.14679
median(infraciclo_nao_loop$acuracia_pontos)
# Quantis
quantile(infraciclo_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 41.94  53.49  76.15 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(infraciclo_nao_loop$acuracia_pontos)
# Histograma
hist(infraciclo_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# Histograma das distâncias
hist(infraciclo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_loop_pontos <- 
  data.frame(infraciclo_sem_loop = quantile(infraciclo_nao_loop$acuracia_pontos, 
                                      probs = quantis2, 
                                      na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.78259
mean(infraciclo_nao_loop$acuracia_linhas)
# Mediana  por extensão de linhas: 69.48258
median(infraciclo_nao_loop$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.40  49.03  69.48  93.60  98.78 100.00 100.00
# Boxplot
boxplot(infraciclo_nao_loop$acuracia_linhas)
# Histograma
hist(infraciclo_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# Histograma das infracicloâncias
hist(infraciclo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_loop_linhas <- 
  data.frame(infraciclo_sem_loop = quantile(infraciclo_nao_loop$acuracia_linhas, 
                                      probs = quantis2, 
                                      na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
infraciclo_nao_cm <- infraciclo %>% filter(vg_contramao == 'não')
head(infraciclo_nao_cm)

# Resultado geral por pontos: 73.08652
sum(infraciclo_nao_cm$pts_intsct) / sum(infraciclo_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 81.12517
mean(infraciclo_nao_cm$acuracia_pontos)
# Mediana por pontos: 98.79518
median(infraciclo_nao_cm$acuracia_pontos)
# Quantis
quantile(infraciclo_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 62.50  79.45  97.50 100.00 100.00 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.74  83.33  98.80 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(infraciclo_nao_cm$acuracia_pontos)
# Histograma
hist(infraciclo_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(infraciclo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_cm_pontos <- 
  data.frame(infraciclo_sem_cm = quantile(infraciclo_nao_cm$acuracia_pontos, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.11369
mean(infraciclo_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 92.05211
median(infraciclo_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 68.40  79.16  92.05  99.56 100.00 100.00 100.00
# Boxplot
boxplot(infraciclo_nao_cm$acuracia_linhas)
# Histograma
hist(infraciclo_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(infraciclo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_cm_linhas <- 
  data.frame(infraciclo_sem_cm = quantile(infraciclo_nao_cm$acuracia_linhas, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Com contramão -------------------------------------

# Resultados para viagens com contramão
# filter: removed 51,568 rows (40%), 78,186 rows remaining
infraciclo_sim_cm <- infraciclo %>% filter(vg_contramao == 'sim')
head(infraciclo_sim_cm)

# Resultado geral por pontos: 55.2781
sum(infraciclo_sim_cm$pts_intsct) / sum(infraciclo_sim_cm$pts_viagem) * 100
# Média de acurácia por pontos: 59.45782
mean(infraciclo_sim_cm$acuracia_pontos)
# Mediana por pontos: 60.71429
median(infraciclo_sim_cm$acuracia_pontos)
# Quantis
quantile(infraciclo_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
# 25% 33.33%    50%    75%    85%    95%    99% 
# 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 33.82  42.59  60.71  87.26  96.20 100.00 100.00
# Boxplot
boxplot(infraciclo_sim_cm$acuracia_pontos)
# Histograma
hist(infraciclo_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(infraciclo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_com_cm_pontos <- 
  data.frame(infraciclo_com_cm = quantile(infraciclo_sim_cm$acuracia_pontos, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 53.22722
mean(infraciclo_sim_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 53.47679
median(infraciclo_sim_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 27.14  35.95  53.48  78.77  89.22 100.00 100.00 
# Boxplot
boxplot(infraciclo_sim_cm$acuracia_linhas)
# Histograma
hist(infraciclo_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(infraciclo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_com_cm_linhas <- 
  data.frame(infraciclo_com_cm = quantile(infraciclo_sim_cm$acuracia_linhas, 
                                    probs = quantis2, 
                                    na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas') # %>% 
# pivot_longer(infraciclo_sim_cm, names_to = "perfil_rotas") %>%
# pivot_wider(id_cols = perfil_rotas, names_from = quantiles)



#------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------

# Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# filter: removed 81,664 rows (63%), 48,090 rows remaining
infraciclo_nao_cm_lp_pq <- infraciclo %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
head(infraciclo_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)

# Resultado geral por pontos: 77.80061
sum(infraciclo_nao_cm_lp_pq$pts_intsct) / sum(infraciclo_nao_cm_lp_pq$pts_viagem) * 100
# Média de acurácia por pontos: 83.80915
mean(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# Mediana por pontos: 100
median(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# Quantis
quantile(infraciclo_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 74.65  87.88 100.00 100.00 100.00 100.00 100.00
# Boxplot
boxplot(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# Histograma
hist(infraciclo_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# Histograma das distâncias
hist(infraciclo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_cm_lp_pq_pontos <- 
  data.frame(infraciclo_sem_cm_lp_pq = quantile(infraciclo_nao_cm_lp_pq$acuracia_pontos, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.50033
mean(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# Mediana  por extensão de linhas: 91.97572
median(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 69.51  79.63  91.98  99.26 100.00 100.00 100.00
# Boxplot
boxplot(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# Histograma
hist(infraciclo_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(infraciclo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
maior_infraciclo_sem_cm_lp_pq_linhas <- 
  data.frame(infraciclo_sem_cm_lp_pq = quantile(infraciclo_nao_cm_lp_pq$acuracia_linhas, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


#----------------------------- Resultados finais  ------------------------------

maior_infraciclo_pontos %>% 
  left_join(maior_infraciclo_sem_loop_pontos,     by = 'quantiles_ptos') %>%
  left_join(maior_infraciclo_sem_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(maior_infraciclo_com_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(maior_infraciclo_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
#   quantiles_ptos infraciclo_geral infraciclo_sem_loop infraciclo_sem_cm infraciclo_com_cm infraciclo_sem_cm_lp_pq
# 1            25%         37.93103            38.29787          60.71429          31.66667                68.88889
# 2         33.33%         48.78049            49.18033          77.27273          40.00000                83.63636
# 3            50%         71.11111            71.42857          97.05882          57.35294                99.00990
# 4         66.66%         91.54930            91.66667         100.00000          76.00000               100.00000
# 5            75%         98.64865            98.79518         100.00000          85.00000               100.00000
# 6            85%        100.00000           100.00000         100.00000          95.00000               100.00000
# 7            95%        100.00000           100.00000         100.00000         100.00000               100.00000
# 8            99%        100.00000           100.00000         100.00000         100.00000               100.00000

maior_infraciclo_linhas %>% 
  left_join(maior_infraciclo_sem_loop_linhas,     by = 'quantiles_linhas') %>%
  left_join(maior_infraciclo_sem_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(maior_infraciclo_com_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(maior_infraciclo_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
#   quantiles_linhas infraciclo_geral infraciclo_sem_loop infraciclo_sem_cm infraciclo_com_cm infraciclo_sem_cm_lp_pq
# 1              25%         33.39980            33.26669          63.45275          24.61019                64.87885
# 2           33.33%         44.20673            44.03210          75.55511          32.83474                76.33764
# 3              50%         65.19329            64.98904          90.82545          49.88571                90.84034
# 4           66.66%         84.42549            84.16653          97.30397          67.06924                97.09146
# 5              75%         92.22889            91.98819          99.31136          76.09649                99.00952
# 6              85%         98.37302            98.20655         100.00000          87.40098               100.00000
# 7              95%        100.00000           100.00000         100.00000         100.00000               100.00000
# 8              99%        100.00000           100.00000         100.00000         100.00000               100.00000



# ------------------------------------------------------------------------------
# Ver resultados - menor custo da rota
# ------------------------------------------------------------------------------

# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
peso <- resultados %>% group_by(trip_id) %>% filter(weight == min(weight))  %>% ungroup()
# peso %>% select(alt) %>% distinct()


# Resultado geral por pontos: 60.91341
sum(peso$pts_intsct) / sum(peso$pts_viagem) * 100
# Média de acurácia por pontos: 67.67853
mean(peso$acuracia_pontos)
# Mediana por pontos: 75
median(peso$acuracia_pontos)
# Quantis por pontos
quantile(peso$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.08 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 40.62  52.25  75.00 100.00 100.00 100.00 100.00
# Boxplot
boxplot(peso$acuracia_pontos)
# Histograma
hist(peso$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das pesoâncias
hist(peso$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_peso_pontos <- 
  data.frame(peso_geral = quantile(peso$acuracia_pontos, 
                                         probs = quantis2, 
                                         na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.77826
mean(peso$acuracia_linhas)
# Mediana  por extensão de linhas: 69.38161
median(peso$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.17  48.73  69.38  93.87  98.99 100.00 100.00
# Boxplot
boxplot(peso$acuracia_linhas)
# Histograma
hist(peso$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das pesoâncias
hist(peso$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_peso_linhas <- 
  data.frame(peso_geral = quantile(peso$acuracia_linhas, 
                                         probs = quantis2, 
                                         na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem loop -----------------------------------------

# Resultados para viagens sem loop
# filter: removed 776 rows (1%), 128,978 rows remaining
peso_nao_loop <- peso %>% filter(vg_loop == 'não')
head(peso_nao_loop)

# Resultado geral por pontos: 61.43477
sum(peso_nao_loop$pts_intsct) / sum(peso_nao_loop$pts_viagem) * 100
# Média de acurácia por pontos: 67.89371
mean(peso_nao_loop$acuracia_pontos)
# Mediana por pontos: 75.60976
median(peso_nao_loop$acuracia_pontos)
# Quantis
quantile(peso_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 41.00  52.63  75.61 100.00 100.00 100.00 100.00
# Boxplot
boxplot(peso_nao_loop$acuracia_pontos)
# Histograma
hist(peso_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# Histograma das distâncias
hist(peso_nao_loop$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_loop_pontos <- 
  data.frame(peso_sem_loop = quantile(peso_nao_loop$acuracia_pontos, 
                                            probs = quantis2, 
                                            na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.64785
mean(peso_nao_loop$acuracia_linhas)
# Mediana  por extensão de linhas: 69.16791
median(peso_nao_loop$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.04  48.57  69.17  93.70  98.86 100.00 100.00
# Boxplot
boxplot(peso_nao_loop$acuracia_linhas)
# Histograma
hist(peso_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# Histograma das pesoâncias
hist(peso_nao_loop$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_loop_linhas <- 
  data.frame(peso_sem_loop = quantile(peso_nao_loop$acuracia_linhas, 
                                            probs = quantis2, 
                                            na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
peso_nao_cm <- peso %>% filter(vg_contramao == 'não')
head(peso_nao_cm)

# Resultado geral por pontos: 72.29892
sum(peso_nao_cm$pts_intsct) / sum(peso_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 80.69979
mean(peso_nao_cm$acuracia_pontos)
# Mediana por pontos: 98.66667
median(peso_nao_cm$acuracia_pontos)
# Quantis
quantile(peso_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#    25% 33.33%    50%    75%    85%    95%    99% 
# 62.50  79.45  97.50 100.00 100.00 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 66.67  82.61  98.67 100.00 100.00 100.00 100.00
# Boxplot
boxplot(peso_nao_cm$acuracia_pontos)
# Histograma
hist(peso_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(peso_nao_cm$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_cm_pontos <- 
  data.frame(peso_sem_cm = quantile(peso_nao_cm$acuracia_pontos, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 79.90915
mean(peso_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 92.15191
median(peso_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.81  78.93  92.15  99.62 100.00 100.00 100.00
# Boxplot
boxplot(peso_nao_cm$acuracia_linhas)
# Histograma
hist(peso_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(peso_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_cm_linhas <- 
  data.frame(peso_sem_cm = quantile(peso_nao_cm$acuracia_linhas, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Com contramão -------------------------------------

# Resultados para viagens com contramão
# filter: removed 51,568 rows (40%), 78,186 rows remaining
peso_sim_cm <- peso %>% filter(vg_contramao == 'sim')
head(peso_sim_cm)

# Resultado geral por pontos: 54.69066
sum(peso_sim_cm$pts_intsct) / sum(peso_sim_cm$pts_viagem) * 100
# Média de acurácia por pontos: 59.09029
mean(peso_sim_cm$acuracia_pontos)
# Mediana por pontos: 60
median(peso_sim_cm$acuracia_pontos)
# Quantis
quantile(peso_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
# 25% 33.33%    50%    75%    85%    95%    99% 
# 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 33.33  41.86  60.00  87.10  96.26 100.00 100.00 
# Boxplot
boxplot(peso_sim_cm$acuracia_pontos)
# Histograma
hist(peso_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(peso_sim_cm$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_peso_com_cm_pontos <- 
  data.frame(peso_com_cm = quantile(peso_sim_cm$acuracia_pontos, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 53.13904
mean(peso_sim_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 53.24767
median(peso_sim_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 27.04  35.73  53.25  78.68  89.34 100.00 100.00
# Boxplot
boxplot(peso_sim_cm$acuracia_linhas)
# Histograma
hist(peso_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(peso_sim_cm$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_peso_com_cm_linhas <- 
  data.frame(peso_com_cm = quantile(peso_sim_cm$acuracia_linhas, 
                                          probs = quantis2, 
                                          na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas') # %>% 
# pivot_longer(peso_sim_cm, names_to = "perfil_rotas") %>%
# pivot_wider(id_cols = perfil_rotas, names_from = quantiles)



#------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------

# Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# filter: removed 81,671 rows (63%), 48,083 rows remaining
peso_nao_cm_lp_pq <- peso %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
head(peso_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)

# Resultado geral por pontos: 76.93455
sum(peso_nao_cm_lp_pq$pts_intsct) / sum(peso_nao_cm_lp_pq$pts_viagem) * 100
# Média de acurácia por pontos: 83.35822
mean(peso_nao_cm_lp_pq$acuracia_pontos)
# Mediana por pontos: 100
median(peso_nao_cm_lp_pq$acuracia_pontos)
# Quantis
quantile(peso_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo ajustando o LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 73.47  87.40 100.00 100.00 100.00 100.00 100.00
# Boxplot
boxplot(peso_nao_cm_lp_pq$acuracia_pontos)
# Histograma
hist(peso_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# Histograma das distâncias
hist(peso_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)

# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_cm_lp_pq_pontos <- 
  data.frame(peso_sem_cm_lp_pq = quantile(peso_nao_cm_lp_pq$acuracia_pontos, 
                                                probs = quantis2, 
                                                na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.28447
mean(peso_nao_cm_lp_pq$acuracia_linhas)
# Mediana  por extensão de linhas: 92.05867
median(peso_nao_cm_lp_pq$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 68.88  79.40  92.06  99.32 100.00 100.00 100.00 
# Boxplot
boxplot(peso_nao_cm_lp_pq$acuracia_linhas)
# Histograma
hist(peso_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(peso_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)


# Guardar em dataframe para juntar aos demais resultados
menor_peso_sem_cm_lp_pq_linhas <- 
  data.frame(peso_sem_cm_lp_pq = quantile(peso_nao_cm_lp_pq$acuracia_linhas, 
                                                probs = quantis2, 
                                                na.rm = TRUE)) %>% 
  rownames_to_column(var = 'quantiles_linhas')


#----------------------------- Resultados finais  ------------------------------

menor_peso_pontos %>% 
  left_join(menor_peso_sem_loop_pontos,     by = 'quantiles_ptos') %>%
  left_join(menor_peso_sem_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_peso_com_cm_pontos,       by = 'quantiles_ptos') %>%
  left_join(menor_peso_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
#   quantiles_ptos peso_geral peso_sem_loop peso_sem_cm peso_com_cm peso_sem_cm_lp_pq
# 1            25%         37.93103            38.29787          60.71429          31.66667                68.88889
# 2         33.33%         48.78049            49.18033          77.27273          40.00000                83.63636
# 3            50%         71.11111            71.42857          97.05882          57.35294                99.00990
# 4         66.66%         91.54930            91.66667         100.00000          76.00000               100.00000
# 5            75%         98.64865            98.79518         100.00000          85.00000               100.00000
# 6            85%        100.00000           100.00000         100.00000          95.00000               100.00000
# 7            95%        100.00000           100.00000         100.00000         100.00000               100.00000
# 8            99%        100.00000           100.00000         100.00000         100.00000               100.00000

menor_peso_linhas %>% 
  left_join(menor_peso_sem_loop_linhas,     by = 'quantiles_linhas') %>%
  left_join(menor_peso_sem_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_peso_com_cm_linhas,       by = 'quantiles_linhas') %>%
  left_join(menor_peso_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
#   quantiles_linhas peso_geral peso_sem_loop peso_sem_cm peso_com_cm peso_sem_cm_lp_pq
# 1              25%         33.39980            33.26669          63.45275          24.61019                64.87885
# 2           33.33%         44.20673            44.03210          75.55511          32.83474                76.33764
# 3              50%         65.19329            64.98904          90.82545          49.88571                90.84034
# 4           66.66%         84.42549            84.16653          97.30397          67.06924                97.09146
# 5              75%         92.22889            91.98819          99.31136          76.09649                99.00952
# 6              85%         98.37302            98.20655         100.00000          87.40098               100.00000
# 7              95%        100.00000           100.00000         100.00000         100.00000               100.00000
# 8              99%        100.00000           100.00000         100.00000         100.00000               100.00000





# ------------------------------------------------------------------------------
# Comparativos - menor tempo, menor distância, maior infra cicloviária
# ------------------------------------------------------------------------------

gerar_summary <- function(df_col, var_name) {
  stats <- summary(df_col) %>% t() %>% t() %>% as.data.frame()
  stats <- stats %>% select(summary = Var1, !!var_name := Freq)
  return(stats)
}

# Resultados gerais
gerais <- 
  gerar_summary(peso$acuracia_linhas, 'peso') %>% 
  left_join(gerar_summary(tempo$acuracia_linhas, 'tempo'), by = 'summary') %>% 
  left_join(gerar_summary(dist$acuracia_linhas, 'dist'), by = 'summary') %>%
  left_join(gerar_summary(infraciclo$acuracia_linhas, 'ciclo'), by = 'summary')

# Resultados sem contramão
sem_cm <- 
  gerar_summary(peso_nao_cm$acuracia_linhas, 'peso_n_cmao') %>%
  left_join(gerar_summary(tempo_nao_cm$acuracia_linhas, 'tempo_n_cmao'), by = 'summary') %>%
  left_join(gerar_summary(dist_nao_cm$acuracia_linhas, 'dist_n_cmao'), by = 'summary') %>%
  left_join(gerar_summary(infraciclo_nao_cm$acuracia_linhas, 'ciclo_n_cmao'), by = 'summary')

# Resultados sem contramão, loop e com maior uso de estrutura cicloviária
sem_cmlp <- 
  gerar_summary(peso_nao_cm_lp_pq$acuracia_linhas, 'peso_n_cmlp') %>% 
  left_join(gerar_summary(tempo_nao_cm_lp_pq$acuracia_linhas, 'tempo_n_cmlp'), by = 'summary') %>%
  left_join(gerar_summary(dist_nao_cm_lp_pq$acuracia_linhas, 'dist_n_cmlp'), by = 'summary') %>%
  left_join(gerar_summary(infraciclo_nao_cm_lp_pq$acuracia_linhas, 'ciclo_n_cmlp'), by = 'summary')


todos_resultados <- gerais %>% left_join(sem_cm, by = 'summary') %>% left_join(sem_cmlp, by = 'summary')


# Juntar todas as colunas de resultado por tipo em um dataframe  único
juntar_resultados <- function(res1, res2, res3, res4, bb_title) {
  
  juntos <- 
    # Para criar as colunas com os nomes das variáveis, só pode ser um tibble, não
    # um dataframe
    tibble(!!cat1 := res1,
           !!cat2 := res2,
           !!cat3 := res3,
           !!cat4 := res4) %>% 
    # Adicionar o que vai ser o título do conjunto de gráficos no facet_wrap - aqui,
    # o nrow() precisa vir após a criação do dataframe para registrar as linhas
    mutate(tipo = sprintf('%s (n = %i)', bb_title, nrow(.))) %>% 
    # Pivot longer para deixar no melhor formato para o boxplot
    pivot_longer(cols = !tipo,
                 names_to = 'cat',
                 values_to = 'value') 
}

# Definir categorias para boxplots
cat1 <- '1. Menor Peso'; cat2 <- '2. Menor Tempo'; cat3 <- '3. Menor Distância'; cat4 <- '4. Mais InfraCiclo'
  
bp1 <- juntar_resultados(peso$acuracia_linhas, 
                         tempo$acuracia_linhas, 
                         dist$acuracia_linhas, 
                         infraciclo$acuracia_linhas, 
                         'A. Todas as rotas consideradas')
bp2 <- juntar_resultados(peso_nao_cm$acuracia_linhas, 
                         tempo_nao_cm$acuracia_linhas, 
                         dist_nao_cm$acuracia_linhas, 
                         infraciclo_nao_cm$acuracia_linhas,
                         'B. Rotas sem contramão')

bp <- rbind(bp1, bp2)


# https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph
p <- ggplot(data = bp, aes(x = cat, y = value)) 
p <- p + geom_boxplot(aes(fill = cat))
p <- p + facet_wrap( ~ tipo, dir = 'h', as.table = TRUE)
p <- p + xlab("tipo de rota") + ylab("quantis") + ggtitle("Comparativo de Acurácia das Rotas Modeladas - Método por extensão de linhas")
p <- p + guides(fill = guide_legend(title = "Tipo de rota"))
p <- p + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
p


# Percentual da extensão de uso da infra cicloviária
round(sum(peso$infra_ciclo) / sum(peso$distance) * 100, 2) # 30.91
round(sum(tempo$infra_ciclo) / sum(tempo$distance) * 100, 2) # 30.94
round(sum(dist$infra_ciclo) / sum(dist$distance) * 100, 2) # 29.24
round(sum(infraciclo$infra_ciclo) / sum(infraciclo$distance) * 100, 2) # 34.84

round(sum(peso_nao_cm$infra_ciclo) / sum(peso_nao_cm$distance) * 100, 2) # 42.86
round(sum(tempo_nao_cm$infra_ciclo) / sum(tempo_nao_cm$distance) * 100, 2) # 42.92
round(sum(dist_nao_cm$infra_ciclo) / sum(dist_nao_cm$distance) * 100, 2) # 41.12
round(sum(infraciclo_nao_cm$infra_ciclo) / sum(infraciclo_nao_cm$distance) * 100, 2) # 45.82

# Percentual da extensão de uso da infra cicloviária - somente ciclofaixas
round(sum(peso$ciclofaixa) / sum(peso$distance) * 100, 2) # 4.05
round(sum(tempo$ciclofaixa) / sum(tempo$distance) * 100, 2) # 4.05
round(sum(dist$ciclofaixa) / sum(dist$distance) * 100, 2) # 3.92
round(sum(infraciclo$ciclofaixa) / sum(infraciclo$distance) * 100, 2) # 5.03

round(sum(peso_nao_cm$ciclofaixa) / sum(peso_nao_cm$distance) * 100, 2) # 3.89
round(sum(tempo_nao_cm$ciclofaixa) / sum(tempo_nao_cm$distance) * 100, 2) # 3.89
round(sum(dist_nao_cm$ciclofaixa) / sum(dist_nao_cm$distance) * 100, 2) # 3.82
round(sum(infraciclo_nao_cm$ciclofaixa) / sum(infraciclo_nao_cm$distance) * 100, 2) # 4.52
