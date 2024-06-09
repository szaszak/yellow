library('tidyverse')
library('tidylog')
# library('geosphere')
# library('sf')
# library('mapview')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
# pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_orig_modalt  <- sprintf("%s/11_rotas_modeladas_com_alternativas", pasta_dados)


# Arquivo de resultados das rotas modeladas
resultados <- sprintf('%s/02_ttmatrix_rotas_modeladas_alternativas_acuracia_infraciclo_carac_viagens.csv', pasta_orig_modalt)
resultados <- read_delim(resultados, delim = ';', col_types = 'cciccddddddiiddddddccccddddddcdddd')
resultados <- resultados %>% select(-tmp_id)
head(resultados)

# Quantas viagens temos ao final? 129.755
resultados %>% select(trip_id) %>% distinct() %>% nrow()

# Quantas viagens possuem mais de uma alternativa?
resultados %>% group_by(alt) %>% tally() %>% mutate(perc = n/sum(n)*100)
#    alt      n  perc
#   <int>  <int> <dbl>
# 1     1 129755 66.0 
# 2     2  48316 24.6 
# 3     3  18484  9.40


# Definir quantis para visualizar resultados
quantis <- c(0.25, 0.3333, 0.5, 0.75, 0.85, 0.95, 0.99)
quantis2 <- c(0.2500, 0.3333, 0.5, 0.6666, 0.75, 0.85, 0.95, 1)

# # ------------------------------------------------------------------------------
# # Calcular detour geral (considerando todas as rotas alternativas)
# # ------------------------------------------------------------------------------
# 
# detour <- summary(resultados$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# detour
# # ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# # entre o comprimento da linha reta e da rota calculada
# #   summary detour_modalt
# # 1    Min.      0.000000
# # 2 1st Qu.      1.226677
# # 3  Median      1.385560
# # 4    Mean      1.570610
# # 5 3rd Qu.      1.627134
# # 6    Max.    336.056382
# 
# 
# # Isolar coluna de interesse - detour_mapmatching
# x <- resultados$detour_modalt
# # Pegar o primeiro e quarto quantis
# qnt <- quantile(x, probs = c(0.25, 0.75))
# # Outliers extremos estão a 3 * IQR
# H <- 3 * IQR(x)
# # Retirar outliers do dataframe
# # filter: removed 7,992 rows (6%), 121,763 rows remaining
# detour_sem_outliers <- resultados %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# rm(x, qnt, H)
# # 
# detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# detour_sem_outliers
# #     summary detour_modalt
# # 1    Min.     0.5802978
# # 2 1st Qu.     1.2216741
# # 3  Median     1.3755627
# # 4    Mean     1.4551013
# # 5 3rd Qu.     1.5934747
# # 6    Max.     2.8284905



# ------------------------------------------------------------------------------
# Ver resultados - menor custo da rota (peso)
# ------------------------------------------------------------------------------

# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
peso <- resultados %>% group_by(trip_id) %>% filter(weight == min(weight))  %>% ungroup()
nrow(peso)
# peso %>% select(alt) %>% distinct()


# Resultado geral por pontos: 61.10039
sum(peso$pts_intsct) / sum(peso$pts_viagem) * 100
# Média de acurácia por pontos: 68.06025
mean(peso$acuracia_pontos)
# Mediana por pontos: 75.67568
median(peso$acuracia_pontos)
# Quantis por pontos
quantile(peso$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 40.63  52.38  75.68 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(peso$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(peso$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9787928
# 2    25%  40.6333556
# 3 33.33%  52.3809524
# 4    50%  75.6756757
# 5 66.66%  95.8333333
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(peso$acuracia_pontos)
# Histograma
hist(peso$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das pesoâncias
hist(peso$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_pontos <- 
#   data.frame(peso_geral = quantile(peso$acuracia_pontos, 
#                                    probs = quantis2, 
#                                    na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 64.04311
mean(peso$acuracia_linhas)
# Mediana  por extensão de linhas: 69.48352
median(peso$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.16  48.71  69.48  95.93  98.87 100.00 100.00 
data.frame(freq = quantile(peso$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(peso$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.047996
# 2    25%  37.161560
# 3 33.33%  48.713242
# 4    50%  69.483519
# 5 66.66%  88.037733
# 6    75%  95.926277
# 7    85%  98.866502
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(peso$acuracia_linhas)
# Histograma
hist(peso$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das pesoâncias
hist(peso$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_linhas <- 
#   data.frame(peso_geral = quantile(peso$acuracia_linhas, 
#                                    probs = quantis2, 
#                                    na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')



nrow(peso)
detour <- summary(peso$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
peso %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.185289
# 3  Median      1.348063
# 4    Mean      1.560524
# 5 3rd Qu.      1.595694
# 6    Max.    336.056382


# Isolar coluna de interesse - detour_mapmatching
x <- peso$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- peso %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.179584
# 3  Median      1.337166
# 4    Mean      1.421063
# 5 3rd Qu.      1.556868
# 6    Max.      2.826637



# # -------------------------- Sem loop -----------------------------------------
# 
# # Resultados para viagens sem loop
# # filter: removed 776 rows (1%), 128,978 rows remaining
# peso_nao_loop <- peso %>% filter(vg_loop == 'não')
# head(peso_nao_loop)
# 
# # Resultado geral por pontos: 61.43477
# sum(peso_nao_loop$pts_intsct) / sum(peso_nao_loop$pts_viagem) * 100
# # Média de acurácia por pontos: 67.89371
# mean(peso_nao_loop$acuracia_pontos)
# # Mediana por pontos: 75.60976
# median(peso_nao_loop$acuracia_pontos)
# # Quantis
# quantile(peso_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 41.00  52.63  75.61 100.00 100.00 100.00 100.00
# # Boxplot
# boxplot(peso_nao_loop$acuracia_pontos)
# # Histograma
# hist(peso_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# # Histograma das distâncias
# hist(peso_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_loop_pontos <- 
#   data.frame(peso_sem_loop = quantile(peso_nao_loop$acuracia_pontos, 
#                                       probs = quantis2, 
#                                       na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 63.64785
# mean(peso_nao_loop$acuracia_linhas)
# # Mediana  por extensão de linhas: 69.16791
# median(peso_nao_loop$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(peso_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 37.04  48.57  69.17  93.70  98.86 100.00 100.00
# # Boxplot
# boxplot(peso_nao_loop$acuracia_linhas)
# # Histograma
# hist(peso_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# # Histograma das pesoâncias
# hist(peso_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_loop_linhas <- 
#   data.frame(peso_sem_loop = quantile(peso_nao_loop$acuracia_linhas, 
#                                       probs = quantis2, 
#                                       na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
peso_nao_cm <- peso %>% filter(vg_contramao == 'não')
nrow(peso_nao_cm)
head(peso_nao_cm)

# Resultado geral por pontos: 72.69268
sum(peso_nao_cm$pts_intsct) / sum(peso_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 81.39587
mean(peso_nao_cm$acuracia_pontos)
# Mediana por pontos: 100
median(peso_nao_cm$acuracia_pontos)
# Quantis
quantile(peso_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 66.67  84.13 100.00 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(peso_nao_cm$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(peso_nao_cm$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9868421
# 2    25%  66.6666667
# 3 33.33%  84.1269841
# 4    50% 100.0000000
# 5 66.66% 100.0000000
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(peso_nao_cm$acuracia_pontos)
# Histograma
hist(peso_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(peso_nao_cm$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_cm_pontos <- 
#   data.frame(peso_sem_cm = quantile(peso_nao_cm$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.47692
mean(peso_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 95.06766
median(peso_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(peso_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.94  79.45  95.07  99.11  99.97 100.00 100.00 
data.frame(freq = quantile(peso_nao_cm$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(peso_nao_cm$acuracia_linhas), 
          .before = 1)
#     desc      freq
# 1    Min   1.53576
# 2    25%  67.94032
# 3 33.33%  79.44837
# 4    50%  95.06766
# 5 66.66%  98.29214
# 6    75%  99.10862
# 7    85%  99.96582
# 8    95% 100.00000
# 9   100% 100.00000
# Boxplot
boxplot(peso_nao_cm$acuracia_linhas)
# Histograma
hist(peso_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(peso_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_cm_linhas <- 
#   data.frame(peso_sem_cm = quantile(peso_nao_cm$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')



nrow(peso_nao_cm)
detour <- summary(peso_nao_cm$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
peso_nao_cm %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0995070
# 3  Median     1.2412986
# 4    Mean     1.4394395
# 5 3rd Qu.     1.4149146
# 6    Max.   336.0563818


# Isolar coluna de interesse - detour_mapmatching
x <- peso_nao_cm$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- peso_nao_cm %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0953396
# 3  Median     1.2311637
# 4    Mean     1.2854036
# 5 3rd Qu.     1.3958747
# 6    Max.     2.3609761



# # -------------------------- Com contramão -------------------------------------
# 
# # Resultados para viagens com contramão
# # filter: removed 51,568 rows (40%), 78,186 rows remaining
# peso_sim_cm <- peso %>% filter(vg_contramao == 'sim')
# head(peso_sim_cm)
# 
# # Resultado geral por pontos: 54.69066
# sum(peso_sim_cm$pts_intsct) / sum(peso_sim_cm$pts_viagem) * 100
# # Média de acurácia por pontos: 59.09029
# mean(peso_sim_cm$acuracia_pontos)
# # Mediana por pontos: 60
# median(peso_sim_cm$acuracia_pontos)
# # Quantis
# quantile(peso_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# # 25% 33.33%    50%    75%    85%    95%    99% 
# # 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 33.33  41.86  60.00  87.10  96.26 100.00 100.00 
# # Boxplot
# boxplot(peso_sim_cm$acuracia_pontos)
# # Histograma
# hist(peso_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# # Histograma das distâncias
# hist(peso_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_com_cm_pontos <- 
#   data.frame(peso_com_cm = quantile(peso_sim_cm$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 53.13904
# mean(peso_sim_cm$acuracia_linhas)
# # Mediana  por extensão de linhas: 53.24767
# median(peso_sim_cm$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(peso_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 27.04  35.73  53.25  78.68  89.34 100.00 100.00
# # Boxplot
# boxplot(peso_sim_cm$acuracia_linhas)
# # Histograma
# hist(peso_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(peso_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_com_cm_linhas <- 
#   data.frame(peso_com_cm = quantile(peso_sim_cm$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas') # %>% 
# # pivot_longer(peso_sim_cm, names_to = "perfil_rotas") %>%
# # pivot_wider(id_cols = perfil_rotas, names_from = quantiles)
# 
# 
# 
# #------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------
# 
# # Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# # filter: removed 81,671 rows (63%), 48,083 rows remaining
# peso_nao_cm_lp_pq <- peso %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
# head(peso_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)
# 
# # Resultado geral por pontos: 76.93455
# sum(peso_nao_cm_lp_pq$pts_intsct) / sum(peso_nao_cm_lp_pq$pts_viagem) * 100
# # Média de acurácia por pontos: 83.35822
# mean(peso_nao_cm_lp_pq$acuracia_pontos)
# # Mediana por pontos: 100
# median(peso_nao_cm_lp_pq$acuracia_pontos)
# # Quantis
# quantile(peso_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 73.47  87.40 100.00 100.00 100.00 100.00 100.00
# # Boxplot
# boxplot(peso_nao_cm_lp_pq$acuracia_pontos)
# # Histograma
# hist(peso_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# # Histograma das distâncias
# hist(peso_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_cm_lp_pq_pontos <- 
#   data.frame(peso_sem_cm_lp_pq = quantile(peso_nao_cm_lp_pq$acuracia_pontos, 
#                                           probs = quantis2, 
#                                           na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 80.28447
# mean(peso_nao_cm_lp_pq$acuracia_linhas)
# # Mediana  por extensão de linhas: 92.05867
# median(peso_nao_cm_lp_pq$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(peso_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 68.88  79.40  92.06  99.32 100.00 100.00 100.00 
# # Boxplot
# boxplot(peso_nao_cm_lp_pq$acuracia_linhas)
# # Histograma
# hist(peso_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(peso_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_peso_sem_cm_lp_pq_linhas <- 
#   data.frame(peso_sem_cm_lp_pq = quantile(peso_nao_cm_lp_pq$acuracia_linhas, 
#                                           probs = quantis2, 
#                                           na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')

# ------------------------------------------------------------------------------
# Ver resultados - menor tempo
# ------------------------------------------------------------------------------

# Filtrar viagens - menor tempo por trip_id
tempo <- resultados %>% group_by(trip_id) %>% filter(time == min(time)) %>% ungroup()


# Resultado geral por pontos: 61.06157
sum(tempo$pts_intsct) / sum(tempo$pts_viagem) * 100
# Média de acurácia por pontos: 68.03887
mean(tempo$acuracia_pontos)
# Mediana por pontos: 75.67568
median(tempo$acuracia_pontos)
# Quantis por pontos
quantile(tempo$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 40.62  52.38  75.68 100.00 100.00 100.00 100.00  
data.frame(freq = quantile(tempo$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(tempo$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9787928
# 2    25%  40.6250000
# 3 33.33%  52.3809524
# 4    50%  75.6756757
# 5 66.66%  95.8333333
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(tempo$acuracia_pontos)
# Histograma
hist(tempo$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das distâncias
hist(tempo$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_pontos <- 
#   data.frame(tempo_geral = quantile(tempo$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')# %>% 
# # pivot_longer(tempo_nao_cm, names_to = "perfil_rotas") %>%
# # pivot_wider(id_cols = perfil_rotas, names_from = quantiles)


# Média de acurácia  por extensão de linhas: 64.02095
mean(tempo$acuracia_linhas)
# Mediana  por extensão de linhas: 69.43903
median(tempo$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.16  48.70  69.44  95.89  98.85 100.00 100.00 
data.frame(freq = quantile(tempo$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(tempo$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.047996
# 2    25%  37.161145
# 3 33.33%  48.696697
# 4    50%  69.439028
# 5 66.66%  87.997680
# 6    75%  95.891231
# 7    85%  98.846333
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(tempo$acuracia_linhas)
# Histograma
hist(tempo$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_linhas <- 
#   data.frame(tempo_geral = quantile(tempo$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')



nrow(tempo)
detour <- summary(tempo$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
tempo %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.185208
# 3  Median      1.347985
# 4    Mean      1.560356
# 5 3rd Qu.      1.595366
# 6    Max.    336.056382


# Isolar coluna de interesse - detour_mapmatching
x <- tempo$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- tempo %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.179522
# 3  Median      1.337033
# 4    Mean      1.420895
# 5 3rd Qu.      1.556493
# 6    Max.      2.825772




# -------------------------- Sem loop -----------------------------------------

# # Resultados para viagens sem loop
# # filter: removed 776 rows (1%), 128,978 rows remaining
# tempo_nao_loop <- tempo %>% filter(vg_loop == 'não')
# head(tempo_nao_loop)
# 
# # Resultado geral por pontos: 61.41476 (modelo sem ajustar LTS: 59.39098)
# sum(tempo_nao_loop$pts_intsct) / sum(tempo_nao_loop$pts_viagem) * 100
# # Média de acurácia por pontos: 67.87872 (modelo sem ajustar LTS: 66.65127)
# mean(tempo_nao_loop$acuracia_pontos)
# # Mediana por pontos: 75.55556 (modelo sem ajustar LTS: 73.4375)
# median(tempo_nao_loop$acuracia_pontos)
# # Quantis
# quantile(tempo_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# # Modelo ajustando o LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 40.96  52.63  75.56 100.00 100.00 100.00 100.00
# # Boxplot
# boxplot(tempo_nao_loop$acuracia_pontos)
# # Histograma
# hist(tempo_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# # Histograma das distâncias
# hist(tempo_nao_loop$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_loop_pontos <- 
#   data.frame(tempo_sem_loop = quantile(tempo_nao_loop$acuracia_pontos, 
#                                        probs = quantis2, 
#                                        na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 63.6334
# mean(tempo_nao_loop$acuracia_linhas)
# # Mediana  por extensão de linhas: 69.13994
# median(tempo_nao_loop$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(tempo_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 37.03  48.56  69.14  93.68  98.85 100.00 100.00 
# # Boxplot
# boxplot(tempo_nao_loop$acuracia_linhas)
# # Histograma
# hist(tempo_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(tempo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_loop_linhas <- 
#   data.frame(tempo_sem_loop = quantile(tempo_nao_loop$acuracia_linhas, 
#                                        probs = quantis2, 
#                                        na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
tempo_nao_cm <- tempo %>% filter(vg_contramao == 'não')
nrow(tempo_nao_cm)
head(tempo_nao_cm)

# Resultado geral por pontos: 72.63067
sum(tempo_nao_cm$pts_intsct) / sum(tempo_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 81.34728
mean(tempo_nao_cm$acuracia_pontos)
# Mediana por pontos: 100
median(tempo_nao_cm$acuracia_pontos)
# Quantis
quantile(tempo_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 66.67  84.00 100.00 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(tempo_nao_cm$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(tempo_nao_cm$acuracia_pontos), 
          .before = 1)
# Boxplot
boxplot(tempo_nao_cm$acuracia_pontos)
# Histograma
hist(tempo_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(tempo_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_cm_pontos <- 
#   data.frame(tempo_sem_cm = quantile(tempo_nao_cm$acuracia_pontos, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.4223
mean(tempo_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 95.00635
median(tempo_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(tempo_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.84  79.35  95.01  99.09  99.96 100.00 100.00 
data.frame(freq = quantile(tempo_nao_cm$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(tempo_nao_cm$acuracia_linhas), 
          .before = 1)
#     desc      freq
# 1    Min   1.53576
# 2    25%  67.84079
# 3 33.33%  79.35120
# 4    50%  95.00635
# 5 66.66%  98.27125
# 6    75%  99.08989
# 7    85%  99.96230
# 8    95% 100.00000
# 9   100% 100.00000
# Boxplot
boxplot(tempo_nao_cm$acuracia_linhas)
# Histograma
hist(tempo_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(tempo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_cm_linhas <- 
#   data.frame(tempo_sem_cm = quantile(tempo_nao_cm$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas') 



nrow(tempo_nao_cm)
detour <- summary(tempo_nao_cm$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
tempo_nao_cm %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0996001
# 3  Median     1.2411578
# 4    Mean     1.4392726
# 5 3rd Qu.     1.4144001
# 6    Max.   336.0563818


# Isolar coluna de interesse - detour_mapmatching
x <- tempo_nao_cm$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- tempo_nao_cm %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0953550
# 3  Median     1.2310419
# 4    Mean     1.2852455
# 5 3rd Qu.     1.3955338
# 6    Max.     2.3586210




# -------------------------- Com contramão -------------------------------------

# # Resultados para viagens com contramão
# # filter: removed 51,568 rows (40%), 78,186 rows remaining
# tempo_sim_cm <- tempo %>% filter(vg_contramao == 'sim')
# head(tempo_sim_cm)
# 
# # Resultado geral por pontos: 54.68525 (modelo sem ajustar LTS: 52.69799)
# sum(tempo_sim_cm$pts_intsct) / sum(tempo_sim_cm$pts_viagem) * 100
# # Média de acurácia por pontos: 59.09282 (modelo sem ajustar LTS: 57.89866)
# mean(tempo_sim_cm$acuracia_pontos)
# # Mediana por pontos: 60 (modelo sem ajustar LTS: 58.13953)
# median(tempo_sim_cm$acuracia_pontos)
# # Quantis
# quantile(tempo_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# # 25% 33.33%    50%    75%    85%    95%    99% 
# # 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# # Modelo ajustando o LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 33.33  41.86  60.00  87.10  96.25 100.00 100.00 
# # Boxplot
# boxplot(tempo_sim_cm$acuracia_pontos)
# # Histograma
# hist(tempo_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# # Histograma das distâncias
# hist(tempo_sim_cm$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_com_cm_pontos <- 
#   data.frame(tempo_com_cm = quantile(tempo_sim_cm$acuracia_pontos, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 53.14242
# mean(tempo_sim_cm$acuracia_linhas)
# # Mediana  por extensão de linhas: 53.27057
# median(tempo_sim_cm$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(tempo_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 27.04  35.75  53.27  78.68  89.33 100.00 100.00 
# # Boxplot
# boxplot(tempo_sim_cm$acuracia_linhas)
# # Histograma
# hist(tempo_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(tempo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_com_cm_linhas <- 
#   data.frame(tempo_com_cm = quantile(tempo_sim_cm$acuracia_linhas, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas') # %>% 
# # pivot_longer(tempo_sim_cm, names_to = "perfil_rotas") %>%
# # pivot_wider(id_cols = perfil_rotas, names_from = quantiles)



#------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------

# # Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# # filter: removed 81,662 rows (63%), 48,092 rows remaining
# tempo_nao_cm_lp_pq <- tempo %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
# head(tempo_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)
# 
# # Resultado geral por pontos: 76.90294
# sum(tempo_nao_cm_lp_pq$pts_intsct) / sum(tempo_nao_cm_lp_pq$pts_viagem) * 100
# # Média de acurácia por pontos: 83.33861
# mean(tempo_nao_cm_lp_pq$acuracia_pontos)
# # Mediana por pontos: 100
# median(tempo_nao_cm_lp_pq$acuracia_pontos)
# # Quantis
# quantile(tempo_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo ajustando o LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 73.44  87.34 100.00 100.00 100.00 100.00 100.00 
# # Boxplot
# boxplot(tempo_nao_cm_lp_pq$acuracia_pontos)
# # Histograma
# hist(tempo_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# # Histograma das distâncias
# hist(tempo_nao_cm_lp_pq$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_cm_lp_pq_pontos <- 
#   data.frame(tempo_sem_cm_lp_pq = quantile(tempo_nao_cm_lp_pq$acuracia_pontos, 
#                                            probs = quantis2, 
#                                            na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 80.26486
# mean(tempo_nao_cm_lp_pq$acuracia_linhas)
# # Mediana  por extensão de linhas: 92.03825
# median(tempo_nao_cm_lp_pq$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(tempo_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 68.88  79.38  92.04  99.32 100.00 100.00 100.00 
# # Boxplot
# boxplot(tempo_nao_cm_lp_pq$acuracia_linhas)
# # Histograma
# hist(tempo_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(tempo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_tempo_sem_cm_lp_pq_linhas <- 
#   data.frame(tempo_sem_cm_lp_pq = quantile(tempo_nao_cm_lp_pq$acuracia_linhas, 
#                                            probs = quantis2, 
#                                            na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


#----------------------------- Resultados finais  ------------------------------

# menor_tempo_pontos %>% 
#   left_join(menor_tempo_sem_loop_pontos,     by = 'quantiles_ptos') %>%
#   left_join(menor_tempo_sem_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(menor_tempo_com_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(menor_tempo_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
# #   quantiles_ptos tempo_geral tempo_sem_loop tempo_sem_cm tempo_com_cm tempo_sem_cm_lp_pq
# # 1            25%    40.57971       40.96386     66.36669     33.33333           73.43750
# # 2         33.33%    52.19802       52.63158     82.50000     41.86047           87.34177
# # 3            50%    75.00000       75.55556     98.63480     60.00000          100.00000
# # 4         66.66%    94.00000       94.11765    100.00000     78.48913          100.00000
# # 5            75%   100.00000      100.00000    100.00000     87.09677          100.00000
# # 6            85%   100.00000      100.00000    100.00000     96.25000          100.00000
# # 7            95%   100.00000      100.00000    100.00000    100.00000          100.00000
# # 8            99%   100.00000      100.00000    100.00000    100.00000          100.00000
# 
# menor_tempo_linhas %>% 
#   left_join(menor_tempo_sem_loop_linhas,     by = 'quantiles_linhas') %>%
#   left_join(menor_tempo_sem_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(menor_tempo_com_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(menor_tempo_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
# #   quantiles_linhas tempo_geral tempo_sem_loop tempo_sem_cm tempo_com_cm tempo_sem_cm_lp_pq
# # 1              25%    37.16614       37.03287     67.75137     27.03954           68.87950
# # 2           33.33%    48.72080       48.56424     78.84185     35.74535           79.37975
# # 3              50%    69.35618       69.13994     92.10369     53.27057           92.03825
# # 4           66.66%    87.10410       86.88825     97.74499     70.27222           97.54054
# # 5              75%    93.85557       93.67638     99.60741     78.67937           99.31612
# # 6              85%    98.97951       98.84766    100.00000     89.33308          100.00000
# # 7              95%   100.00000      100.00000    100.00000    100.00000          100.00000
# # 8              99%   100.00000      100.00000    100.00000    100.00000          100.00000



# ------------------------------------------------------------------------------
# Ver resultados - menor distância
# ------------------------------------------------------------------------------

# -------------------------- resultado geral------------------------------------

# Filtrar viagens - menor tempo por trip_id
dist <- resultados %>% group_by(trip_id) %>% filter(distance == min(distance)) %>% ungroup()
nrow(dist)

# Resultado geral por pontos: 60.35085
sum(dist$pts_intsct) / sum(dist$pts_viagem) * 100
# Média de acurácia por pontos: 67.60759
mean(dist$acuracia_pontos)
# Mediana por pontos: 75
median(dist$acuracia_pontos)
# Quantis por pontos
quantile(dist$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 40.00  51.47  75.00 100.00 100.00 100.00 100.00
data.frame(freq = quantile(dist$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(dist$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.7371007
# 2    25%  40.0000000
# 3 33.33%  51.4705882
# 4    50%  75.0000000
# 5 66.66%  95.5056180
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(dist$acuracia_pontos)
# Histograma
hist(dist$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das distâncias
hist(dist$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_pontos <- 
#   data.frame(dist_geral = quantile(dist$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 63.61581
mean(dist$acuracia_linhas)
# Mediana  por extensão de linhas: 68.73781
median(dist$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 36.35  47.83  68.74  95.85  98.88 100.00 100.00 
data.frame(freq = quantile(dist$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(dist$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.228205
# 2    25%  36.353129
# 3 33.33%  47.833456
# 4    50%  68.737810
# 5 66.66%  87.650045
# 6    75%  95.846122
# 7    85%  98.875982
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(dist$acuracia_linhas)
# Histograma
hist(dist$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_linhas <- 
#   data.frame(dist_geral = quantile(dist$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')




nrow(dist)
detour <- summary(dist$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
dist %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.184207
# 3  Median      1.346549
# 4    Mean      1.558764
# 5 3rd Qu.      1.593352
# 6    Max.    336.056382


# Isolar coluna de interesse - detour_mapmatching
x <- dist$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- dist %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.178501
# 3  Median      1.335494
# 4    Mean      1.419290
# 5 3rd Qu.      1.554618
# 6    Max.      2.820430



# # -------------------------- Sem loop -----------------------------------------
# 
# # Resultados para viagens sem loop
# # filter: removed 776 rows (1%), 128,978 rows remaining
# dist_nao_loop <- dist %>% filter(vg_loop == 'não')
# head(dist_nao_loop)
# 
# # Resultado geral por pontos: 60.65751
# sum(dist_nao_loop$pts_intsct) / sum(dist_nao_loop$pts_viagem) * 100
# # Média de acurácia por pontos: 67.43494
# mean(dist_nao_loop$acuracia_pontos)
# # Mediana por pontos: 74.80157
# median(dist_nao_loop$acuracia_pontos)
# # Quantis
# quantile(dist_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 40.00  51.72  74.80 100.00 100.00 100.00 100.00 
# # Boxplot
# boxplot(dist_nao_loop$acuracia_pontos)
# # Histograma
# hist(dist_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# # Histograma das distâncias
# hist(dist_nao_loop$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_loop_pontos <- 
#   data.frame(dist_sem_loop = quantile(dist_nao_loop$acuracia_pontos, 
#                                        probs = quantis2, 
#                                        na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 63.20913
# mean(dist_nao_loop$acuracia_linhas)
# # Mediana  por extensão de linhas: 68.37213
# median(dist_nao_loop$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(dist_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 36.25  47.70  68.37  93.55  98.84 100.00 100.00 
# # Boxplot
# boxplot(dist_nao_loop$acuracia_linhas)
# # Histograma
# hist(dist_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(dist_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_loop_linhas <- 
#   data.frame(dist_sem_loop = quantile(dist_nao_loop$acuracia_linhas, 
#                                        probs = quantis2, 
#                                        na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,568 rows remaining
dist_nao_cm <- dist %>% filter(vg_contramao == 'não')
nrow(dist_nao_cm)
head(dist_nao_cm)

# Resultado geral por pontos: 71.86248
sum(dist_nao_cm$pts_intsct) / sum(dist_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 80.89652
mean(dist_nao_cm$acuracia_pontos)
# Mediana por pontos: 100
median(dist_nao_cm$acuracia_pontos)
# Quantis
quantile(dist_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 65.29  82.76 100.00 100.00 100.00 100.00 100.00
data.frame(freq = quantile(dist_nao_cm$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(dist_nao_cm$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.7371007
# 2    25%  65.2892562
# 3 33.33%  82.7586207
# 4    50% 100.0000000
# 5 66.66% 100.0000000
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(dist_nao_cm$acuracia_pontos)
# Histograma
hist(dist_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(dist_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_cm_pontos <- 
#   data.frame(dist_sem_cm = quantile(dist_nao_cm$acuracia_pontos, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.02022
mean(dist_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 94.93231
median(dist_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(dist_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 66.79  78.57  94.93  99.13  99.97 100.00 100.00 
data.frame(freq = quantile(dist_nao_cm$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(dist_nao_cm$acuracia_linhas), 
          .before = 1)
#     desc      freq
# 1    Min   1.53576
# 2    25%  66.78954
# 3 33.33%  78.56902
# 4    50%  94.93231
# 5 66.66%  98.31270
# 6    75%  99.12801
# 7    85%  99.96729
# 8    95% 100.00000
# 9   100% 100.00000
# Boxplot
boxplot(dist_nao_cm$acuracia_linhas)
# Histograma
hist(dist_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(dist_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_cm_linhas <- 
#   data.frame(dist_sem_cm = quantile(dist_nao_cm$acuracia_linhas, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


nrow(dist_nao_cm)
detour <- summary(dist_nao_cm$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
dist_nao_cm %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0987965
# 3  Median     1.2400624
# 4    Mean     1.4376891
# 5 3rd Qu.     1.4119849
# 6    Max.   336.0563818


# Isolar coluna de interesse - detour_mapmatching
x <- dist_nao_cm$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- dist_nao_cm %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
rm(x, qnt, H)
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0945629
# 3  Median     1.2299905
# 4    Mean     1.2835838
# 5 3rd Qu.     1.3936300
# 6    Max.     2.3513760


# # -------------------------- Com contramão -------------------------------------
# 
# # Resultados para viagens com contramão
# # filter: removed 51,568 rows (40%), 78,186 rows remaining
# dist_sim_cm <- dist %>% filter(vg_contramao == 'sim')
# head(dist_sim_cm)
# 
# # Resultado geral por pontos: 53.99676
# sum(dist_sim_cm$pts_intsct) / sum(dist_sim_cm$pts_viagem) * 100
# # Média de acurácia por pontos: 58.67675
# mean(dist_sim_cm$acuracia_pontos)
# # Mediana por pontos: 59.32203
# median(dist_sim_cm$acuracia_pontos)
# # Quantis
# quantile(dist_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# # 25% 33.33%    50%    75%    85%    95%    99% 
# # 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 32.61  41.18  59.32  86.81  96.10 100.00 100.00 
# # Boxplot
# boxplot(dist_sim_cm$acuracia_pontos)
# # Histograma
# hist(dist_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# # Histograma das distâncias
# hist(dist_sim_cm$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_com_cm_pontos <- 
#   data.frame(dist_com_cm = quantile(dist_sim_cm$acuracia_pontos, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 52.73209
# mean(dist_sim_cm$acuracia_linhas)
# # Mediana  por extensão de linhas: 52.62296
# median(dist_sim_cm$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(dist_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 26.58  35.21  52.62  78.25  89.11 100.00 100.00 
# # Boxplot
# boxplot(dist_sim_cm$acuracia_linhas)
# # Histograma
# hist(dist_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(dist_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_com_cm_linhas <- 
#   data.frame(dist_com_cm = quantile(dist_sim_cm$acuracia_linhas, 
#                                      probs = quantis2, 
#                                      na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas') # %>% 
# # pivot_longer(dist_sim_cm, names_to = "perfil_rotas") %>%
# # pivot_wider(id_cols = perfil_rotas, names_from = quantiles)
# 
# 
# 
# #------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------
# 
# # Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# # filter: removed 81,681 rows (63%), 48,073 rows remaining
# dist_nao_cm_lp_pq <- dist %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
# head(dist_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)
# 
# # Resultado geral por pontos: 75.94904
# sum(dist_nao_cm_lp_pq$pts_intsct) / sum(dist_nao_cm_lp_pq$pts_viagem) * 100
# # Média de acurácia por pontos: 82.81114
# mean(dist_nao_cm_lp_pq$acuracia_pontos)
# # Mediana por pontos: 100
# median(dist_nao_cm_lp_pq$acuracia_pontos)
# # Quantis
# quantile(dist_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 71.88  86.36 100.00 100.00 100.00 100.00 100.00
# # Boxplot
# boxplot(dist_nao_cm_lp_pq$acuracia_pontos)
# # Histograma
# hist(dist_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# # Histograma das distâncias
# hist(dist_nao_cm_lp_pq$dist_total, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_cm_lp_pq_pontos <- 
#   data.frame(dist_sem_cm_lp_pq = quantile(dist_nao_cm_lp_pq$acuracia_pontos, 
#                                            probs = quantis2, 
#                                            na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 79.76859
# mean(dist_nao_cm_lp_pq$acuracia_linhas)
# # Mediana  por extensão de linhas: 91.88069
# median(dist_nao_cm_lp_pq$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(dist_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 67.68  78.63  91.88  99.33 100.00 100.00 100.00 
# # Boxplot
# boxplot(dist_nao_cm_lp_pq$acuracia_linhas)
# # Histograma
# hist(dist_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(dist_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# menor_dist_sem_cm_lp_pq_linhas <- 
#   data.frame(dist_sem_cm_lp_pq = quantile(dist_nao_cm_lp_pq$acuracia_linhas, 
#                                            probs = quantis2, 
#                                            na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')
# 
# 
# #----------------------------- Resultados finais  ------------------------------
# 
# menor_dist_pontos %>% 
#   left_join(menor_dist_sem_loop_pontos,     by = 'quantiles_ptos') %>%
#   left_join(menor_dist_sem_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(menor_dist_com_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(menor_dist_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
# #   quantiles_ptos dist_geral dist_sem_loop dist_sem_cm dist_com_cm dist_sem_cm_lp_pq
# # 1            25%   39.72603      40.00000    64.80000    32.60870          71.87500
# # 2         33.33%   51.32743      51.72414    81.39535    41.17647          86.36364
# # 3            50%   74.35897      74.80157    98.36732    59.32203         100.00000
# # 4         66.66%   93.75000      93.91304   100.00000    78.04878         100.00000
# # 5            75%  100.00000     100.00000   100.00000    86.81319         100.00000
# # 6            85%  100.00000     100.00000   100.00000    96.10390         100.00000
# # 7            95%  100.00000     100.00000   100.00000   100.00000         100.00000
# # 8            99%  100.00000     100.00000   100.00000   100.00000         100.00000
# 
# menor_dist_linhas %>% 
#   left_join(menor_dist_sem_loop_linhas,     by = 'quantiles_linhas') %>%
#   left_join(menor_dist_sem_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(menor_dist_com_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(menor_dist_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
# #   quantiles_linhas dist_geral dist_sem_loop dist_sem_cm dist_com_cm dist_sem_cm_lp_pq
# # 1              25%   36.38620      36.25173    66.61286    26.58158          67.68214
# # 2           33.33%   47.86979      47.70430    78.13253    35.20755          78.63329
# # 3              50%   68.60054      68.37213    91.95001    52.62296          91.88069
# # 4           66.66%   86.79114      86.54402    97.75353    69.71199          97.53787
# # 5              75%   93.74398      93.54910    99.62973    78.24867          99.33078
# # 6              85%   98.97735      98.84237   100.00000    89.10774         100.00000
# # 7              95%  100.00000     100.00000   100.00000   100.00000         100.00000
# # 8              99%  100.00000     100.00000   100.00000   100.00000         100.00000



# ------------------------------------------------------------------------------
# Ver resultados - maior uso de infra cicloviária
# ------------------------------------------------------------------------------

# Filtrar viagens - menor tempo por trip_id
infraciclo <- resultados %>% group_by(trip_id) %>% filter(infra_ciclo == max(infra_ciclo)) %>% ungroup()
nrow(infraciclo)
# Pelo visto, há resultados em que a extensão de uso de ciclovias é o mesmo em 
# mais de uma alternativa. Pegar então a de menor custo dentro desses resultados
infraciclo <- infraciclo %>% group_by(trip_id) %>% filter(weight == min(weight)) %>% ungroup()
nrow(infraciclo)


# Resultado geral por pontos: 61.47631
sum(infraciclo$pts_intsct) / sum(infraciclo$pts_viagem) * 100
# Média de acurácia por pontos: 68.29225
mean(infraciclo$acuracia_pontos)
# Mediana por pontos: 75.90361
median(infraciclo$acuracia_pontos)
# Quantis por pontos
quantile(infraciclo$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 41.25  52.83  75.90 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(infraciclo$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(infraciclo$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9787928
# 2    25%  41.2500000
# 3 33.33%  52.8301887
# 4    50%  75.9036145
# 5 66.66%  95.7335194
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(infraciclo$acuracia_pontos)
# Histograma
hist(infraciclo$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das infracicloâncias
hist(infraciclo$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_pontos <- 
#   data.frame(infraciclo_geral = quantile(infraciclo$acuracia_pontos, 
#                                    probs = quantis2, 
#                                    na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 64.00448
mean(infraciclo$acuracia_linhas)
# Mediana  por extensão de linhas: 69.49522
median(infraciclo$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 37.20  48.77  69.50  95.68  98.79 100.00 100.00 
data.frame(freq = quantile(infraciclo$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(infraciclo$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.047996
# 2    25%  37.200884
# 3 33.33%  48.774724
# 4    50%  69.495222
# 5 66.66%  87.790486
# 6    75%  95.679910
# 7    85%  98.791121
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(infraciclo$acuracia_linhas)
# Histograma
hist(infraciclo$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das infracicloâncias
hist(infraciclo$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_linhas <- 
#   data.frame(infraciclo_geral = quantile(infraciclo$acuracia_linhas, 
#                                    probs = quantis2, 
#                                    na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')



nrow(infraciclo)
detour <- summary(infraciclo$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
infraciclo %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.      0.000000
# 2 1st Qu.      1.194496
# 3  Median      1.362600
# 4    Mean      1.575161
# 5 3rd Qu.      1.616048
# 6    Max.    336.056382


# Isolar coluna de interesse - detour_mapmatching
x <- infraciclo$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- infraciclo %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0945629
# 3  Median     1.2299905
# 4    Mean     1.2835838
# 5 3rd Qu.     1.3936300
# 6    Max.     2.3513760

# 
# # -------------------------- Sem loop -----------------------------------------
# 
# # Resultados para viagens sem loop
# # ilter: removed 776 rows (1%), 128,978 rows remaining
# infraciclo_nao_loop <- infraciclo %>% filter(vg_loop == 'não')
# head(infraciclo_nao_loop)
# 
# # Resultado geral por pontos: 62.09994
# sum(infraciclo_nao_loop$pts_intsct) / sum(infraciclo_nao_loop$pts_viagem) * 100
# # Média de acurácia por pontos: 68.28531
# mean(infraciclo_nao_loop$acuracia_pontos)
# # Mediana por pontos: 76.14679
# median(infraciclo_nao_loop$acuracia_pontos)
# # Quantis
# quantile(infraciclo_nao_loop$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# #    25% 33.33%    50%    75%    85%    95%    99% 
# # 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 41.94  53.49  76.15 100.00 100.00 100.00 100.00 
# # Boxplot
# boxplot(infraciclo_nao_loop$acuracia_pontos)
# # Histograma
# hist(infraciclo_nao_loop$acuracia_pontos, main = 'Histograma viagens sem loop (avaliação por pontos)')
# # Histograma das distâncias
# hist(infraciclo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_loop_pontos <- 
#   data.frame(infraciclo_sem_loop = quantile(infraciclo_nao_loop$acuracia_pontos, 
#                                       probs = quantis2, 
#                                       na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 63.78259
# mean(infraciclo_nao_loop$acuracia_linhas)
# # Mediana  por extensão de linhas: 69.48258
# median(infraciclo_nao_loop$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(infraciclo_nao_loop$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 37.40  49.03  69.48  93.60  98.78 100.00 100.00
# # Boxplot
# boxplot(infraciclo_nao_loop$acuracia_linhas)
# # Histograma
# hist(infraciclo_nao_loop$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Loop (avaliação por extensão de linhas)')
# # Histograma das infracicloâncias
# hist(infraciclo_nao_loop$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_loop_linhas <- 
#   data.frame(infraciclo_sem_loop = quantile(infraciclo_nao_loop$acuracia_linhas, 
#                                       probs = quantis2, 
#                                       na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
infraciclo_nao_cm <- infraciclo %>% filter(vg_contramao == 'não')
nrow(infraciclo_nao_cm)
head(infraciclo_nao_cm)

# Resultado geral por pontos: 73.10924
sum(infraciclo_nao_cm$pts_intsct) / sum(infraciclo_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 81.62845
mean(infraciclo_nao_cm$acuracia_pontos)
# Mediana por pontos: 100
median(infraciclo_nao_cm$acuracia_pontos)
# Quantis
quantile(infraciclo_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 67.62  84.38 100.00 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(infraciclo_nao_cm$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(infraciclo_nao_cm$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9868421
# 2    25%  67.6190476
# 3 33.33%  84.3750000
# 4    50% 100.0000000
# 5 66.66% 100.0000000
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(infraciclo_nao_cm$acuracia_pontos)
# Histograma
hist(infraciclo_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(infraciclo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)

# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_cm_pontos <- 
#   data.frame(infraciclo_sem_cm = quantile(infraciclo_nao_cm$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')


# Média de acurácia  por extensão de linhas: 80.45305
mean(infraciclo_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 94.7522
median(infraciclo_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(infraciclo_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 68.03  79.28  94.75  99.03  99.96 100.00 100.00
data.frame(freq = quantile(infraciclo_nao_cm$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(infraciclo_nao_cm$acuracia_linhas), 
          .before = 1)
#     desc      freq
# 1    Min   1.74391
# 2    25%  68.02648
# 3 33.33%  79.27831
# 4    50%  94.75220
# 5 66.66%  98.19294
# 6    75%  99.02520
# 7    85%  99.95682
# 8    95% 100.00000
# 9   100% 100.00000
# Boxplot
boxplot(infraciclo_nao_cm$acuracia_linhas)
# Histograma
hist(infraciclo_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(infraciclo_nao_cm$distance, pch = 16, cex = 1, breaks = 100)


# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_cm_linhas <- 
#   data.frame(infraciclo_sem_cm = quantile(infraciclo_nao_cm$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')


nrow(infraciclo_nao_cm)
detour <- summary(infraciclo_nao_cm$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
# Fatores de detour menores do que 0.99
infraciclo_nao_cm %>% filter(detour_modalt < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.1024417
# 3  Median     1.2501587
# 4    Mean     1.4484225
# 5 3rd Qu.     1.4272530
# 6    Max.   336.0563818


# Isolar coluna de interesse - detour_mapmatching
x <- infraciclo_nao_cm$detour_modalt
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- infraciclo_nao_cm %>% filter(!(detour_modalt < (qnt[1] - H) | detour_modalt > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_modalt < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_modalt) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_modalt = Freq)
detour_sem_outliers
#   summary detour_modalt
# 1    Min.     0.5802978
# 2 1st Qu.     1.0982136
# 3  Median     1.2399028
# 4    Mean     1.2955160
# 5 3rd Qu.     1.4061727
# 6    Max.     2.4015453


# # -------------------------- Com contramão -------------------------------------
# 
# # Resultados para viagens com contramão
# # filter: removed 51,568 rows (40%), 78,186 rows remaining
# infraciclo_sim_cm <- infraciclo %>% filter(vg_contramao == 'sim')
# head(infraciclo_sim_cm)
# 
# # Resultado geral por pontos: 55.2781
# sum(infraciclo_sim_cm$pts_intsct) / sum(infraciclo_sim_cm$pts_viagem) * 100
# # Média de acurácia por pontos: 59.45782
# mean(infraciclo_sim_cm$acuracia_pontos)
# # Mediana por pontos: 60.71429
# median(infraciclo_sim_cm$acuracia_pontos)
# # Quantis
# quantile(infraciclo_sim_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo sem ajustar LTS:
# # 25% 33.33%    50%    75%    85%    95%    99% 
# # 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 33.82  42.59  60.71  87.26  96.20 100.00 100.00
# # Boxplot
# boxplot(infraciclo_sim_cm$acuracia_pontos)
# # Histograma
# hist(infraciclo_sim_cm$acuracia_pontos, main = 'Histograma Viagens com Contramão (avaliação por pontos)')
# # Histograma das distâncias
# hist(infraciclo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_com_cm_pontos <- 
#   data.frame(infraciclo_com_cm = quantile(infraciclo_sim_cm$acuracia_pontos, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 53.22722
# mean(infraciclo_sim_cm$acuracia_linhas)
# # Mediana  por extensão de linhas: 53.47679
# median(infraciclo_sim_cm$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(infraciclo_sim_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 27.14  35.95  53.48  78.77  89.22 100.00 100.00 
# # Boxplot
# boxplot(infraciclo_sim_cm$acuracia_linhas)
# # Histograma
# hist(infraciclo_sim_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens com Contramão (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(infraciclo_sim_cm$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_com_cm_linhas <- 
#   data.frame(infraciclo_com_cm = quantile(infraciclo_sim_cm$acuracia_linhas, 
#                                     probs = quantis2, 
#                                     na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas') # %>% 
# # pivot_longer(infraciclo_sim_cm, names_to = "perfil_rotas") %>%
# # pivot_wider(id_cols = perfil_rotas, names_from = quantiles)
# 
# 
# 
# #------ Sem loop, sem contramão, sem viagens exclusivas em parques -------------
# 
# # Resultados para viagens sem loop, sem contramão e sem serem exclusivas de parques
# # filter: removed 81,664 rows (63%), 48,090 rows remaining
# infraciclo_nao_cm_lp_pq <- infraciclo %>% filter(vg_loop == 'não' & vg_contramao == 'não' & so_parque == 'não')
# head(infraciclo_nao_cm_lp_pq) %>% select(trip_id, alt, vg_loop, vg_contramao, so_parque)
# 
# # Resultado geral por pontos: 77.80061
# sum(infraciclo_nao_cm_lp_pq$pts_intsct) / sum(infraciclo_nao_cm_lp_pq$pts_viagem) * 100
# # Média de acurácia por pontos: 83.80915
# mean(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# # Mediana por pontos: 100
# median(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# # Quantis
# quantile(infraciclo_nao_cm_lp_pq$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# # Modelo ajustando o LTS:
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 74.65  87.88 100.00 100.00 100.00 100.00 100.00
# # Boxplot
# boxplot(infraciclo_nao_cm_lp_pq$acuracia_pontos)
# # Histograma
# hist(infraciclo_nao_cm_lp_pq$acuracia_pontos, main = 'Histograma Viagens sem Contramão, Loop ou Exclusiva em Parques (avaliação por pontos)')
# # Histograma das distâncias
# hist(infraciclo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_cm_lp_pq_pontos <- 
#   data.frame(infraciclo_sem_cm_lp_pq = quantile(infraciclo_nao_cm_lp_pq$acuracia_pontos, 
#                                           probs = quantis2, 
#                                           na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_ptos')
# 
# 
# # Média de acurácia  por extensão de linhas: 80.50033
# mean(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# # Mediana  por extensão de linhas: 91.97572
# median(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# # Quantis  por extensão de linhas: 
# quantile(infraciclo_nao_cm_lp_pq$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
# #   25% 33.33%    50%    75%    85%    95%    99% 
# # 69.51  79.63  91.98  99.26 100.00 100.00 100.00
# # Boxplot
# boxplot(infraciclo_nao_cm_lp_pq$acuracia_linhas)
# # Histograma
# hist(infraciclo_nao_cm_lp_pq$acuracia_linhas, main = 'Histograma - Resultados Viagens sContramão, Loop ou Exclusiva em Parques (avaliação por extensão de linhas)')
# # Histograma das distâncias
# hist(infraciclo_nao_cm_lp_pq$distance, pch = 16, cex = 1, breaks = 100)
# 
# 
# # Guardar em dataframe para juntar aos demais resultados
# maior_infraciclo_sem_cm_lp_pq_linhas <- 
#   data.frame(infraciclo_sem_cm_lp_pq = quantile(infraciclo_nao_cm_lp_pq$acuracia_linhas, 
#                                           probs = quantis2, 
#                                           na.rm = TRUE)) %>% 
#   rownames_to_column(var = 'quantiles_linhas')
# 
# 
# #----------------------------- Resultados finais  ------------------------------
# 
# maior_infraciclo_pontos %>% 
#   left_join(maior_infraciclo_sem_loop_pontos,     by = 'quantiles_ptos') %>%
#   left_join(maior_infraciclo_sem_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(maior_infraciclo_com_cm_pontos,       by = 'quantiles_ptos') %>%
#   left_join(maior_infraciclo_sem_cm_lp_pq_pontos, by = 'quantiles_ptos')
# #   quantiles_ptos infraciclo_geral infraciclo_sem_loop infraciclo_sem_cm infraciclo_com_cm infraciclo_sem_cm_lp_pq
# # 1            25%         37.93103            38.29787          60.71429          31.66667                68.88889
# # 2         33.33%         48.78049            49.18033          77.27273          40.00000                83.63636
# # 3            50%         71.11111            71.42857          97.05882          57.35294                99.00990
# # 4         66.66%         91.54930            91.66667         100.00000          76.00000               100.00000
# # 5            75%         98.64865            98.79518         100.00000          85.00000               100.00000
# # 6            85%        100.00000           100.00000         100.00000          95.00000               100.00000
# # 7            95%        100.00000           100.00000         100.00000         100.00000               100.00000
# # 8            99%        100.00000           100.00000         100.00000         100.00000               100.00000
# 
# maior_infraciclo_linhas %>% 
#   left_join(maior_infraciclo_sem_loop_linhas,     by = 'quantiles_linhas') %>%
#   left_join(maior_infraciclo_sem_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(maior_infraciclo_com_cm_linhas,       by = 'quantiles_linhas') %>%
#   left_join(maior_infraciclo_sem_cm_lp_pq_linhas, by = 'quantiles_linhas')
# #   quantiles_linhas infraciclo_geral infraciclo_sem_loop infraciclo_sem_cm infraciclo_com_cm infraciclo_sem_cm_lp_pq
# # 1              25%         33.39980            33.26669          63.45275          24.61019                64.87885
# # 2           33.33%         44.20673            44.03210          75.55511          32.83474                76.33764
# # 3              50%         65.19329            64.98904          90.82545          49.88571                90.84034
# # 4           66.66%         84.42549            84.16653          97.30397          67.06924                97.09146
# # 5              75%         92.22889            91.98819          99.31136          76.09649                99.00952
# # 6              85%         98.37302            98.20655         100.00000          87.40098               100.00000
# # 7              95%        100.00000           100.00000         100.00000         100.00000               100.00000
# # 8              99%        100.00000           100.00000         100.00000         100.00000               100.00000






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
round(sum(peso$infra_ciclo) / sum(peso$distance) * 100, 2) # 30.95
round(sum(tempo$infra_ciclo) / sum(tempo$distance) * 100, 2) # 30.99
round(sum(dist$infra_ciclo) / sum(dist$distance) * 100, 2) # 29.37
round(sum(infraciclo$infra_ciclo) / sum(infraciclo$distance) * 100, 2) # 34.97

round(sum(peso_nao_cm$infra_ciclo) / sum(peso_nao_cm$distance) * 100, 2) # 42.94
round(sum(tempo_nao_cm$infra_ciclo) / sum(tempo_nao_cm$distance) * 100, 2) # 43.01
round(sum(dist_nao_cm$infra_ciclo) / sum(dist_nao_cm$distance) * 100, 2) # 41.34
round(sum(infraciclo_nao_cm$infra_ciclo) / sum(infraciclo_nao_cm$distance) * 100, 2) # 46.03

# Percentual da extensão de uso da infra cicloviária - somente ciclofaixas
round(sum(peso$ciclofaixa) / sum(peso$distance) * 100, 2) # 4.11
round(sum(tempo$ciclofaixa) / sum(tempo$distance) * 100, 2) # 4.11
round(sum(dist$ciclofaixa) / sum(dist$distance) * 100, 2) # 3.98
round(sum(infraciclo$ciclofaixa) / sum(infraciclo$distance) * 100, 2) # 5.09

round(sum(peso_nao_cm$ciclofaixa) / sum(peso_nao_cm$distance) * 100, 2) # 3.94
round(sum(tempo_nao_cm$ciclofaixa) / sum(tempo_nao_cm$distance) * 100, 2) # 3.95
round(sum(dist_nao_cm$ciclofaixa) / sum(dist_nao_cm$distance) * 100, 2) # 3.88
round(sum(infraciclo_nao_cm$ciclofaixa) / sum(infraciclo_nao_cm$distance) * 100, 2) # 4.59
