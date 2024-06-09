library('tidyverse')
library('tidylog')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)

# Abrir resultados
resultados <- sprintf('%s/05_ttmatrix_viagens_modeladas_acuracia_infraciclo_carac_viagens.csv', pasta_orig_vs_mod)
resultados <- read_delim(resultados, delim = ';', col_types = 'cccddddddiiddddddccccdcdddd')
head(resultados)



# ------------------------------------------------------------------------------
# Ver resultados - Uso de infraestrutura cicloviária
# ------------------------------------------------------------------------------

# Calcular extensão total da rota
resultados <- resultados %>% mutate(ext_total = via_comum + infra_ciclo, .after = 'ciclofaixa')
head(resultados)


# Extensão em infraestrutura cicloviária geral: 5.76
round(sum(resultados$infra_ciclo) / sum(resultados$ext_total) * 100, 2)

# Extensão em infraestrutura cicloviária - somente ciclofaixas: 4.1
round(sum(resultados$ciclofaixa) / sum(resultados$ext_total) * 100, 2)


# Calcular em viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
resultados_nao_cm <- resultados %>% filter(vg_contramao == 'não')

# Extensão em infraestrutura cicloviária geral: 5.44
round(sum(resultados_nao_cm$infra_ciclo) / sum(resultados_nao_cm$ext_total) * 100, 2)

# Extensão em infraestrutura cicloviária - somente ciclofaixas: 3.97
round(sum(resultados_nao_cm$ciclofaixa) / sum(resultados_nao_cm$ext_total) * 100, 2)


# ------------------------------------------------------------------------------
# Acurácia das rotas modeladas
# ------------------------------------------------------------------------------


# Definir quantis para visualizar resultados
quantis <- c(0.25, 0.3333, 0.5, 0.75, 0.85, 0.95, 0.99)
quantis2 <- c(0.2500, 0.3333, 0.5, 0.6666, 0.75, 0.85, 0.95, 1)


# Resultado geral por pontos: 59.1096
sum(resultados$pts_intsct) / sum(resultados$pts_viagem) * 100
# Média de acurácia por pontos: 66.84476
mean(resultados$acuracia_pontos)
# Mediana por pontos: 73.52941
median(resultados$acuracia_pontos)
# Quantis por pontos
quantile(resultados$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2) %>% t() %>% t() %>% as.data.frame() %>% select(summary = V1, detour_mm = Freq)
# Modelo sem ajustar LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.53 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(resultados$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(resultados$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9787928
# 2    25%  38.4615385
# 3 33.33%  50.0000000
# 4    50%  73.5294118
# 5 66.66%  94.5945946
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(resultados$acuracia_pontos)
# Histograma
hist(resultados$acuracia_pontos, main = 'Histograma - Resultados gerais (avaliação por pontos)')
# Histograma das distâncias
hist(resultados$distance, pch = 16, cex = 1, breaks = 100)



# Média de acurácia  por extensão de linhas: 63.00203
mean(resultados$acuracia_linhas)
# Mediana  por extensão de linhas: 67.67668
median(resultados$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(resultados$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 34.97  46.36  67.68  95.89  99.42 100.00 100.00 
data.frame(freq = quantile(resultados$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(resultados$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.017624
# 2    25%  34.974627
# 3 33.33%  46.359630
# 4    50%  67.676679
# 5 66.66%  87.090930
# 6    75%  95.890810
# 7    85%  99.421230
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(resultados$acuracia_linhas)
# Histograma
hist(resultados$acuracia_linhas, main = 'Histograma - Resultados gerais (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(resultados$distance, pch = 16, cex = 1, breaks = 100)




# -------------------------- Sem contramão -------------------------------------

# Resultados para viagens sem contramão
# filter: removed 78,186 rows (60%), 51,569 rows remaining
resultados_nao_cm <- resultados %>% filter(vg_contramao == 'não')
head(resultados_nao_cm)

# Resultado geral por pontos: 70.69685
sum(resultados_nao_cm$pts_intsct) / sum(resultados_nao_cm$pts_viagem) * 100
# Média de acurácia por pontos: 80.13906
mean(resultados_nao_cm$acuracia_pontos)
# Mediana por pontos: 100
median(resultados_nao_cm$acuracia_pontos)
# Quantis
quantile(resultados_nao_cm$acuracia_pontos, probs = quantis, na.rm = TRUE) %>% round(2)
# Modelo sem ajustar LTS:
#   25% 33.33%    50%    75%    85%    95%    99% 
# 63.16  80.89 100.00 100.00 100.00 100.00 100.00 
data.frame(freq = quantile(resultados_nao_cm$acuracia_pontos, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(resultados_nao_cm$acuracia_pontos), 
          .before = 1)
#     desc        freq
# 1    Min   0.9868421
# 2    25%  63.1578947
# 3 33.33%  80.8917197
# 4    50% 100.0000000
# 5 66.66% 100.0000000
# 6    75% 100.0000000
# 7    85% 100.0000000
# 8    95% 100.0000000
# 9   100% 100.0000000
# Boxplot
boxplot(resultados_nao_cm$acuracia_pontos)
# Histograma
hist(resultados_nao_cm$acuracia_pontos, main = 'Histograma Viagens sem Contramão (avaliação por pontos)')
# Histograma das distâncias
hist(resultados_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)



# Média de acurácia  por extensão de linhas: 79.47286
mean(resultados_nao_cm$acuracia_linhas)
# Mediana  por extensão de linhas: 94.81953
median(resultados_nao_cm$acuracia_linhas)
# Quantis  por extensão de linhas: 
quantile(resultados_nao_cm$acuracia_linhas, probs = quantis, na.rm = TRUE) %>% round(2)
#   25% 33.33%    50%    75%    85%    95%    99% 
# 65.15  77.50  94.82  99.59 100.00 100.00 100.00
data.frame(freq = quantile(resultados_nao_cm$acuracia_linhas, probs = quantis2, na.rm = TRUE)) %>% 
  rownames_to_column(var = 'desc') %>% 
  add_row(desc = 'Min',
          freq = min(resultados_nao_cm$acuracia_linhas), 
          .before = 1)
#     desc       freq
# 1    Min   1.759818
# 2    25%  65.147506
# 3 33.33%  77.503889
# 4    50%  94.819527
# 5 66.66%  98.903737
# 6    75%  99.589877
# 7    85% 100.000000
# 8    95% 100.000000
# 9   100% 100.000000
# Boxplot
boxplot(resultados_nao_cm$acuracia_linhas)
# Histograma
hist(resultados_nao_cm$acuracia_linhas, main = 'Histograma - Resultados Viagens sem Contramão (avaliação por extensão de linhas)')
# Histograma das distâncias
hist(resultados_nao_cm$distance, pch = 16, cex = 1, breaks = 100)



# ------------------------------------------------------------------------------
# Calcular detour
# ------------------------------------------------------------------------------

nrow(resultados)
detour <- summary(resultados$detour_mod) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mod = Freq)
# Fatores de detour menores do que 0.99
resultados %>% filter(detour_mod < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_mod
# 1    Min.   0.000000
# 2 1st Qu.   1.181919
# 3  Median   1.344081
# 4    Mean   1.555417
# 5 3rd Qu.   1.590105
# 6    Max. 335.585497




# Isolar coluna de interesse - detour_mapmatching
x <- resultados$detour_mod
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_sem_outliers <- resultados %>% filter(!(detour_mod < (qnt[1] - H) | detour_mod > (qnt[2] + H)))
rm(x, qnt, H)
# Fatores de detour menores do que 0.99
detour_sem_outliers %>% filter(detour_mod < 0.99) %>% nrow()
# 
nrow(detour_sem_outliers)
detour_sem_outliers <- summary(detour_sem_outliers$detour_mod) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mod = Freq)
detour_sem_outliers
#   summary detour_mod
# 1    Min.   0.000000
# 2 1st Qu.   1.176325
# 3  Median   1.332674
# 4    Mean   1.416103
# 5 3rd Qu.   1.551168
# 6    Max.   2.814206





# -------------------------- Sem contramão -------------------------------------

nrow(resultados_nao_cm)
detour <- summary(resultados_nao_cm$detour_mod) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mod = Freq)
# Fatores de detour menores do que 0.99
resultados_nao_cm %>% filter(detour_mod < 0.99) %>% nrow()
detour
# ATENÇÃO: Fatores de detour menores do que 1 são por alguma pequena diferença
# entre o comprimento da linha reta e da rota calculada
#   summary detour_mod
#   summary  detour_mod
# 1    Min.   0.5802978
# 2 1st Qu.   1.0949605
# 3  Median   1.2377329
# 4    Mean   1.4343244
# 5 3rd Qu.   1.4099713
# 6    Max. 335.5854969


# Isolar coluna de interesse - detour_mapmatching
x <- resultados_nao_cm$detour_mod
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
# filter: removed 7,992 rows (6%), 121,763 rows remaining
detour_ncm_sem_outliers <- resultados_nao_cm %>% filter(!(detour_mod < (qnt[1] - H) | detour_mod > (qnt[2] + H)))
# Fatores de detour menores do que 0.99
resultados_nao_cm %>% filter(detour_mod < 0.99) %>% nrow()
rm(x, qnt, H)
# 
nrow(detour_ncm_sem_outliers)
detour_ncm_sem_outliers <- summary(detour_ncm_sem_outliers$detour_mod) %>% t() %>% t() %>% as.data.frame() %>% select(summary = Var1, detour_mod = Freq)
detour_ncm_sem_outliers
#   summary detour_mod
# 1    Min.  0.5802978
# 2 1st Qu.  1.0906130
# 3  Median  1.2279467
# 4    Mean  1.2800185
# 5 3rd Qu.  1.3906542
# 6    Max.  2.3544054