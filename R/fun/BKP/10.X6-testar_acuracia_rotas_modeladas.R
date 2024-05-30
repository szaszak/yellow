library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('googlePolylines')
detach('package:tidylog')


# ------------------------------------------------------------------------------
# Pastas e arquivos
# ------------------------------------------------------------------------------

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_detours      <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)

# pasta_dados <- sprintf('/Volumes/Expansion/BKP_Ciclocidade_4TB/BKP_Base_GtsRegionais/GitLab/yellow_dados')
# pasta_detours <- '/Users/renataeflavio/Downloads/valhalla/yellow_acuracia_rotas_modeladas'
# dir.create(pasta_detours, recursive = TRUE, showWarnings = FALSE)

# Puxar resultados de viagens, para características de cada uma - ex. com 
# contramão, com parques, com loop etc.
arq_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
arq_modelo <- read_delim(arq_modelo, delim = ';', col_types = "ccccdddddccddccdccdddccccccccccccc")
arq_modelo <- arq_modelo %>% select(trip_id, vg_contramao, vg_loop, vg_exper, vg_parques, dist_total) %>% distinct()
head(arq_modelo)


# Viagens modeladas a partir das origens e destinos originais - olhando os
# script anteriores, o arquivo 03_ttmatrix_viagens_originais.csv corresponde
# a todas as viagens que tiveram algum trecho considerado no modelo e, ao mesmo
# tempo, que não foram divididas em trechos menores (trip_id tem só _00)
viagens <- sprintf('%s/03_ttmatrix_viagens_originais.csv', pasta_detours)
viagens <- read_delim(viagens, delim = ';', col_types = "cccddddcc")
head(viagens)


# Puxar listagem de viagens originais (latlon virá do map matching)
pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
mm_files1 <- list.files(pasta_mm_1, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files2 <- list.files(pasta_mm_2, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files3 <- list.files(pasta_mm_3, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files <- rbind(mm_files1, mm_files2, mm_files3) %>% rename(arq = '.')

# Limpar ambiente
rm(mm_files1, mm_files2, mm_files3, pasta_mm_1, pasta_mm_2, pasta_mm_3, 
   pasta_base_modelo, pasta_map_matching)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Transforma linha de polyline para dataframe com latlongs
polyline_to_latlong <- function(polyline, trip_id){
  # polyline <- viagem$poly; trip_id <- viagem$trip_id
  this <- as.data.frame(decode(as.character(polyline)))
  this <- this %>% 
    # Formatação dos pontos está fora do lugar: 
    # de -235.641 para -23.5641 - ajeitar
    mutate(trip_id  = trip_id,
           lat = str_replace(lat, '\\.', ''),
           lon = str_replace(lon, '\\.', ''),
           lat = as.double(str_replace(lat, '-23', '-23.')),
           lon = as.double(str_replace(lon, '-46', '-46.')))
  
  return(this)
}


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


# ------------------------------------------------------------------------------
# Analisar percentual dos pontos GPS do map matching dentro das rotas modeladas
# ------------------------------------------------------------------------------

# Definir arquivo de saída
out_file <- sprintf('%s/05_acuracia_rotas_modeladas.csv', pasta_detours)

# Se arquivo existe, checar o que já foi rodado para desconsiderar
if (file.exists(out_file)) {
  
  tmp <- read_delim(out_file, delim = ';', col_types = "ciidcccc")
  
  viagens <- viagens %>% filter(!trip_id %in% tmp$trip_id)
  
  rm(tmp)
  
} 


# Para todas as viagens modeladas, calcular proporcional de pontos que se
# intersecionam com buffer criado a partir da rota original (map matching)
for (trip in  viagens$trip_id) {
  # trip <- '409002_00'
  # print(trip)
  
  # Manter somente polyline da viagem modelada de interesse
  viagem <- viagens %>% filter(trip_id == trip) %>% select(trip_id, poly)
  
  # Transformar polyline da viagem modelada em shapefile SIRGAS23S
  this <- polyline_to_latlong(viagem$poly, trip)
  # Se for somente 1 ou 0 pontos, ignorar
  if (nrow(this) <= 1) { next }
  this <- df_latlong_to_sf(this, trip)
  this <- st_transform(this, 31983)
  # mapview(this)
  
  # Aplicar buffer de 50 metros - a distância está sendo definida pela 
  # literatura, mas poderia ser menor até
  buffer <- st_buffer(this, 50)
  # mapview(buffer)
  
  
  # Abrir arquivo com latlon da viagem original - as coordenadas que serão
  # consideradas são as resultantes do map matching
  that <- mm_files %>% filter(str_detect(arq, trip)) %>% pull()
  that <- read_delim(that, delim = ';', col_types = "cidddddddicddcccddd")
  that <- that %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
  that <- that %>% distinct()
  that <- df_latlong_to_sf(that, trip, st_type = 'POINT')
  that <- st_transform(that, 31983) %>% add_column(idx = 1:nrow(.))
  # mapview(that, cex = 3, zcol = 'idx')
  
  # mapview(that, cex = 3, zcol = 'idx', layer.name = 'Rota original') + mapview(buffer) + mapview(this)
  
  
  # Quantos pontos do map matching estão dentro do buffer considerado?
  those <- filter(that, st_intersects(that, buffer, sparse = FALSE))
  
  # Puxar características das viagens: se tem contramão, se passou em parque,
  # se tem loop e se foi marcada como de ciclista experiente
  vg_carac <- arq_modelo %>% filter(trip_id == trip)
  
  
  # Guardar resultados em um dataframe
  resultado <- data.frame(trip_id      = trip,
                          pts_intsct   = nrow(those),
                          pts_viagem   = nrow(that),
                          acuracia  = nrow(those) / nrow(that) * 100)
  
  # Inserir percentual de concordância da rota modelada com a original
  resultado <- resultado %>% left_join(vg_carac, by = 'trip_id')
  
  
  # Gravar resultados
  if (file.exists(out_file)) {
    write_delim(resultado, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(resultado, out_file, delim = ';', append = FALSE)
  }
  
  
  # Limpar ambiente
  rm(viagem, this, buffer, that, those, vg_carac, resultado)
  
}



# (trip <- sample_n(viagens, 1) %>% select(trip_id) %>% pull())
# 
# viagem <- viagens %>% filter(trip_id == trip)
# viagem %>% select(trip_id, poly)
# 
# this <- polyline_to_latlong(viagem$poly, trip)
# this <- df_latlong_to_sf(this, trip)
# this <- st_transform(this, 31983)
# # mapview(this)
# 
# buffer <- st_buffer(this, 50)
# # mapview(buffer)
# 
# in_file2 <- mm_files %>% filter(str_detect(arq, trip)) %>% pull()
# that <- read_delim(in_file2, delim = ';', col_types = "cidddddddicddcccddd")
# that <- that %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
# that <- df_latlong_to_sf(that, trip, st_type = 'POINT')
# that <- st_transform(that, 31983) %>% add_column(idx = 1:nrow(.))
# # mapview(that, cex = 3, zcol = 'idx')
# 
# mapview(that, cex = 3, zcol = 'idx', layer.name = 'Rota original') + mapview(buffer) + mapview(this)
# 
# those <- filter(that, st_intersects(that, buffer, sparse = FALSE))
# nrow(those) / nrow(that) * 100


# ------------------------------------------------------------------------------
# Ver resultados
# ------------------------------------------------------------------------------

resultados <- read_delim(out_file, delim = ';', col_types = 'ciidcccc')
head(resultados)

quantis <- c(0.25, 0.3333, 0.5, 0.75, 0.85, 0.95, 0.99)


# Resultado geral: 58.89442
sum(resultados$pts_intsct) / sum(resultados$pts_viagem) * 100
# Média de acurácia: 66.4416
mean(resultados$acuracia)
# Mediana: 73.07692
median(resultados$acuracia)
# Quantis
quantile(resultados$acuracia, probs = quantis, na.rm = TRUE) %>% round(2)
# 25% 33.33%    50%    75%    85%    95%    99% 
# 38.46  50.00  73.08 100.00 100.00 100.00 100.00
# Boxplot
boxplot(resultados$acuracia)
# Histograma
hist(resultados$acuracia, main = 'Histograma - Resultados gerais')
# Histograma das distâncias
hist(resultados$dist_total, pch = 16, cex = 1, breaks = 100)

# # Plotar correlação entre as variáveis
# plot(resultados$dist_total, resultados$acuracia, cex = 1, pch = 20)
# abline(lm(resultados$acuracia ~ resultados$dist_total), col = "red", lwd = 3)
# # Correlação Pearson (https://r-coder.com/correlation-plot-r/)
# text(paste("Correlation:", round(cor(resultados$dist_total, resultados$acuracia), 2)), x = 25, y = 95)



# Resultados para viagens sem loop
res_nao_loop <- resultados %>% filter(vg_loop == 'não')

# Resultado geral: 59.39098
sum(res_nao_loop$pts_intsct) / sum(res_nao_loop$pts_viagem) * 100
# Média de acurácia: 66.65127
mean(res_nao_loop$acuracia)
# Mediana: 73.4375
median(res_nao_loop$acuracia)
# Quantis
quantile(res_nao_loop$acuracia, probs = quantis, na.rm = TRUE) %>% round(2)
# 25% 33.33%    50%    75%    85%    95%    99% 
# 38.78  50.00  73.44 100.00 100.00 100.00 100.00
# Boxplot
boxplot(res_nao_loop$acuracia)
# Histograma
hist(res_nao_loop$acuracia, main = 'Histograma somente para viagens sem loop')
# Histograma das distâncias
hist(res_nao_loop$dist_total, pch = 16, cex = 1, breaks = 100)


# # Plotar correlação entre as variáveis
# plot(res_nao_loop$dist_total, res_nao_loop$acuracia, cex = 1, pch = 20)
# abline(lm(res_nao_loop$acuracia ~ res_nao_loop$dist_total), col = "red", lwd = 3)
# # Correlação Pearson (https://r-coder.com/correlation-plot-r/)
# round(cor(res_nao_loop$dist_total, res_nao_loop$acuracia), 2)
# text(paste("Correlation:", round(cor(res_nao_loop$dist_total, res_nao_loop$acuracia), 2)), x = 25, y = 95)



# Resultados para viagens sem contramao - 60% da base tem contramão
res_nao_cm <- resultados %>% filter(vg_contramao == 'não')

# Resultado geral: 70.23178
sum(res_nao_cm$pts_intsct) / sum(res_nao_cm$pts_viagem) * 100
# Média de acurácia: 79.39418
mean(res_nao_cm$acuracia)
# Mediana: 97.5
median(res_nao_cm$acuracia)
# Quantis
quantile(res_nao_cm$acuracia, probs = quantis, na.rm = TRUE) %>% round(2)
# 25% 33.33%    50%    75%    85%    95%    99% 
# 62.50  79.45  97.50 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(res_nao_cm$acuracia)
# Histograma
hist(res_nao_cm$acuracia, main = 'Histograma somente para viagens sem contramão')
# Histograma das distâncias
hist(res_nao_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# # Plotar correlação entre as variáveis
# plot(res_nao_cm$dist_total, res_nao_cm$acuracia, cex = 1, pch = 20)
# abline(lm(res_nao_cm$acuracia ~ res_nao_cm$dist_total), col = "red", lwd = 3)
# # Correlação Pearson (https://r-coder.com/correlation-plot-r/)
# round(cor(res_nao_cm$dist_total, res_nao_cm$acuracia), 2)
# text(paste("Correlation:", round(cor(res_nao_cm$dist_total, res_nao_cm$acuracia), 2)), x = 25, y = 95)



# Resultados para viagens com contramao - 60% da base tem contramão
res_sim_cm <- resultados %>% filter(vg_contramao == 'sim')

# Resultado geral: 52.69799
sum(res_sim_cm$pts_intsct) / sum(res_sim_cm$pts_viagem) * 100
# Média de acurácia: 57.89866
mean(res_sim_cm$acuracia)
# Mediana: 58.13953
median(res_sim_cm$acuracia)
# Quantis
quantile(res_sim_cm$acuracia, probs = quantis, na.rm = TRUE) %>% round(2)
# 25% 33.33%    50%    75%    85%    95%    99% 
# 31.82  40.00  58.14  86.00  95.56 100.00 100.00 
# Boxplot
boxplot(res_sim_cm$acuracia)
# Histograma
hist(res_sim_cm$acuracia, main = 'Histograma somente para viagens com contramão')
# Histograma das distâncias
hist(res_sim_cm$dist_total, pch = 16, cex = 1, breaks = 100)

# # Plotar correlação entre as variáveis
# plot(res_sim_cm$dist_total, res_sim_cm$acuracia, cex = 1, pch = 20)
# abline(lm(res_sim_cm$acuracia ~ res_sim_cm$dist_total), col = "red", lwd = 3)
# # Correlação Pearson (https://r-coder.com/correlation-plot-r/)
# round(cor(res_sim_cm$dist_total, res_sim_cm$acuracia), 2)
# text(paste("Correlation:", round(cor(res_sim_cm$dist_total, res_sim_cm$acuracia), 2)), x = 25, y = 95)



# Resultados para viagens sem contramao e sem loop
res_nao_cmloop <- resultados %>% filter(vg_contramao == 'não' & vg_loop == 'não')

# Resultado geral: 71.3976
sum(res_nao_cmloop$pts_intsct) / sum(res_nao_cmloop$pts_viagem) * 100
# Média de acurácia: 79.82251
mean(res_nao_cmloop$acuracia)
# Mediana: 97.72727
median(res_nao_cmloop$acuracia)
# Quantis
quantile(res_nao_cmloop$acuracia, probs = quantis, na.rm = TRUE) %>% round(2)
# 25% 33.33%    50%    75%    85%    95%    99% 
# 63.83  80.40  97.73 100.00 100.00 100.00 100.00 
# Boxplot
boxplot(res_nao_cmloop$acuracia)
# Histograma
hist(res_nao_cmloop$acuracia, main = 'Histograma somente para viagens sem contramão e sem loop')
# Histograma das distâncias
hist(res_nao_cmloop$dist_total, pch = 16, cex = 1, breaks = 100)


# # Plotar correlação entre as variáveis
# plot(res_nao_cmloop$dist_total, res_nao_cmloop$acuracia, cex = 1, pch = 20)
# abline(lm(res_nao_cmloop$acuracia ~ res_nao_cmloop$dist_total), col = "red", lwd = 3)
# # Correlação Pearson (https://r-coder.com/correlation-plot-r/)
# round(cor(res_nao_cmloop$dist_total, res_nao_cmloop$acuracia), 2)
# text(paste("Correlation:", round(cor(res_nao_cmloop$dist_total, res_nao_cmloop$acuracia), 2)), x = 25, y = 95)
