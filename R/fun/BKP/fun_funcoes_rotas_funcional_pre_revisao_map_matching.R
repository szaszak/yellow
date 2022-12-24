# -------------------------------------------
# Funções gerais de cálculo
# -------------------------------------------

# Pegar a duração total da viagem (em segundos) - o cálculo é feito pela 
# diferença entre o último ponto GPS e o primeiro
# get_elapsed_time <- function(trip_df, ts_col = 'timestamps') {
#   # Cálculo é feito pela diferença entre o último ponto GPS e o primeiro
#   trip_df <- trip_df %>% select({{ts_col}})
#   return(max(trip_df) - min(trip_df))
# }
get_elapsed_time <- function(trip_df) {
  return(max(trip_df$timestamps) - min(trip_df$timestamps))
}


# Criar colunas com cálculos de velocidade, distância e aceleração entre pontos
calcular_vel_dist_acel <- function(trip_df) {  
  # Referência: https://stackoverflow.com/questions/50007783/calculating-geographic-distance-between-two-rows-in-data-table

  trip_df <- trip_df %>%
    # Trazer os latlongs seguintes para uma coluna própria...
    mutate(lat_to = shift(lat, type = 'lead'),
           lon_to = shift(lon, type = 'lead'),
           # ... para poder calcular a distância entre eles...
           dist_m = distVincentyEllipsoid(cbind(lon, lat), cbind(lon_to, lat_to)),
           # ... a velocidade e a aceleração no trecho
           speed_kph = dist_m / time_s * 3.6,
           accel_ms2 = (dist_m / time_s) / time_s) %>%
    # Descartar colunas temporárias com latlongs do ponto seguinte
    select(-c(lat_to, lon_to)) %>% 
    # Converter a coluna de timestamps para dia-mes-ano-hora
    # add_column(time = as.POSIXlt(.$timestamps, origin = '1970-01-01'), .before = 'timestamps') %>% 
    # Substituir NAs por zeros em colunas específicas
    mutate(time_s = replace_na(time_s, 0),
           dist_m = replace_na(dist_m, 0),
           speed_kph = replace_na(speed_kph, 0),
           accel_ms2 = replace_na(accel_ms2, 0))
}


# Calcular distância percorrida, velocidade média e velocidade máxima
gerar_resumo_viagem <- function(trip_df){
  trip_duration_s <- max(trip_df$timestamps) - min(trip_df$timestamps)
  trip_distance_m <- round(sum(trip_df$dist_m), 2)
  avg_speed <- round(trip_distance_m / trip_duration_s * 3.6, 1)
  max_speed <- round(max(trip_df$speed_kph), 1)
  print(paste('[ORIG] distância: ', trip_distance_m, 'm',
              'tempo: ', round(trip_duration_s, 2), 'seg', 
              'velocidade: ', avg_speed, 'km/h',
              'velocidade_max: ', max_speed, 'km/h'))
}


# -------------------------------------------
# Filtros para serem aplicados em rotas
# -------------------------------------------

# Retirar pontos GPS que possuem as mesmas coordenadas latlong
retirar_pontos_sobrepostos <- function(trip_df) {
  trip_df <- trip_df %>% distinct(trip_id, lat, lon, .keep_all = TRUE)
  return(trip_df)
}


## Retirar viagens com tempos menores do que o tempo mínimo (sem segundos)
#excluir_viagens_curtas <- function(trip_df, tempo_min = 30) {

#  # Selecionar viagens que estão abaixo do tempo mínimo
#  viagens_tempos <- 
#    trip_df %>% 
#    # Isolar colunas de interesse
#    select(trip_id, timestamps) %>% 
#    group_by(trip_id) %>% 
#    # Calcular tempo total da viagem
#    summarise(trip_time = max(timestamps) - min(timestamps)) %>% 
#    # Filtrar viagens abaixo do tempo mínimo e isolar coluna de trip_id
#    filter(trip_time <= tempo_min) %>% 
#    select(trip_id)

#  # Retirar viagens abaixo do tempo mínimo
#  trip_df <- trip_df %>% filter(trip_id %nin% viagens_tempos$trip_id)
#  
#  return(trip_df)
#}


# Detectar se há quebras na viagem maiores do que o tempo máximo definido,
# retorna os índices dessas quebras
detectar_quebras_em_viagens <- function(trip_df, max_break_time = 90){
  # Houve quebra de viagens? Uma quebra é definida pela perda de sinal por mais
  # do que 90s, ou mais do que 18 sinais consecutivos. Uma viagem a 12 km/h, ou
  # 3,333 m/s percorreria 100m nesse intervalo de tempo, o que pode prejudicar
  # o map matching. No artigo dos técnicos da Microsoft, 90s é o último intervalo
  # de tempo do segundo bloco a partir do qual o map matching perde eficiência
  # https://www.microsoft.com/en-us/research/publication/hidden-markov-map-matching-noise-sparseness/
  
  # Detectar quantidade de quebras - resultado é o index de onde estão as quebras
  n_quebras <- 
    trip_df %>% 
    # Criar coluna de index
    mutate(index = 1:nrow(trip_df)) %>% 
    # Filtrar linhas em que tempo entre um ponto e outro é maior do que limite
    filter(time_s > max_break_time) %>% 
    # Manter somente a coluna de index
    select(index)
  
  return(n_quebras)
}


## Retirar quebras na viagem, retornando o maior trecho - válido somente para
## viagens no máximo duas quebras
#retirar_quebras_em_viagens <- function(trip_df, n_quebras){
#  # Se houver até duas quebras, pegar o maior trecho da viagem
#  if (nrow(n_quebras) == 0) {
#  
#    message('\nSem quebras de viagens detectadas, continuando...')    
#    return(trip_df)
#    
#  } else if (nrow(n_quebras) == 1) {
#  
#    message('\nDetectada uma quebra, dividindo as viagens e pegando a maior...')
#    
#    # Qual o tamanho das viagens se divididas?
#    size1 <- n_quebras$index - 1
#    size2 <- nrow(trip_df) - n_quebras$index
#    
#    # Pegar somente a maior viagem
#    if (size1 > size2) {
#      trip_df <- trip_df %>% slice(1:size1)
#    } else {
#      trip_df <- trip_df %>% slice((n_quebras$index + 1):nrow(trip_df))
#    }
#    
#    return(trip_df)
#    
#  } else if (nrow(n_quebras) == 2) {
#    message('\nDetectada duas quebras, dividindo as viagens e pegando a maior...')
#    
#    # Qual o tamanho das viagens se divididas?
#    size1 <- n_quebras$index[[1]] - 1
#    size2 <- (n_quebras$index[[2]] - 1) - (n_quebras$index[[1]] + 1)
#    size3 <- nrow(trip_df) - n_quebras$index[[2]]
#    
#    # Detectar a maior viagem e retornar
#    if (size1 > size2 & size1 > size3) { # primeira parte é maior
#      trip_df <- trip_df %>% slice(1:n_quebras$index[[1]] - 1)
#    } else if (size2 > size1 & size2 > size3) { # segunda parte é maior
#      trip_df <- trip_df %>% slice((n_quebras$index[[1]] + 1):(n_quebras$index[[2]] - 1))
#    } else { # terceira parte é maior
#      trip_df <- trip_df %>% slice((n_quebras$index[[2]] + 1):nrow(trip_df))
#    }
#    
#    return(trip_df)

#  } else { # n_quebras > 2

#    # Se há mais de duas quebras, descartar trip_df e retornar NULL
#    message('\nDetectadas mais de duas quebras, ignorar viagem...')
#    return(NULL)
#  }
#}


# Retirar outliers extremos, segundo proposto por Favero e Belfiore (2021)
retirar_outliers_extremos <- function(trip_df){
  # Alterado da implementação em:
  # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102
  
  # Isolar coluna de interesse - velocidade
  x <- trip_df$speed_kph
  # Pegar o primeiro e quarto quantis 
  qnt <- quantile(x, probs = c(0.25, 0.75))
  # Outliers extremos estão a 3 * IQR
  H <- 3 * IQR(x) 
  # Retirar outliers do dataframe
  trip_df <- trip_df %>% filter(!(speed_kph < (qnt[1] - H) | speed_kph > (qnt[2] + H)))
  
  # Isolar coluna de interesse - aceleração
  y <- trip_df$accel_ms2
  # Pegar o primeiro e quarto quantis 
  qnt <- quantile(y, probs = c(0.25, 0.75))
  # Outliers extremos estão a 3 * IQR
  H <- 3 * IQR(y) 
  # Retirar outliers do dataframe
  trip_df <- trip_df %>% filter(!(accel_ms2 < (qnt[1] - H) | accel_ms2 > (qnt[2] + H)))

}


# Remover outliers extremos de forma iterativa, até que não haja nenhum
retirar_outliers_extremos_iterativo <- function(viagem, max_iteracoes = 10) {
  
  iteracao <- 0; condicao <- TRUE
  while (condicao == TRUE) {
    
    # Atualizar iteração - viagens que passarem por 2 ou mais iterações serão 
    # marcadas como suspeitas de dispersão do sinal por multicaminho
    iteracao <- iteracao + 1
    # print(iteracao)
    
    # Registrar quantidade de pontos antes da remoção dos outliers
    n_row_antes <- nrow(viagem)
    
    # Criar/Atualizar coluna com o cálculo de tempo entre um ponto e outro
    viagem <- viagem %>% mutate(time_s = c(diff(timestamps), 0))
    # time_s = timestamps - lag(timestamps), # cálculo na linha posterior
    # time_s = c(diff(timestamps), 0),       # cálculo na mesma linha
    
    # Criar colunas com cálculos de velocidade, distância e aceleração entre pontos
    viagem <- calcular_vel_dist_acel(viagem)
    
    # Retirar outliers extremos de velocidade e aceleração
    viagem <- retirar_outliers_extremos(viagem)
    
    # Se quantidade de linhas antes/depois for a mesma (se não houve nova
    # remoção de outliers extremos), alterar condição para sair do loop
    if (n_row_antes == nrow(viagem) | iteracao == max_iteracoes) { condicao = FALSE }
    
  }
  
  # print(sprintf('Iterações outliers extremos: %i', iteracao))
  
  return(list(iteracao, viagem))
}


# Validar trechos de viagem com relação à duração, quantidade de pontos e 
# frequência de sinal mínimas. Retorna código de processamento
validar_trecho <- function(trecho, tempo_viagem, tempo_min_viagem, qtd_min_pontos, time_dif_col) {
  
  # Trecho tem duração maior ou igual ao mínimo?  
  if (tempo_viagem >= tempo_min_viagem) {
    cp_f <- '0'
  } else {
    cp_f <- 'X'
  }
  
  # Trecho tem quantidade de pontos maior ou igual à mínima?
  qtd_pontos <- nrow(trecho)
  if (qtd_pontos >= qtd_min_pontos) {
    cp_g <- '0'
  } else {
    cp_g <- 'X'
  }

  # Trecho tem frequência de sinal menor ou igual à mínima?
  freq_sinal <- mean(trecho[[time_dif_col]])
  if (freq_sinal <= interv_med_ptos) {
    cp_h <- '0'
  } else {
    cp_h <- 'X'
  }
  
  
  # Atualizar código de processamento do trecho
  cod_proc <- sprintf('%s%s%s', cp_f, cp_g, cp_h)
  
  return(cod_proc)

}


# Detectar quantidade de pontos do trecho/viagem que estão dentro de um centróide de x metros de raio
detectar_proporcao_centroide <- function(df_vg, tam_raio = 100) {
  
  # Guardar médias de lat e long em um dataframe simples
  centroid <- data.frame(lon = mean(df_vg$lon), lat = mean(df_vg$lat))
  # Transformar em sf como WGS84 e converter para SIRGAS200023S
  centroid <- centroid %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% st_transform(31983)
  
  # Criar um buffer de raio = tam_raio (em metros)
  centroid_buffer <- centroid %>% st_buffer(dist = tam_raio)
  
  # Converter dataframe da viagem também para SIRGAS
  df_vg <- df_vg %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% st_transform(31983)
  # mapview(df_vg, cex = 3) + mapview(centroid_buffer, cex = 3)
  
  # Quantos pontos deste trecho da viagem intersecionam com o buffer do centróide?
  df_intesec <- df_vg %>% filter(st_intersects(df_vg, centroid_buffer, sparse = FALSE))
  
  # Retornar proporção de pontos que intersecionam, frente ao total
  return(round(nrow(df_intesec) / nrow(df_vg) * 100, 2))
}


# Desenha os pontos originais do trecho e o buffer em torno do centróide
desenhar_centroide <- function(trecho, tam_raio = 100) {

  # Guardar médias de lat e long em um dataframe simples
  centroid <- data.frame(lon = mean(trecho$lon), lat = mean(trecho$lat))
  # Transformar em sf como WGS84 e converter para SIRGAS200023S
  centroid <- centroid %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% st_transform(31983)
  
  # Criar um buffer de raio = tam_raio (em metros)
  centroid_buffer <- centroid %>% st_buffer(dist = 100)
  
  # Converter dataframe da viagem também para SIRGAS
  trecho <- trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% st_transform(31983)
  
  # Desenhar pontos originais com centroide
  # https://r-spatial.github.io/mapview/articles/mapview_02-advanced.html
  mapview(trecho, cex = 3, col.regions = "red") + mapview(centroid_buffer, cex = 3)
  }


# ----------------------------------------------------------
# Salvar arquivos resultantes
# ----------------------------------------------------------
salvar_resultados_map_matching <- function(viagem, sel_trip, cp_a, shape_rota,
                                           pasta_viagens_gpkg, pasta_viagens_pngs, 
                                           pasta_viagens_csv1, pasta_viagens_csv2) {
  
  # Em formato shapefile .gpkg
  out_file1 <- sprintf('%s/%s_%s.gpkg', pasta_viagens_gpkg, sel_trip, cp_a)
  st_write(shape_rota, out_file1, driver = 'GPKG', append = FALSE, quiet = TRUE)
  
  # Salvar imagem da rota, para conferência
  png(sprintf('%s/%s_%s.png', pasta_viagens_pngs, sel_trip, cp_a))
  plot(shape_rota$geometry)
  dev.off()
  
  # Em dataframe .csv1 - trecho resumido processado, referente ao shape da rota
  out_file2 <- sprintf('%s/%s_%s.csv', pasta_viagens_csv1, sel_trip, cp_a)
  shape_rota <- shape_rota %>% st_drop_geometry()
  write_delim(shape_rota, out_file2, delim = ';')
  
  # Em dataframe .csv2 - trecho processado, com todos os pontos e atributos
  out_file3 <- sprintf('%s/%s_%s.csv', pasta_viagens_csv2, sel_trip, cp_a)
  write_delim(viagem, out_file3, delim = ';')
  
}

