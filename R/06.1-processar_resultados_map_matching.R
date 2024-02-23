# carregar bibliotecas
source('fun/setup.R')
source('fun/st_dbscan.R')
options(dplyr.summarise.inform = FALSE)
# library('matrixStats')

# -----------------------------------------------------------------------------
# Associar osm_id com qgis_id
# -----------------------------------------------------------------------------

associar_atrib_viario <- function(resultados, pontos_viario, atrib_viario) {
  
  # Da base de resultados do map matching, selecionar os osm_ids únicos
  sel_osm_ids <- resultados %>% distinct(edges.way_id)
  
  # Da base de centróides do viário, isolar somente os trechos que possuem os
  # osm_ids existentes nesta viagem - isso ajuda o processamento seguinte, do 
  # st_nearest_feature(), a rodar mais rápido
  viarios_sel <- 
    pontos_viario %>% 
    filter(osm_id %in% sel_osm_ids$edges.way_id)
  
  # viarios_sel %>% mapview(zcol = 'qgis_id')
  
  
  # Criar shape dos pontos associados ao viário - para isso, vamos usar os dados
  # de latlong resultantes do map matching em vez dos originais
  matched_points <- 
    resultados %>% 
    select(seq_order, edges.way_id, matched_points.lat, matched_points.lon) %>% 
    st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326)
  
  # mapview(matched_points)
  # mapview(matched_points) + mapview(viarios_sel, color = 'red', cex = 2)
  
  
  # Para cada osm_id existente na rota, vamos fazer uma associação de nearest
  # feature com o centróide do víário - o importante neste passo é que só sejam
  # associados os objetos que tenham o mesmo osm_id, por isso está sendo feito
  # neste momento com um loop. Talvez haja uma forma mais elegante de fazer isso
  tmp_df <- data.frame()
  for (id in sel_osm_ids$edges.way_id) {
    # print(id)
    # id <- sel_osm_ids$edges.way_id[1]
    # id <- '933975898'
    
    # Filtrar somente linhas de resultados e altimetrias com o mesmo osm_id
    tmp_resultados  <- matched_points %>% filter(edges.way_id == id)
    tmp_pontos_viario <- viarios_sel %>% filter(osm_id == id)
    
    # Os pontos resultantes do map matching sempre devem ser associados com os
    # pontos selecionados do mapa OSM a cada 2m:
    if (nrow(tmp_pontos_viario) != 0) {
      # Puxar os dados do ponto de altimetria mais próximo (do mesmo osm_id)
      tmp_out <- 
        tmp_pontos_viario %>% 
        slice(st_nearest_feature(tmp_resultados, tmp_pontos_viario)) %>% 
        st_drop_geometry() %>% 
        select(qgis_id, distance, elev_mdt) %>% 
        # Adicionar a coluna de sq_order, que vai permitir reordenar o dataframe
        add_column(seq_order = tmp_resultados$seq_order)
      
    } else {
      # Na raríssima exceção em que essa associação não puder ser feita, criar
      # um placeholder com qgis_id e elev_mdt iguais a NA. Esse placeholder vai
      # ter o mesmo tamanho esperado devido à coluna seq_order
      tmp_out <- data.frame(qgis_id  = NA,
                            distance = NA,
                            elev_mdt = NA, 
                            seq_order = tmp_resultados$seq_order)
    }
    
    
    
    # Resultado é um dataframe com as colunas qis_id e seq_order - a ordem de
    # sequência é a mesma dos dataframe de resultados e matched_points
    tmp_df <- tmp_df %>% rbind(tmp_out)
    
  }
  
  # Unir os qgis_id tendo como base a coluna de ordem de sequência (seq_order)
  resultados <- resultados %>% left_join(tmp_df, by = 'seq_order')
  # resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326) %>% mapview(zcol = 'qgis_id')
  
  
  # Finalmente, associar os atributos de viário à base
  resultados <- 
    resultados %>% 
    left_join(atrib_viario, by = c('edges.way_id' = 'osm_id', 'qgis_id'))
  
  
}



# -----------------------------------------------------------------------------
# Consertar o sinal do gradiente - considerar sentido da linha do shape de viário
# -----------------------------------------------------------------------------

# O gradiente inserido até o momento (elev_grad_sent_linha) considera o sentido
# do desenho do viário no shapefile do OSM - para vias unidirecionais, este
# sentido é o mesmo do fluxo de veículos. Já a coluna elev_mdt se refere ao 
# ponto de altimetria mais próximo ao ponto GPS resultante do processo de map 
# matching. É preciso agora avaliar o sinal de elevação (positivo/negativo) para
# ver se está correto e, se não estiver, atualizá-lo


consertar_sinal_declividade <- function(resultados) {
  
  # Usar o algoritmo do st_dbscan para reconhecer os clusters - especificamente,
  # queremos linhas que tenham o mesmo osm_id e qgis_id e estejam a uma distância
  # de seq_order máxima de 1 entre um e outro. Isso porque se um mesmo bloco de
  # osm_id e qgis_id se repetem (ver viagem 027463_002 como exemplo), a nova
  # passagem por um trecho repetido pertence a outro cluster
  
  # Inputs da função:
  # x      - Longitude, in degrees
  # y      - Latitude, in degrees
  # time   - Timestamps
  # eps1   - Maximum geographical coordinate (spatial) distance value
  # eps2   - Maximum non-spatial distance value
  # minpts - Minimum number of points within Eps1 and Eps2 distance
  clusters <- 
    st_dbscan(x    = resultados$edges.way_id,
              y    = resultados$qgis_id, 
              time = resultados$seq_order, 
              eps1 = 0,
              eps2 = 1, # distância em seq_order tem de ser 1
              minpts = 1, # cluster pode ter 1 ponto, sem problemas
              dry  = TRUE) %>% 
    as.data.frame() 
  
  
  # Quantos pontos há por cluster? Em outras palavras, quantos há por qgis_id?
  n_clusters <- 
    clusters %>% 
    group_by(cluster, .drop = FALSE) %>% 
    summarise(n_pontos = n())
  
  
  # Inserir colunas de clusters de elevação e quantidade de pontos por cluster
  resultados <- 
    resultados %>% 
    add_column(cluster = clusters$cluster, .after = 'seq_order') %>% 
    left_join(n_clusters, by = 'cluster')
  
  
  # Queremos agora descobrir se o deslocamento da pessoa é o mesmo do sentido
  # da linha traçada no shapefile. A forma de descobrir isso é usando a coluna
  # "distance", vinda do shape 
  sentido_desloc <- 
    resultados %>%
    # select(cluster, edges.way_id, qgis_id, elev_grad_sent_linha, distance, elev_mdt, n_pontos) %>%
    group_by(cluster, n_pontos, elev_grad_sent_linha) %>%
    summarise(dist_inicial = first(distance),
              dist_final   = last(distance),
              elev_inicial = first(elev_mdt),
              elev_final   = last(elev_mdt)) %>%
    # Sentido é igual ao do desenho da linha quando distance final é maior do
    # que distance inicial e oposto quando é o contrário. Porém, neste momento
    # alguns sentidos ficarão indefinidos - é o caso de qgis-id que possuem
    # somente 1 ponto ou nos quais os pontos estão sobrepostos, resultando em
    # uma associação de todos os pontos ao mesmo distance
    mutate(var_dist   = dist_final - dist_inicial,
           linha_sent = case_when(var_dist > 0  ~ 'des_linha',
                                  var_dist < 0  ~ 'opo_linha',
                                  var_dist == 0 ~ 'indef')) %>% 
    ungroup()
  
  # resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326) %>% mapview()
  
  # Para pontos de clusters que estão sem sentido definido, vamos avaliar a
  # variação de altimetria dos pontos imediatamente anterior e posterior a ele
  # para ver se estamos em uma subida ou em uma descida
  sentido_desloc <- 
    sentido_desloc %>% 
    # Puxar altimetria dos pontos anterior e posterior e calcular variação
    mutate(elev_pto_ant = shift(elev_final,   type = 'lag'),
           elev_pto_pos = shift(elev_inicial, type = 'lead'),
           var_elev = elev_pto_pos - elev_pto_ant) %>% 
    # Classificar subida ou descida
    mutate(elev_sent = case_when(var_elev < 0 ~ 'descida',
                                  var_elev > 0 ~ 'subida',
                                  TRUE ~ 'possivel_plano'))
  
  # Atualizar coluna de sentido da linha de acordo com o cálculo de diferenças
  # entre as altimetrias
  sentido_desloc <- 
    sentido_desloc %>% 
    select(c(cluster, n_pontos, elev_grad_sent_linha, linha_sent, elev_sent)) %>% 
    mutate(linha_sent = case_when(
      # Trechos de descida em que o gradiente é negativo estão seguindo o desenho da linha
      linha_sent == 'indef' & elev_sent == 'descida' & elev_grad_sent_linha < 0 ~ 'des_linha',
      # Trechos de descida em que o gradiente é positivo são contrários ao desenho da linha
      linha_sent == 'indef' & elev_sent == 'descida' & elev_grad_sent_linha > 0 ~ 'opo_linha',
      # Trechos de subida em que o gradiente é negativo são contrários ao desenho da linha
      linha_sent == 'indef' & elev_sent == 'subida' & elev_grad_sent_linha < 0 ~ 'opo_linha',
      # Trechos de subida em que o gradiente é positivo estão seguindo o desenho da linha
      linha_sent == 'indef' & elev_sent == 'subida' & elev_grad_sent_linha > 0 ~ 'des_linha',
      # Demais casos permaneceriam como indefinidos e muito provavelmente se referem
      # a trechos de plano. Isso porque são um dos dois casos: 
      # (1) pontos que estão a 1 ponto por trecho de viário (qgis_id) em que os 
      # pontos anteriores e posteriores não apresentam variação de altimetria; ou
      # (2) pontos estacionários ao início/fim de uma viagem (são clusters com mais
      # de 1 ponto), que não foram associados a mais de 1 qgis_id e que já saíram
      # para outro qgis_id ao se deslocar. Podemos deixá-los como idefinidos
      TRUE ~ linha_sent))
  
  # Revisar os sinais dos gradientes de declividade
  sentido_desloc <- 
    sentido_desloc %>% 
    # Deslocamentos no mesmo sentido do desenho da linha mantêm o sinal, senão 
    # o sinal é invertido
    mutate(elev_grad_rev = case_when(linha_sent == 'des_linha' ~ elev_grad_sent_linha,
                                     linha_sent == 'opo_linha' ~ elev_grad_sent_linha * -1),
           # Agora que temos certeza sobre o sentido da linha e o gradiente,
           # atualizar a classificação na coluna elev_sent. Trechos sem 
           # classificação vão ficar como NA
           elev_sent = case_when(elev_grad_rev < 0 ~ 'descida',
                                 elev_grad_rev > 0 ~ 'subida')) %>% 
    # Selecionar colunas de interesse, mantendo o dado sobre se a viagem está ou
    # não no sentido da via (para detectar contramão)
    select(cluster, linha_sent, elev_sent, elev_grad_rev)
  
  
  # Agregar gradiente de declividade com sinal corrigido ao dataframe principal
  resultados <- 
    resultados %>% 
    left_join(sentido_desloc, by = 'cluster')
  
  # resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326) %>% mapview(zcol = 'elev_grad_rev')
  
  
}


# -----------------------------------------------------------------------------
# Calcular medianas de velocidades
# -----------------------------------------------------------------------------

# Insere no dataframe as colunas as medianas de velocidades para 3 e 5 pontos
calcular_medianas_velocidades <- function(resultados, speed_col) {
  # speed_col <- 'speed_kph'
  
  # Criar colunas de velocidades, para cálculo das medianas - cada coluna seguinte
  # começa na linha de velocidade anterior da coluna atual
  median_speeds <- 
    resultados %>% 
    select(s1 = {{speed_col}}) %>% 
    mutate(s2 = shift(s1, type = 'lead'), 
           s3 = shift(s2, type = 'lead'),
           s4 = shift(s3, type = 'lead'),
           s5 = shift(s3, type = 'lead'))
  
  # Calcular medianas, tendo como base as 3 ou 5 colunas de velocidades
  # https://stackoverflow.com/questions/54366592/r-how-to-take-median-of-rows-in-dataframe
  median_speeds$ms3 <- apply(median_speeds[1:3], 1, median, na.rm = TRUE)
  median_speeds$ms5 <- apply(median_speeds[1:5], 1, median, na.rm = TRUE)
  
  # Agrupar resultados ao dataframe original
  resultados <- resultados %>% mutate(speed_m3_kph = median_speeds$ms3,
                                      speed_m5_kph = median_speeds$ms5)
  
}


# -----------------------------------------------------------------------------
# Agregar dados para modelo
# -----------------------------------------------------------------------------

# Gerar dataframe final com resultados agregados por cluster para serem usados no modelo
agregar_resultados <- function(resultados) {
  
  # Gerar um datafrane
  resultados <- 
    resultados %>% 
    group_by(trip_id, edges.way_id, qgis_id, cluster, linha_sent, elev_sent) %>% 
    summarise(edges.length = mean(edges.length),
              n_pontos     = first(n_pontos),
              ts_inicio    = min(timestamps),
              tempo_trecho = max(timestamps) - min(timestamps),
              qgisid_ext_m = first(length_m),
              vel_med_gps  = mean(speed_kph),
              vel_med_m3   = mean(speed_m3_kph),
              vel_med_m5   = mean(speed_m5_kph),
              # curv_h       = first(curv_h),
              # qtd_lotes    = first(lotes),
              # class_via    = first(class_via),
              # infra_ciclo  = first(infra_ciclo_2018),
              # via_restr    = first(via_restr),
              elev_grad_rev = first(elev_grad_rev)
    ) %>% 
    mutate(speed_kph = edges.length / tempo_trecho * 3.6, .after = 'qgisid_ext_m') %>% 
    arrange(cluster) %>% 
    ungroup()
  
  # # Agregar dados de data (conversão do timestamp), hora e dia da semana
  # resultados <- 
  #   resultados %>% 
  #   # Converter a coluna de timestamps para dia-mes-ano-hora
  #   add_column(dh_inicio = as.POSIXlt(.$ts_inicio, origin = '1970-01-01'), 
  #              .after = 'ts_inicio') %>% 
  #   # Inserir colunas de dia da semana e hora
  #   mutate(dia_semana = weekdays(dh_inicio, abbreviate = TRUE),
  #          fx_hora    = format(dh_inicio, "%H"),
  #          .after = 'dh_inicio')
  # 
  # 
  # # Retirar arcos do viário (qgis_id) com quantidade de pontos menor do que 2
  # resultados <- resultados %>% filter(n_pontos >= 2) %>% select(-cluster)
  
}


# -----------------------------------------------------------------------------
# Abrir todos resultados do map matching - base com pontos por trecho de viário
# -----------------------------------------------------------------------------

agrupar_para_modelos <- function(open_file) {
  # this_name <- '092831_00.csv'
  # this_name <- '034766_00.csv'
  # this_file <- arqs_a_processar %>% add_column(seq_order = 1:nrow(.)) %>% filter(csv_name == this_name) %>% select(seq_order) %>% pull()
  # open_file <- arqs_a_processar$csv_file[this_file]
  # open_file <- arqs_a_processar$csv_file[1]
  # filter(edges.way_id == '255818925')
  
  # Resgatar nome do arquivo para usar depois como referência
  file_id <- lapply(strsplit(open_file, split = '/'), tail, 1)
  file_id <- str_split(file_id, '\\.')[[1]][1]
  print(file_id)
  
  # Ler arquivo resultante do map matching
  resultados <- read_delim(open_file, 
                           delim = ';', 
                           col_types = 'cidddddddicddccciii',
                           col_select = c(trip_id,
                                          timestamps,
                                          # lat,
                                          # lon,
                                          # time_s,
                                          # dist_m,
                                          speed_kph,
                                          edges.way_id,
                                          # edges.id,
                                          edges.length,
                                          matched_points.lat, 
                                          matched_points.lon
                           ))
  
  # Atualizar o trip_id para incluir o identificador de trecho (_00, _01, etc)
  resultados <- 
    resultados %>% 
    mutate(trip_id = file_id) %>% 
    add_column(seq_order = 1:nrow(.), .after = 'trip_id')
  
  # Associar qgis_id ao trecho do viário e incluir os dados de atributo do viário
  resultados <- associar_atrib_viario(resultados, pontos_viario, atrib_viario)
  
  # Avaliar trajeto e consertar sinal de deciclidades
  resultados <- consertar_sinal_declividade(resultados)
  
  # Calcular medianas de velocidades
  resultados <- calcular_medianas_velocidades(resultados, speed_kph)
  
  # Agregar resultados para exportar para modelo
  resultados <- agregar_resultados(resultados)
  
  
  # Na rara ocasião em que pontos no nada não puderam ser associados de um
  # osm_id a um qgis_id (função associar_atrib_viario()), a rota passa a ser 
  # arriscada - checar se isso aconteceu
  qgis_id_vazios <- resultados %>% filter(is.na(qgis_id)) %>% nrow()
  
  # Se houve problemas na associação, descartar rota e salvar dataframe vazio -
  # os arquivos resultantes terão só o header e 174 bytes (serão 29 arquivos)
  if (qgis_id_vazios > 0) { resultados <- resultados %>% slice(0) }
  
  # Salvar .csv
  out_file <- sprintf('%s/%s_agrupado.csv', pasta_trechos_proc, file_id)
  write_delim(resultados, out_file, delim = ';')
  
  
}



# -----------------------------------------------------------------------------
# Estrutura de pastas e arquivos
# -----------------------------------------------------------------------------

# Dados a atualizar de acordo com o mês a rodar
ano_mes <- '201811'
ano_mes <- '201812'
ano_mes <- '201901'

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_mes_mapmatch <- sprintf("%s/%s", pasta_map_matching, ano_mes)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_trechos_proc <- sprintf("%s/A_trechos_processados/%s", pasta_modelos, ano_mes)
dir.create(pasta_trechos_proc, recursive = TRUE, showWarnings = FALSE)

# Abrir pontos de viário para associação entre osm_id (edges.way_id) e qgis_id
pontos_viario <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped_selecionados.gpkg', pasta_map_matching)
pontos_viario <- read_sf(pontos_viario)
# pontos_viario %>% filter(osm_id == '255818925') %>% mapview()

# Selecionar somente pontos do viário que serão usados neste batch
sel_osm_id <- sprintf('%s/%s_osmids_selecionados.csv', pasta_map_matching, ano_mes)
sel_osm_id <- read_delim(sel_osm_id, delim = ';', col_types = 'c')
pontos_viario <- pontos_viario %>% filter(osm_id %in% sel_osm_id$edges.way_id)
rm(sel_osm_id)


# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
# Deixar somente colunas de interesse para esta etapa - lembrando que aqui a 
# coluna de elevação se refere ao sentido da linha traçada no shapefile do OSM
atrib_viario <- atrib_viario %>% select('osm_id', 'qgis_id', 'length_m', 'elev_grad_sent_linha')


# # -----------------------------------------------------------------------------
# # Isolar ids de viagens a serem processadas com base nos shapes de dist_total
# # -----------------------------------------------------------------------------
# 
# Abrir arquivo de resultados agregados (rota completa)
open_file <- sprintf('%s/%s_map_matching_rotas_completas.csv', pasta_mes_mapmatch, ano_mes)
res_agreg <- read_delim(open_file, delim = ';', col_types = 'cciiiddddTT')
# 
# # res_agreg1 <- res_agreg
# # res_agreg2 <- res_agreg
# # res_agreg3 <- res_agreg
# # res_agreg <- res_agreg1 %>% rbind(res_agreg2) %>% rbind(res_agreg3)
# # head(res_agreg)
# # 
# # res_agreg %>% select(trip_id) %>% distinct()
# # res_agreg %>% 
# #   mutate(cod_proc = str_sub(cod_proc, 1, 2),
# #          id_trecho = str_c(trip_id, cod_proc, sep = '_')) %>% 
# #   select(id_trecho) %>% 
# #   distinct() %>% 
# #   nrow()
# # 
# # res_agreg %>% 
# #   mutate(cod_proc = str_sub(cod_proc, 1, 2),
# #          id_trecho = str_c(trip_id, cod_proc, sep = '_')) %>% 
# #   select(id_trecho) %>% 
# #   group_by(id_trecho) %>% 
# #   tally() %>% 
# #   filter( n > 1)
# 
# # 8 arquivos estão repetidos, provavelmente por inteseção entre as viradas de mês
# # id_trecho     n
# # <chr>     <int>
# #   1 002306_01     2
# # 2 035270_02     2
# # 3 035270_03     2
# # 4 056206_01     2
# # 5 166687_01     2
# # 6 198705_07     2
# # 7 234584_00     2
# # 8 323082_00     2
# # 9 331740_01     2
# # 10 379721_01     2
# # 11 382736_09     2
# 
# # head(res_agreg)
# # names(res_agreg)
# # hist(res_agreg$veloc_vg_kph, breaks = 1280, xlim = c(0, 30))
# # summary(res_agreg$veloc_vg_kph)
# 
# 
# # Aplicar filtros - trechos com velocidade média mínima e máxima
# # TODO - Documentar estes filtros
# res_agreg <- 
#   res_agreg %>% 
#   filter(veloc_vg_kph > 4 & veloc_vg_kph <= 30) %>% 
#   # filter(prop_centr_100 < 33) %>%
#   filter(prop_centr_100 < 50) %>% 
#   select(trip_id, cod_proc, tempo_total, dist_total, veloc_vg_kph)
# 
# # hist(res_agreg$veloc_vg_kph, breaks = 1280, xlim = c(0, 30))
# # summary(res_agreg$veloc_vg_kph)
# 
# 
# Construir base de ids a serem processados
ids_a_processar <-
  res_agreg %>%
  mutate(cod_proc = substr(cod_proc, 1, 2),
         csv_name = str_c(trip_id, '_', cod_proc, '.csv', sep = '')) %>%
  select(csv_name)

# Limpar ambiente
rm(open_file, res_agreg)
# head(ids_a_processar)


# -----------------------------------------------------------------------------
# Listar arquivos resultantes do map matching (base com pontos) a serem processados
# -----------------------------------------------------------------------------

# Puxar todos os nomes de arquivos dos .csv resultantes, com path completo
pasta_csv_bruto  <- sprintf('%s/viagens_processadas_csv', pasta_mes_mapmatch)
arqs_a_processar <- list.files(pasta_csv_bruto, pattern = '*\\.csv', full.names = TRUE)
arqs_a_processar <- data.frame(csv_file = arqs_a_processar)

# Criar coluna com o nome do arquivo sem path e filtrar somente os que passaram 
# pelo filtro da etapa anterior, de velocidades médias mínimas e máximas
arqs_a_processar <- 
  arqs_a_processar %>% 
  mutate(csv_name = lapply(strsplit(csv_file, split = '/'), tail, 1)) %>% 
  filter(csv_name %in% ids_a_processar$csv_name)

# Limpar ambiente
rm(pasta_csv_bruto,  ids_a_processar)
# head(arqs_a_processar)


# -----------------------------------------------------------------------------
# Processar todos os resultados do map matching e preparar base para modelo
# -----------------------------------------------------------------------------

# Atualizar arquivos a serem processados, removendo os que já foram
arqs_processados <- list.files(pasta_trechos_proc, pattern = '*\\.csv', full.names = FALSE)
arqs_processados <- arqs_processados %>% str_replace("_agrupado", "")
arqs_a_processar <- arqs_a_processar %>% filter(csv_name %nin% arqs_processados)
nrow(arqs_a_processar)
arqs_a_processar %>% head()

# Remover tidylog para todar base completa
detach("package:tidylog")

# Rodar para um arquivo de teste (pegar csv_name e usar dentro das funções)
# arqs_a_processar %>% sample_n(1)

# Rodar função para todos os arquivos- single thread
lapply(arqs_a_processar$csv_file, agrupar_para_modelos)

# # Rodar função para todos os arquivos- multi thread (Jupyter)
# (start = Sys.time())
# future::plan(future::multicore)
# invisible(future.apply::future_lapply(X   = arqs_a_processar$csv_file, 
#                                       FUN = agrupar_para_modelos, 
#                                       future.packages = c('dplyr', 'sf'), 
#                                       future.seed = TRUE))
# Sys.time() - start