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
    
    # Puxar os dados do ponto de altimetria mais próximo (do mesmo osm_id)
    tmp_out <- 
      tmp_pontos_viario %>% 
      slice(st_nearest_feature(tmp_resultados, tmp_pontos_viario)) %>% 
      st_drop_geometry() %>% 
      select(qgis_id, elev_mdt) %>% 
      # Adicionar a coluna de sq_order, que vai permitir reordenar o dataframe
      add_column(seq_order = tmp_resultados$seq_order)
    
    # Resultado é um dataframe com as colunas qis_id e seq_order - a ordem de
    # sequência é a mesma dos dataframe de resultados e matched_points
    tmp_df <- tmp_df %>% rbind(tmp_out)
    
  }
  
  # Unir os qgis_id tendo como base a coluna de ordem de sequência (seq_order)
  resultados <- resultados %>% left_join(tmp_df, by = 'seq_order')
  # resultados %>% st_as_sf(coords = c('matched_points.lon', 'matched_points.lat'), crs = 4326) %>% mapview(zcol = 'qgis_id')
  
  # Inserir coluna com variação de elecação entre pontos
  resultados <- resultados %>% mutate(altimetria_var = c(diff(elev_mdt), 0))
  
  # Finalmente, associar os atributos de viário à base
  resultados <- 
    resultados %>% 
    left_join(atrib_viario, by = c('edges.way_id' = 'osm_id', 'qgis_id'))
  
  
}



# -----------------------------------------------------------------------------
# Consertar o sinal do gradiente
# -----------------------------------------------------------------------------

# O gradiente inserido até o momento ignora o sentido da via - ele foi definido
# apenas como o ponto de altimetria mais próximo ao ponto GPS resultante do
# processo de map matching. Este ponto poderia ser positivo ou negativo de forma
# arbitrária. Precisamos endereçar isso


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
              eps2 = 1, # distância em seq_order tem de ser 1, se não
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
  
  
  # Agrupar trechos conforme seus clusters e somar as variações de isovalor. O que
  # queremos é saber se a variação será positiva ou negativa, para então aplicar
  # este sinal à coluna de elev_grad
  declividades_revisadas <- 
    resultados %>% 
    select(cluster, edges.way_id, qgis_id, elev_grad_abs, altimetria_var) %>% 
    group_by(cluster, edges.way_id, qgis_id, elev_grad_abs) %>% 
    summarise(alt_var_trecho = sum(altimetria_var)) %>%
    mutate(elev_grad = ifelse(alt_var_trecho >= 0, elev_grad_abs, elev_grad_abs * -1),
           elev_sent = ifelse(elev_grad >= 0, 'subida', 'descida')) %>% 
    select(-c(elev_grad_abs, alt_var_trecho))
  
  
  # Agregar gradiente de declividade com sinal corrigido ao dataframe principal
  resultados <- 
    resultados %>% 
    left_join(declividades_revisadas, by = c('edges.way_id', 'qgis_id', 'cluster'))
  
  # resultados %>% mapview(zcol = 'elev_grad')
  
  
}


# -----------------------------------------------------------------------------
# Calcular medianas de velocidades
# -----------------------------------------------------------------------------

# Insere no dataframe as colunas as medianas de velocidades para 3 e 5 pontos
calcular_medianas_velocidades <- function(df, speed_col) {
  # df <- resultados; speed_col <- 'speed_kph'
  
  # Criar colunas de velocidades, para cálculo das medianas - cada coluna seguinte
  # começa na linha de velocidade anterior da coluna atual
  median_speeds <- 
    df %>% 
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
  df <- df %>% mutate(speed_m3_kph = median_speeds$ms3,
                      speed_m5_kph = median_speeds$ms5)
  
}


# -----------------------------------------------------------------------------
# Agregar dados para modelo
# -----------------------------------------------------------------------------

# Gerar dataframe final com resultados agregados para serem udados no modelo
agregar_resultados <- function(resultados) {
  
  # Gerar um datafrane
  resultados <- 
    resultados %>% 
    group_by(trip_id, edges.way_id, qgis_id, cluster, elev_sent) %>% 
    summarise(n_pontos     = first(n_pontos),
              ts_inicio    = min(timestamps),
              tempo_trecho = max(timestamps) - min(timestamps),
              length_m     = first(length_m),
              vel_med_gps  = mean(speed_kph),
              vel_med_m3   = mean(speed_m3_kph),
              vel_med_m5   = mean(speed_m5_kph),
              # curv_h       = first(curv_h),
              # qtd_lotes    = first(lotes),
              # class_via    = first(class_via),
              # infra_ciclo  = first(infra_ciclo_2018),
              # via_restr    = first(via_restr),
              elev_grad    = first(elev_grad)
    ) %>% 
    mutate(speed_kph = length_m / tempo_trecho * 3.6, .after = 'length_m') %>% 
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
  # this_name <- '090771_01.csv'
  # this_file <- arqs_a_processar %>% add_column(seq_order = 1:nrow(.)) %>% filter(csv_name == this_name) %>% select(seq_order) %>% pull()
  # open_file <- arqs_a_processar$csv_file[this_file]
  # filter(edges.way_id == '255818925')
  
  # Resgatar nome do arquivo para usar depois como referência
  file_id <- lapply(strsplit(open_file, split = '/'), tail, 1)
  file_id <- str_split(file_id, '\\.')[[1]][1]
  print(file_id)
  
  
  resultados <- read_delim(open_file, 
                           delim = ';', 
                           col_types = 'cidddddddicddccciiiddcc',
                           col_select = c(trip_id,
                                          timestamps,
                                          # lat,
                                          # lon,
                                          # time_s,
                                          # dist_m,
                                          speed_kph,
                                          edges.way_id,
                                          # edges.id,
                                          # ISOVALOR,
                                          matched_points.lat, 
                                          matched_points.lon,
                                          # isovalor_var
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
  
  
  # Salvar .csv
  out_file <- sprintf('%s/%s_agrupado.csv', pasta_trechos_proc, file_id)
  write_delim(resultados, out_file, delim = ';')
  
  
}



# -----------------------------------------------------------------------------
# Estrutura de pastas e arquivos
# -----------------------------------------------------------------------------

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_trechos_proc <- sprintf("%s/01_trechos_processados", pasta_modelos)
dir.create(pasta_trechos_proc, recursive = TRUE, showWarnings = FALSE)

# Abrir centroides de viário para associação entre osm_id (edges.way_id) e qgis_id
# pontos_viario <- sprintf('%s/viario_pontos_para_associacao_osmid_qgisid_2m_com_z.gpkg', pasta_atrib_viario)
pontos_viario <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped.gpkg', pasta_elevacao)
pontos_viario <- read_sf(pontos_viario)
# pontos_viario %>% filter(osm_id == '255818925') %>% mapview()

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccdddiccc')
# Deixar somente colunas de interesse para esta etapa
atrib_viario <- atrib_viario %>% select('osm_id', 'qgis_id', 'length_m', 'elev_grad_abs')


# -----------------------------------------------------------------------------
# Isolar ids de viagens a serem processadas com base nos shapes de distância
# -----------------------------------------------------------------------------

pasta_base1 <- sprintf("%s/05_testes_viagens_20181111", pasta_dados)
open_file1  <- sprintf('%s/viagens_processadas_todas.csv', pasta_base1)
result_agreg1 <- read_delim(open_file1, delim = ';', col_types = 'cciidddddiddiiiiiiicc')

pasta_base2 <- sprintf("%s/05_testes_viagens_20181112-20181117", pasta_dados)
open_file2  <- sprintf('%s/viagens_processadas_todas.csv', pasta_base2)
result_agreg2 <- read_delim(open_file2, delim = ';', col_types = 'cciidddddiddiiiiiiicc')

result_agreg <- rbind(result_agreg1, result_agreg2)


# head(result_agreg)
# names(result_agreg)

# Aplicar filtros - trechos com velocidade média mínima e máxima
result_agreg <- 
  result_agreg %>% 
  filter(veloc > 4 & veloc <= 30) %>% 
  # filter(prop_centr_100 < 33) %>%
  filter(prop_centr_100 < 50) %>% 
  select(trip_id, cod_proc, tempo, dist, veloc)

ids_a_processar <- 
  result_agreg %>% 
  mutate(cod_proc = substr(cod_proc, 1, 2),
         csv_name = str_c(trip_id, '_', cod_proc, '.csv', sep = '')) %>% 
  select(csv_name)

head(ids_a_processar)

# Limpar ambiente
rm(result_agreg1, result_agreg2, open_file1, open_file2, result_agreg)


# -----------------------------------------------------------------------------
# Listar arquivos resultantes do map matching (base com pontos) a serem processados
# -----------------------------------------------------------------------------

# Puxar todos os nomes de arquivos dos .csv resultantes, com path completo
pasta_csv_bruto1 <- sprintf('%s/viagens_processadas_csv2', pasta_base1)
result_arqs1 <- list.files(pasta_csv_bruto1, pattern = '*\\.csv', full.names = TRUE)
result_arqs1 <- data.frame(csv_file = result_arqs1)

pasta_csv_bruto2 <- sprintf('%s/viagens_processadas_csv2', pasta_base2)
result_arqs2 <- list.files(pasta_csv_bruto2, pattern = '*\\.csv', full.names = TRUE)
result_arqs2 <- data.frame(csv_file = result_arqs2)

# Juntar tudo em um único dataframe
arqs_a_processar <- rbind(result_arqs1, result_arqs2)


# Criar coluna com o nome do arquivo sem path e filtrar somente os que passaram 
# pelo filtro da etapa anterior, de velocidades médias mínimas e máximas
arqs_a_processar <- 
  arqs_a_processar %>% 
  mutate(csv_name = lapply(strsplit(csv_file, split = '/'), tail, 1)) %>% 
  filter(csv_name %in% ids_a_processar$csv_name)


# Limpar ambiente
rm(result_arqs1, result_arqs2, pasta_csv_bruto1, pasta_csv_bruto2, ids_a_processar)
head(arqs_a_processar)




# -----------------------------------------------------------------------------
# Processar todos os resultados do map matching e preparar base para modelo
# -----------------------------------------------------------------------------

# Atualizar arquivos a serem processados, removendo os que já foram
arqs_processados <- list.files(pasta_trechos_proc, pattern = '*\\.csv', full.names = FALSE)
arqs_processados <- arqs_processados %>% str_replace("_agrupado", "")
arqs_a_processar <- arqs_a_processar %>% filter(csv_name %nin% arqs_processados)
arqs_a_processar %>% head()

detach("package:tidylog")

# TODO: future::apply + rodar no Jupyter para multithread
lapply(arqs_a_processar$csv_file, agrupar_para_modelos)



