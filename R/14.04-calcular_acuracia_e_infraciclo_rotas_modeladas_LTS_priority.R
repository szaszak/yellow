library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('googlePolylines')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_graphhopper  <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs      <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)

pasta_aop_2024_2028 <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_lts_priority  <- sprintf("%s/02_teste_lts_priority", pasta_aop_2024_2028)
pasta_osm_way_ids   <- sprintf("%s/01_osm_way_ids_rotas_modeladas", pasta_lts_priority)
# pasta_rotas_modalt <- sprintf("%s/02_rotas_modeladas_alternatives", pasta_lts_priority)
pasta_rotas_modalc <- sprintf("%s/03_rotas_modeladas_alter_carac", pasta_lts_priority)
dir.create(pasta_rotas_modalc, recursive = TRUE, showWarnings = FALSE)


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


# Insere as distâncias calculadas em vias comuns ou infra cicloviária no df original
calcular_dist_ciclo <- function(trip_id_alt) {
  # trip_id_alt <- tmpid
  line <- rotas_m %>% filter(tmp_id == trip_id_alt)
  
  # Puxar osm_ids da rota modelada
  trip_id_alt <- list.files(pasta_osm_way_ids, pattern = trip_id_alt, recursive = FALSE, full.names = TRUE)
  trip_id_alt <- read_delim(trip_id_alt, delim = ';', col_types = 'cc')
  # trip_way_ids <- paste0("'", trip_id_alt$osm_way_id, "'", collapse = ",")
  
  # Filtrar osm_ids da rota modelada do viário completo de SP
  viario_rota <- viario_sp %>% filter(osm_id %in% trip_id_alt$osm_way_id)
  # mapview(viario_rota)
  
  # Transformar o polyline da rota modelada em shapefile
  shape_rota <- polyline_to_latlong(line$poly, line$trip_id)
  shape_rota <- df_latlong_to_sf(shape_rota, line$trip_id)
  shape_rota <- shape_rota %>% st_transform(31983) %>% mutate(dist = round(st_length(.), 4))
  # mapview(shape_rota)
  
  
  # ----------------------------------------------------------------------------
  # Calcular detour
  # ----------------------------------------------------------------------------
  
  # Adicionar extensão da linha do shapefile da rota modelada e calcular detour
  line <- line %>% mutate(dist_linha_modalt = as.double(shape_rota$dist),
                          detour_modalt = dist_linha_modalt / dist_reta, 
                          .after = 'detour_mm')
  # line %>% select(trip_id, dist_total, dist_reta, dist_linha, detour_mm, dist_linha_modalt, detour_modalt)
  
  
  # ----------------------------------------------------------------------------
  # Calcular extensões percorridas em vias comuns e infra cicloviária
  # ----------------------------------------------------------------------------
  
  # A partir de um pequeno buffer criado no polyline da rota modelada, fazer uma 
  # interseção nos osm_ids originais - isso porque os osm_ids podem ter várias 
  # quadras e o segmento percorrido ser só um trechinho dele
  buffer_rota <- st_buffer(shape_rota, 2)
  viario_rota_cropped <- suppressWarnings(st_intersection(viario_rota, buffer_rota))
  # mapview(viario_rota_cropped) + mapview(buffer_rota)
  
  # Recaulcular as extensões dos arcos (no caso, as extensões percorridas dentro
  # daquele osm_id), transformar em dataframe e isolar colunas de interesse
  viario_rota_cropped <- viario_rota_cropped %>% mutate(new_ext = as.double(st_length(.)), .after = 'length_m')
  viario_rota_cropped <- viario_rota_cropped %>% st_drop_geometry() %>% select(osm_id, new_ext)
  
  # Juntar com dados da infraestrutura cicloviária
  viario_rota_cropped <- 
    viario_rota_cropped %>% 
    left_join(infra_ciclo, by = 'osm_id') %>% 
    mutate(infra_ciclo = ifelse(is.na(infra_ciclo), 'via_comum', infra_ciclo))
  
  # Fator de ajuste para as distâncias - vamos aplicar um proporcional geral a partir
  # da diferença entre a extensão total da rota modelada e a calculada agora
  fator_correcao <- line$distance / sum(viario_rota_cropped$new_ext)
  viario_rota_cropped <- viario_rota_cropped %>% mutate(ext_rev = new_ext * fator_correcao)
  
  # Agrupar extensões por tipo de viário percorrido
  viario_rota_cropped <- 
    viario_rota_cropped %>% 
    group_by(infra_ciclo) %>% 
    summarise(ext = sum(ext_rev)) %>% 
    ungroup() %>% 
    mutate(trip_id = line$trip_id) %>% 
    pivot_wider(id_cols = trip_id,
                names_from = 'infra_ciclo',
                values_from = ext)
  
  # Checar se todas as colunas de tipo de viário estão como colunas - se não, inserir
  for (i in c('ciclo_expressa', 'ciclo_comum', 'ciclofaixa', 'via_comum')) {
    if (!i %in% names(viario_rota_cropped)) {
      # Inserir nova coluna como NA (NA_real_, NA_character_)
      viario_rota_cropped <- viario_rota_cropped %>% mutate(!!i := 0)
    }
    
  }
  
  # Somar extensões percorridas em infra cicloviária
  viario_rota_cropped <- viario_rota_cropped %>% mutate(infra_ciclo = ciclo_expressa + ciclo_comum + ciclofaixa,
                                                        .after = 'via_comum')
  
  # Juntar todas as infos ao dataframe original e reordenar colunas
  trip_out <- 
    line %>% 
    left_join(viario_rota_cropped, by = 'trip_id') %>% 
    relocate(c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa), 
             .before = 'vg_contramao')
  
  
  # ----------------------------------------------------------------------------
  # Calcular acurácia da rota modelada - por pontos
  # ----------------------------------------------------------------------------
  
  # Aplicar buffer de 50 metros - a distância está sendo definida pela 
  # literatura, mas poderia ser menor até
  buffer <- st_buffer(shape_rota, 50)
  # mapview(buffer)
  
  # Abrir arquivo com latlon da viagem original - as coordenadas que serão
  # consideradas são as resultantes do map matching
  that <- mm_files %>% filter(str_detect(arq, line$trip_id)) %>% pull()
  that <- read_delim(that, delim = ';', col_types = "cidddddddicddcccddd")
  that <- that %>% select(trip_id, lat = matched_points.lat, lon = matched_points.lon)
  that <- that %>% distinct()
  that <- df_latlong_to_sf(that, trip, st_type = 'POINT')
  that <- st_transform(that, 31983) %>% add_column(idx = 1:nrow(.))
  # mapview(that, cex = 3, zcol = 'idx') + mapview(buffer)
  
  # Quantos pontos do map matching estão dentro do buffer considerado?
  those <- filter(that, st_intersects(that, buffer, sparse = FALSE))
  # mapview(those)
  
  # Guardar resultados em um dataframe
  resultado <- data.frame(trip_id      = line$trip_id,
                          pts_intsct   = nrow(those),
                          pts_viagem   = nrow(that),
                          acuracia_pontos  = nrow(those) / nrow(that) * 100)
  
  # Juntar todas as infos ao dataframe original e reordenar colunas
  trip_out <- 
    trip_out %>% 
    left_join(resultado, by = 'trip_id') %>% 
    relocate(c(pts_intsct, pts_viagem, acuracia_pontos), .after = 'speed')
  
  
  # ----------------------------------------------------------------------------
  # Calcular acurácia da rota modelada - por extensão
  # ----------------------------------------------------------------------------
  
  # Transformar pontos da rota original (map matching) em linestring
  this <- df_latlong_to_sf(that, line$trip_id)
  # mapview(this)
  
  # Quanto das linhas da rota original está dentro do buffer considerado?
  thou <- suppressWarnings(st_intersection(this, buffer))
  # mapview(thou) + mapview(buffer)
  
  # Calcular extensão dos trechos que estão dentro do buffer
  thou <- thou %>% mutate(ext_linhas = as.double(st_length(.)))
  thou <- thou %>% st_drop_geometry() %>% select(trip_id = trip_id.1 , ext_linhas)
  
  # Calcular acurácia das linhas - se a extensão das linhas for maior do que
  # a distância da rota, acurácia vai ser 100%
  thou <- thou %>% mutate(acuracia_linhas = ifelse(line$distance >= ext_linhas,
                                                   ext_linhas / line$distance * 100,
                                                   100))
  
  
  # Juntar todas as infos ao dataframe original e reordenar colunas
  trip_out <- 
    trip_out %>% 
    left_join(thou, by = 'trip_id') %>% 
    relocate(c(ext_linhas, acuracia_linhas), .after = 'speed')
  
  
  # Gravar resultados
  out_file <- sprintf('%s/%s_modalt_c.csv', pasta_rotas_modalc, line$tmp_id)
  write_delim(trip_out, out_file, delim = ';')
  
}



# ------------------------------------------------------------------------------
# Rede cicloviária 2019
# ------------------------------------------------------------------------------

# Abrir arquivo com osm_ids de ciclovias expressas em 2019 - a classificação que
# está no dataframe de atributos do viário, acima, contém osm_ids das vias 
# próximas, já que o map matching foi feito no modo 'pedestrian' e ao passar
# por essas vias, o osm_id que ia ser considerado era o do viário
ciclo_expressas <- sprintf('%s/00_atributos_ciclovias_expressas_2019.csv', pasta_gh_pbfs)
ciclo_expressas <- read_delim(ciclo_expressas, delim = ';', col_types = 'c') %>% distinct()
ciclo_expressas <- ciclo_expressas %>% mutate(infra_ciclo = 'ciclo_expressa')

# Abrir o arquivo com osm_ids de vias com ciclofaixa em 2019
ciclo_ciclofx <- sprintf('%s/02_atributos_ciclofaixas_lcn.csv', pasta_gh_pbfs)
ciclo_ciclofx <- read_delim(ciclo_ciclofx, delim = ';', col_types = 'c') %>% distinct()
ciclo_ciclofx <- ciclo_ciclofx %>% mutate(infra_ciclo = 'ciclofaixa')

# Abrir o arquivo com osm_ids de ciclovias comuns (não expressas) em 2019
ciclo_comuns <- sprintf('%s/01_atributos_ciclovias_comuns_2019.csv', pasta_gh_pbfs)
ciclo_comuns <- read_delim(ciclo_comuns, delim = ';', col_types = 'c') %>% distinct()

# Abrir arquivo de trechos de ciclovias comuns em que não há semáforos ou interseções
ciclo_ciclov_semsem <- sprintf('%s/03_atributos_ciclovias_comuns_sem_semaforo.csv', pasta_gh_pbfs)
ciclo_ciclov_semsem <- read_delim(ciclo_ciclov_semsem, delim = ';', col_types = 'c') %>% distinct()

# Ciclovias comuns serão ciclo_comuns + ciclovias sem semáforos ou interseções
ciclo_comuns <- rbind(ciclo_comuns, ciclo_ciclov_semsem)
ciclo_comuns <- ciclo_comuns %>% mutate(infra_ciclo = 'ciclo_comum')


# Juntar tudo em um único dataframe
infra_ciclo <- rbind(ciclo_expressas, ciclo_ciclofx, ciclo_comuns)
rm(ciclo_expressas, ciclo_ciclofx, ciclo_comuns, ciclo_ciclov_semsem)


# ------------------------------------------------------------------------------
# Bases de dados a serem utilizadas
# ------------------------------------------------------------------------------

# Puxar listagem de viagens originais (latlon virá do map matching)
pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
mm_files1 <- list.files(pasta_mm_1, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files2 <- list.files(pasta_mm_2, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files3 <- list.files(pasta_mm_3, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files <- rbind(mm_files1, mm_files2, mm_files3) %>% rename(arq = '.')
rm(mm_files1, mm_files2, mm_files3, pasta_mm_1, pasta_mm_2, pasta_mm_3)
# head(mm_files)


# Abrir origens e destinos das rotas iniciais da Yellow - são consideradas aqui
# (pelos scripts anteriores) as rotas que (a) tiveram algum trecho considerado
# no modelo; (b) que não foram divididas em trechos menores; e (c) em que o
# trecho único considerado é o inicial (possui trip_id com _00)
ods_vgs <- sprintf('%s/03_rotas_originais_infraciclo_detour_carac_viagens.csv', pasta_orig_vs_mod)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'cccdddddccccdddddddd')
# Remover viagens com origem e destino no mesmo qgis_id
ods_vgs <- ods_vgs %>% filter(qgis_id.x != qgis_id.y)
# Remover colunas de uso de infraestrutura cicloviária das rotas originais (map
# matching). Dados de latlong já vão estar presentes nas rotas modeladas
ods_vgs <- ods_vgs %>% select(-c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa,
                                 lat.x, lon.x, lat.y, lon.y))
head(ods_vgs)


# Abrir viário de SP com osm_ids
viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_osm_sp))
# Nos interessam os osm_ids e, talvez, a extensão original dos arcos da rede
viario_sp <- viario_sp %>% select(osm_id, length_m)
head(viario_sp)


# ------------------------------------------------------------------------------
# Calcular acurácia das rotas modeladas e distâncias percorridas em infra ciclo
# ------------------------------------------------------------------------------

# Abrir rotas modeladas com alternativas
rotas_m <- sprintf('%s/01_ttmatrix_rotas_modeladas_de_viagens_originais_com_alternativas.csv', pasta_lts_priority)
rotas_m <- read_delim(rotas_m, delim = ';', col_types = 'cccddddcdddd')
rotas_m <- rotas_m %>% select(-c(qgis_id.x, qgis_id.y))

# Remover rotas com origem muito perto do destino (qgis_id diferente, mas 
# distance igual a zero)
# rotas_m <- rotas_m %>% filter(distance != 0)

# Juntar com dados das viagens originais
rotas_m <- left_join(ods_vgs, rotas_m, by = 'trip_id')
rotas_m <- rotas_m %>% relocate(c(distance, weight, time, speed), .after = 'qgis_id.y')
head(rotas_m)

# Adicionar identificação da rota alternativa no dataframe, conforme o trip_id
# https://stackoverflow.com/questions/28647954/add-an-index-or-counter-to-a-dataframe-by-group-in-r
rotas_m <- rotas_m %>% group_by(trip_id) %>% mutate(alt = row_number(trip_id), .after = 'trip_id') %>% ungroup()
rotas_m <- rotas_m %>% mutate(tmp_id = str_c(trip_id, alt, sep = '_'), .before = 'trip_id')
head(rotas_m)


# suppressPackageStartupMessages(library('tidylog'))

# # Listar todos os arquivos de resultados em um dataframe único
# arqs_resultados <- data.frame(arq = list.files(pasta_rotas_modalc, recursive = FALSE, full.names = TRUE))
# arqs_resultados <- arqs_resultados %>% mutate(tmp_id = str_sub(arq, -24, -14))
# # head(arqs_resultados)

# # Remover arquivos que já foram executados
# rotas_m <- rotas_m %>% filter(!tmp_id %in% arqs_resultados$tmp_id)
# rm(arqs_resultados)
# head(rotas_m)

# Inserir cálculos de extensões percorridas em vias comuns ou infra cicloviária
# para cada uma das rotas calculadas
detach("package:tidylog")
for (tmpid in rotas_m$tmp_id) { 
  tmpid <- rotas_m$tmp_id[1]
  # Se der erro de Error in `select()`: ! Can't select columns that don't exist. ✖ Column `trip_id` doesn't exist.
  # é porque precisa descompactar as pastas, que foram compactadas:
  # pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
  # pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
  # pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
  calcular_dist_ciclo(tmpid) 
}




# ------------------------------------------------------------------------------
# Juntar todos os resultados
# ------------------------------------------------------------------------------

# Arquivo de saída
out_file <- sprintf('%s/02_ttmatrix_rotas_modeladas_alternativas_acuracia_infraciclo_carac_viagens.csv', pasta_lts_priority)

# Listar todos os arquivos de resultados em um dataframe único
arqs_resultados <- data.frame(arq = list.files(pasta_rotas_modalc, recursive = FALSE, full.names = TRUE))

# Resultados já vão estar ordenados conforme o nome, o que significa 
# que já estarão ordenados com relação ao trip_id e à coluna alt
for (arq in arqs_resultados$arq) {
  # Abrir arquivo de resultados
  arq <- read_delim(arq, delim = ';', col_types = cols(.default = "c"))
  
  # Guardar resultados 
  if (file.exists(out_file)) {
    write_delim(arq, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(arq, out_file, delim = ';', append = FALSE)
  }
  
}