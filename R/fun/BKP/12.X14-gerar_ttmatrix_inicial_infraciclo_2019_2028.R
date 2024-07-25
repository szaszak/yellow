# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('googlePolylines')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_alter)

ano <- '2019'; min_thres <- 40; sec_thres <- min_thres * 60
# ano <- '2028'; min_thres <- 40; sec_thres <- min_thres * 60

if (ano == '2019') {
  pasta_tmp_osmids  <- sprintf("%s/E_%s_osm_way_ids_tmp_%s_min", pasta_aoprv_alter, ano, min_thres)
} else if (ano == '2028') {
  pasta_tmp_osmids  <- sprintf("%s/F_%s_osm_way_ids_tmp_%s_min", pasta_aoprv_alter, ano, min_thres)
}
dir.create(pasta_tmp_osmids, recursive = TRUE, showWarnings = FALSE)
rm(min_thres)

if (ano == '2019') {
  pasta_tmp_divididas <- sprintf("%s/X_%s_tmp_base_dividida", pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  pasta_tmp_divididas <- sprintf("%s/Y_%s_tmp_base_dividida", pasta_aoprv_alter, ano)
}


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
dist_ciclo_sep_osmids <- function(trip_id_alt) {
  # trip_id_alt <- rotas_ciclo$alt_id[1]
  # 89a81044d93ffff-89a81046d53ffff-1
  # trip_id_alt <- '000ed3-001d3b-1'
  
  line    <- rotas_ciclo %>% filter(alt_id == trip_id_alt)
  line_id <- str_sub(line$alt_id, 1, 6)
  # line$poly
  
  # Puxar osm_ids da rota modelada
  if (ano == '2019') {
    trip_osm_ids <- list.files(pasta_ids_aopt_19, pattern = line_id, recursive = FALSE, full.names = TRUE) 
  } else if (ano == '2028') {
    trip_osm_ids <- list.files(pasta_ids_aopt_28, pattern = line_id, recursive = FALSE, full.names = TRUE) 
  }
  
  trip_osm_ids <- read_delim(trip_osm_ids, delim = ';', col_types = 'ciic')
  
  # Criar um id que contemple o número da rota alternativa
  trip_osm_ids <- trip_osm_ids %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'))
  
  # Isolar a viagem de interesse
  trip_osm_ids <- trip_osm_ids %>% filter(alt_id == trip_id_alt)
  
  # Remover osm_ids repetidos - no momento do buffer com o shapefile da rota,
  # o intersection vai pegar todos os trechos que os osm_ids aparecem e vai
  # somar essas distâncias. Osm_ids repetidos são quando a rota entra, sai e
  # entra de novo em um mesmo osm_id
  trip_osm_ids <- trip_osm_ids %>% distinct(osm_way_id)
  # trip_way_ids <- paste0("'", trip_osm_ids$osm_way_id, "'", collapse = ",")
  
  # Juntar infraestrutura cicloviária do ano de referência
  trip_osm_ids <- 
    trip_osm_ids %>% 
    left_join(infra_ciclo, by = c('osm_way_id' = 'osm_id')) %>% 
    mutate(infra_ciclo = ifelse(is.na(infra_ciclo), 'via_comum', infra_ciclo))
  
  # Essa rota passa por estrutura cicloviária?
  qtd_infraciclo <- trip_osm_ids %>% filter(str_detect(infra_ciclo, 'ciclo')) %>% nrow()
  
  # # Depurar aviso de...
  # # In left_join(., viario_rota_cropped, by = c(osm_way_id = "osm_id")) :
  # # Detected an unexpected many-to-many relationship between `x` and `y`.
  # # ℹ Row 1 of `x` matches multiple rows in `y`.
  # # ℹ Row 32 of `y` matches multiple rows in `x`.
  # osm_ids_repetidos <- trip_osm_ids %>% group_by(osm_way_id) %>% tally() %>% filter(n > 1) %>% nrow()
  # if (osm_ids_repetidos > 0 & qtd_infraciclo > 0) { print(trip_id_alt) }
  # # trip_osm_ids %>% filter(osm_way_id == '928656996')
  
  if (qtd_infraciclo > 0) {
    # Se passa, calcular as extensões por estrutura cicloviária
    
    # ----------------------------------------------------------------------------
    # Calcular extensões percorridas em vias comuns e infra cicloviária
    # ----------------------------------------------------------------------------
    
    # Filtrar osm_ids da rota modelada do viário completo de SP
    viario_rota <- viario_sp %>% filter(osm_id %in% trip_osm_ids$osm_way_id)
    # mapview(viario_rota)
    
    # Transformar o polyline da rota modelada em shapefile
    shape_rota <- polyline_to_latlong(line$poly, trip_id_alt)
    shape_rota <- df_latlong_to_sf(shape_rota, trip_id_alt)
    shape_rota <- shape_rota %>% st_transform(31983) %>% mutate(dist = round(st_length(.), 4))
    # mapview(shape_rota)
    
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
    trip_osm_ids <- trip_osm_ids %>% left_join(viario_rota_cropped, by = c('osm_way_id' = 'osm_id'))
    
    # Fator de ajuste para as distâncias - vamos aplicar um proporcional geral a partir
    # da diferença entre a extensão total da rota modelada e a calculada agora
    fator_correcao <- line$distance / sum(viario_rota_cropped$new_ext)
    trip_osm_ids    <- trip_osm_ids %>% mutate(ext_rev = new_ext * fator_correcao)
    
    
    # Exportar osm_ids com infra cicloviária e extensões percorridas
    ids_out <- trip_osm_ids %>% filter(infra_ciclo != 'via_comum') %>% mutate(hex_id_alt = trip_id_alt, 
                                                                              .before = 'osm_way_id')
    # if (file.exists(ids_file)) {
    #   write_delim(ids_out, ids_file, delim = ';', append = TRUE)
    # } else {
    #   write_delim(ids_out, ids_file, delim = ';', append = FALSE)
    # }
    
    out_ids_tmp_file <- sprintf('%s/%s_%s_tmp_osmids.csv', pasta_tmp_osmids, ano, line_id)
    if (file.exists(out_ids_tmp_file)) {
      write_delim(ids_out, out_ids_tmp_file, delim = ';', append = TRUE)
    } else {
      write_delim(ids_out, out_ids_tmp_file, delim = ';', append = FALSE)
    }
    
    
    # Agrupar extensões por tipo de viário percorrido
    trip_osm_ids <- 
      trip_osm_ids %>% 
      group_by(infra_ciclo) %>% 
      summarise(ext = sum(ext_rev)) %>% 
      ungroup() %>% 
      mutate(hex_id_alt = trip_id_alt) %>% 
      pivot_wider(id_cols = hex_id_alt,
                  names_from = 'infra_ciclo',
                  values_from = ext)
    
    # Checar se todas as colunas de tipo de viário estão como colunas - se não, inserir
    for (i in c('ciclo_expressa', 'ciclo_comum', 'ciclofaixa', 'via_comum')) {
      if (!i %in% names(trip_osm_ids)) {
        # Inserir nova coluna como NA (NA_real_, NA_character_)
        trip_osm_ids <- trip_osm_ids %>% mutate(!!i := 0)
      }
      
    }
    
    # Somar extensões percorridas em infra cicloviária
    trip_osm_ids <- trip_osm_ids %>% mutate(infra_ciclo = ciclo_expressa + ciclo_comum + ciclofaixa,
                                            .after = 'via_comum')
    
  } else {
    # Se não passa, toda a extensão dela vai ser por vias comuns
    trip_osm_ids <- data.frame(hex_id_alt     = trip_id_alt,
                               via_comum      = line$distance, 
                               infra_ciclo    = 0, 
                               ciclo_expressa = 0, 
                               ciclo_comum    = 0, 
                               ciclofaixa     = 0)
  }
  
  # Juntar todas as infos ao dataframe original e reordenar colunas
  trip_out <- 
    line %>% 
    left_join(trip_osm_ids, by = c('alt_id' = 'hex_id_alt')) %>% 
    relocate(c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa), 
             .before = 'poly')
  
  # Reconstituir hex_ids originais e coluna com a numeração da rota alternativa
  trip_out <- 
    trip_out %>% 
    mutate(alt_id = str_replace(alt_id, 
                                '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                '89a81\\1ffff-89a81\\2ffff_\\3'),
           .before = 'alt_id') %>% 
    separate(alt_id, into = c('hex_id', 'alt'), sep = '_', remove = TRUE)
  
  
  # Gravar resultados
  if (file.exists(out_file)) {
    write_delim(trip_out, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(trip_out, out_file, delim = ';', append = FALSE)
  }
  
  # Gravar id no arquivo somente para ids já processados
  this <- data.frame(id = trip_id_alt)
  if (file.exists(out_file_ids)) {
    write_delim(this, out_file_ids, delim = ';', append = TRUE)
  } else {
    write_delim(this, out_file_ids, delim = ';', append = FALSE)
  }
  
  
}


# ------------------------------------------------------------------------------
# Tratamento - rotas que passaram por alguma infra cicloviária
# ------------------------------------------------------------------------------

# Definir arquivos de saída
out_file     <- sprintf('%s/tmp_ttmatrix_%s_infraciclo.csv', pasta_aoprv_alter, ano)
out_file_ids <- sprintf('%s/tmp_ttmatrix_%s_infraciclo_ids.csv', pasta_aoprv_alter, ano)

# Abrir cópia do viário de SP com osm_ids
viario_sp <- read_sf(sprintf('%s/tmp_sao_paulo_osm_filtrado.gpkg', pasta_aoprv_alter))
head(viario_sp)

# Abrir infra cicloviária
infra_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_alter, ano)
infra_ciclo <- read_delim(infra_ciclo, delim = ';', col_types = 'cc')
head(infra_ciclo)


detach("package:tidylog")

# Arquivos a processar
arqs <- list.files(pasta_tmp_divididas, pattern = '^tmp_base_dividida_20[0-9]{2}_[0-9]{3}.csv', full.names = TRUE)
for (arq in arqs) {
  # arq <- arqs[1]
  print(arq)
  
  # Abrir base de rotas a serem processadas
  rotas_ciclo <- read_delim(arq, delim = ';', col_types = 'cddddc')
  
  # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
  # do dataframe rotas_ciclo se houver  
  if (file.exists(out_file)) {
    arqs_resultados <- read_delim(out_file_ids, delim = ';', col_types = "c")
    rotas_ciclo <- rotas_ciclo %>% filter(!alt_id %in% arqs_resultados$id)
    rm(arqs_resultados)
    print(nrow(rotas_ciclo))
  }
  
  # Se arquivo já foi completamente processado, removê-lo e passar para o seguinte
  if (nrow(rotas_ciclo) == 0) { 
    file.remove(arq) 
    next
  }
  
  # Limpar memória
  gc(T)
  
  # Processar rotas com infraciclo - melhor rodar no Jupyter;
  # se forem só as rotas originais, é ok rodar no RStudio
  # for (id in rotas_ciclo$alt_id) { dist_ciclo_sep_osmids(id) }
  # Rodar função para todos os arquivos- multi thread (Jupyter)
  (start = Sys.time())
  future::plan(future::multicore)
  invisible(future.apply::future_lapply(X   = rotas_ciclo$alt_id,
                                        FUN = dist_ciclo_sep_osmids,
                                        future.seed = TRUE))
  Sys.time()
  Sys.time() - start
  
  # Remover arquivo inteiramente processado
  file.remove(arq) 
  
  # Limpar memória
  rm(rotas_ciclo)  
  gc(T)
  
}

# Limpar ambiente
rm(i, out_file, infra_ciclo, rotas_ciclo, viario_sp)
gc(T)


# # ------------------------------------------------------------------------------
# # Finalizar ttmatrix inicial - precisa corrigir o tempo de trechos em ciclovia
# # ------------------------------------------------------------------------------
# 
# library('tidylog')
# 
# # Arquivo temporário de ttmatrix - rotas somente em vias comuns
# ttmatrix_vias_comuns <- sprintf('%s/tmp_ttmatrix_%s_rotas_vias_comuns.csv', pasta_aoprv_alter, ano)
# ttmatrix_vias_comuns <- read_delim(ttmatrix_vias_comuns, delim = ';', col_types = cols(.default = "c"))
# # ttmatrix_vias_comuns <- ttmatrix_vias_comuns %>% select(-c(lat.x, lat.y, lon.x, lon.y))
# 
# # Arquivo temporário de ttmatrix - rotas que passam por infraestrutura cicloviária
# ttmatrix_rotas_ciclo <- sprintf('%s/tmp_ttmatrix_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# ttmatrix_rotas_ciclo <- read_delim(ttmatrix_rotas_ciclo, delim = ';', col_types = cols(.default = "c"))
# 
# # Juntar ambas em um dataframe único e limpar ambiente
# ttmatrix <- rbind(ttmatrix_vias_comuns, ttmatrix_rotas_ciclo) %>% arrange(hex_id)
# rm(ttmatrix_vias_comuns, ttmatrix_rotas_ciclo)
# gc(T)
# 
# 
# # Abrir dados de hexágonos - vamos reconstituir latlon x e y em ttmatrix_rotas_ciclo
# hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
# hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')
# 
# # Recriar latlon x e y para inserir no dataframe principal
# hex_com_vizinhos <- 
#   hex_com_vizinhos %>% 
#   mutate(url = str_extract(url, '-23.[0-9]*%2C-46.[0-9]*&point=-23.[0-9]*%2C-46.[0-9]*')) %>% 
#   mutate(url = str_replace_all(url, '&point=|%2C', ',')) %>%
#   separate(url, into = c('lat.x', 'lon.x', 'lat.y', 'lon.y'), sep = ',')
# 
# # Gerar ttmatrix final e gravar
# ttmatrix <- ttmatrix %>% left_join(hex_com_vizinhos, by = c('hex_id' = 'id'))
# 
# if (ano == '2019') {
#   out_file <- sprintf('%s/02_ttmatrix_inicial_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   out_file <- sprintf('%s/06_ttmatrix_inicial_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# }
# 
# write_delim(ttmatrix, out_file, delim = ';')
# 
# # Remover arquivos temporários e limpar ambiente
# file.remove(list.files(pasta_aoprv_alter, pattern = 'tmp_.+.csv', recursive = FALSE, full.names = TRUE))
# rm(out_file, ttmatrix, hex_com_vizinhos, ttmatrix_rotas_ciclo, ttmatrix_vias_comuns)
# 
# 
# # ------------------------------------------------------------------------------
# # Compensar tempos em ciclovias devido à velocidade alterada, gerar ttmatrix final
# # ------------------------------------------------------------------------------
# 
# # No ajuste do modelo, aumentamos a velocidade das ciclovias de +0.139447 km/h
# # para +1.94586 km/h, uma diferença de 1.80586 km/h. Fizemos isso para que as 
# # ciclovias ficassem mais atrativas e fossem escolhidas como rota, mas isso 
# # significa que esses trechos percorridos em ciclovia estão sendo mais rápidos
# # do que deveriam. Vamos compensar isso. Importante comentar que como o valor
# # extra foi aplicado na tag 'cycleway', ele afeta tanto as ciclovias comuns
# # quanto as ciclovias expressas.
# 
# # Para o cálculo, precisaríamos não apenas da velocidade média da viagem, mas da
# # velocidade média específica para os trechos em ciclovia. Conseguiríamos, assim,
# # aplicar a equação:
# # dif_tempo = tempo_corrigido - tempo original, em que
# # tempo_corrigido = dist_em_ciclovia / (velocidade_no_trecho + 0.14); e
# # tempo_original  = dist_em_ciclovia / (velocidade_no_trecho + 1.95)
# # 
# # Como não temos essas velocidades, vamos usar a velocidade média, em metros por
# # segundo, como referência para o cálculo, mudando a fórmula para:
# # dif_tempo = tempo_corrigido - tempo original, em que
# # tempo_corrigido = dist_em_ciclovia / (velocidade_média + 0.14); e
# # tempo_original  = dist_em_ciclovia / (velocidade_média + 1.95)
# 
# # A fórmula fica a seguinte, sendo que teremos que transformar todas as velocidades
# # de km/h para m/s, já que as distâncias percorridas estão em metros e queremos
# # os resultados em segundos para ajustar na coluna time (já em segundos)
# # dif_tempo = dist_em_ciclovia / (velocidade_média + 0.14) - dist_em_ciclovia / (velocidade_média + 1.95)
# # 1.050*((1/(10.7+0.139447))-(1/(10.7+1.945864)))*3600
# 
# if (ano == '2019') {
#   ttmatrix <- sprintf('%s/02_ttmatrix_inicial_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   ttmatrix <- sprintf('%s/06_ttmatrix_inicial_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# }
# 
# ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'cidddddddddcdddd')
# 
# # Criar coluna de time_adj, onde os tempos serão compensados
# ttmatrix <- 
#   ttmatrix %>% 
#   mutate(time_dif = ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (0.139447 / 3.6))) - ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (1.945864 / 3.6))),
#          time_adj  = time + time_dif,
#          .before = 'speed')
# 
# # Filtrar novamente pelo limite de tempo de 40 min, agora pela coluna time_adj
# ttmatrix <- ttmatrix %>% filter(time_adj <= sec_thres)
# 
# 
# # Selecionar as rotas a serem consideradas - se há mais uma alternativa, considerar
# # a com mais uso de infra cicloviária - primeiro descobrir quais são...
# alt_nao_unica <- ttmatrix %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% ungroup()
# # ... depois, filtrar do ttmatrix
# alt_nao_unica <- ttmatrix %>% filter(hex_id %in% alt_nao_unica$hex_id)
# 
# # Por contraposição, isolar as rotas que só têm 1 alternativa
# alt_unica <- ttmatrix %>% filter(!hex_id %in% alt_nao_unica$hex_id)
# # nrow(alt_nao_unica) + nrow(alt_unica) == nrow(ttmatrix)
# 
# # Guardar qual o tamanho que o dataframe de alternativas não únicas tem que ter
# # após selecionada uma única alternativa, das existentes
# qtd_linhas_final <- ttmatrix %>% select(hex_id) %>% distinct() %>% nrow() # 1052
# faltam_linhas <- qtd_linhas_final - nrow(alt_unica) # 764
# 
# # 89a81044d93ffff-89a81046d47ffff -> alternativa 3
# # alt_nao_unica %>% filter(infra_ciclo > 0) %>% select(hex_id, alt, infra_ciclo)%>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo))
# 
# # Tentar puxar a rota que usa maior extensão percorrida em infra cicloviário
# alt_nao_unica <- alt_nao_unica %>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo))
# # Se ainda houver empate, pegar a de menor tempo
# if (nrow(alt_nao_unica) > qtd_linhas_final) {
#   alt_nao_unica <- alt_nao_unica %>% filter(time_adj == min(time_adj))
# }
# # Se ainda houver empate, pegar a de menor distância
# if (nrow(alt_nao_unica) > qtd_linhas_final) {
#   alt_nao_unica <- alt_nao_unica %>% filter(distance == min(distance))
# }
# # Se ainda houver empate, pegar a primeira alternativa
# if (nrow(alt_nao_unica) > qtd_linhas_final) {
#   alt_nao_unica <- alt_nao_unica %>% filter(alt == min(alt))
# }
# alt_nao_unica <- alt_nao_unica %>% ungroup()
# # alt_nao_unica %>% filter(hex_id == '89a81044d93ffff-89a81046d47ffff')
# 
# 
# # Juntar rotas escolhidas em dataframe de saída
# ttmatrix_final <- rbind(alt_unica, alt_nao_unica) %>% arrange(hex_id)
# # ttmatrix_final %>% filter(infra_ciclo > 0) %>% sample_n(20)
# # ttmatrix_final %>% filter(time_dif > 0) %>% sample_n(20)
# 
# # Gravar resultados
# if (ano == '2019') {
#   out_file <- sprintf('%s/03_ttmatrix_final_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   out_file <- sprintf('%s/07_ttmatrix_final_%s_infraciclo.csv', pasta_aoprv_alter, ano)
# }
# write_delim(ttmatrix_final, out_file, delim = ';')
# 
# 
# # Selecionar ids encurtados que aparecem na ttmatrix_final para uso na seção seguinte
# ids_ttmatrix_final <- 
#   ttmatrix_final %>% 
#   mutate(hex_id_alt = str_replace(hex_id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2'), .before = 'hex_id') %>% 
#   mutate(hex_id_alt = str_c(hex_id_alt, alt, sep = '-')) %>% 
#   select(hex_id_alt)
# 
# # Limpar ambiente
# rm(out_file, ttmatrix, ttmatrix_final, alt_nao_unica, alt_unica, faltam_linhas, qtd_linhas_final)
# 
# 
# # ------------------------------------------------------------------------------
# # Selecionar osm_ids que aparecem na ttmatrix final
# # ------------------------------------------------------------------------------
# 
# # Definir arquivo onde osm_ids das rotas serão juntados
# ids_file <- sprintf('%s/osm_way_ids_aop_ciclo_%s_tmp.csv', pasta_aoprv_alter, ano)
# 
# # Juntar arquivos que possuem os osm_ids das rotas percorridas com infraciclo
# osmid_tmp_files <- list.files(pattern = sprintf('^%s_([0-9a-z]{6}_tmp_osmids.csv)', ano), 
#                               pasta_tmp_osmids, 
#                               recursive = FALSE,
#                               full.names = TRUE)
# for (tmp_file in osmid_tmp_files) { 
#   # tmp_file <- osmid_tmp_files[1]
#   this <- read_delim(tmp_file, delim = ';', col_types = cols(.default = "c"))
#   if (file.exists(ids_file)) {
#     write_delim(this, ids_file, delim = ';', append = TRUE)
#   } else {
#     write_delim(this, ids_file, delim = ';', append = FALSE)
#   }
# }
# 
# rm(this, tmp_file, osmid_tmp_files)
# 
# 
# # Abrir resultado da junção com todos os osm_ids
# ids_finais <- read_delim(ids_file, delim = ';', col_types = 'cccdd')
# 
# # Filtrar osm_ids das rotas com infraciclo que correspondem à ttmatrix final
# ids_finais <- ids_finais %>% filter(hex_id_alt %in% ids_ttmatrix_final$hex_id_alt)
# 
# # Gravar osm_ids com infra ciclo utilizados na ttmatrix final
# if (ano == '2019') {
#   ids_file_out <- sprintf('%s/04_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   ids_file_out <- sprintf('%s/08_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_alter, ano)
# }
# 
# write_delim(ids_finais, ids_file_out, delim = ';')
# 
# # Apagar arquivos temporários de osm_ids das rotas com infraciclo percorridas
# file.remove(ids_file)
# # unlink(pasta_tmp_osmids, recursive = TRUE)
# 
# # # Deixar colunas de hex_id e alt no padrão do ttmatrix
# # ids_finais <- ids_finais %>% mutate(hex_id_alt = str_replace(hex_id_alt, 
# #                                                              '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
# #                                                              '89a81\\1ffff-89a81\\2ffff_\\3'))
# # ids_finais <- ids_finais %>% separate(hex_id_alt, into = c('hex_id', 'alt'), sep = '_', remove = TRUE)
# 
# 
