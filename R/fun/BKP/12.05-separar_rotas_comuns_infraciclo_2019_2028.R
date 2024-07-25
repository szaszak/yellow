# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_osm_sp      <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs     <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
pasta_osmids_aopt2 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt2  <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_teste)

# ano <- '2019'
ano <- '2028'


# ------------------------------------------------------------------------------
# Rede cicloviária
# ------------------------------------------------------------------------------

# Abrir arquivo com osm_ids de ciclovias expressas em 2019 - a classificação que
# está no dataframe de atributos do viário, acima, contém osm_ids das vias 
# próximas, já que o map matching foi feito no modo 'pedestrian' e ao passar
# por essas vias, o osm_id que ia ser considerado era o do viário
ciclo_expressas <- sprintf('%s/00_atributos_ciclovias_expressas_2019.csv', pasta_gh_pbfs)
ciclo_expressas <- read_delim(ciclo_expressas, delim = ';', col_types = 'c') %>% distinct()
ciclo_expressas <- ciclo_expressas %>% mutate(infra_ciclo = 'ciclo_expressa')

# Abrir o arquivo com osm_ids de ciclovias comuns (não expressas) em 2019
ciclo_comuns <- sprintf('%s/01_atributos_ciclovias_comuns_2019.csv', pasta_gh_pbfs)
ciclo_comuns <- read_delim(ciclo_comuns, delim = ';', col_types = 'c') %>% distinct()

# Abrir o arquivo com osm_ids de vias com ciclofaixa em 2019
ciclo_ciclofx <- sprintf('%s/02_atributos_ciclofaixas_lcn.csv', pasta_gh_pbfs)
ciclo_ciclofx <- read_delim(ciclo_ciclofx, delim = ';', col_types = 'c') %>% distinct()
ciclo_ciclofx <- ciclo_ciclofx %>% mutate(infra_ciclo = 'ciclofaixa')

# Abrir arquivo de trechos de ciclovias comuns em que não há semáforos ou interseções
ciclo_ciclov_semsem <- sprintf('%s/03_atributos_ciclovias_comuns_sem_semaforo.csv', pasta_gh_pbfs)
ciclo_ciclov_semsem <- read_delim(ciclo_ciclov_semsem, delim = ';', col_types = 'c') %>% distinct()

# Ciclovias comuns serão ciclo_comuns + ciclovias sem semáforos ou interseções
ciclo_comuns <- rbind(ciclo_comuns, ciclo_ciclov_semsem)
ciclo_comuns <- ciclo_comuns %>% mutate(infra_ciclo = 'ciclo_comum') %>% distinct()


# Juntar tudo em um único dataframe
infra_ciclo <- rbind(ciclo_expressas, ciclo_ciclofx, ciclo_comuns) %>% distinct(osm_id, .keep_all = TRUE)


# Incluir Rede cicloviária 2028, se ano for 2028
if (ano == '2028') {
  
  # Abrir shape com a marcação da rede cicloviária de referência
  ciclo_futura <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id_redes_cicloviarias_2019_2028.gpkg', pasta_graphhopper)
  ciclo_futura <- read_sf(ciclo_futura) %>% filter(rede_cicloviaria == 'referencia')
  # mapview(ciclo_futura)
  ciclo_futura <- ciclo_futura %>% st_drop_geometry() %>% select(osm_id) %>% distinct()
  # head(ciclo_futura)
  
  # Antes de juntar, garantir que o que já está marcado como rede de 2019 não seja
  # conflitante com o de 2028
  ciclo_futura <- ciclo_futura %>% filter(!osm_id %in% infra_ciclo$osm_id)
  
  # Todas as estruturas futuras foram tratadas como ciclofaixa na criação do PBF 2028
  ciclo_futura <- ciclo_futura %>% mutate(infra_ciclo = 'ciclofaixa')
  # head(ciclo_futura)
  
  # Juntar tudo em um único dataframe
  infra_ciclo <- rbind(infra_ciclo, ciclo_futura) %>% distinct()
  
  # Checar - ficou algum osm_id repetido?
  # infra_ciclo %>% group_by(osm_id) %>% tally() %>% filter(n > 1)
  rm(ciclo_futura)
}

# Gravar osm_ids com infraciclo
out_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_teste, ano)
write_delim(infra_ciclo, out_ciclo, delim = ';')

# Limpar ambiente
rm(ciclo_expressas, ciclo_ciclofx, ciclo_comuns, ciclo_ciclov_semsem, out_ciclo)
gc(T)


# ------------------------------------------------------------------------------
# Identificação de quais rotas passaram ou não por infra cicloviária
# ------------------------------------------------------------------------------

# Abrir base de rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-')) %>% select(alt_id, distance)
head(rotas)

# Separar ids de origem, para arquivos
ids_origem <- rotas %>% select(id_orig = alt_id) %>% mutate(id_orig = str_sub(id_orig, 1, 6)) %>% distinct()

# Separar ids das rotas que passaram só por vias comuns e as que passaram por
# alguma infraestrutura cicloviária ao longo do caminho
for (id in ids_origem$id_orig) {
  # id <- '044d93'
  print(id)
  
  # Abrir arquivo de OSM-ids
  # TODO: incluir 2028
  if (ano == '2019') {
    osm_ids <- sprintf('%s/%s.csv', pasta_osmids_aopt, id)
  } else if (ano == '2028') {
    osm_ids <- sprintf('%s/%s.csv', pasta_osmids_aopt2, id)
  }
  
  osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'ciic')
  osm_ids <- osm_ids %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'))
  head(osm_ids)
  
  # Manter somente OSM_IDs relacionados às rotas dentro do limite de tempo estabelecido
  osm_ids <- osm_ids %>% filter(alt_id %in% rotas$alt_id) %>% select(alt_id, osm_way_id)
  
  # Juntar com dados de infra_ciclo
  osm_ids <- osm_ids %>% left_join(infra_ciclo, by = c('osm_way_id' = 'osm_id'))
  head(osm_ids)
  
  
  # Quais rotas passaram por alguma infra cicloviária?
  rotas_ciclo <- osm_ids %>% filter(!is.na(infra_ciclo)) %>% select(alt_id) %>% distinct()
  
  # Gravar essas rotas em um arquivo temporário
  tmp_file1 <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_teste, ano)
  if (file.exists(tmp_file1)) {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = FALSE)
  }
  
  
  # Quais rotas não passaram por nenhuma infra cicloviária?
  rotas_vias_comuns <- osm_ids %>% filter(!alt_id %in% rotas_ciclo$alt_id) %>% select(alt_id) %>% distinct()
  
  # Gravar essas rotas em um arquivo temporário
  tmp_file2 <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_teste, ano)
  if (file.exists(tmp_file2)) {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = FALSE)
  }
  
  
}

rm(ids_origem, id, osm_ids, rotas_ciclo, infra_ciclo, rotas_vias_comuns, 
   tmp_file1, tmp_file2, rotas)


# ------------------------------------------------------------------------------
# Checagem - ficou alguma linha processada cujos resultados não estão nas pastas?
# ------------------------------------------------------------------------------

if (ano == '2019') {
  rotas <- sprintf('%s/01_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')


# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_teste, ano)
rotas_vias_comuns <- read_delim(rotas_vias_comuns, delim = ';', col_types = 'c')

# Abrir rotas que só passaram por vias comuns
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_teste, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Aqui estão todas as rotas para gerar o ttmatrix final
rotas_a_processar <- rbind(rotas_vias_comuns, rotas_vias_ciclo)


# 89a81044d93ffff-89a81046b67ffff 
# 44d93-46b67-1 
faltam <- rotas %>% filter(!alt_id %in% rotas_a_processar$alt_id)


if (nrow(faltam) > 0) {
  # Guardar resultados - base integral
  hex_com_vizinhos <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
  hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')
  
  faltam <- faltam %>% select(id = alt_id) %>% mutate(id = str_replace(id, 
                                                                       '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                                                       '89a81\\1ffff-89a81\\2ffff'))
  
  out_file <- sprintf('%s/xxx_faltam_processar.csv', pasta_aoprv_teste)
  write_delim(faltam, out_file, delim = ';')
  
  print('ATENÇÃO: RODAR NOVAMENTE OS SCRIPTS 12.0X-teste_ttmatrix_hexagono_ZL_rede20XX.R')
  
  rm(out_file, hex_com_vizinhos)
  
} else {
  print('TODOS OS ARQUIVOS QUE DEVERIAM TER SIDO PROCESSADOS ESTÃO OK')
}

# Limpar ambiente
rm(faltam, rotas_a_processar, rotas_vias_ciclo, rotas_vias_comuns, rotas)
gc(T)


# ------------------------------------------------------------------------------
# Tratamento - rotas que só passaram por vias comuns
# ------------------------------------------------------------------------------

# Abrir dados de hexágonos - vamos reconstituir latlon x e y
hex_com_vizinhos <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')

# Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')

# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_teste, ano)
rotas_vias_comuns <- read_delim(rotas_vias_comuns, delim = ';', col_types = 'c')

# Filtrar rotas de interesse
rotas_comuns <- rotas %>% filter(alt_id %in% rotas_vias_comuns$alt_id) %>% select(-alt_id)
rotas_comuns <- rotas_comuns %>% mutate(hex_id = str_replace(hex_id, 
                                                             '^([a-z0-9]{6})-([a-z0-9]{6})', 
                                                             '89a81\\1ffff-89a81\\2ffff'))
rm(rotas, rotas_vias_comuns)


# # Recriar latlon x e y para inserir no dataframe principal
# hex_com_vizinhos <- hex_com_vizinhos %>% filter(id %in% rotas_comuns$hex_id)
# hex_com_vizinhos <- 
#   hex_com_vizinhos %>% 
#   mutate(url = str_extract(url, '-23.[0-9]*%2C-46.[0-9]*&point=-23.[0-9]*%2C-46.[0-9]*')) %>% 
#   mutate(url = str_replace_all(url, '&point=|%2C', ',')) %>%
#   separate(url, into = c('lat.x', 'lon.x', 'lat.y', 'lon.y'), sep = ',')
# 
# # Juntar latlon x e y aos resultados
# rotas_comuns <- rotas_comuns %>% left_join(hex_com_vizinhos, by = c('hex_id' = 'id'))

# Constituir resumo da rota, com colunas que vão ficar no ttmatrix final
rotas_comuns <- 
  rotas_comuns %>% 
  mutate(via_comum      = distance, 
         infra_ciclo    = 0, 
         ciclo_expressa = 0, 
         ciclo_comum    = 0, 
         ciclofaixa     = 0,
         .before = 'poly')

# Gravar resultados
out_file <- sprintf('%s/tmp_ttmatrix_%s_rotas_vias_comuns.csv', pasta_aoprv_teste, ano)
write_delim(rotas_comuns, out_file, delim = ';')


# Limpar ambiente
rm(hex_com_vizinhos, rotas_comuns, out_file)
gc(T)



# ------------------------------------------------------------------------------
# Tratamento - rotas que passaram por alguma infra cicloviária
# ------------------------------------------------------------------------------

# Abrir viário de SP com osm_ids
# viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_osm_sp))
# Nos interessam os osm_ids e, talvez, a extensão original dos arcos da rede
# viario_sp <- viario_sp %>% select(osm_id, length_m)
viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp))
# Nos interessam os osm_ids e, talvez, a extensão original dos arcos da rede
viario_sp <- viario_sp %>% select(osm_id) %>% st_transform(31983) %>% mutate(length_m = round(st_length(.), 4),
                                                                             .before = 'geom')
head(viario_sp)

# Abrir infra cicloviária
infra_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_teste, ano)
infra_ciclo <- read_delim(infra_ciclo, delim = ';', col_types = 'c')
head(infra_ciclo)

# Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
rotas <- rotas %>% select(-c(hex_id, alt))

# Abrir rotas que só passaram por vias comuns
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_teste, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Filtrar rotas de interesse
rotas_ciclo <- rotas %>% filter(alt_id %in% rotas_vias_ciclo$alt_id)
rm(rotas, rotas_vias_ciclo)

# Definir arquivo de saída
ids_file <- sprintf('%s/osm_way_ids_aop_ciclo_%s_tmp.csv', pasta_aoprv_teste, ano)
out_file <- sprintf('%s/tmp_ttmatrix_%s_infraciclo.csv', pasta_aoprv_teste, ano)

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
  # trip_id_alt <- rotas_ciclo$alt_id[1]
  # 89a81044d93ffff-89a81046d53ffff-1
  # trip_id_alt <- '044d93-046d53-1'
  
  line   <- rotas_ciclo %>% filter(alt_id == trip_id_alt)
  id     <- str_sub(line$alt_id, 1, 6)
  # line$poly
  
  # Puxar osm_ids da rota modelada
  # TODO: Atualizar pasta para ano 2028
  if (ano == '2019') {
    trip_osm_ids <- list.files(pasta_osmids_aopt, pattern = id, recursive = FALSE, full.names = TRUE) 
  } else if (ano == '2028') {
    trip_osm_ids <- list.files(pasta_osmids_aopt2, pattern = id, recursive = FALSE, full.names = TRUE) 
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
  
  if (qtd_infraciclo == 0) {
    # Se não passa, toda a extensão dela vai ser por vias comuns
    trip_osm_ids <- data.frame(hex_id_alt     = trip_id_alt,
                               via_comum      = line$distance, 
                               infra_ciclo    = 0, 
                               ciclo_expressa = 0, 
                               ciclo_comum    = 0, 
                               ciclofaixa     = 0)
    
    
    # Se passa, calcular as extensões por estrutura cicloviária
  } else {
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
    if (file.exists(ids_file)) {
      write_delim(ids_out, ids_file, delim = ';', append = TRUE)
    } else {
      write_delim(ids_out, ids_file, delim = ';', append = FALSE)
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
  
}


detach("package:tidylog")
for (i in rotas_ciclo$alt_id) { calcular_dist_ciclo(i) }

# Limpar ambiente
rm(i, out_file, ids_file, infra_ciclo, rotas_ciclo, viario_sp)
gc(T)

# ------------------------------------------------------------------------------
# Finalizar ttmatrix inicial - precisa corrigir o tempo de trechos em ciclovia
# ------------------------------------------------------------------------------

library('tidylog')

# Arquivo temporário de ttmatrix - rotas somente em vias comuns
ttmatrix_vias_comuns <- sprintf('%s/tmp_ttmatrix_%s_rotas_vias_comuns.csv', pasta_aoprv_teste, ano)
ttmatrix_vias_comuns <- read_delim(ttmatrix_vias_comuns, delim = ';', col_types = cols(.default = "c"))
# ttmatrix_vias_comuns <- ttmatrix_vias_comuns %>% select(-c(lat.x, lat.y, lon.x, lon.y))

# Arquivo temporário de ttmatrix - rotas que passam por infraestrutura cicloviária
ttmatrix_rotas_ciclo <- sprintf('%s/tmp_ttmatrix_%s_infraciclo.csv', pasta_aoprv_teste, ano)
ttmatrix_rotas_ciclo <- read_delim(ttmatrix_rotas_ciclo, delim = ';', col_types = cols(.default = "c"))

# Juntar ambas em um dataframe único e limpar ambiente
ttmatrix <- rbind(ttmatrix_vias_comuns, ttmatrix_rotas_ciclo) %>% arrange(hex_id)
rm(ttmatrix_vias_comuns, ttmatrix_rotas_ciclo)
gc(T)


# Abrir dados de hexágonos - vamos reconstituir latlon x e y em ttmatrix_rotas_ciclo
hex_com_vizinhos <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')

# Recriar latlon x e y para inserir no dataframe principal
hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = str_extract(url, '-23.[0-9]*%2C-46.[0-9]*&point=-23.[0-9]*%2C-46.[0-9]*')) %>% 
  mutate(url = str_replace_all(url, '&point=|%2C', ',')) %>%
  separate(url, into = c('lat.x', 'lon.x', 'lat.y', 'lon.y'), sep = ',')

# Gerar ttmatrix final e gravar
ttmatrix <- ttmatrix %>% left_join(hex_com_vizinhos, by = c('hex_id' = 'id'))

if (ano == '2019') {
  out_file <- sprintf('%s/02_ttmatrix_inicial_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  out_file <- sprintf('%s/06_ttmatrix_inicial_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
}

write_delim(ttmatrix, out_file, delim = ';')

# Remover arquivos temporários
file.remove(list.files(pasta_aoprv_teste, pattern = 'tmp_.+.csv', recursive = FALSE, full.names = TRUE))


# ------------------------------------------------------------------------------
# Compensar tempos em ciclovias devido à velocidade alterada, gerar ttmatrix final
# ------------------------------------------------------------------------------

# No ajuste do modelo, aumentamos a velocidade das ciclovias de +0.139447 km/h
# para +1.94586 km/h, uma diferença de 1.80586 km/h. Fizemos isso para que as 
# ciclovias ficassem mais atrativas e fossem escolhidas como rota, mas isso 
# significa que esses trechos percorridos em ciclovia estão sendo mais rápidos
# do que deveriam. Vamos compensar isso. Importante comentar que como o valor
# extra foi aplicado na tag 'cycleway', ele afeta tanto as ciclovias comuns
# quanto as ciclovias expressas.

# Para o cálculo, precisaríamos não apenas da velocidade média da viagem, mas da
# velocidade média específica para os trechos em ciclovia. Conseguiríamos, assim,
# aplicar a equação:
# dif_tempo = tempo_corrigido - tempo original, em que
# tempo_corrigido = dist_em_ciclovia / (velocidade_no_trecho + 0.14); e
# tempo_original  = dist_em_ciclovia / (velocidade_no_trecho + 1.95)
# 
# Como não temos essas velocidades, vamos usar a velocidade média, em metros por
# segundo, como referência para o cálculo, mudando a fórmula para:
# dif_tempo = tempo_corrigido - tempo original, em que
# tempo_corrigido = dist_em_ciclovia / (velocidade_média + 0.14); e
# tempo_original  = dist_em_ciclovia / (velocidade_média + 1.95)

# A fórmula fica a seguinte, sendo que teremos que transformar todas as velocidades
# de km/h para m/s, já que as distâncias percorridas estão em metros e queremos
# os resultados em segundos para ajustar na coluna time (já em segundos)
# dif_tempo = dist_em_ciclovia / (velocidade_média + 0.14) - dist_em_ciclovia / (velocidade_média + 1.95)
# 1.050*((1/(10.7+0.139447))-(1/(10.7+1.945864)))*3600

if (ano == '2019') {
  ttmatrix <- sprintf('%s/02_ttmatrix_inicial_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  ttmatrix <- sprintf('%s/06_ttmatrix_inicial_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
}

ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'cidddddddddcdddd')

# Criar coluna de time_adj, onde os tempos serão compensados
ttmatrix <- 
  ttmatrix %>% 
  mutate(time_dif = ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (0.139447 / 3.6))) - ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (1.945864 / 3.6))),
         time_adj  = time + time_dif,
         .before = 'speed')

# Filtrar novamente pelo limite de tempo de 40 min, agora pela coluna time_adj
ttmatrix <- ttmatrix %>% filter(time_adj <= 2400)


# Selecionar as rotas a serem consideradas - se há mais uma alternativa, considerar
# a com mais uso de infra cicloviária - primeiro descobrir quais são...
alt_nao_unica <- ttmatrix %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% ungroup()
# ... depois, filtrar do ttmatrix
alt_nao_unica <- ttmatrix %>% filter(hex_id %in% alt_nao_unica$hex_id)

# Por contraposição, isolar as rotas que só têm 1 alternativa
alt_unica <- ttmatrix %>% filter(!hex_id %in% alt_nao_unica$hex_id)
# nrow(alt_nao_unica) + nrow(alt_unica) == nrow(ttmatrix)

# Guardar qual o tamanho que o dataframe de alternativas não únicas tem que ter
# após selecionada uma única alternativa, das existentes
qtd_linhas_final <- ttmatrix %>% select(hex_id) %>% distinct() %>% nrow() # 1052
faltam_linhas <- qtd_linhas_final - nrow(alt_unica) # 764

# 89a81044d93ffff-89a81046d47ffff -> alternativa 3
# alt_nao_unica %>% filter(infra_ciclo > 0) %>% select(hex_id, alt, infra_ciclo)%>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo))

# Tentar puxar a rota que usa maior extensão percorrida em infra cicloviário
alt_nao_unica <- alt_nao_unica %>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo))
# Se ainda houver empate, pegar a de menor tempo
if (nrow(alt_nao_unica) > qtd_linhas_final) {
  alt_nao_unica <- alt_nao_unica %>% filter(time_adj == min(time_adj))
}
# Se ainda houver empate, pegar a de menor distância
if (nrow(alt_nao_unica) > qtd_linhas_final) {
  alt_nao_unica <- alt_nao_unica %>% filter(distance == min(distance))
}
# Se ainda houver empate, pegar a primeira alternativa
if (nrow(alt_nao_unica) > qtd_linhas_final) {
  alt_nao_unica <- alt_nao_unica %>% filter(alt == min(alt))
}
alt_nao_unica <- alt_nao_unica %>% ungroup()
# alt_nao_unica %>% filter(hex_id == '89a81044d93ffff-89a81046d47ffff')


# Juntar rotas escolhidas em dataframe de saída
ttmatrix_final <- rbind(alt_unica, alt_nao_unica) %>% arrange(hex_id)
# ttmatrix_final %>% filter(infra_ciclo > 0) %>% sample_n(20)
# ttmatrix_final %>% filter(time_dif > 0) %>% sample_n(20)

if (ano == '2019') {
  out_file <- sprintf('%s/03_ttmatrix_final_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  out_file <- sprintf('%s/07_ttmatrix_final_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
}

write_delim(ttmatrix_final, out_file, delim = ';')



# Selecionar ids encurtados que aparecem na ttmatrix_final
ids_ttmatrix_final <- 
  ttmatrix_final %>% 
  mutate(hex_id_alt = str_replace(hex_id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2'), .before = 'hex_id') %>% 
  mutate(hex_id_alt = str_c(hex_id_alt, alt, sep = '-')) %>% 
  select(hex_id_alt)

# Filtrar osm_ids das rotas com infraciclo que correspondem à ttmatrix final
ids_file <- sprintf('%s/osm_way_ids_aop_ciclo_%s_tmp.csv', pasta_aoprv_teste, ano)

ids_finais <- read_delim(ids_file, delim = ';', col_types = 'cccdd')
ids_finais <- ids_finais %>% filter(hex_id_alt %in% ids_ttmatrix_final$hex_id_alt)

# Gravar osm_ids com infra ciclo utilizados na ttmatrix final
if (ano == '2019') {
  ids_file_out <- sprintf('%s/04_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  ids_file_out <- sprintf('%s/08_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_teste, ano)
}

write_delim(ids_finais, ids_file_out, delim = ';')

# apagar arquivo temporário de osm_ids das rotas com infraciclo percorridas
file.remove(ids_file)


# # Deixar colunas de hex_id e alt no padrão do ttmatrix
# ids_finais <- ids_finais %>% mutate(hex_id_alt = str_replace(hex_id_alt, 
#                                                              '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
#                                                              '89a81\\1ffff-89a81\\2ffff_\\3'))
# ids_finais <- ids_finais %>% separate(hex_id_alt, into = c('hex_id', 'alt'), sep = '_', remove = TRUE)


