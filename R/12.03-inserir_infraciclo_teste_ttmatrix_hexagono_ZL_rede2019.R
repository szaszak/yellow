library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
library('googlePolylines')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_graphhopper  <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs      <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
# pasta_osmids_aopt2 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt2  <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_teste)


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
  # trip_id_alt <- rotas_m$hex_id_alt[3]
  # trip_id_alt <- rotas_m %>% filter(id_hex.y == '89a81046d53ffff') %>% head(1) %>% select(hex_id_alt) %>% pull()
  # trip_id_alt <- '89a81046b2fffff-89a8100990bffff_3'
  line <- rotas_m %>% filter(hex_id_alt == trip_id_alt)
  # line$poly
  
  # Puxar osm_ids da rota modelada
  trip_osm_ids <- list.files(pasta_osmids_aopt, pattern = trip_id_alt, recursive = FALSE, full.names = TRUE)
  trip_osm_ids <- read_delim(trip_osm_ids, delim = ';', col_types = 'cc')
  # Remover osm_ids repetidos - no momento do buffer com o shapefile da rota,
  # o intersection vai pegar todos os trechos que os osm_ids aparecem e vai
  # somar essas distâncias. Osm_ids repetidos são quando a rota entra, sai e
  # entra de novo em um mesmo osm_id
  trip_osm_ids <- trip_osm_ids %>% distinct(osm_way_id, .keep_all = TRUE)
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
    trip_osm_ids <- data.frame(hex_id_alt    = trip_id_alt,
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
    left_join(trip_osm_ids, by = 'hex_id_alt') %>% 
    relocate(c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa), 
             .before = 'poly')
  
  
  # Gravar resultados
  if (file.exists(out_file)) {
    write_delim(trip_out, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(trip_out, out_file, delim = ';', append = FALSE)
  }
  
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
# Calcular acurácia das rotas modeladas e distâncias percorridas em infra ciclo
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

# Abrir rotas modeladas com alternativas
rotas_m <- sprintf('%s/01_ttmatrix_teste_hexagono_ZL_2019.csv', pasta_aoprv_teste)
rotas_m <- read_delim(rotas_m, delim = ';', col_types = 'ciccddddcdddd')

# Remover rotas com origem muito perto do destino (qgis_id diferente, mas 
# distance igual a zero) e rotas com alt = NA, que foram as em que o destino
# ficava fora do shape de viário de SP
rotas_m <- rotas_m %>% filter(distance != 0 & !is.na(alt))

# Criar id único
rotas_m <- rotas_m %>% mutate(hex_id_alt = str_c(hex_id, alt, sep = '_'), .before = 'hex_id')


# Remover arquivos que já foram executados
# suppressPackageStartupMessages(library('tidylog'))
out_file <- sprintf('%s/02_ttmatrix_teste_hexagono_ZL_2019_infraciclo.csv', pasta_aoprv_teste)
if (file.exists(out_file)) {
  this <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
  
  rotas_m <- rotas_m %>% filter(!hex_id_alt %in% this$hex_id_alt)
  rm(this)
}


# Inserir cálculos de extensões percorridas em vias comuns ou infra cicloviária
# para cada uma das rotas calculadas
detach("package:tidylog")
for (tmpid in rotas_m$hex_id_alt) { calcular_dist_ciclo(tmpid) }


