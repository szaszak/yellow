# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
dir.create(pasta_base_modelo, recursive = TRUE, showWarnings = FALSE)


# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccdddididdiddccccici')

head(atrib_viario)


# Abrir arquivos processados
base_modelo <- list.files(path = pasta_base_agrup, 
                          pattern = '^\\d{6}_trechos_processados_todos.csv', 
                          recursive = FALSE, 
                          full.names = TRUE)

# arq_trechos_proc <- sprintf('%s/trechos_processados_todos.csv', pasta_base_agrup)
# base_modelo <- read_delim(arq_trechos_proc, delim = ';', col_types = 'cccicidddddddd')
base_modelo <- 
  lapply(X = base_modelo, FUN = read_delim, delim = ';', col_types = 'ccciiccidddddddd') %>% 
  rbindlist(fill = TRUE)

head(base_modelo)


# Arquivos com distâncias totais por trecho
ano_mes <- c('201811', '201812', '201901')
arqs_dist_tot <- c()
for (a_m in ano_mes) {
  search_folder <- sprintf('%s/%s', pasta_map_matching, a_m)
  result_files  <- list.files(search_folder, 
                              pattern = '^\\d{6}_map_matching_rotas_completas.csv', 
                              recursive = FALSE, 
                              full.names = TRUE)
  
  arqs_dist_tot <- c(arqs_dist_tot, result_files)
}

# Abrir arquivos, já selecionando somente colunas de interesse
ext_totais <- 
  lapply(X = arqs_dist_tot, FUN = read_delim, delim = ';', col_types = 'cciiiddddTT') %>% 
  rbindlist(fill = TRUE) %>% 
  select(trip_id, cod_proc, dist_total)

head(ext_totais)

# Limpar ambiente
rm(ano_mes, arqs_dist_tot, search_folder, result_files)


# ------------------------------------------------------------------------------
# Filtros referentes à base processada
# ------------------------------------------------------------------------------

# Retirar alguns qgis_id que estão dando problema - valores precisam ser re-checados
# caso o mapa base OSM do processamento tenha sido alterado
qgis_id_problematicos <- c(
  '035959' # Pista da Raia da USP
)

trip_id_problematicos <- c(
  # '013918_12' # Túnel da 9 de Julho
)
base_modelo <- base_modelo %>% filter(qgis_id %nin% qgis_id_problematicos & trip_id %nin% trip_id_problematicos)


# # Retirar arcos do viário (qgis_id) com quantidade de pontos menor do que 3
# base_modelo <- base_modelo %>% filter(n_pontos > 2) %>% select(-cluster)

# Quantos trechos possuem pelo menos três viagens por sentido da via?
viagens_por_trecho <- base_modelo %>% group_by(qgis_id, elev_sent) %>% tally()
viagens_validas <- viagens_por_trecho %>% filter(n > 2)
head(viagens_validas)

# Retirar da base do modelo os trechos com menos de uma viagem por sentido
base_modelo <- base_modelo %>% filter(qgis_id %in% viagens_validas$qgis_id)
rm(viagens_por_trecho, viagens_validas)

# Retirar da base os trechos em que o sentido do deslocamento não foi identificado
base_modelo <- base_modelo %>% filter(!is.na(elev_sent))

head(base_modelo)


# ------------------------------------------------------------------------------
# Extensões totais dos trechos
# ------------------------------------------------------------------------------

# Simplificar dataframe de extensões totais
ext_totais <- 
  ext_totais %>% 
  # Recriar coluna trip_id, considerando os dois primeiros caractares de cod_proc
  mutate(cod_proc   = substr(cod_proc, 1, 2),
         trecho_id  = str_c(trip_id, '_', cod_proc, sep = '')) %>% 
  select(-cod_proc, vg_id = trip_id, dist_total_trecho_m = dist_total) %>% 
  # Retirar duplicatas
  distinct()

# Juntar extensões totais à base principal - teremos, temporariamente, uma coluna
# chamada 'vg_id' que se refere ao id original da viagem
base_modelo <- base_modelo %>% left_join(ext_totais, by = c('trip_id' = 'trecho_id'))

head(base_modelo)


# ------------------------------------------------------------------------------
# Registros de quantidades de viagens e trechos
# ------------------------------------------------------------------------------

# Quantas viagens serão consideradas no modelo final? - 164.585
n_viagens <- base_modelo %>% select(vg_id) %>% distinct() %>% nrow()

# Quantos trechos dessas viagens serão consideradas no modelo final? - 182.912
n_trechos <- base_modelo %>% select(trip_id) %>% distinct() %>% nrow()

# Retirar coluna temporária de vg_id da base geral
base_modelo <- base_modelo %>% select(-vg_id)


# ------------------------------------------------------------------------------
# Juntar atributos de viário que ainda não estão associados à base
# ------------------------------------------------------------------------------

# Deixar somente colunas de interesse para esta etapa
atrib_viario <- atrib_viario %>% select(qgis_id, curv_h, lotes_tot, lotes_15m, lotes_30m, 
                                        dens_lotes_100m, dens_lotes_100m_15m, dens_lotes_100m_30m,
                                        class_via, infra_ciclo, via_restr,
                                        osm_oneway, osm_lanes, osm_surface, osm_maxspeed)

# Associar os atributos de viário restantes à base
base_modelo <- base_modelo %>% left_join(atrib_viario, by = 'qgis_id')
head(base_modelo)


# Quantos qgis_id temos? # 26.987
base_modelo %>% select(qgis_id) %>% distinct() %>% nrow()

# Quantos trechos de via (qgis_id) estão sem info de sentido? # 0
base_modelo %>% filter(is.na(osm_oneway)) %>% select(qgis_id) %>% distinct()

# Quantos trechos de via (qgis_id) estão sem info de quantidade de faixas? # 12816/26987*100 = 47,5%
base_modelo %>% filter(is.na(osm_lanes)) %>% select(qgis_id) %>% distinct() %>% nrow()

# Quantos trechos de via (qgis_id) estão sem info de superfície? # 6661/26987*100 = 24,7%
base_modelo %>% filter(is.na(osm_surface)) %>% select(qgis_id) %>% distinct() %>% nrow()

# Quantos trechos de via (qgis_id) estão sem info de velocidade máxima? # 16633/26987*100 = 61,6%
base_modelo %>% filter(is.na(osm_maxspeed)) %>% select(qgis_id) %>% distinct() %>% nrow()


# ------------------------------------------------------------------------------
# Finais de semana e feriados
# ------------------------------------------------------------------------------

# Feriados 2018 e 2019
feriados <- c(as.Date('2018-10-12'), # Nossa Senhora Aparecida
              as.Date('2018-11-02'), # Finados (sexta)
              as.Date('2018-11-15'), # Procl. República (quinta)
              as.Date('2018-12-24'), # Natal (terça)
              as.Date('2019-01-01'), # Ano novo (terça)
              as.Date('2019-02-13'), # Carnaval (terça)
              as.Date('2019-03-30'), # Sexta feira santa (sábado)
              as.Date('2019-04-21')  # Tiradentes (domingo)
)


# Agregar dados de data (conversão do timestamp), hora e dia da semana
base_modelo <- 
  base_modelo %>% 
  # Converter a coluna de timestamps para dia-mes-ano-hora
  add_column(dh_inicio = as.POSIXlt(.$ts_inicio, origin = '1970-01-01'), 
             .after = 'ts_inicio') %>% 
  # Inserir colunas de dia da semana e hora
  mutate(dia_semana = weekdays(dh_inicio, abbreviate = TRUE),
         fx_hora    = format(dh_inicio, "%H"),
         .after = 'dh_inicio') %>% 
  # Converter coluna de hora para numeric
  mutate(fx_hora = as.numeric(fx_hora))


# Atualizar coluna de dia da semana para marcar feriados
base_modelo <- 
  base_modelo %>% 
  mutate(dia_semana  = ifelse(as.Date(dh_inicio) %in% feriados, 'fer', dia_semana))

# Descartar coluna de dh_inicio
base_modelo <- base_modelo %>% select(-dh_inicio)

head(base_modelo)


# ------------------------------------------------------------------------------
# Exportar resultados
# ------------------------------------------------------------------------------

# Exportar resultados
out_file <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
write_delim(base_modelo, out_file, delim = ';')
