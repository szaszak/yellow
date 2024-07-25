# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_osm_sp      <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_alter)


# ------------------------------------------------------------------------------
# Gerar cópia do shape de viário de SP com metragem dos arcos
# ------------------------------------------------------------------------------

# Uma cópia desse shape está sendo gerada aqui porque o Jupyter está dando algum
# erro no script seguinte ao tentar puxar as metragens das linhas para uma coluna

# Abrir viário de SP com osm_ids
# viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id.gpkg', pasta_osm_sp))
# Nos interessam os osm_ids e, talvez, a extensão original dos arcos da rede
# viario_sp <- viario_sp %>% select(osm_id, length_m)
viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp))
# Nos interessam os osm_ids e, talvez, a extensão original dos arcos da rede
viario_sp <- viario_sp %>% select(osm_id) %>% st_transform(31983) %>% mutate(length_m = round(st_length(.), 4),
                                                                             .before = 'geom')
head(viario_sp)

out_viario <- sprintf('%s/tmp_sao_paulo_osm_filtrado.gpkg', pasta_aoprv_alter)
st_write(viario_sp, out_viario, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Dividir resultados para processamento - fazer para os anos 2019 e, depois, 2028
# ------------------------------------------------------------------------------

ano <- '2019'
# ano <- '2028'

if (ano == '2019') {
  pasta_tmp_divididas <- sprintf("%s/X_%s_tmp_base_dividida", pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  pasta_tmp_divididas <- sprintf("%s/Y_%s_tmp_base_dividida", pasta_aoprv_alter, ano)
}
dir.create(pasta_tmp_divididas, recursive = TRUE, showWarnings = FALSE)


# Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
rotas <- rotas %>% select(-c(hex_id, alt))

# Abrir rotas que só passaram por vias com infra cicloviária
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Filtrar rotas de interesse
rotas_ciclo <- rotas %>% filter(alt_id %in% rotas_vias_ciclo$alt_id)
head(rotas_ciclo)

# Limpar ambiente
rm(rotas, rotas_vias_ciclo)
gc(T)


# Dividir resultados em arquivos menores, para processamento
# Definir intervalo que custe pouca memória do future
intervalo <- 60000
max_value <- nrow(rotas_ciclo)
# max_value/ intervalo # 110 vezes

# Selecionar quantidade de linhas de acordo com intervalo
counter <- 1
for (i in seq(1, max_value, intervalo)) {
  # i <- 1;
  
  # Se valor máximo pelo intervalo for maior que dataframe,
  # considerar tamanho do dataframe como máximo
  interval_max <- i + intervalo - 1
  if (interval_max > max_value) { interval_max <- max_value }
  print(sprintf('%i - %i', i, interval_max))
  
  # Puxar só linhas correspondentes ao intervalo
  rotas_div <- rotas_ciclo %>% slice(i:interval_max)
  
  # Gravar arquivo temporário
  out_file <- sprintf('%s/tmp_base_dividida_%s_%s.csv', pasta_tmp_divididas, ano, str_pad(counter, 3, pad = '0'))
  write_delim(rotas_div, out_file, delim = ';')
  
  # Atualizar counter
  counter <- counter + 1
  
}
