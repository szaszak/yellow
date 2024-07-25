# Como o processamento para as rotas que passaram por infra cicloviária vai ser
# muito pesado/longo, separa a base dessas rotas em cerca de ~8.100 arquivos,
# com o nome referente ao id do hexágono de origem. Esses arquivos serão 
# processados de fato no script seguinte

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_ssd         <- "/media/livre/SSD120GB/yellow"
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)


# ano <- '2019'
ano <- '2028'

if (ano == '2019') {
  pasta_tmp_divididas <- sprintf("%s/X_%s_tmp_base_dividida", pasta_ssd, ano)
} else if (ano == '2028') {
  pasta_tmp_divididas <- sprintf("%s/Y_%s_tmp_base_dividida", pasta_ssd, ano)
}
dir.create(pasta_tmp_divididas, recursive = TRUE, showWarnings = FALSE)


# Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id  = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
rotas <- rotas %>% mutate(base_id = str_sub(alt_id, 1, 6), .before = 'alt_id')
rotas <- rotas %>% select(-c(hex_id, alt))

# Abrir rotas que só passaram por vias com infra cicloviária
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Deixar somente rotas que passaram por infra cicloviária
rotas <- rotas %>% filter(alt_id %in% rotas_vias_ciclo$alt_id)

# Criar base de ids de origem para fazer a separação dos arquivos
base_ids <- rotas %>% select(base_id) %>% distinct()

# Separar base por hexágono de origem
detach("package:tidylog")
for (sel_id in base_ids$base_id) {
  # sel_id <- '00000b'
  # print(sel_id)
  
  # Definir arquivo de saída
  rotas_out_file <- sprintf('%s/%s_dividida_%s.csv', pasta_tmp_divididas, sel_id, ano)
  # Se já existir, desconsiderar; se não, continuar a partir de onde estava
  if (file.exists(rotas_out_file)) { next }
  
  # Puxar somente as rotas com o mesmo hexágono de origem e que passaram por
  # infra cicloviária
  rotas_out <- rotas %>% filter(base_id == sel_id)
  rotas_out <- rotas_out %>% select(-base_id)
  
  # Gravar, tendo o hexágono de origem como base do nome
  write_delim(rotas_out, rotas_out_file, delim = ';')
}

