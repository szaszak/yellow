# Como o processamento para as rotas que passaram por infra cicloviária vai ser
# muito pesado/longo, separa a base dessas rotas por id de hexágono de origem. Os 
# arquivos serão processados de fato no script seguinte

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados          <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)

# ano <- '2024'; min_thres <- 15
ano <- '2028'; min_thres <- 15

if (ano == '2024') {
  pasta_tmp_divididas <- sprintf("%s/X_%s_tmp_base_dividida", pasta_ttmatrix_24_28, ano)
} else if (ano == '2028') {
  pasta_tmp_divididas <- sprintf("%s/Y_%s_tmp_base_dividida", pasta_ttmatrix_24_28, ano)
}
dir.create(pasta_tmp_divididas, recursive = TRUE, showWarnings = FALSE)


# Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2024') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id  = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
rotas <- rotas %>% mutate(base_id = str_sub(alt_id, 1, 6), .before = 'alt_id')
rotas <- rotas %>% filter(time <= 900) %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()
rotas <- rotas %>% select(-c(hex_id, alt))

# Abrir rotas que só passaram por vias com infra cicloviária
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Deixar somente rotas que passaram por infra cicloviária
rotas <- rotas %>% filter(alt_id %in% rotas_vias_ciclo$alt_id)

# Limpar ambiente
rm(rotas_vias_ciclo)
gc(T)


# Remover rotas já processadas - aqui é o seguinte: se já houver os arquivos 03 e 
# 06 abaixo, remover todas as linhas que já foram processadas e estão registradas 
# neles. O passo a seguir vai processar somente as linhas restantes e jogar nas 
# pastas X_ ou Y_. É dessas pastas que o script seguinte vai puxar o que foi 
# separado para finalmente juntar aos arquivos de resultados já  existentes. Ou 
# seja, as pastas X_ ou Y_ não precisam estar cheias, pois nelas somente são 
# depositados os arquivos que deverão ser processados para somar aos já existentes
if (ano == '2024') {
  rotas_proc <- sprintf('%s/06_tmp_ttmatrix_%s_rotas_infra_ciclo_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
} else if (ano == '2028') {
  rotas_proc <- sprintf('%s/07_tmp_ttmatrix_%s_rotas_infra_ciclo_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
}
if (file.exists(rotas_proc)) {
  # Abrir arquivo de resultados
  rotas_proc <- read_delim(rotas_proc, delim = ';', col_select = c('hex_id', 'alt'), col_types = 'cc')
  # Encurtar id das rotas OD
  rotas_proc <- 
    rotas_proc %>% 
    mutate(hex_id = str_replace(hex_id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2'),
           alt_id = str_c(hex_id, alt, sep = '-'))
  
  # Manter somente rotas não processadas
  rotas <- rotas %>% filter(!alt_id %in% rotas_proc$alt_id)
  
  # Limpar ambiente
  rm(rotas_proc)
  gc(T)
  
} else {
  rm(rotas_proc)
  
}


# Criar base de ids de origem para fazer a separação dos arquivos
base_ids <- rotas %>% select(base_id) %>% distinct()

# Separar base por hexágono de origem
detach("package:tidylog")
for (sel_id in base_ids$base_id) {
  # sel_id <- '000127'
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
