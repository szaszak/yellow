# Das rotas com menor weight até 15 minutos, separa as que passaram por rede cicloviária
# e as que não passaram. Para as que não passaram, já gera a ttmatrix inicial,
# enquanto as que passaram vão ser processadas nos scripts seguintes

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_graphhopper   <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs       <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_2024_2028 <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_redes_24_28   <- sprintf("%s/01_redes_2024_2028", pasta_aop_2024_2028)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)
pasta_ids_aopt_24 <- sprintf("%s/A_2024_osm_way_ids_aop", pasta_ttmatrix_24_28)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_ttmatrix_24_28)

# ano <- '2024'; min_thres <- 15
ano <- '2028'; min_thres <- 15


# ------------------------------------------------------------------------------
# Consolidação redes cicloviárias 2024 e de referência 2028
# ------------------------------------------------------------------------------

# Rede cicloviária 2024
ciclo_ciclov24 <- sprintf('%s/rede_consolidada_ciclovias_2024.gpkg', pasta_redes_24_28)
ciclo_ciclov24 <- read_sf(ciclo_ciclov24)

ciclo_ciclofx24 <- sprintf('%s/rede_consolidada_ciclofaixas_2024.gpkg', pasta_redes_24_28)
ciclo_ciclofx24 <- read_sf(ciclo_ciclofx24)


if (ano == '2024') {
  # Juntar tudo em um único dataframe
  infra_ciclo <- 
    rbind(ciclo_ciclov24, ciclo_ciclofx24) %>% 
    distinct(osm_id, .keep_all = TRUE)
  
  infra_ciclo %>% st_drop_geometry() %>% group_by(infra_ciclo) %>% tally()
    
} else if (ano == '2028') {
  
  # Rede cicloviária 2028
  ciclo_2028 <- sprintf('%s/rede_consolidada_2028_sem_2024.gpkg', pasta_redes_24_28)
  ciclo_2028 <- read_sf(ciclo_2028)
  
  # Consolidar rede 2028: juntar ciclovias comuns e expressas com ciclofaixas
  infra_ciclo <- rbind(ciclo_ciclov24, ciclo_ciclofx24, ciclo_2028) %>% distinct(osm_id, .keep_all = TRUE)
  
  # rbind(ciclo_ciclov24, ciclo_ciclofx24) %>% st_drop_geometry() %>% group_by(infra_ciclo) %>% tally()
  infra_ciclo %>% st_drop_geometry() %>% group_by(infra_ciclo) %>% tally()
  
  # Limpar ambiente
  rm(ciclo_2028)
  
}


# Limpar ambiente
rm(ciclo_ciclov24, ciclo_ciclofx24)


# Isolar colunas de interesse para exportar
infra_ciclo <- infra_ciclo %>% st_drop_geometry() %>% select(osm_id, infra_ciclo) %>% distinct()
# infra_ciclo %>% filter(osm_id %in% c('358376530', '311571202', '220294302'))


# Gravar osm_ids com infraciclo
out_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_ttmatrix_24_28, ano)
write_delim(infra_ciclo, out_ciclo, delim = ';')



# ------------------------------------------------------------------------------
# Filtrar somente rotas até 15 min e que possuem o menor weight
# ------------------------------------------------------------------------------

# Abrir base de rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2024') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_select = c('hex_id', 'alt', 'distance', 'weight', 'time'), col_types = 'ciddd')

# 2024: 49270; 2028: 51357
rotas <- rotas %>% filter(time <= 900) %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()

# # Algumas rotas em que o menor weight é a terceira alternativa (2024):
# this <- rotas %>% group_by(hex_id) %>% filter(weight == min(weight) & alt > 1) %>% ungroup() %>% select(hex_id) %>% distinct()
# # Algumas rotas que possuem mais de uma opção e será escolhida da de menor peso:
# rotas %>% filter(hex_id %in% this$hex_id) %>% head(20)
# rotas %>% filter(hex_id %in% c('002833-0028cb', '005887-005873'))

# Checagem: há mais de uma rota possível para o mesmo par origem-destino?
rotas %>% select(hex_id) %>% distinct() %>% nrow() == nrow(rotas)

# Manter somente colunas de interesse
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-')) %>% select(alt_id, distance)
head(rotas)
gc(T)



# ------------------------------------------------------------------------------
# Identificar rotas passaram ou não por infra cicloviária - leva cerca de 10 min
# ------------------------------------------------------------------------------

# Checar rotas que já foram processadas para exclusão - aqui, vamos ler o arquivo
# existente e remover os ids já processados do dataframe 'rotas'. Esse mesmo
# arquivo vai ser atualizado logo adiante, portanto não vamos apagá-lo
tmp_file1 <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
if (file.exists(tmp_file1)) {
  # Abrir rotas que só passaram por vias comuns
  rotas_vias_ciclo <- read_delim(tmp_file1, delim = ';', col_types = 'c')
  rotas_vias_ciclo <- rotas_vias_ciclo %>% filter(!is.na(alt_id))
  
  # Abrir rotas que só passaram por vias comuns
  tmp_file2 <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
  rotas_vias_comuns <- read_delim(tmp_file2, delim = ';', col_types = 'c')
  rotas_vias_comuns <- rotas_vias_comuns %>% filter(!is.na(alt_id))
  
  # Aqui estão todas as rotas para gerar o ttmatrix final
  rotas_processadas <- rbind(rotas_vias_comuns, rotas_vias_ciclo)
  
  # Excluir rotas que já foram processadas
  rotas <- rotas %>% filter(!alt_id %in% rotas_processadas$alt_id)
  
  # Caso haja algum processamento relacionado a esses id_origens, removê-los para
  # garantir que todos foram processados e algum erro não os fez parar no meio
  rotas_vias_ciclo  <- rotas_vias_ciclo  %>% filter(!alt_id %in% rotas$alt_id)
  rotas_vias_comuns <- rotas_vias_comuns %>% filter(!alt_id %in% rotas$alt_id)
  
  # Reescrever arquivos de rotas_vias_comuns e rotas_vias_ciclo
  write_delim(rotas_vias_ciclo,  tmp_file1, delim = ';')
  write_delim(rotas_vias_comuns, tmp_file2, delim = ';')
  
  # Limpar ambiente
  rm(rotas_processadas, rotas_vias_ciclo, rotas_vias_comuns, tmp_file1, tmp_file2)
  gc(T)
  
} else {
  
  rm(tmp_file1)
}



# Separar ids de origem, para arquivos
ids_origem <- rotas %>% select(id_orig = alt_id) %>% mutate(id_orig = str_sub(id_orig, 1, 6)) %>% distinct()


# Separar ids das rotas que passaram só por vias comuns e as que passaram por
# alguma infraestrutura cicloviária ao longo do caminho. Neste momento, caso o
# arquivo 'tmp_ids_rotas_vias_ciclo_%s.csv' já exista, não apagá-lo pois ele
# será atualizado com as novas rotas processadas
detach('package:tidylog')
start <- Sys.time()
for (line_id in ids_origem$id_orig) {
  # line_id <- '000003'
  # print(line_id)
  
  # Abrir arquivo de OSM-ids
  if (ano == '2024') {
    osm_ids <- sprintf('%s/%s.csv', pasta_ids_aopt_24, line_id)
  } else if (ano == '2028') {
    osm_ids <- sprintf('%s/%s.csv', pasta_ids_aopt_28, line_id)
  }
  
  osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'ciic')
  osm_ids <- osm_ids %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'))
  # head(osm_ids)
  
  # Manter somente OSM_IDs relacionados às rotas dentro do limite de tempo estabelecido
  osm_ids <- osm_ids %>% filter(alt_id %in% rotas$alt_id) %>% select(alt_id, osm_way_id)
  
  # Juntar com dados de infra_ciclo
  osm_ids <- osm_ids %>% left_join(infra_ciclo, by = c('osm_way_id' = 'osm_id'))
  # head(osm_ids)
  
  
  # Quais rotas passaram por alguma infra cicloviária?
  rotas_ciclo <- osm_ids %>% filter(!is.na(infra_ciclo)) %>% select(alt_id) %>% distinct()
  
  # Gravar essas rotas em um arquivo temporário
  tmp_file1 <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
  if (file.exists(tmp_file1)) {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = FALSE)
  }
  
  
  # Quais rotas não passaram por nenhuma infra cicloviária?
  rotas_vias_comuns <- osm_ids %>% filter(!alt_id %in% rotas_ciclo$alt_id) %>% select(alt_id) %>% distinct()
  
  # Gravar essas rotas em um arquivo temporário
  tmp_file2 <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
  if (file.exists(tmp_file2)) {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = FALSE)
  }
  
  
}
Sys.time() - start

# Limpar ambiente
rm(line_id, osm_ids, rotas_ciclo, rotas_vias_comuns, tmp_file1, tmp_file2,
   ids_origem, rotas, infra_ciclo)
gc(T)


# ------------------------------------------------------------------------------
# Checagem - ficou alguma linha processada cujos resultados não estão nas pastas?
# ------------------------------------------------------------------------------

library('tidylog')

if (ano == '2024') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/05_base_alternatives_%s_res09_20min.csv', pasta_ttmatrix_24_28, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
rotas <- rotas %>% filter(time <= 900) %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()


# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
rotas_vias_comuns <- read_delim(rotas_vias_comuns, delim = ';', col_types = 'c')

# Abrir rotas que só passaram por vias comuns
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Aqui estão todas as rotas para gerar o ttmatrix final
rotas_a_processar <- rbind(rotas_vias_comuns, rotas_vias_ciclo)


# 89a81044d93ffff-89a81046b67ffff 
# 44d93-46b67-1 
faltam <- rotas %>% filter(!alt_id %in% rotas_a_processar$alt_id)
faltam %>% select(alt_id) %>% mutate(alt_id = str_sub(alt_id, 1, 6)) %>% distinct()


if (nrow(faltam) > 0) {
  # Guardar resultados - base integral
  hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_ttmatrix_24_28)
  hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')
  
  faltam <- faltam %>% select(id = alt_id) %>% mutate(id = str_replace(id, 
                                                                       '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                                                       '89a81\\1ffff-89a81\\2ffff'))
  
  out_file <- sprintf('%s/xxx_faltam_processar.csv', pasta_ttmatrix_24_28)
  write_delim(faltam, out_file, delim = ';')
  
  print('ATENÇÃO: RODAR NOVAMENTE OS SCRIPTS 12.0X-teste_ttmatrix_hexagono_ZL_rede20XX.R')
  
  rm(out_file, hex_com_vizinhos)
  
} else {
  print('TODOS OS ARQUIVOS QUE DEVERIAM TER SIDO PROCESSADOS ESTÃO OK')
}

# Limpar ambiente
rm(rotas_a_processar, rotas_vias_ciclo, rotas_vias_comuns)
gc(T)


# ------------------------------------------------------------------------------
# Tratamento - rotas que só passaram por vias comuns
# ------------------------------------------------------------------------------

# # Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
# if (ano == '2024') {
#   rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_ttmatrix_24_28, ano)
# } else if (ano == '2028') {
#   rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_ttmatrix_24_28, ano)
# }
# rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
# rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')

# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
rotas_vias_comuns <- read_delim(rotas_vias_comuns, delim = ';', col_types = 'c')

# Filtrar rotas de interesse
rotas_comuns <- rotas %>% filter(alt_id %in% rotas_vias_comuns$alt_id) %>% select(-alt_id)
rotas_comuns <- rotas_comuns %>% mutate(hex_id = str_replace(hex_id, 
                                                             '^([a-z0-9]{6})-([a-z0-9]{6})', 
                                                             '89a81\\1ffff-89a81\\2ffff'))

# Limpar ambiente
rm(rotas, rotas_vias_comuns)
gc(T)


# Constituir resumo da rota, com colunas que vão ficar no ttmatrix final
rotas_comuns <- 
  rotas_comuns %>% 
  mutate(via_comum      = distance, 
         infra_ciclo    = 0, 
         ciclo_expressa = 0, 
         ciclovia_comum = 0, 
         ciclofaixa     = 0,
         .before = 'poly')

# Gravar resultados
if (ano == '2024') {
  out_file <- sprintf('%s/06_tmp_ttmatrix_%s_rotas_vias_comuns_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
} else if (ano == '2028') {
  out_file <- sprintf('%s/07_tmp_ttmatrix_%s_rotas_vias_comuns_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
}
# Garantir que arquivo existente não será sobrescrito
if (file.exists(out_file)) { file.rename(from = out_file, to = sprintf('%s_BKP_APAGAR', out_file)) }
write_delim(rotas_comuns, out_file, delim = ';')


# Limpar ambiente
rm(rotas_comuns, out_file)
gc(T)
