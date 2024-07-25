# De todas as rotas com até 40 minutos, separa as que passaram por rede cicloviária
# e as que não passaram. Para as que não passaram, já gera a ttmatrix inicial,
# enquanto as que passaram vão ser processadas nos scripts seguintes

# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_osm_sp      <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs     <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_alter)

ano <- '2019'
# ano <- '2028'



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
out_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_alter, ano)
write_delim(infra_ciclo, out_ciclo, delim = ';')

# Limpar ambiente
rm(ciclo_expressas, ciclo_ciclofx, ciclo_comuns, ciclo_ciclov_semsem, out_ciclo)
gc(T)


# ------------------------------------------------------------------------------
# Identificar rotas passaram ou não por infra cicloviária - leva cerca de 3h30
# ------------------------------------------------------------------------------

# Abrir base de rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-')) %>% select(alt_id, distance)
head(rotas)
gc(T)



# Checar rotas que já foram processadas para exclusão

# Abrir rotas que só passaram por vias comuns
tmp_file1 <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_ciclo <- read_delim(tmp_file1, delim = ';', col_types = 'c')
rotas_vias_ciclo <- rotas_vias_ciclo %>% filter(!is.na(alt_id))

# Abrir rotas que só passaram por vias comuns
tmp_file2 <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_comuns <- read_delim(tmp_file2, delim = ';', col_types = 'c')
rotas_vias_comuns <- rotas_vias_comuns %>% filter(!is.na(alt_id))

# Aqui estão todas as rotas para gerar o ttmatrix final
rotas_a_processar <- rbind(rotas_vias_comuns, rotas_vias_ciclo)

# Excluir rotas que já foram processadas
rotas <- rotas %>% filter(!alt_id %in% rotas_a_processar$alt_id)

# Caso haja algum processamento relacionado a esses id_origens, removê-los para
# garantir que todos foram processados e algum erro não os fez parar no meio
rotas_vias_ciclo  <- rotas_vias_ciclo  %>% filter(!alt_id %in% rotas$alt_id)
rotas_vias_comuns <- rotas_vias_comuns %>% filter(!alt_id %in% rotas$alt_id)

# Reescrever arquivos de rotas_vias_comuns e rotas_vias_ciclo
write_delim(rotas_vias_ciclo,  tmp_file1, delim = ';')
write_delim(rotas_vias_comuns, tmp_file2, delim = ';')

# Limpar ambiente
rm(rotas_a_processar, rotas_vias_ciclo, rotas_vias_comuns, tmp_file1, tmp_file2)
gc(T)


# Separar ids de origem, para arquivos
ids_origem <- rotas %>% select(id_orig = alt_id) %>% mutate(id_orig = str_sub(id_orig, 1, 6)) %>% distinct()


# Separar ids das rotas que passaram só por vias comuns e as que passaram por
# alguma infraestrutura cicloviária ao longo do caminho
detach('package:tidylog')
start <- Sys.time()
for (line_id in ids_origem$id_orig) {
  # line_id <- '000003'
  # print(line_id)
  
  # Abrir arquivo de OSM-ids
  if (ano == '2019') {
    osm_ids <- sprintf('%s/%s.csv', pasta_ids_aopt_19, line_id)
  } else if (ano == '2028') {
    osm_ids <- sprintf('%s/%s.csv', pasta_ids_aopt_28, line_id)
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
  tmp_file1 <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
  if (file.exists(tmp_file1)) {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_ciclo, tmp_file1, delim = ';', append = FALSE)
  }
  
  
  # Quais rotas não passaram por nenhuma infra cicloviária?
  rotas_vias_comuns <- osm_ids %>% filter(!alt_id %in% rotas_ciclo$alt_id) %>% select(alt_id) %>% distinct()
  
  # Gravar essas rotas em um arquivo temporário
  tmp_file2 <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_alter, ano)
  if (file.exists(tmp_file2)) {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = TRUE)
  } else {
    write_delim(rotas_vias_comuns, tmp_file2, delim = ';', append = FALSE)
  }
  
  
}
Sys.time() - start

# Limpar ambiente
rm(line_id, osm_ids, rotas_ciclo, rotas_vias_comuns, tmp_file1, tmp_file2,
   ids_origem, rotas)
gc(T)


# ------------------------------------------------------------------------------
# Checagem - ficou alguma linha processada cujos resultados não estão nas pastas?
# ------------------------------------------------------------------------------

library('tidylog')

if (ano == '2019') {
  rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')


# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_comuns <- read_delim(rotas_vias_comuns, delim = ';', col_types = 'c')

# Abrir rotas que só passaram por vias comuns
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Aqui estão todas as rotas para gerar o ttmatrix final
rotas_a_processar <- rbind(rotas_vias_comuns, rotas_vias_ciclo)


# 89a81044d93ffff-89a81046b67ffff 
# 44d93-46b67-1 
faltam <- rotas %>% filter(!alt_id %in% rotas_a_processar$alt_id)
faltam %>% select(alt_id) %>% mutate(alt_id = str_sub(alt_id, 1, 6)) %>% distinct()


if (nrow(faltam) > 0) {
  # Guardar resultados - base integral
  hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
  hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')
  
  faltam <- faltam %>% select(id = alt_id) %>% mutate(id = str_replace(id, 
                                                                       '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                                                       '89a81\\1ffff-89a81\\2ffff'))
  
  out_file <- sprintf('%s/xxx_faltam_processar.csv', pasta_aoprv_alter)
  write_delim(faltam, out_file, delim = ';')
  
  print('ATENÇÃO: RODAR NOVAMENTE OS SCRIPTS 12.0X-teste_ttmatrix_hexagono_ZL_rede20XX.R')
  
  rm(out_file, hex_com_vizinhos)
  
} else {
  print('TODOS OS ARQUIVOS QUE DEVERIAM TER SIDO PROCESSADOS ESTÃO OK')
}

# Limpar ambiente
rm(faltam, rotas_a_processar, rotas_vias_ciclo, rotas_vias_comuns, infra_ciclo)
gc(T)


# ------------------------------------------------------------------------------
# Tratamento - rotas que só passaram por vias comuns
# ------------------------------------------------------------------------------

# # Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
# if (ano == '2019') {
#   rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
# }
# rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
# rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')

# Abrir rotas que só passaram por vias comuns
rotas_vias_comuns <- sprintf('%s/tmp_ids_rotas_vias_comuns_%s.csv', pasta_aoprv_alter, ano)
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
         ciclo_comum    = 0, 
         ciclofaixa     = 0,
         .before = 'poly')

# Gravar resultados
if (ano == '2019') {
  out_file <- sprintf('%s/02_tmp_ttmatrix_%s_rotas_vias_comuns.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  out_file <- sprintf('%s/05_tmp_ttmatrix_%s_rotas_vias_comuns.csv', pasta_aoprv_alter, ano)
}
write_delim(rotas_comuns, out_file, delim = ';')


# Limpar ambiente
rm(rotas_comuns, out_file)
gc(T)

