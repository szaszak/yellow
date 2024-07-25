# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
pasta_osmids_aopt2 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt2  <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_teste)

# ano <- '2019'; min_thres <- 40; sec_thres <- min_thres * 60
ano <- '2028'; min_thres <- 40; sec_thres <- min_thres * 60


# ------------------------------------------------------------------------------
# Identificação de quais rotas passaram ou não por infra cicloviária
# ------------------------------------------------------------------------------

# Abrir base de rotas com alternativas, resultado do routing via GrahHopper
if (ano == '2019') {
  rotas <- sprintf('%s/01_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  rotas <- sprintf('%s/04_base_teste_alternatives_%s_res09_40min.csv', pasta_aoprv_teste, ano)
}
rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'cidddddddddc')
# rotas <- rotas %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'), .before = 'hex_id')
head(rotas)


# Quais rotas passaram por alguma infra cicloviária?
rotas_ciclovias <- rotas %>% filter(ciclo_comum > 0 | ciclo_expressa > 0)

# Quais rotas não passaram por nenhuma infra cicloviária?
rotas_vias_comuns <- rotas %>% filter(ciclo_comum == 0 & ciclo_expressa == 0)
nrow(rotas_vias_comuns) + nrow(rotas_ciclovias) == nrow(rotas)


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

# Criar coluna de time_adj, onde os tempos serão compensados
rotas_ciclovias <- 
  rotas_ciclovias %>% 
  mutate(time_dif = ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (0.139447 / 3.6))) - ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (1.945864 / 3.6))),
         time_adj  = time + time_dif,
         .before = 'speed')

# No caso das rotas que não passam por 'cycleways', ajuste de tempo será zero
# e tempo ajustado é igual ao tempo original
rotas_vias_comuns <- rotas_vias_comuns %>% mutate(time_dif = 0,
                                                  time_adj = time,
                                                  .before  = 'speed')

# Reconstituir dataframe completo, para gerar ttmatrix final
rotas <- rbind(rotas_ciclovias, rotas_vias_comuns)
rm(rotas_ciclovias, rotas_vias_comuns)


# ------------------------------------------------------------------------------
# Finalizar ttmatrix
# ------------------------------------------------------------------------------

# Filtrar pelo limite de tempo, considerando a coluna time_adj
rotas <- rotas %>% filter(time_adj <= sec_thres)

# Selecionar as rotas a serem consideradas - se há mais uma alternativa, considerar
# a com mais uso de infra cicloviária - primeiro descobrir quais são...
alt_nao_unica <- rotas %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% ungroup()
# ... depois, filtrar do ttmatrix
alt_nao_unica <- rotas %>% filter(hex_id %in% alt_nao_unica$hex_id)

# Por contraposição, isolar as rotas que só têm 1 alternativa
alt_unica <- rotas %>% filter(!hex_id %in% alt_nao_unica$hex_id)
# nrow(alt_nao_unica) + nrow(alt_unica) == nrow(rotas)

# Guardar qual o tamanho que o dataframe de alternativas não únicas tem que ter
# após selecionada uma única alternativa, das existentes
qtd_linhas_final <- rotas %>% select(hex_id) %>% distinct() %>% nrow() # 1052
# faltam_linhas <- qtd_linhas_final - nrow(alt_unica) # 764


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
ttmatrix <- rbind(alt_unica, alt_nao_unica) %>% arrange(hex_id)
# ttmatrix %>% filter(infra_ciclo > 0) %>% sample_n(20)
# ttmatrix %>% filter(time_dif > 0) %>% sample_n(20)

# Selecionar ids encurtados que aparecem na ttmatrix para uso na seção seguinte
ids_ttmatrix <- 
  ttmatrix %>% 
  mutate(hex_id_alt = str_c(hex_id, alt, sep = '-')) %>% 
  select(hex_id, hex_id_alt)


# ------------------------------------------------------------------------------
# Recompor dados de latlon e gravar resultados
# ------------------------------------------------------------------------------

# Abrir dados de hexágonos - vamos reconstituir latlon x e y em ttmatrix_rotas_ciclo
hex_com_vizinhos <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')

# Recriar latlon x e y para inserir no dataframe principal
hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = str_extract(url, '-23.[0-9]*%2C-46.[0-9]*&point=-23.[0-9]*%2C-46.[0-9]*')) %>% 
  mutate(url = str_replace_all(url, '&point=|%2C', ',')) %>%
  separate(url, into = c('lat.x', 'lon.x', 'lat.y', 'lon.y'), sep = ',')

# Juntar dados de latlon
ttmatrix <- 
  ttmatrix %>% 
  # select(hex_id) %>% 
  mutate(hex_id = str_c('89a81', str_sub(hex_id, 1, 6), 'ffff-89a81', str_sub(hex_id, 8, 13), 'ffff', sep = '')) %>% 
  left_join(hex_com_vizinhos, by = c('hex_id' = 'id'))


# Gravar resultados
if (ano == '2019') {
  out_file <- sprintf('%s/02_ttmatrix_final_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
} else if (ano == '2028') {
  out_file <- sprintf('%s/05_ttmatrix_final_teste_hexagono_ZL_%s_infraciclo.csv', pasta_aoprv_teste, ano)
}
write_delim(ttmatrix, out_file, delim = ';')

# Limpar ambiente
rm(out_file, ttmatrix, alt_nao_unica, alt_unica, qtd_linhas_final, hex_com_vizinhos, rotas)
gc(T)


# ------------------------------------------------------------------------------
# Selecionar osm_ids que aparecem na ttmatrix final
# ------------------------------------------------------------------------------

# Juntar arquivos que possuem os osm_ids das rotas percorridas com infraciclo
if (ano == '2019') {
  ids_file    <- sprintf('%s/03_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_teste, ano)
  osmid_files <- data.frame(arqs = list.files(pasta_osmids_aopt, recursive = FALSE, full.names = TRUE))
} else if (ano == '2028') {
  ids_file    <- sprintf('%s/06_osm_way_ids_aop_ciclo_%s.csv', pasta_aoprv_teste, ano)
  osmid_files <- data.frame(arqs = list.files(pasta_osmids_aopt2, recursive = FALSE, full.names = TRUE))
}

# Quantidade de arquivos a serem abertos tem que ser a mesma dos hex_id na ttmatrix - 
# o que muda é dentro deles, onde pode haver mais de uma alternativa
osmid_files <- osmid_files %>% mutate(hex_id = str_extract(arqs, '[0-9a-z]{6}-[0-9a-z]{6}'))
osmid_files <- osmid_files %>% filter(hex_id %in% ids_ttmatrix$hex_id)
head(osmid_files)
# # Checando algumas rotas que ficaram de fora (são por tempo)
# this <- list.files(pasta_rotas_aopt, '044d93-073573_modalt_2019.csv', full.names = TRUE)
# this <- read_delim(this, delim = ';', col_types = 'cidddddddddc')


detach("package:tidylog")
for (this_file in osmid_files$arqs) { 
  # this_file <- osmid_files %>% slice(122) %>% select(arqs) %>% pull()
  # this_file <- osmid_files %>% filter(hex_id == '044d93-073303') %>% select(arqs) %>% pull()
  
  # TODO: revisar esta linha quando rodar com a base toda - colunas X1 e smoothie devem ter sido removidas
  # this <- read_delim(this_file, delim = ';', col_types = 'cicdc')
  this <- read_delim(this_file, delim = ';', col_types = 'ccicdcc') %>% select(-c(X1, smoothie))
  # this %>% select(infra_ciclo) %>% distinct()
  # tail(this)

  # Remover alternativas não selecionadas
  this <- this %>% mutate(hex_id_alt = str_c(hex_id, alt, sep = '-'))
  this <- this %>% filter(hex_id_alt %in% ids_ttmatrix$hex_id_alt)
  # Só nos interessam os osm_way_ids com infra cicloviária
  this <- this %>% filter(infra_ciclo != 'via_comum')
  
  # Se ainda houver dados após os filtros
  if (nrow(this) > 0) {
    # Reordenar colunas
    this <- this %>% select(hex_id_alt, osm_way_id, infra_ciclo, dist)
    
    # Gravar selecionados
    if (file.exists(ids_file)) {
      write_delim(this, ids_file, delim = ';', append = TRUE)
    } else {
      write_delim(this, ids_file, delim = ';', append = FALSE)
    }
  }
  
}

rm(this)



# Checagem - alguns arquivos que existem na pasta de rotas não estão na pasta de
# osm_ids? Por quê? Resposta: são todos com resultados NA
# hex_id          alt distance weight  time speed  poly via_comum infra_ciclo ciclo_expressa ciclo_comum ciclofaixa
# <chr>         <int>    <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>       <dbl>          <dbl>       <dbl> <chr>     
#     1 046b2f-044b57    NA       NA     NA    NA    NA    NA        NA          NA             NA          NA NA   

# rotas <- data.frame(arqs = list.files(pasta_rotas_aopt)) %>% mutate(arqs = str_extract(arqs, '[0-9a-z]{6}-[0-9a-z]{6}'))
# head(rotas)
# 
# ids   <- data.frame(arqs = list.files(pasta_osmids_aopt)) %>% mutate(arqs = str_extract(arqs, '[0-9a-z]{6}-[0-9a-z]{6}'))
# head(ids)
# 
# rotas %>% filter(arqs %nin% ids$arqs)
# 
# rotas <- data.frame(arqs = list.files(pasta_rotas_aopt, full.names = TRUE))
# rotas <- rotas %>% filter(str_detect(arqs, '046b2f-044b57'))
# rotas <- read_delim(rotas$arqs, delim = ';', col_types = 'cidddddddddc')



# # Deixar colunas de hex_id e alt no padrão do ttmatrix
# ids_finais <- ids_finais %>% mutate(hex_id_alt = str_replace(hex_id_alt, 
#                                                              '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
#                                                              '89a81\\1ffff-89a81\\2ffff_\\3'))
# ids_finais <- ids_finais %>% separate(hex_id_alt, into = c('hex_id', 'alt'), sep = '_', remove = TRUE)


