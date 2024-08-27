# Junta as ttmatrix iniciais de rotas em vias comuns e rotas que passam por infra 
# cicloviária, recalcula os tempos em cycleway devido ao ajuste que fizemos no 
# momento do roteamento e, finalmente, escolhe a rota com maior uso de infra ciclo
# para compor a ttmatrix final de 40 minutos

# carregar bibliotecas
library('tidyverse')
library('tidylog')

# Definir ano e limites de tempo para ttmatrix final
# ano <- '2019'; min_thres <- 15; sec_thres <- min_thres * 60
ano <- '2028'; min_thres <- 15; sec_thres <- min_thres * 60

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ttmat <- sprintf("%s/01_ttmatrix", pasta_aop_optimum, ano)
dir.create(pasta_opaop_ttmat, recursive = TRUE, showWarnings = TRUE)


# ------------------------------------------------------------------------------
# Abrir ttmatrix iniciais (vias comuns e vias com infra cicloviária)
# ------------------------------------------------------------------------------

if (ano == '2019') {
  tt_ini_vias_comuns  <- sprintf("%s/02_tmp_ttmatrix_%s_rotas_vias_comuns.csv", pasta_aoprv_alter, ano)
  tt_ini_vias_ciclos  <- sprintf("%s/03_tmp_ttmatrix_%s_rotas_infra_ciclo.csv", pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  tt_ini_vias_comuns  <- sprintf("%s/05_tmp_ttmatrix_%s_rotas_vias_comuns.csv", pasta_aoprv_alter, ano)
  tt_ini_vias_ciclos  <- sprintf("%s/06_tmp_ttmatrix_%s_rotas_infra_ciclo.csv", pasta_aoprv_alter, ano)
}

tt_ini_vias_comuns <- read_delim(tt_ini_vias_comuns, delim = ';', col_types = 'cidddddddddc')
tt_ini_vias_comuns <- tt_ini_vias_comuns %>% distinct() %>% filter(!is.na(hex_id))
tt_ini_vias_ciclos <- read_delim(tt_ini_vias_ciclos, delim = ';', col_types = 'cidddddddddc')
tt_ini_vias_ciclos <- tt_ini_vias_ciclos %>% distinct() %>% filter(!is.na(hex_id))


# ------------------------------------------------------------------------------
# Compensar tempos em cycleways devido à velocidade alterada
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
tt_ini_vias_ciclos <- 
  tt_ini_vias_ciclos %>% 
  mutate(time_dif = ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (0.139447 / 3.6))) - ((ciclo_comum + ciclo_expressa) / ((speed / 3.6) + (1.945864 / 3.6))),
         time_adj  = time + time_dif,
         .before = 'speed')

# No caso das rotas que não passam por 'cycleways', ajuste de tempo será zero
# e tempo ajustado é igual ao tempo original
tt_ini_vias_comuns <- tt_ini_vias_comuns %>% mutate(time_dif = 0,
                                                    time_adj = time,
                                                    .before  = 'speed')

# Reconstituir dataframe completo, para gerar ttmatrix final
ttmatrix <- rbind(tt_ini_vias_ciclos, tt_ini_vias_comuns)

# Limpar ambiente
rm(tt_ini_vias_ciclos, tt_ini_vias_comuns)
gc(T)


# ------------------------------------------------------------------------------
# Refazer filtro de tempo máximo de deslocamento
# ------------------------------------------------------------------------------

# Como as rotas que passam por ciclovias comuns e expressas tiveram seus tempos
# totais reajustados, o filtro por limite máximo de tempo precisa ser refeito

# Filtrar pelo limite de tempo, considerando a coluna time_adj
ttmatrix <- ttmatrix %>% filter(time_adj <= sec_thres)


# ------------------------------------------------------------------------------
# Selecionar rotas que passam por mais infra ciclo e finalizar ttmatrix
# ------------------------------------------------------------------------------

# Selecionar as rotas a serem consideradas - se há mais uma alternativa, considerar
# a com mais uso de infra cicloviária - primeiro descobrir quais são...
alt_nao_unica <- ttmatrix %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% ungroup()
# ... depois, filtrar do ttmatrix
# 2019 (15 min): filter: removed 438,315 rows (39%), 697,948 rows remaining
# 2028 (15 min): filter: removed 437,574 rows (33%), 891,673 rows remaining
alt_nao_unica <- ttmatrix %>% filter(hex_id %in% alt_nao_unica$hex_id)

# Por contraposição, isolar as rotas que só têm 1 alternativa
# 2019 (15 min): filter: removed 697,948 rows (61%), 438,315 rows remaining
# 2028 (15 min): filter: removed 891,673 rows (67%), 437,574 rows remaining
alt_unica <- ttmatrix %>% filter(!hex_id %in% alt_nao_unica$hex_id)
# nrow(alt_nao_unica) + nrow(alt_unica) == nrow(rotas)

# Guardar qual o tamanho que o dataframe de alternativas não únicas tem que ter
# após selecionada uma única alternativa, das existentes
# 2019 (15 min): 734,631
# 2028 (15 min): 809,403
qtd_linhas_final <- ttmatrix %>% select(hex_id) %>% distinct() %>% nrow()
# 2019 (15 min): 296,316
# 2028 (15 min): 371,829
faltam_linhas <- qtd_linhas_final - nrow(alt_unica)

# Limpar ambiente antes de continuar
rm(ttmatrix)
gc(T)


# Tentar puxar a rota que usa maior extensão percorrida em infra cicloviário
# 2019 (15 min): filter (grouped): removed 173,184 rows (25%), 524,764 rows remaining
# 2028 (15 min): filter (grouped): removed 502,260 rows (56%), 389,413 rows remaining
alt_nao_unica <- alt_nao_unica %>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo))
# Se ainda houver empate, pegar a de menor tempo
if (nrow(alt_nao_unica) > faltam_linhas) {
  # 2019 (15 min): filter (grouped): removed 228,448 rows (44%), 296,316 rows remaining
  # 2028 (15 min): filter (grouped): removed 17,584 rows (5%), 371,829 rows remaining
  alt_nao_unica <- alt_nao_unica %>% filter(time_adj == min(time_adj))
}
# Se ainda houver empate, pegar a de menor distância
if (nrow(alt_nao_unica) > faltam_linhas) {
  # 2019 (15 min): NA
  # 2028 (15 min): NA
  alt_nao_unica <- alt_nao_unica %>% filter(distance == min(distance))
}
# Se ainda houver empate, pegar a primeira alternativa
# alt_nao_unica %>% group_by(hex_id) %>% tally() %>% filter(n > 1)
if (nrow(alt_nao_unica) > faltam_linhas) {
  # 2019 (15 min): NA
  # 2028 (15 min): NA
  alt_nao_unica <- alt_nao_unica %>% filter(alt == min(alt))
}
alt_nao_unica <- alt_nao_unica %>% ungroup()
# alt_nao_unica %>% filter(hex_id == '89a81044d93ffff-89a81046d47ffff')


# Juntar rotas escolhidas em dataframe de saída
ttmatrix_final <- rbind(alt_unica, alt_nao_unica) %>% arrange(hex_id)
# ttmatrix_final %>% filter(infra_ciclo > 0) %>% sample_n(20)
# ttmatrix_final %>% filter(time_dif > 0) %>% sample_n(20)


# Gravar resultados
out_file <- sprintf('%s/ttmatrix_%s_res09_%smin.csv', pasta_opaop_ttmat, ano, min_thres)
write_delim(ttmatrix_final, out_file, delim = ';')


# # Limpar ambiente
# rm(alt_unica, alt_nao_unica)
# gc(T)


# ------------------------------------------------------------------------------
# Recompor dados de latlon e gravar resultados
# ------------------------------------------------------------------------------

# Se precisar recompor latlons originais das origens e destinos, puxar do arquivo
# que foi a origem do routing (contém o id e a URL para o routing), a seguir

# # Abrir resultado da ttmatrix sem dados de latlon
# out_file <- sprintf('%s/01_ttmatrix_%s_res09_%smin.csv', pasta_opaop_ttmat, ano, min_thres)
# ttmatrix_final <- read_delim(out_file, delim = ';', col_types = 'cidddddddddc')
# 
# # Abrir dados de hexágonos - vamos reconstituir latlon x e y em ttmatrix_rotas_ciclo
# hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
# hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = 'cc')
# 
# hex_com_vizinhos <- 
#   hex_com_vizinhos %>% 
#   mutate(url = str_extract(url, '-23.[0-9]*%2C-46.[0-9]*&point=-23.[0-9]*%2C-46.[0-9]*')) %>% 
#   mutate(url = str_replace_all(url, '&point=|%2C', ',')) %>%
#   separate(url, into = c('lat.x', 'lon.x', 'lat.y', 'lon.y'), sep = ',')
# 
# 
# # Juntar dados de latlon à ttmatrix final
# ttmatrix_final <- ttmatrix_final %>% left_join(hex_com_vizinhos, by = c('hex_id' = 'id'))
# 
# 
# # Gravar resultados
# out_file <- sprintf('%s/01_ttmatrix_%s_res09_%smin.csv', pasta_opaop_ttmat, ano, min_thres)
# write_delim(ttmatrix_final, out_file, delim = ';')
