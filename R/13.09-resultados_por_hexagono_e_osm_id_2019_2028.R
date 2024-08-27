# Agrega os resultados do lpSolve por hexágono e por trecho de osm_id.

# carregar bibliotecas
library('tidyverse')
library('tidylog')

# Definir ano de análise e limite máximo de tempo
# ano <- '2019'; tempo_max <- '15'
ano <- '2028'; tempo_max <- '15'

# Qual solução usar? A primeira considera população de interesse maior do que as
# matrículas disponíveis; a segunda ajusta a população para caber nas matrículas
versao_solucao <- 2

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ttmat <- sprintf("%s/01_ttmatrix", pasta_aop_optimum, ano)
if (versao_solucao == 1) {
  pasta_aop_lpsolve <- sprintf("%s/03_lpSolve1_pop_maior_que_mat", pasta_aop_optimum)
} else if (versao_solucao == 2) {
  pasta_aop_lpsolve <- sprintf("%s/04_lpSolve2_pop_ajustada", pasta_aop_optimum)
}
pasta_opaop_ano   <- sprintf("%s/%s", pasta_aop_lpsolve, ano)
rm(versao_solucao)


# Origens, destinos e quantidade de viagens
ods_vgs <- sprintf('%s/01_lpsolve_resultados_viagens_por_par_OD_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'ccid')
# ods_vgs %>% filter(!is.na(time)) %>% select(orig, dest) %>% distinct() # 10.237
head(ods_vgs)

# Osm_ids que faziam parte da ttmatrix utilizada
osm_ids <- sprintf('%s/ttmatrix_osmids_%s_res09_%smin.csv', pasta_opaop_ttmat, ano, tempo_max)
osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'cccd')
# osm_ids %>% select(orig, dest) %>% distinct() # 2019 - 245.187
head(osm_ids)

# Desmembrar coluna hex_id_alt em origem, destino e número da alternativa da rota
osm_ids <- 
  osm_ids %>% 
  mutate(hex_id_alt = str_replace(hex_id_alt, 
                                  '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                  '89a81\\1ffff-89a81\\2ffff-\\3')) %>% 
  separate(hex_id_alt, into = c('orig', 'dest', 'alt'), sep = '-', remove = TRUE)

head(osm_ids)


# ------------------------------------------------------------------------------
# Corrigir trechos de osm_id que estão como rede de referência mas não são
# ------------------------------------------------------------------------------

# No script 12.12, fizemos um filtro da rede futura que puxou osm_ids que têm
# pequenas partes marcadas como rede de referência quando o restante desses 
# mesmos osm_ids não possuem rede cicloviária. Um exemplo é o osm_id 264984110, 
# que tem 8,06 metros de rede de referência dentre os seus 818,49 metros de 
# viário. Vamos corrigir essa classificação aqui.

if (ano == '2028') {
  # Abrir shape com a marcação da rede cicloviária de referência
  ciclo_futura <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id_redes_cicloviarias_2019_2028.gpkg', pasta_graphhopper)
  ciclo_futura <- read_sf(ciclo_futura) %>% st_drop_geometry()
  
  # # Redes: '2019', 'referencia', 'NA'
  # ciclo_futura %>% filter(osm_id == '264984110')
  # ciclo_futura %>% select(rede_cicloviaria) %>% distinct()
  
  # Puxar osm_ids com pelo menos 50% de extensão de rede cicloviária
  ciclo_futura <- 
    ciclo_futura %>% 
    # filter(osm_id == '264984110') %>% 
    # Tudo o que for NA vai ser marcado como 'sem rede', enquanto as redes 2019 e
    # de referência vão ser marcadas ambas como de referência
    mutate(rede = ifelse(is.na(rede_cicloviaria), 'sem rede', 'referencia')) %>% 
    group_by(osm_id, rede) %>% 
    summarise(ext = sum(length_m)) %>% 
    mutate(perc = ext / sum(ext) * 100) %>% 
    ungroup() %>% 
    # Manter somente os osm_ids com mais do que 50% de extensão com rede cicloviária
    filter(rede == 'referencia' & perc > 50)
  
  # Corrigir: nos osm_ids com infra cicloviária calculada percorrida pelo routing,
  # tudo o que não estiver na rede de referência (2019 + 2028) e que tenha pelo
  # menos x% de extensão com reder cicloviária, vai ser remarcado 
  osm_ids <- 
    osm_ids %>% 
    mutate(infra_ciclo = ifelse(osm_way_id %in% ciclo_futura$osm_id, infra_ciclo, as.character(NA))) %>% 
    filter(!is.na(infra_ciclo))
  
  # Gravar resultados para serem usados no script seguinte
  write_delim(osm_ids %>% select(osm_id = osm_way_id, infra_ciclo) %>% distinct(), 
              sprintf('%s/tmp_infra_ciclo_%s_revisada.csv', pasta_aoprv_alter, ano), 
              delim = ';')
}


# ------------------------------------------------------------------------------
# Chegar alguns dados principais do lpSolve
# ------------------------------------------------------------------------------

# Método lpSolve 2
# 2019 - Viagens totais na solução: 374.159
# 2028 - Viagens totais na solução: 374.282
ods_vgs %>% select(viagens) %>% sum()
# 2019 - Viagens pares OD tempo maior que limite: 25.863 (6.912302% do total)
# 2028 - Viagens pares OD tempo maior que limite: 23.177 (6.19239% do total)
ods_vgs %>% filter(is.na(time)) %>% select(viagens) %>% sum()
# 2019 - Viagens válidas: 348.296 (93.0877% do total)
# 2028 - Viagens válidas: 351.105 (93.80761% do total)
ods_vgs %>% filter(!is.na(time)) %>% select(viagens) %>% sum()



# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por hexágono
# ------------------------------------------------------------------------------

# Qual o número total de viagens geradas por cada hexágono de origem?
# Qual a extensão percorrida em infra cicloviária quando somadas todas as rotas
# que saem de cada hexágono de origem?

# # Visão por hexágonos de origem: quanto de distância (em metros) cada hexágono 
# # percorreu em infraestrutura cicloviária em determinado ano base? 
# # IMPORTANTE: desta forma, a partir dos osm_ids, estamos olhando SOMENTE as
# # extensões percorridas em infra cicloviária
# hex_resultados <- 
#   osm_ids %>%
#   # Agrupar: extensão percorrida em infra cicloviária para cada par OD
#   group_by(orig, dest) %>% 
#   summarise(ext_ciclo = sum(ext_percorrida)) %>% 
#   # Juntar quantidade de viagens para cada par OD
#   left_join(ods_vgs, by = c('orig', 'dest')) %>% 
#   # Queremos somente rotas em que houve viagens
#   filter(!is.na(viagens)) %>% 
#   # ungroup() %>% select(viagens) %>% sum()
#   # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
#   # percorrida por rota
#   mutate(ext_ciclo_vgs = ext_ciclo * viagens) %>% 
#   # Calcular total de viagens em cada hexágono de origem e total da extensão
#   # (em metros) percorrida em infra cicloviária a partir daquela origem
#   group_by(orig) %>% 
#   summarise(viagens_tot     = sum(viagens),
#             ext_ciclo_tot_m = sum(ext_ciclo_vgs))
# 
# # Calcular percentual de participação do todo
# hex_resultados <- 
#   hex_resultados %>% 
#   mutate(perc_part = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)
# 
# sum(hex_resultados$viagens_tot)


# Agrupar: extensão percorrida em infra cicloviária para cada par OD
ciclo_od <- osm_ids %>% group_by(orig, dest) %>% summarise(ext_ciclo = sum(ext_percorrida)) %>% ungroup()

# Gerar resultados iniciais por pares OD - aqui, ainda há viagens que ainda estão
# acima do limite de tempo estabelecido (time == NA) e viagens que passaram por
# zero metros de infraestrutura cicloviária (ext_ciclo_vgs == 0)
ods_resultados <- 
  # Partindo dos pares OD que vêm dos resultados do lpSolve
  ods_vgs %>% 
  # Juntar quantidade de viagens para cada par OD
  left_join(ciclo_od, by = c('orig', 'dest')) %>% 
  # Substituir NAs por zero para viagens que não passaram por infra_ciclo
  mutate(ext_ciclo = ifelse(is.na(ext_ciclo), 0, ext_ciclo)) %>% 
  # Agrupar: extensão percorrida em infra cicloviária para cada par OD
  group_by(orig, dest) %>% 
  # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
  # percorrida por rota
  mutate(ext_ciclo_vgs = ext_ciclo * viagens) %>% 
  ungroup()


# Agrupar por hexágonos, somente as viagens dentro do limite de tempo - resultado
# é hexágonos com viagens todas a tempo ou em parte a tempo
hex_resultados_tempo_ok <- 
  ods_resultados %>% 
  filter(!is.na(time)) %>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(orig) %>% 
  summarise(viagens_a_tempo = sum(viagens),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs)) %>% 
  mutate(perc_part = ext_ciclo_tot_m / sum(.$ext_ciclo_tot_m) * 100) %>% 
  ungroup()
  
# sample_n(hex_resultados_tempo_ok, 20)
# hex_resultados_tempo_ok %>% filter(is.na(perc_part))

# Agrupar por hexágonos, somente as viagens fora do limite de tempo - resultado
# é hexágonos com viagens todas fora do tempo ou em parte fora do tempo
hex_resultados_fora_tempo <- 
  ods_resultados %>% 
  filter(is.na(time)) %>% 
  # Calcular total de viagens em cada hexágono de origem
  group_by(orig) %>% 
  summarise(viagens_fora_tempo = sum(viagens)) %>% 
  ungroup()


# Juntar resultados em um dataframe único
hex_resultados <- 
  hex_resultados_tempo_ok %>% 
  full_join(hex_resultados_fora_tempo, by = 'orig') %>% 
  replace(is.na(.), 0) %>%
  mutate(tot_viagens = viagens_a_tempo + viagens_fora_tempo) %>%
  # Categorizar hexágonos de origem segundo viagens presentes nos resultados
  mutate(class_hex_orig = case_when(viagens_a_tempo > 0 & viagens_fora_tempo == 0 ~ 'viagens todas a tempo',
                                    viagens_a_tempo == 0 & viagens_fora_tempo > 0 ~ 'viagens fora do tempo',
                                    viagens_a_tempo > 0 & viagens_fora_tempo > 0 ~ 'viagens parc. a tempo',
                                    TRUE ~ NA),
         .after = 'orig')

head(hex_resultados)

# Checagem
sum(hex_resultados$tot_viagens) == sum(ods_vgs$viagens)
# sum(hex_resultados$perc_part)

# Gravar resultados
hex_resultados_out <- sprintf('%s/07_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(hex_resultados, hex_resultados_out, delim = ';')


# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por osm_id
# ------------------------------------------------------------------------------

# Qual o número total de viagens que passam por cada osm_id?
# Quanto cada osm_id é importante? Ou seja, qual a extensão em infra cicloviária 
# percorrida em cada osm_id quando somadas todas as rotas que passam por aquele
# osm_id?


# Visão por infraestrutura cicloviária utilizada: quanto de distância (em metros) 
# foi percorrido em cada osm_id em determinado ano base?
osm_id_resultados <- 
  ods_vgs %>% 
  # Juntar quantidade de viagens para cada par OD
  left_join(osm_ids, by = c('orig', 'dest')) %>% 
  # Queremos somente os osm_id em que há infra cicloviária. Todas elas vão estar
  # dentro do tempo delimitado. Há viagens que possuem tempo 0 mas não têm
  # osm_ids - isso porque são as viagens de um hexágono para ele mesmo, então
  # não usam osm_ids no caminho:
  # filter(!is.na(time)) %>% sample_n(20)
  filter(!is.na(osm_way_id)) %>%
  # Substituir NAs por zero para viagens que não passaram por infra_ciclo
  mutate(ext_percorrida = ifelse(is.na(ext_percorrida), 0, ext_percorrida)) %>% 
  # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
  # percorrida por rota
  mutate(ext_ciclo_vgs = ext_percorrida * viagens)



# Considerar somente as viagens dentro do limite de tempo
hex_resultados_tempo_ok <- 
  osm_id_resultados %>% 
  filter(!is.na(time)) %>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(orig) %>% 
  summarise(viagens_a_tempo = sum(viagens),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs)) %>% 
  mutate(perc_part = ext_ciclo_tot_m / sum(.$ext_ciclo_tot_m) * 100) %>% 
  ungroup()



%>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(osm_way_id) %>% 
  summarise(viagens_tot     = sum(viagens),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs))


# # Visão por infraestrutura cicloviária utilizada: quanto de distância (em metros) 
# # foi percorrido em cada osm_id em determinado ano base?
# osm_id_resultados <- 
#   osm_ids %>%
#   # Juntar quantidade de viagens para cada par OD
#   left_join(ods_vgs, by = c('orig', 'dest')) %>% 
#   # Queremos somente rotas em que houve viagens
#   filter(!is.na(viagens)) %>% 
#   # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
#   # percorrida por rota
#   mutate(ext_ciclo_vgs = ext_percorrida * viagens) %>% 
#   # Calcular total de viagens em cada hexágono de origem e total da extensão
#   # (em metros) percorrida em infra cicloviária a partir daquela origem
#   group_by(osm_way_id) %>% 
#   summarise(viagens_tot     = sum(viagens),
#             ext_ciclo_tot_m = sum(ext_ciclo_vgs))

# Checagem - soma das extensões percorridas em infraciclo devem ser as mesmas
sum(hex_resultados$ext_ciclo_tot_m) == sum(osm_id_resultados$ext_ciclo_tot_m)

# Calcular percentual de participação do todo
osm_id_resultados <- 
  osm_id_resultados %>% 
  mutate(perc_part = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)

head(osm_id_resultados)
# sum(osm_id_resultados$perc_part)

# Gravar resultados
osm_id_resultados_out <- sprintf('%s/08_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(osm_id_resultados, osm_id_resultados_out, delim = ';')

