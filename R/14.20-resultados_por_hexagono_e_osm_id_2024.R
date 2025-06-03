# Agrega os resultados do lpSolve por hexágono e por trecho de osm_id. Faremos
# somente para 2024, pois no script seguinte usaremos a mesma alocação de 2024
# só que com as rotas de 2028

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')

# Definir ano de análise e limite máximo de tempo
ano <- '2024'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados          <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)
pasta_lpsolve_24_28  <- sprintf("%s/05_lpsolve_2024_2028", pasta_aop_2024_2028)
pasta_opaop_ano      <- sprintf("%s/%s", pasta_lpsolve_24_28, ano)


# Origens, destinos e quantidade de viagens - linhas com time = NA são as que 
# entraram na solução por terem custo alto (100000), mas cujo tempo estava, na 
# verdade, acima do limite permitido. Esse alto custo foi substituído por NA
ods_vgs <- sprintf('%s/01_lpsolve_resultados_viagens_por_par_OD_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'ccid')
# ods_vgs %>% filter(!is.na(time)) %>% select(orig, dest) %>% distinct() # 10.237
head(ods_vgs)

# Abrir a ttmatrix para pegar a coluna de distâncias
ttmatrix <- sprintf('%s/10_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, tempo_max)
ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'cidddddddddc', col_select = c('hex_id', 'distance'))
ttmatrix <- ttmatrix %>% separate(hex_id, into = c('orig', 'dest'), sep = '-', remove = TRUE)
ods_vgs <- ods_vgs %>% left_join(ttmatrix, by = c('orig', 'dest'))

# Osm_ids que faziam parte da ttmatrix utilizada
osm_ids <- sprintf('%s/12_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, tempo_max)
osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'cccd')
# osm_ids %>% select(orig, dest) %>% distinct() # 2024 - 245.187
head(osm_ids)

# Desmembrar coluna hex_id_alt em origem, destino e número da alternativa da rota
osm_ids <- 
  osm_ids %>% 
  mutate(hex_id_alt = str_replace(hex_id_alt, 
                                  '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                  '89a81\\1ffff-89a81\\2ffff-\\3')) %>% 
  separate(hex_id_alt, into = c('orig', 'dest', 'alt'), sep = '-', remove = TRUE)

head(osm_ids)

# Limpar ambiente
rm(ttmatrix)

# ------------------------------------------------------------------------------
# Chegar alguns dados principais do lpSolve
# ------------------------------------------------------------------------------

# 2024 - Viagens totais na solução: 334.318
# 2028 - Viagens totais na solução: 
ods_vgs %>% select(viagens) %>% sum()
# 2024 - Viagens pares OD tempo maior que limite: 10.317 (3.085984% do total)
# 2028 - Viagens pares OD tempo maior que limite:  ( % do total)
ods_vgs %>% filter(is.na(time)) %>% select(viagens) %>% sum()
# 2024 - Viagens válidas: 324.001(96.91402% do total)
# 2028 - Viagens válidas:  ( % do total)
ods_vgs %>% filter(!is.na(time)) %>% select(viagens) %>% sum()

# Qual é o tempo total da solução (custo total social)?
# 2024 - 126739437 segundos
# 2028 -   segundos (diferença de  segundos)
ods_vgs %>% filter(!is.na(time)) %>% mutate(time_all = time * viagens) %>% select(time_all) %>% sum()


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
  # Juntar extensão percorrida em infraestrutura cicloviária para cada par OD
  left_join(ciclo_od, by = c('orig', 'dest')) %>% 
  # Substituir NAs por zero para viagens que não passaram por infra_ciclo
  mutate(ext_ciclo = ifelse(is.na(ext_ciclo), 0, ext_ciclo))


# # Checar média de extensão cicloviária por viagem
# ods_resultados %>% filter(ext_ciclo > 0) %>% group_by(orig) %>% tally() %>% filter(n > 1)
# orig                n
# <chr>           <int>
# 1 89a81000317ffff     2
# 2 89a8100039bffff     2
# 3 89a810003a7ffff     2
# 4 89a8100080bffff     2
# 5 89a81000a67ffff     2
# ods_resultados %>% filter(orig %in% c('89a81000317ffff', '89a8100039bffff', '89a810003a7ffff', '89a8100080bffff', '89a81000a67ffff'))

# Agrupar por hexágonos, somente as viagens dentro do limite de tempo - resultado
# é hexágonos com viagens todas a tempo ou em parte a tempo
hex_resultados_tempo_ok <- 
  ods_resultados %>% 
  filter(!is.na(time)) %>% 
  # origens com mais de um destino, para teste:
  # filter(orig %in% c('89a81000317ffff', '89a81000393ffff', '89a8100039bffff')) %>% 
  # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
  # percorrida por rota
  mutate(subtotal_tempo = viagens * time,
         subtotal_dist  = viagens * distance,
         ext_ciclo_vgs  = viagens * ext_ciclo) %>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(orig) %>% 
  summarise(viagens_a_tempo = sum(viagens),
            tempo_total     = sum(subtotal_tempo),
            dist_total      = sum(subtotal_dist),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs)) %>% 
  mutate(tempo_medio_vgs   = tempo_total / viagens_a_tempo,
         infraciclo_por_vg = ext_ciclo_tot_m / viagens_a_tempo,
         uso_perc_ciclo    = ext_ciclo_tot_m / dist_total * 100,
         particip_perc_extciclo_vs_todo = ext_ciclo_tot_m / sum(.$ext_ciclo_tot_m) * 100) %>% 
  # Checar média de extensão cicloviária por viagem
  # filter(orig %in% c('89a81000317ffff', '89a8100039bffff', '89a810003a7ffff', '89a8100080bffff', '89a81000a67ffff'))
  ungroup()

# sample_n(hex_resultados_tempo_ok, 20)
# hex_resultados_tempo_ok %>% filter(is.na(particip_perc_extciclo_vs_todo))

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
  relocate(viagens_fora_tempo, .before = 'viagens_a_tempo') %>% 
  # tail(20) %>% 
  replace(is.na(.), 0) %>%
  mutate(tot_viagens = viagens_a_tempo + viagens_fora_tempo, .after = 'orig') %>%
  # Categorizar hexágonos de origem segundo viagens presentes nos resultados
  mutate(class_hex_orig = case_when(viagens_a_tempo > 0 & viagens_fora_tempo == 0 ~ 'viagens todas a tempo',
                                    viagens_a_tempo == 0 & viagens_fora_tempo > 0 ~ 'viagens fora do tempo',
                                    viagens_a_tempo > 0 & viagens_fora_tempo > 0  ~ 'viagens parc. a tempo',
                                    TRUE ~ NA),
         .after = 'orig')

head(hex_resultados)

# Checagem
sum(hex_resultados$tot_viagens) == sum(ods_vgs$viagens)
# sum(hex_resultados$perc_part)

# Gravar resultados
hex_resultados_out <- sprintf('%s/07_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(hex_resultados, hex_resultados_out, delim = ';')

# Limpar ambiente
rm(ciclo_od, hex_resultados_tempo_ok, hex_resultados_fora_tempo, hex_resultados_out)

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
  mutate(ext_ciclo_vgs = ext_percorrida * viagens) %>% 
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
  mutate(particip_perc_viagens_vs_todo  = viagens_tot / sum(viagens_tot) * 100,
         particip_perc_extciclo_vs_todo = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)

head(osm_id_resultados)
# sum(osm_id_resultados$perc_part)

# Gravar resultados
osm_id_resultados_out <- sprintf('%s/08_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(osm_id_resultados, osm_id_resultados_out, delim = ';')
