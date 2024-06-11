library('tidyverse')
library('tidylog')
library('sf')
# library('mapview')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)

res2019 <- sprintf('%s/02_ttmatrix_teste_hexagono_ZL_2019_infraciclo.csv', pasta_aoprv_teste)
res2019 <- read_delim(res2019, delim = ';', col_types = 'cciccdddddddddcdddd')
head(res2019)

res2028 <- sprintf('%s/04_ttmatrix_teste_hexagono_ZL_2028_infraciclo.csv', pasta_aoprv_teste)
res2028 <- read_delim(res2028, delim = ';', col_types = 'cciccdddddddddcdddd')
head(res2028)

# Quantos NAs temos na base e onde?
data.frame(nas = colSums(is.na(res2019))) %>% filter(nas > 0)
data.frame(nas = colSums(is.na(res2028))) %>% filter(nas > 0)


# ------------------------------------------------------------------------------
# Compensar tempos em ciclovias, devido à velocidade alterada
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

# No exemplo a seguir, temos a velocidade média de 10,7 km/h (2,97 mps), distância
# total de 11.307 metros e distância em ciclovia de 1.050 metros. Isso resulta
# em uma compensação de 49,8 segundos ao tempo final.
# res2019 %>%
#   # Remover linhas que não passaram por ciclovias
#   filter(ciclo_comum > 1000) %>%
#   mutate(index = row_number(),
#          speed_mps = speed / 3.6) %>%
#   select(index, speed, speed_mps, distance, time, ciclo_comum) %>%
#   slice(2) %>%
#   # dif_tempo = dist_em_ciclovia / (velocidade_média + 0.14) - dist_em_ciclovia / (velocidade_média + 1.95)
#   mutate(dif_tempo = (ciclo_comum / (speed_mps + (0.139447 / 3.6))) - (ciclo_comum / (speed_mps + (1.945864 / 3.6))),
#          time_adj  = time + dif_tempo)
#   index speed speed_mps distance  time ciclo_comum dif_tempo time_adj
#   <int> <dbl>     <dbl>    <dbl> <dbl>       <dbl>     <dbl>    <dbl>
# 1     2  10.7      2.97   11307. 3802.       1050.      49.7    3851.


# dif_tempo = dist_em_ciclovia / (velocidade_média + 0.14) - dist_em_ciclovia / (velocidade_média + 1.95)
# 1.050*((1/(10.7+0.139447))-(1/(10.7+1.945864)))*3600
res2019 <- 
  res2019 %>% 
  mutate(time_dif = (ciclo_comum / ((speed / 3.6) + (0.139447 / 3.6))) - (ciclo_comum / ((speed / 3.6) + (1.945864 / 3.6))),
         time_adj  = time + time_dif,
         .before = 'via_comum')

res2028 <- 
  res2028 %>% 
  mutate(time_dif = (ciclo_comum / ((speed / 3.6) + (0.139447 / 3.6))) - (ciclo_comum / ((speed / 3.6) + (1.945864 / 3.6))),
         time_adj  = time + time_dif,
         .before = 'via_comum')


# ------------------------------------------------------------------------------
# Dados IPEA
# ------------------------------------------------------------------------------

# Colunas a serem removidas ao abrir as bases de população e oportunidades
remove_cols <- c('year', 'abbrev_muni', 'name_muni', 'code_muni')

# IPEA - Shape de grid hexagonal (resolução 9)
hexagonos <- sprintf('%s/aop_hex_grid_v2.gpkg', dados_originais)
hexagonos <- read_sf(hexagonos) %>% filter(abbrev_muni == 'spo') %>% select(id_hex)
head(hexagonos)

# IPEA - População
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
ipea_pop <- sprintf('%s/aop_population_2010_v2.csv', dados_originais)
ipea_pop <- read_delim(ipea_pop, delim = ',', col_types = "ccccciiiiiiiiiiiiiidii")
ipea_pop <- ipea_pop %>% filter(abbrev_muni == 'spo') %>% select(-all_of(remove_cols))
# Selecionar tipos de população
ipea_pop <- 
  ipea_pop %>% 
  select(id_hex,
         # P001 - Total number of residents
         # P002 - Number of white residents
         # P003 - Number of black residents
         # P004 - Number of indigenous residents
         # P005 - Number of asian-descendents residents
         # P006 - Number of men
         # P007 - Number of women
         matches('P00[1-7]')
  )
head(ipea_pop)


# IPEA - Oportunidades
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
ipea_oport <- sprintf('%s/aop_landuse_2019_v2.csv', dados_originais)
ipea_oport <- read_delim(ipea_oport, delim = ',', col_types = "ccccciiiiiiiiiiiiiiiii")
ipea_oport <- ipea_oport %>% filter(abbrev_muni == 'spo') %>% select(-all_of(remove_cols))
# Selecionar oportunidades totais
ipea_oport <- 
  ipea_oport %>% 
  select(id_hex,
         # T001 - Total number of formal jobs
         # E001 - Total number of public schools
         # M001 - Total number of school enrollments
         # S001 - Total number of healthcare facilities
         # C001 - Total number of Social Assistance Reference Centers (CRAS)
         T001,
         E001,
         M001,
         S001,
         C001
  )
head(ipea_oport)


# # IPEA - Acesso a Oportunidades - Modos Ativos
# # https://ipeagit.github.io/aopdata/reference/read_access.html
# ipea_aop <- sprintf('%s/aop_access_active_2019_v2.csv', dados_originais)
# ipea_aop <- read_delim(ipea_aop, delim = ',', col_types = cols(.default = "c"))
# ipea_aop <- ipea_aop %>% filter(abbrev_muni == 'spo') %>% select(-all_of(remove_cols))
# # Selecionar colunas de acesso a oportunidades para 30 minutos
# ipea_aop <- 
#   ipea_aop %>% 
#   filter(mode == 'bicycle') %>% 
#   select(id_hex,
#          # CMA - Cumulative opportunity measure (active)
#          # TT - All jobs
#          # ST - All healthcare facilities
#          # ET - All public schools
#          # MT - All school enrollments
#          # CT - All Social Assistance Reference Centers (CRAS)
#          CMATT30, CMAST30, CMAET30, CMAMT30, CMACT30,
#          # CMP - Cumulative opportunity measure (passive)
#          # PT - All population
#          # PH - Men
#          # PM - Women
#          # PB - White population
#          # PA - Asian-descendent population
#          # PI - Indigenous population
#          # PN - Back population
#          CMPPT30, CMPPH30, CMPPM30, CMPPB30, CMPPA30, CMPPI30, CMPPN30
#   )
# head(ipea_aop)


# ------------------------------------------------------------------------------
# Juntar oportunidades acessadas
# ------------------------------------------------------------------------------

# Oportunidades acessadas a menos de 15 minutos (30 min = 1800 seg)
tempo_max <- 900

aop2019_15 <- 
  res2019 %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex.y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex.x) %>% 
  summarise(CMATT15 = sum(T001, na.rm = TRUE),
            CMAST15 = sum(S001, na.rm = TRUE),
            CMAET15 = sum(E001, na.rm = TRUE),
            CMAMT15 = sum(M001, na.rm = TRUE),
            CMACT15 = sum(C001, na.rm = TRUE))

# # Checar resultados
# ipea_aop %>% select(id_hex) %>% sample_n(1)
# ipea_aop %>% filter(id_hex == '89a81039adbffff')

# Oportunidades acessadas a menos de 30 minutos (30 min = 1800 seg)
tempo_max <- 1800
# Calcular acesso a oportunidades - IPEA está acessando tempos de 900 a 1050 seg
aop2019_30 <- 
  res2019 %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex.y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex.x) %>% 
  summarise(CMATT30 = sum(T001, na.rm = TRUE),
            CMAST30 = sum(S001, na.rm = TRUE),
            CMAET30 = sum(E001, na.rm = TRUE),
            CMAMT30 = sum(M001, na.rm = TRUE),
            CMACT30 = sum(C001, na.rm = TRUE))


# Oportunidades acessadas a menos de 40 minutos (30 min = 1800 seg)
tempo_max <- 2400
# Calcular acesso a oportunidades - IPEA está acessando tempos de 900 a 1050 seg
aop2019_45 <- 
  res2019 %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex.y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex.x) %>% 
  summarise(CMATT45 = sum(T001, na.rm = TRUE),
            CMAST45 = sum(S001, na.rm = TRUE),
            CMAET45 = sum(E001, na.rm = TRUE),
            CMAMT45 = sum(M001, na.rm = TRUE),
            CMACT45 = sum(C001, na.rm = TRUE))


# Juntar todos os acessos a oportunidades por faixa de tempo
aop_2019 <- 
  aop2019_15 %>% 
  left_join(aop2019_30, by = 'id_hex.x') %>% 
  left_join(aop2019_45, by = 'id_hex.x') %>% 
  rename(id_hex = id_hex.x)


# Juntar ao shape de hexágonos
hexagonos <- hexagonos %>% left_join(aop_2019, by = 'id_hex')

# Jogar coluna de geom para a última
hexagonos <- hexagonos %>% select(-geom, geom)

# Substituir NAs por zeros
hexagonos <- hexagonos %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))

head(hexagonos)


# Gravar oportunidades SP
out_shape <- sprintf('%s/hex_agregado_oportunidades_res09_2019.gpkg', pasta_oportunidades)
st_write(hexagonos, out_shape, driver = 'GPKG', append = FALSE)



this <- hexagonos %>% filter(CMATT15 > 0) %>% select(id_hex, matches('T15'))
mapview(this)














# 1075 pares OD
res2019 %>% select(hex_id) %>% distinct() %>% nrow()
res2028 %>% select(hex_id) %>% distinct() %>% nrow()

# Quantas viagens possuem mais de uma alternativa?
res2019 %>% group_by(alt) %>% tally() %>% mutate(perc = n/sum(n)*100)
#     alt     n  perc
#   <int> <int> <dbl>
# 1     1  1075  39.4
# 2     2   972  35.7
# 3     3   679  24.9
res2028 %>% group_by(alt) %>% tally() %>% mutate(perc = n/sum(n)*100)
#     alt     n  perc
#   <int> <int> <dbl>
# 1     1  1075  40.0
# 2     2   917  34.1
# 3     3   698  25.9


# ------------------------------------------------------------------------------
# Ver resultados - maior uso de infra cicloviária
# ------------------------------------------------------------------------------

ciclo19 <- res2019 %>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo)) %>% ungroup()
ciclo19 <- ciclo19 %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()

ciclo28 <- res2028 %>% group_by(hex_id) %>% filter(infra_ciclo == max(infra_ciclo)) %>% ungroup()
ciclo28 <- ciclo28 %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()

sum(ciclo19$infra_ciclo) / sum(ciclo19$distance)
sum(ciclo28$infra_ciclo) / sum(ciclo28$distance)

result19 <- 
  data.frame(ano = 2019,
             dist_total = sum(ciclo19$distance),
             dist_ciclo = sum(ciclo19$infra_ciclo),
             dist_ciclv = sum(ciclo19$ciclo_comum),
             dist_cicfx = sum(ciclo19$ciclofaixa)) %>% 
  mutate(perc_ciclo = round(dist_ciclo / dist_total * 100, 2),
         perc_ciclv = round(dist_ciclv / dist_total * 100, 2),
         perc_cicfx = round(dist_cicfx / dist_total * 100, 2))

result28 <- 
  data.frame(ano = 2028,
             dist_total = sum(ciclo28$distance),
             dist_ciclo = sum(ciclo28$infra_ciclo),
             dist_ciclv = sum(ciclo28$ciclo_comum),
             dist_cicfx = sum(ciclo28$ciclofaixa)) %>% 
  mutate(perc_ciclo = round(dist_ciclo / dist_total * 100, 2),
         perc_ciclv = round(dist_ciclv / dist_total * 100, 2),
         perc_cicfx = round(dist_cicfx / dist_total * 100, 2))

rbind(result19, result28)
#    ano dist_total dist_ciclo dist_ciclv dist_cicfx perc_ciclo perc_ciclv perc_cicfx
# 1 2019    7470581   178256.1   11373.96   163725.4       2.39       0.15       2.19
# 2 2028    7618108  5823566.2    6449.11  5813963.0      76.44       0.08      76.32


# ------------------------------------------------------------------------------
# Ver resultados - menor custo
# ------------------------------------------------------------------------------

custo19 <- res2019 %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()

custo28 <- res2028 %>% group_by(hex_id) %>% filter(weight == min(weight)) %>% ungroup()

sum(custo19$infra_ciclo) / sum(custo19$distance)
sum(custo28$infra_ciclo) / sum(custo28$distance)

res_custo19 <- 
  data.frame(ano = 2019,
             dist_total = sum(custo19$distance),
             dist_ciclo = sum(custo19$infra_ciclo),
             dist_ciclv = sum(custo19$ciclo_comum),
             dist_cicfx = sum(custo19$ciclofaixa)) %>% 
  mutate(perc_ciclo = round(dist_ciclo / dist_total * 100, 2),
         perc_ciclv = round(dist_ciclv / dist_total * 100, 2),
         perc_cicfx = round(dist_cicfx / dist_total * 100, 2))

res_custo28 <- 
  data.frame(ano = 2028,
             dist_total = sum(custo28$distance),
             dist_ciclo = sum(custo28$infra_ciclo),
             dist_ciclv = sum(custo28$ciclo_comum),
             dist_cicfx = sum(custo28$ciclofaixa)) %>% 
  mutate(perc_ciclo = round(dist_ciclo / dist_total * 100, 2),
         perc_ciclv = round(dist_ciclv / dist_total * 100, 2),
         perc_cicfx = round(dist_cicfx / dist_total * 100, 2))

rbind(res_custo19, res_custo28)
#    ano dist_total dist_ciclo dist_ciclv dist_cicfx perc_ciclo perc_ciclv perc_cicfx
# 1 2019    7388531   142824.6   3228.794   138543.3       1.93       0.04       1.88
# 2 2028    7343858  5079531.3   2377.506  5076102.2      69.17       0.03      69.12
