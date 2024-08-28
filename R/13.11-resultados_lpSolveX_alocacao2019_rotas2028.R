# Agrega os resultados do lpSolve por hexágono e por trecho de osm_id. Neste 
# script, vamos pegar a alocação da população vinda da solução de 2019 mas as
# rotas vindas da matriz de tempos de 2028, como se a mesma população fosse 
# para as mesmas escolas, mas com nova infraestrutura cicloviária disponível

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')

# Definir ano de análise e limite máximo de tempo
ano1 <- '2019'; ano2 <- '2028'; tempo_max <- '15'

# Qual solução usar? A primeira considera população de interesse maior do que as
# matrículas disponíveis; a segunda ajusta a população para caber nas matrículas
versao_solucao <- 2

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ttmat <- sprintf("%s/01_ttmatrix", pasta_aop_optimum)
pasta_opaop_dados <- sprintf("%s/02_dados_pop_mat", pasta_aop_optimum)
if (versao_solucao == 1) {
  pasta_aop_lpsolve <- sprintf("%s/03_lpSolve1_pop_maior_que_mat", pasta_aop_optimum)
} else if (versao_solucao == 2) {
  pasta_aop_lpsolve <- sprintf("%s/04_lpSolve2_pop_ajustada", pasta_aop_optimum)
}
pasta_opaop_ano1   <- sprintf("%s/%s", pasta_aop_lpsolve, ano1)
pasta_opaop_ano2   <- sprintf("%s/%s", pasta_aop_lpsolve, ano2)
pasta_aop_lpsolveX <- sprintf("%s/05_lpSolveX_alocacao2019_rotas2028", pasta_aop_optimum)
dir.create(pasta_aop_lpsolveX, recursive = TRUE, showWarnings = FALSE)
rm(versao_solucao)


# Origens, destinos e quantidade de viagens de 2019
ods_vgs <- sprintf('%s/01_lpsolve_resultados_viagens_por_par_OD_%s_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'ccid')
# ods_vgs %>% filter(!is.na(time)) %>% select(orig, dest) %>% distinct() # 10.237
head(ods_vgs)

# Matriz de tempo de 2028
ttmatrix <- sprintf('%s/ttmatrix_%s_res09_%smin.csv', pasta_opaop_ttmat, ano2, tempo_max)
ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'cidddddddddddc', col_select = c('hex_id', 'time'))
ttmatrix <- ttmatrix %>% separate(hex_id, into = c('orig', 'dest'), remove = TRUE)
head(ttmatrix)

# Juntar novos tempos à solução do lpSolve para 2019
ods_vgs <- 
  ods_vgs %>% 
  left_join(ttmatrix, by = c('orig', 'dest')) %>% 
  # Tempos para origens e destinos iguais é zero
  mutate(time.y = ifelse(orig == dest, 0, time.y))

# Checagem de tempos - só há 3 viagens em pares OD em 2019 que não possuem 
# relativo em 2028 e 1 em 2028 que não possui relativo em 2019. Tudo ok
# ods_vgs %>% filter(is.na(time.y) & !is.na(time.x))
# ods_vgs %>% filter(!is.na(time.y) & is.na(time.x))
ods_vgs <- ods_vgs %>% select(orig, dest, viagens, time = time.y)
head(ods_vgs)


# Osm_ids que faziam parte da ttmatrix utilizada - no caso, 2028
osm_ids <- sprintf('%s/ttmatrix_osmids_%s_res09_%smin.csv', pasta_opaop_ttmat, ano2, tempo_max)
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

if (ano2 == '2028') {
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
  
  # # Gravar resultados para serem usados no script seguinte
  # write_delim(osm_ids %>% select(osm_id = osm_way_id, infra_ciclo) %>% distinct(), 
  #             sprintf('%s/tmp_infra_ciclo_%s_revisada.csv', pasta_aoprv_alter, ano), 
  #             delim = ';')
}


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
  # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
  # percorrida por rota
  mutate(ext_ciclo_vgs = ext_ciclo * viagens)


# Agrupar por hexágonos, somente as viagens dentro do limite de tempo - resultado
# é hexágonos com viagens todas a tempo ou em parte a tempo
hex_resultados_tempo_ok <- 
  ods_resultados %>% 
  filter(!is.na(time)) %>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(orig) %>% 
  summarise(viagens_a_tempo = sum(viagens),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs),
            tempo_medio_vgs_a_tempo = mean(time)) %>% 
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
hex_resultados_out <- sprintf('%s/01_lpSolveX_resultados_por_hexagono_alocacao%s_rotas%s_res09_%smin.csv', pasta_aop_lpsolveX, ano1, ano2, tempo_max)
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
  mutate(perc_part = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)

head(osm_id_resultados)
# sum(osm_id_resultados$perc_part)

# Gravar resultados
osm_id_resultados_out <- sprintf('%s/02_lpSolveX_resultados_por_osmid_alocacao%s_rotas%s_res09_%smin.csv', pasta_aop_lpsolveX, ano1, ano2, tempo_max)
write_delim(osm_id_resultados, osm_id_resultados_out, delim = ';')


# ------------------------------------------------------------------------------
# Base geral - hexágonos com população de interesse e oportunidades de interesse
# ------------------------------------------------------------------------------

# IPEA - Shape de grid hexagonal (resolução 9)
hexagonos <- sprintf('%s/aop_hex_grid_v2.gpkg', pasta_ipea)
hexagonos <- read_sf(hexagonos) %>% filter(abbrev_muni == 'spo') %>% select(id_hex)
head(hexagonos)
# mapview(hexagonos)

# Demanda: quantidade de pessoas por hexágono
pop <- sprintf('%s/hex_grid_sp_res09_dados_censo_por_hexagono.csv', pasta_opaop_dados)
pop <- read_delim(pop, delim = ';', col_types = cols(.default = "c"))
# pop %>% filter(!id_hex %in% hex_sp$id_hex)
pop <- pop %>% mutate_at(2:ncol(.), as.numeric)
# Garantir que haja população
pop <- pop %>% filter(pessoas_15_17_hex > 0)
pop <- pop %>% select(id_hex, pop_15_17 = pessoas_15_17_hex)
# Checar se algum id ficou duplicado por qualquer motivo
# pop %>% group_by(orig) %>% tally() %>% filter(n > 1) %>% nrow()
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.csv', pasta_opaop_dados)
mat <- read_delim(mat, delim = ';', col_types = cols(.default = "c"))
# mat %>% filter(!id_hex %in% hex_sp$id_hex)
mat <- mat %>% mutate_at(2:ncol(.), as.numeric)
# Garantir que haja matrículas
mat <- mat %>% filter(matriculas_idades_15_17 > 0)
# Checar se algum id ficou duplicado por qualquer motivo
# mat %>% group_by(id_hex) %>% tally() %>% filter(n > 1) %>% nrow()
mat <- mat %>% select(id_hex, mat_15_17 = matriculas_idades_15_17)
head(mat)

# Ajustar população proporcional ao número de matrículas existentes
proporcao_pop_mat <- sum(pop$pop_15_17) / sum(mat$mat_15_17)
pop <- pop %>% mutate(pop_prop_15_17 = round(pop_15_17 / proporcao_pop_mat))
# A nova população de interesse será agora a população reduzida proporcionalmente
head(pop)

# Juntar população e matrículas aos hexágonos
hex_pop_op <- 
  hexagonos %>% 
  st_drop_geometry() %>% 
  left_join(mat, by = 'id_hex') %>% 
  left_join(pop, by = 'id_hex') %>% 
  replace(is.na(.), 0)

head(hex_pop_op)


# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por hexágono
# ------------------------------------------------------------------------------

# Resultados por hexágono do lpSolve para o ano 1
hex_resultados_1 <- sprintf('%s/07_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
hex_resultados_1 <- read_delim(hex_resultados_1, delim = ';', col_types = 'ccidddii')
hex_resultados_1 <- hex_resultados_1 %>% select(orig,
                                                hexclas_19 = class_hex_orig,
                                                vgs_ok_19_tempo = tempo_medio_vgs_a_tempo,
                                                vgs_ok_19  = viagens_a_tempo,
                                                vgs_nok_19 = viagens_fora_tempo,
                                                vgs_tot_19 = tot_viagens,
                                                ext_cic_19 = ext_ciclo_tot_m,
                                                perccic_19 = perc_part)
head(hex_resultados_1)

# Juntar aos resultados também as origens impossíveis
orig_impossiveis_1 <- sprintf('%s/06_lpsolve_hexagonos_origens_impossiveis_%s_%smin.gpkg', pasta_opaop_ano1, ano1, tempo_max)
orig_impossiveis_1 <- read_sf(orig_impossiveis_1) %>% st_drop_geometry() %>% select(orig = id_hex)
orig_impossiveis_1 <- orig_impossiveis_1 %>% mutate(hexclas_19 = 'hex origem impossivel',
                                                    vgs_ok_19_tempo = as.numeric(NA),
                                                    vgs_ok_19  = as.numeric(NA),
                                                    vgs_nok_19 = as.numeric(NA),
                                                    vgs_tot_19 = as.numeric(NA),
                                                    ext_cic_19 = as.numeric(NA),
                                                    perccic_19 = as.numeric(NA))

hex_resultados_1 <- hex_resultados_1 %>% rbind(orig_impossiveis_1) %>% arrange(orig)
hex_resultados_1 %>% filter(hexclas_19 == 'hex origem impossivel')
head(hex_resultados_1)


# Resultados por hexágono do lpSolveX - alocação de 2019 e rotas de 2028
hex_resultados_2 <- hex_resultados
hex_resultados_2 <- hex_resultados_2 %>% select(orig,
                                                hexclas_28X = class_hex_orig,
                                                vgs_ok_28X_tempo = tempo_medio_vgs_a_tempo,
                                                vgs_ok_28X  = viagens_a_tempo,
                                                vgs_nok_28X = viagens_fora_tempo,
                                                vgs_tot_28X = tot_viagens,
                                                ext_cic_28X = ext_ciclo_tot_m,
                                                perccic_28X = perc_part)
head(hex_resultados_2)

# Juntar aos resultados também as origens impossíveis, que em tese viriam de 2028
orig_impossiveis_2 <- sprintf('%s/06_lpsolve_hexagonos_origens_impossiveis_%s_%smin.gpkg', pasta_opaop_ano2, ano2, tempo_max)
orig_impossiveis_2 <- read_sf(orig_impossiveis_2) %>% st_drop_geometry() %>% select(orig = id_hex)
orig_impossiveis_2 <- orig_impossiveis_2 %>% mutate(hexclas_28X = 'hex origem impossivel',
                                                    vgs_ok_28X_tempo = as.numeric(NA),
                                                    vgs_ok_28X  = as.numeric(NA),
                                                    vgs_nok_28X = as.numeric(NA),
                                                    vgs_tot_28X = as.numeric(NA),
                                                    ext_cic_28X = as.numeric(NA),
                                                    perccic_28X = as.numeric(NA))

hex_resultados_2 <- hex_resultados_2 %>% rbind(orig_impossiveis_2) %>% arrange(orig)
head(hex_resultados_2)
# sample_n(hex_resultados_2, 20)


# Juntar resultados na grade completa de hexágonos
hex_resultados <- 
  hex_pop_op %>% 
  left_join(hex_resultados_1, by = c('id_hex' = 'orig')) %>% 
  left_join(hex_resultados_2, by = c('id_hex' = 'orig')) %>% 
  # Demarcar hexágonos sem população ou matrícula
  mutate(hexclas_19 = ifelse(is.na(hexclas_19), 'hexag sem pop ou mat', hexclas_19),
         hexclas_28X = ifelse(is.na(hexclas_28X), 'hexag sem pop ou mat', hexclas_28X)) %>% 
  # Quais as diferenças de viagens feitas dentro do tempo e extensão percorrida
  # em infraestrutura cicloviária dessas mesmas viagens entre os dois cenários?
  mutate(dif_vgs_ok = vgs_ok_28X - vgs_ok_19,
         dif_cic_ok = ext_cic_28X - ext_cic_19)


# # Há casos em que dif_cic será negativo. Mapeei duas situações quando o método
# # do lpSolve é utilizado invertendo a matriz (população > matrículas). No segundo
# # método (população <= matrículas), só o último acontece:
# hex_resultados %>% filter(dif_cic_ok < 0) %>% select(id_hex, matches('^vgs_ok'), matches('^ext_cic'), dif_cic_ok) %>% sample_n(20)
# # 1. Houve viagens em 2019 mas não em 2028, pois o lpSolve fez a solução 
# # desconsiderando algumas pessoas que em 2019 geravam menos custo do que em 2028
# hex_resultados %>% filter(id_hex %in% c('89a8100f21bffff', '89a81005b43ffff', '89a8107062fffff'))
# # 2. Há viagens nos dois cenários, mas possivelmente há mais alternativas de
# # rotas cicláveis, o que faz com que os trajetos escolhidos sejam mais curtos
# # (consequentemente, reduzindo o uso de infra cicloviária)
# hex_resultados %>% filter(id_hex %in% c('89a81072d2fffff', '89a8100dd03ffff', '89a81072d33ffff'))
# 
# # Há linhas em que há mais viagens no segundo cenário mas menos uso de infra cicloviária?
# hex_resultados %>% filter(vgs_ok_28 > vgs_ok_19 & dif_cic_ok < 0)
# 
# # No segundo cenário, há várias linhas em que há mais viagens dentro do tempo
# # limite do que no primeiro, 
# hex_resultados %>% filter(vgs_ok_28 > vgs_ok_19) %>% select(id_hex, matches('^vgs_'), matches('^ext_cic'), dif_cic_ok) %>% sample_n(20)
# sum(hex_resultados$vgs_ok_19, na.rm = TRUE) # 348.296
# sum(hex_resultados$vgs_ok_28, na.rm = TRUE) # 351.105 (2.809 a mais)


# Juntar resultados ao shapefile de hexágonos
hexagonos <- 
  hexagonos %>% 
  left_join(hex_resultados, by = 'id_hex') %>% 
  relocate(c(perccic_19, perccic_28X, geom), .after = last_col())

# Gravar resultados
out_hex <- sprintf('%s/03_hexagonos_sp_resultados_metodo_lpSolveX_res09_2019_2028.gpkg', pasta_aop_lpsolveX)
st_write(hexagonos, out_hex, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


hexagonos %>% st_drop_geometry() %>% select(matches('_tempo')) %>% summary()
# vgs_ok_19_tempo vgs_ok_28X_tempo
# Min.   :  0.0   Min.   :  0.0   
# 1st Qu.:152.3   1st Qu.:148.4   
# Median :350.5   Median :337.2   
# Mean   :369.1   Mean   :358.3   
# 3rd Qu.:583.1   3rd Qu.:565.7   
# Max.   :900.0   Max.   :900.0   
# NA's   :4624    NA's   :4625 