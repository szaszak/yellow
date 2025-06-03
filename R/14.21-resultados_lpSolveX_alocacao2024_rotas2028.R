# Agrega os resultados do lpSolve por hexágono e por trecho de osm_id. Neste 
# script, vamos pegar a alocação da população vinda da solução de 2024 mas as
# rotas vindas da matriz de tempos de 2028, como se a mesma população fosse 
# para as mesmas escolas, mas com nova infraestrutura cicloviária disponível

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')

# Definir ano de análise e limite máximo de tempo
ano1 <- '2024'; ano2 <- '2028'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados          <- "../../yellow_dados"
pasta_osm_sp     <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_graphhopper   <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs       <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_redes_24_28    <- sprintf("%s/01_redes_2024_2028", pasta_aop_2024_2028)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)
pasta_lpsolve_24_28  <- sprintf("%s/05_lpsolve_2024_2028", pasta_aop_2024_2028)
pasta_opaop_ano1     <- sprintf("%s/%s", pasta_lpsolve_24_28, ano1)
pasta_aop_lpsolveX <- sprintf("%s/lpSolveX_alocacao2024_rotas2028", pasta_lpsolve_24_28)
dir.create(pasta_aop_lpsolveX, recursive = TRUE, showWarnings = FALSE)



# Origens, destinos e quantidade de viagens de 2024 - linhas com time = NA são as 
# que entraram na solução por terem custo alto (100000), mas cujo tempo estava, 
# na verdade, acima do limite permitido. Esse alto custo foi substituído por NA
ods_vgs <- sprintf('%s/01_lpsolve_resultados_viagens_por_par_OD_%s_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'ccid')
# ods_vgs %>% filter(!is.na(time)) %>% select(orig, dest) %>% distinct() # 7.926
head(ods_vgs)

# Matriz de tempo de 2028
# ttmatrix <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
ttmatrix <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'ciidddddddddc', col_select = c('hex_id', 'time', 'distance'))
ttmatrix <- ttmatrix %>% separate(hex_id, into = c('orig', 'dest'), remove = TRUE)
head(ttmatrix)

# # Checagem: Todos os pares OD de 2024 possuem relação com a ttmatrix de 2028? 
# this <- ods_vgs  %>% mutate(id_hex = str_c(orig, dest, sep = '-'))
# that <- ttmatrix %>% mutate(id_hex = str_c(orig, dest, sep = '-'))
# # Sim. No caso da ttmarix 2028, ela necessariamente vai ter mais pares OD porque 
# # não passou pelo filtro do lpSolve
# this %>% filter(!id_hex %in% that$id_hex & !is.na(time) & orig != dest)
# rm(this, that)


# Juntar novos tempos à solução do lpSolve para 2024
ods_vgs <- 
  ods_vgs %>% 
  left_join(ttmatrix, by = c('orig', 'dest')) %>% 
  # Tempos para origens e destinos iguais é zero
  mutate(time.y = ifelse(orig == dest, 0, time.y))

# Checagem de tempos - todas as viagens de 2024 possuem correspondência em 2028 - Sim
# ods_vgs %>% filter(is.na(time.y) & !is.na(time.x))
# ods_vgs %>% filter(!is.na(time.y) & is.na(time.x))
ods_vgs <- ods_vgs %>% select(orig, dest, viagens, time = time.y, distance)
head(ods_vgs)


# Osm_ids que faziam parte da ttmatrix utilizada - no caso, 2028
# osm_ids <- sprintf('%s/13_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
osm_ids <- sprintf('%s/13_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'cccd')
head(osm_ids)

# Desmembrar coluna hex_id_alt em origem, destino e número da alternativa da rota
osm_ids <- 
  osm_ids %>% 
  mutate(hex_id_alt = str_replace(hex_id_alt, 
                                  '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                  '89a81\\1ffff-89a81\\2ffff-\\3')) %>% 
  separate(hex_id_alt, into = c('orig', 'dest', 'alt'), sep = '-', remove = TRUE)

# Checar se correção do PBF de 2028 funcionou: os osm_ids abaixo eram de estruturas
# que estavam marcadas como rede de referência mas consistiam em pequenos trechos
# com estrutura cicloviária diante de grandes trechos sem. A correção foi feita
# no script 14.02-editar_pbf_para_graphhopper_rede_2028. Dos ids abaixo, somente
# o '1002404094' deve estar presente como ciclovia comum:
# osm_ids %>% filter(osm_way_id %in% c('264984110', '1001832992', '1002404094', '1002429998'))

head(osm_ids)

# Limpar ambiente
rm(ttmatrix)

# # Checagem: Todos os grupos de osm_ids de 2028 que serão associados estão sendo 
# # considerados?
# this <- ods_vgs  %>% mutate(id_hex = str_c(orig, dest, sep = '-'))
# that <- osm_ids %>% mutate(id_hex = str_c(orig, dest, sep = '-'))
# # Sim. Os pares OD que não possuem osm ids associados são porque não passam por
# # infraestrutura cicloviária
# those <- this %>% filter(!id_hex %in% that$id_hex & orig != dest) %>% select(id_hex) %>% distinct()
# ttmatrix <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
# ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = 'ciidddddddddc')
# ttmatrix %>% filter(hex_id %in% those$id_hex) %>% select(infra_ciclo) %>% distinct()
# rm(this, that, those, ttmatrix)


# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por hexágono
# ------------------------------------------------------------------------------

# Qual o número total de viagens geradas por cada hexágono de origem?
# Qual a extensão percorrida em infra cicloviária quando somadas todas as rotas
# que saem de cada hexágono de origem?

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

ods_resultados %>% 
  # filter(orig %in% c('89a81000317ffff', '89a8100039bffff', '89a810003a7ffff', '89a8100080bffff', '89a81000a67ffff')) %>%
  filter(orig %in% c('89a81000317ffff')) %>% 
  arrange(orig) %>% 
  select(-time) %>% 
  mutate(perc_cic = ext_ciclo / distance * 100)
  
# hex_resultados_tempo_ok %>% 
#   filter(orig == '89a81000317ffff') %>% 
#   select(orig, viagens_a_tempo, dist_total, ext_ciclo_tot_m, infraciclo_por_vg, uso_perc_ciclo)

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
  replace(is.na(.), 0) %>%
  mutate(tot_viagens = viagens_a_tempo + viagens_fora_tempo, .after = 'orig') %>%
  # Categorizar hexágonos de origem segundo viagens presentes nos resultados
  mutate(class_hex_orig = case_when(viagens_a_tempo > 0 & viagens_fora_tempo == 0 ~ 'viagens todas a tempo',
                                    viagens_a_tempo == 0 & viagens_fora_tempo > 0 ~ 'viagens fora do tempo',
                                    viagens_a_tempo > 0 & viagens_fora_tempo > 0  ~ 'viagens parc. a tempo',
                                    TRUE ~ NA),
         .after = 'orig')

head(hex_resultados, 10)

# Checagem
sum(hex_resultados$tot_viagens) == sum(ods_vgs$viagens)
# sum(hex_resultados$perc_part)

# Gravar resultados
hex_resultados_out <- sprintf('%s/01_lpSolveX_resultados_por_hexagono_alocacao%s_rotas%s_res09_%smin.csv', pasta_aop_lpsolveX, ano1, ano2, tempo_max)
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
osm_id_resultados_out <- sprintf('%s/02_lpSolveX_resultados_por_osmid_alocacao%s_rotas%s_res09_%smin.csv', pasta_aop_lpsolveX, ano1, ano2, tempo_max)
write_delim(osm_id_resultados, osm_id_resultados_out, delim = ';')


# Limpar ambiente
rm(osm_id_resultados_out, osm_id_resultados, osm_ids)


# ------------------------------------------------------------------------------
# Base geral - hexágonos com população de interesse e oportunidades de interesse
# ------------------------------------------------------------------------------

# Demanda: quantidade de pessoas por hexágono
pop <- sprintf('%s/01_shape_resumido_estudantes_por_hexagono.gpkg', pasta_ttmatrix_24_28)
pop <- read_sf(pop)
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/02_matriculas_censo_escolar_2023_associadas_a_hexagonos.gpkg', pasta_ttmatrix_24_28)
mat <- read_sf(mat) %>% st_drop_geometry()
# Aplicar filtros de escolas públicas sem restrição e com vagas de ensino médio
mat <- mat %>% filter(QT_MAT_MED > 0 & CA_CATEGORIA == 'Pública' & CA_RESTRICAO == 'ESCOLA EM FUNCIONAMENTO E SEM RESTRIÇÃO DE ATENDIMENTO')
mat <- mat %>% group_by(id_hex) %>% summarise(matriculas_totais = sum(QT_MAT_MED)) %>% ungroup()
head(mat)

# Isolar somente hexágonos
hexagonos <- pop %>% select(id_hex)


# Juntar população e matrículas aos hexágonos
hex_pop_op <- 
  pop %>% 
  st_drop_geometry() %>% 
  left_join(mat, by = 'id_hex') %>% 
  replace(is.na(.), 0)

head(hex_pop_op)

# Limpar ambiente
rm(pop, mat)

# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por hexágono
# ------------------------------------------------------------------------------

# Resultados por hexágono do lpSolve para o ano 1
hex_resultados_1 <- sprintf('%s/07_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
hex_resultados_1 <- read_delim(hex_resultados_1, delim = ';', col_types = 'cciiidddd')
hex_resultados_1 <- hex_resultados_1 %>% select(orig,
                                                hexclas_24 = class_hex_orig,
                                                vgs_tot_24 = tot_viagens,
                                                vgs_nok_24 = viagens_fora_tempo,
                                                vgs_ok_24  = viagens_a_tempo,
                                                vgs_ok_24_tempo_med = tempo_medio_vgs,
                                                vgs_ok_24_tempo_tot = tempo_total,
                                                ext_tot_24  = dist_total,
                                                ext_cic_24  = ext_ciclo_tot_m,
                                                cic_vg_24   = infraciclo_por_vg,
                                                cic_perc_24 = uso_perc_ciclo,
                                                perccic_24  = particip_perc_extciclo_vs_todo)
head(hex_resultados_1)

# Juntar aos resultados também as origens impossíveis
orig_impossiveis <- sprintf('%s/06_lpsolve_hexagonos_impossiveis_%s_%smin.gpkg', pasta_opaop_ano1, ano1, tempo_max)
orig_impossiveis <- read_sf(orig_impossiveis) %>% st_drop_geometry() %>% select(orig = id_hex)
orig_impossiveis <- orig_impossiveis %>% mutate(hexclas_24 = 'hex origem impossivel',
                                                vgs_tot_24 = as.numeric(NA),
                                                vgs_nok_24  = as.numeric(NA),
                                                vgs_ok_24 = as.numeric(NA),
                                                vgs_ok_24_tempo_med = as.numeric(NA),
                                                vgs_ok_24_tempo_tot = as.numeric(NA),
                                                ext_tot_24  = as.numeric(NA),
                                                ext_cic_24  = as.numeric(NA),
                                                cic_vg_24   = as.numeric(NA),
                                                cic_perc_24 = as.numeric(NA),
                                                perccic_24  = as.numeric(NA))

hex_resultados_1 <- hex_resultados_1 %>% rbind(orig_impossiveis) %>% arrange(orig)
hex_resultados_1 %>% filter(hexclas_24 == 'hex origem impossivel')
head(hex_resultados_1)

# Ficou algum hexágono de fora?
hex_pop_op %>% filter(!id_hex %in% hex_resultados_1$orig & estudantes_totais > 0 & matriculas_totais > 0)
hex_resultados_1 %>% group_by(orig) %>% tally() %>% filter(n > 1)


# Resultados por hexágono do lpSolveX - alocação de 2024 e rotas de 2028
hex_resultados_2 <- hex_resultados
hex_resultados_2 <- hex_resultados_2 %>% select(orig,
                                                hexclas_28X = class_hex_orig,
                                                vgs_tot_28X = tot_viagens,
                                                vgs_nok_28X = viagens_fora_tempo,
                                                vgs_ok_28X  = viagens_a_tempo,
                                                vgs_ok_28X_tempo_med = tempo_medio_vgs,
                                                vgs_ok_28X_tempo_tot = tempo_total,
                                                ext_tot_28X  = dist_total,
                                                ext_cic_28X  = ext_ciclo_tot_m,
                                                cic_vg_28X   = infraciclo_por_vg,
                                                cic_perc_28X = uso_perc_ciclo,
                                                perccic_28X  = particip_perc_extciclo_vs_todo)
head(hex_resultados_2)

# Juntar aos resultados também as origens impossíveis, também de 2024
names(orig_impossiveis) <- names(hex_resultados_2)
hex_resultados_2 <- hex_resultados_2 %>% rbind(orig_impossiveis) %>% arrange(orig)
head(hex_resultados_2)
# sample_n(hex_resultados_2, 20)

# Ficou algum hexágono de fora?
hex_pop_op %>% filter(!id_hex %in% hex_resultados_2$orig & estudantes_totais > 0 & matriculas_totais > 0)
hex_resultados_2 %>% group_by(orig) %>% tally() %>% filter(n > 1)


# Juntar resultados na grade completa de hexágonos
hex_resultados <- 
  hex_pop_op %>% 
  left_join(hex_resultados_1, by = c('id_hex' = 'orig')) %>% 
  left_join(hex_resultados_2, by = c('id_hex' = 'orig')) %>% 
  # Demarcar hexágonos sem população ou matrícula
  mutate(hexclas_24 = ifelse(is.na(hexclas_24), 'hexag sem pop ou mat', hexclas_24),
         hexclas_28X = ifelse(is.na(hexclas_28X), 'hexag sem pop ou mat', hexclas_28X)) %>% 
  # Quais as diferenças de viagens feitas dentro do tempo e extensão percorrida
  # em infraestrutura cicloviária dessas mesmas viagens entre os dois cenários?
  mutate(dif_vgs_ok = vgs_ok_28X - vgs_ok_24,
         dif_cic_ok = ext_cic_28X - ext_cic_24,
         dif_cic_vg_ok = cic_vg_28X - cic_vg_24)

# # Há casos em que dif_cic será negativo. Mapeei duas situações quando o método
# # do lpSolve é utilizado invertendo a matriz (população > matrículas). No segundo
# # método (população <= matrículas), só o último acontece:
# hex_resultados %>% filter(dif_cic_ok < 0) %>% select(id_hex, matches('^vgs_ok'), matches('^ext_cic'), dif_cic_ok) %>% sample_n(20)
# # 1. Houve viagens em 2024 mas não em 2028, pois o lpSolve fez a solução 
# # desconsiderando algumas pessoas que em 2024 geravam menos custo do que em 2028
# hex_resultados %>% filter(id_hex %in% c('89a8100f21bffff', '89a81005b43ffff', '89a8107062fffff'))
# # 2. Há viagens nos dois cenários, mas possivelmente há mais alternativas de
# # rotas cicláveis, o que faz com que os trajetos escolhidos sejam mais curtos
# # (consequentemente, reduzindo o uso de infra cicloviária)
# hex_resultados %>% filter(id_hex %in% c('89a81072d2fffff', '89a8100dd03ffff', '89a81072d33ffff'))

# hex_resultados %>% select(dif_cic_ok) %>% distinct() %>% arrange(dif_cic_ok)
# hex_resultados %>% filter(dif_cic_ok < -8000) %>% select(4:11)
# hex_resultados %>% filter(dif_cic_ok < -8000) %>% select(12:ncol(.))
# hex_resultados %>% filter(dif_cic_ok < -8000) %>% select(1:3)
# 
# Como checar:
# hex_resultados %>% filter(id_hex == '89a81072e13ffff') %>% select(id_hex, estudantes_totais, hexclas_24, matches('_cic'))
# ods_vgs %>% filter(orig == '89a81072e13ffff')
# 
# Qual a rota selecionada?
# test_id_hex_orig <- '89a81072e13ffff'
# test_id_hex_orig <- '89a81072e1bffff'
# test_id_rota <- ods_resultados %>% filter(orig == test_id_hex_orig) %>% mutate(orig_dest = str_c(orig, dest, sep = '-')) %>% select(orig_dest) %>% pull()
# 
# 
# ttmatrix_24 <- sprintf('%s/10_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano1, tempo_max)
# ttmatrix_24 <- read_delim(ttmatrix_24, delim = ';', col_types = 'cidddddddddc')
# 
# ttmatrix_28 <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano2, tempo_max)
# ttmatrix_28 <- read_delim(ttmatrix_28, delim = ';', col_types = 'ciidddddddddc')
# 
# ttmatrix_24 %>% filter(hex_id == test_id_rota) %>% select(hex_id, time, distance, via_comum, infra_ciclo)
# ttmatrix_28 %>% filter(hex_id == test_id_rota) %>% select(hex_id, time, distance, via_comum, infra_ciclo)
# 
# Abrir arquivos de ttmatrix de 2024 e 2028 na pasta pasta_ttmatrix_24_28,
# copiar os polylines e comparar pelo link abaixo (vai precisar do QGIS para
# comparar as redes 2024 e 2028 e entender o que aconteceu):
# https://valhalla.github.io/demos/polyline/?unescape=true&polyline6=false

# Juntar resultados ao shapefile de hexágonos
hexagonos <- 
  hexagonos %>% 
  left_join(hex_resultados, by = 'id_hex') %>% 
  relocate(c(perccic_24, perccic_28X, geom), .after = last_col())

# Gravar resultados
out_hex <- sprintf('%s/hexagonos_sp_resultados_metodo_lpSolveX_res09_2024_2028.gpkg', pasta_lpsolve_24_28)
st_write(hexagonos, out_hex, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


hexagonos %>% st_drop_geometry() %>% select(matches('_tempo')) %>% summary()
# vgs_ok_24_tempo_med vgs_ok_24_tempo_tot vgs_ok_28X_tempo_med vgs_ok_28X_tempo_tot
# Min.   :  0.0       Min.   :     0      Min.   :  0.0        Min.   :     0      
# 1st Qu.:226.7       1st Qu.:  2765      1st Qu.:222.9        1st Qu.:  2716      
# Median :385.6       Median :  9976      Median :379.4        Median :  9866      
# Mean   :401.4       Mean   : 16512      Mean   :395.8        Mean   : 16329      
# 3rd Qu.:572.0       3rd Qu.: 23072      3rd Qu.:565.2        3rd Qu.: 22888      
# Max.   :900.0       Max.   :254422      Max.   :900.0        Max.   :254422      
# NA's   :7384        NA's   :7384        NA's   :7384         NA's   :7384 

# Limpar ambiente
rm(hex_resultados_1, hex_resultados_2, orig_impossiveis, hex_pop_op, hex_resultados, 
   out_hex, ods_resultados, ods_vgs)


# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por osm_id
# ------------------------------------------------------------------------------

# Abrir cópia do viário de SP com osm_ids
viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp))
viario_sp <- viario_sp %>% select(osm_id) %>% st_transform(31983)
viario_sp <- viario_sp %>% mutate(ext = round(st_length(.), 4))
head(viario_sp)


# Infraestrutura cicloviária ano 1
ciclo_24 <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_ttmatrix_24_28, ano1)
ciclo_24 <- read_delim(ciclo_24, delim = ';', col_types = 'cc')
ciclo_24 <- ciclo_24 %>% rename(infra_ciclo_24 = infra_ciclo)
head(ciclo_24)

# Infraestrutura cicloviária ano 2
ciclo_28 <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_ttmatrix_24_28, ano2)
ciclo_28 <- read_delim(ciclo_28, delim = ';', col_types = 'cc')
ciclo_28 <- ciclo_28 %>% rename(infra_ciclo_28 = infra_ciclo)
head(ciclo_28)


# Resultados por osm_id do lpSolve para o ano 1
viario_resultados_1 <- sprintf('%s/08_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
viario_resultados_1 <- read_delim(viario_resultados_1, delim = ';', col_types = 'ciddd')
viario_resultados_1 <- viario_resultados_1 %>% rename(vgs_24 = viagens_tot,
                                                      cic_24 = ext_ciclo_tot_m,
                                                      perc_vg_24  = particip_perc_viagens_vs_todo,
                                                      perc_ext_24 = particip_perc_extciclo_vs_todo)
head(viario_resultados_1)

# Resultados por hexágono do lpSolveX para o ano 2
viario_resultados_2 <- sprintf('%s/02_lpSolveX_resultados_por_osmid_alocacao%s_rotas%s_res09_%smin.csv', pasta_aop_lpsolveX, ano1, ano2, tempo_max)
viario_resultados_2 <- read_delim(viario_resultados_2, delim = ';', col_types = 'ciddd')
viario_resultados_2 <- viario_resultados_2 %>% rename(vgs_28  = viagens_tot,
                                                      cic_28  = ext_ciclo_tot_m,
                                                      perc_vg_28  = particip_perc_viagens_vs_todo,
                                                      perc_ext_28 = particip_perc_extciclo_vs_todo)
head(viario_resultados_2)


# Juntar resultados ao shapefile de viário
viario_sp <- 
  viario_sp %>% 
  left_join(viario_resultados_1, by = c('osm_id' = 'osm_way_id')) %>%
  left_join(viario_resultados_2, by = c('osm_id' = 'osm_way_id')) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(dif_vgs = vgs_28 - vgs_24,
         dif_cic = cic_28 - cic_24) %>% 
  left_join(ciclo_24, by = 'osm_id') %>%
  left_join(ciclo_28, by = 'osm_id') %>%
  relocate(c(perc_vg_24, perc_ext_24, perc_vg_28, perc_ext_28, geom), .after = last_col())

# Converter medida para double
viario_sp <- viario_sp %>% mutate(ext = as.double(ext))

head(viario_sp)

# Gravar resultados
out_viario <- sprintf('%s/viario_osm_sp_resultados_metodo_lpSolveX_res09_%s_%s.gpkg', pasta_lpsolve_24_28, ano1, ano2)
st_write(viario_sp, out_viario, driver = 'GPKG', append = FALSE, delete_layer = TRUE)



# ------------------------------------------------------------------------------
# Estatísticas dos resultados
# ------------------------------------------------------------------------------

# Criar categorias de intervalos para refletir os mapas do QGIS:
# https://gis.stackexchange.com/questions/132775/how-does-the-qgis-construct-class-intervals
# If a point value is 0.8 it will be rendered with the symbol you assigned to the first class, 0.60 - 0.80.
# (0.60 - 0.80]
# (0.80 - 1.00]

# Tempo médio 2024
hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_24 != 'hex origem impossivel' & hexclas_24 != 'viagens fora do tempo') %>% 
  group_by(hexclas_24) %>% tally()
  select(id_hex, vgs_ok_24_tempo_med) %>% 
  mutate(tempo_medio_fx = case_when(between(vgs_ok_24_tempo_med,   0.00, 180.00) ~ '00-03 minutos',
                                    between(vgs_ok_24_tempo_med, 180.01, 360.00) ~ '03-06 minutos',
                                    between(vgs_ok_24_tempo_med, 360.01, 540.00) ~ '06-09 minutos',
                                    between(vgs_ok_24_tempo_med, 540.01, 720.00) ~ '09-12 minutos',
                                    between(vgs_ok_24_tempo_med, 720.01, 900.00) ~ '12-15 minutos',
                                    is.na(vgs_ok_24_tempo_med) ~ 'Mais de 15 min')) %>% 
  group_by(tempo_medio_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100)

# Tempo médio 2028
hexagonos %>% 
  st_drop_geometry() %>%
  # filter(hexclas_24 != 'hexag sem pop ou mat') %>% 
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_28X != 'hex origem impossivel' & hexclas_28X != 'viagens fora do tempo') %>% 
  select(id_hex, vgs_ok_28X_tempo_med) %>% 
  mutate(tempo_medio_fx = case_when(between(vgs_ok_28X_tempo_med,   0.00, 180.00) ~ '00-03 minutos',
                                    between(vgs_ok_28X_tempo_med, 180.01, 360.00) ~ '03-06 minutos',
                                    between(vgs_ok_28X_tempo_med, 360.01, 540.00) ~ '06-09 minutos',
                                    between(vgs_ok_28X_tempo_med, 540.01, 720.00) ~ '09-12 minutos',
                                    between(vgs_ok_28X_tempo_med, 720.01, 900.00) ~ '12-15 minutos',
                                    is.na(vgs_ok_28X_tempo_med) ~ 'Mais de 15 min')) %>% 
  group_by(tempo_medio_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100)


max(hexagonos$cic_vg_24, na.rm = TRUE) # 2398.389
max(hexagonos$cic_vg_28X, na.rm = TRUE) # 2623.132

# Uso médio de infra cicloviária 2024
hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_24 != 'hex origem impossivel' & hexclas_24 != 'viagens fora do tempo') %>% 
  select(id_hex, cic_vg_24) %>% 
  mutate(uso_medio_fx = case_when(between(cic_vg_24,    0.00,  500.00) ~ '0000-0500 metros',
                                  between(cic_vg_24,  500.01, 1000.00) ~ '0500-1000 metros',
                                  between(cic_vg_24, 1000.01, 1500.00) ~ '1000-1500 metros',
                                  between(cic_vg_24, 1500.01, 2000.00) ~ '1500-2000 metros',
                                  between(cic_vg_24, 2000.01, 2650.00) ~ '2000-2650 metros',
                                  TRUE ~ NA)) %>% 
  group_by(uso_medio_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100) %>% 
  arrange(desc(uso_medio_fx))


# Uso médio de infra cicloviária 2028
hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_28X != 'hex origem impossivel' & hexclas_28X != 'viagens fora do tempo') %>% 
  select(id_hex, cic_vg_28X) %>% 
  mutate(uso_medio_fx = case_when(between(cic_vg_28X,    0.00,  500.00) ~ '0000-0500 metros',
                                  between(cic_vg_28X,  500.01, 1000.00) ~ '0500-1000 metros',
                                  between(cic_vg_28X, 1000.01, 1500.00) ~ '1000-1500 metros',
                                  between(cic_vg_28X, 1500.01, 2000.00) ~ '1500-2000 metros',
                                  between(cic_vg_28X, 2000.01, 2650.00) ~ '2000-2650 metros',
                                  TRUE ~ NA)) %>% 
  group_by(uso_medio_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100) %>% 
  arrange(desc(uso_medio_fx))


# % de uso da rede cicloviária frente ao total
hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_24 != 'hex origem impossivel' & hexclas_24 != 'viagens fora do tempo') %>% 
  select(id_hex, cic_perc_24) %>% 
  mutate(perc_uso_ciclo_fx = case_when(between(cic_perc_24,  0.00,  20.00001) ~ '00-20%',
                                       between(cic_perc_24, 20.000011,  40.00001) ~ '20-40%',
                                       between(cic_perc_24, 40.000011,  60.00001) ~ '40-60%',
                                       between(cic_perc_24, 60.000011,  80.00001) ~ '60-80%',
                                       between(cic_perc_24, 80.000011, 100.00001) ~ '80-100%',
                                       TRUE ~ NA)) %>% 
  group_by(perc_uso_ciclo_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100) %>% 
  arrange(desc(perc_uso_ciclo_fx))


hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_24 != 'hex origem impossivel' & hexclas_24 != 'viagens fora do tempo') %>% 
  mutate(ext_viagens = ext_tot_24 / vgs_ok_24) %>% 
  # filter(ext_viagens > 500) %>% 
  select(ext_viagens, cic_perc_24) %>% 
  summary()



# % de uso da rede cicloviária frente ao total
hexagonos %>% 
  st_drop_geometry() %>%
  # filter(estudantes_totais > 0 & hexclas_24 != 'hex origem impossivel') %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_28X != 'hex origem impossivel' & hexclas_28X != 'viagens fora do tempo') %>% 
  select(id_hex, cic_perc_28X, hexclas_28X) %>% 
  # Incluindo os decimais longos para classificar números redondos (ex. 60.00),
  # que não estão sendo pegos pelo between(), embora deveriam
  mutate(perc_uso_ciclo_fx = case_when(between(cic_perc_28X,  0.00,  20.00001) ~ '00-20%',
                                       between(cic_perc_28X, 20.000011,  40.00001) ~ '20-40%',
                                       between(cic_perc_28X, 40.000011,  60.00001) ~ '40-60%',
                                       between(cic_perc_28X, 60.000011,  80.00001) ~ '60-80%',
                                       between(cic_perc_28X, 80.000011, 100.00001) ~ '80-100%',
                                       TRUE ~ NA)) %>% 
  # filter(is.na(perc_uso_ciclo_fx) & hexclas_28X != 'hex origem impossivel')
  group_by(perc_uso_ciclo_fx) %>% 
  tally() %>% 
  mutate(perc_tot = n / sum(n) * 100)  %>% 
  arrange(desc(perc_uso_ciclo_fx))


hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0) %>% 
  filter(hexclas_28X != 'hex origem impossivel' & hexclas_28X != 'viagens fora do tempo') %>% 
  mutate(ext_viagens = ext_tot_28X / vgs_ok_28X) %>% 
  # filter(ext_viagens > 500) %>% 
  select(ext_viagens, cic_perc_28X) %>% 
  summary()

# # % de uso da rede cicloviária frente ao total (pares OD distantes pelo menos 500 m)
# hexagonos %>% 
#   st_drop_geometry() %>%
#   filter(estudantes_totais > 0) %>% 
#   filter(hexclas_24 != 'hex origem impossivel' & hexclas_24 != 'viagens fora do tempo') %>% 
#   mutate(ext_viagens = ext_tot_24 / vgs_ok_24) %>% 
#   filter(ext_viagens > 500) %>% 
#   select(id_hex, cic_perc_24) %>% 
#   mutate(perc_uso_ciclo_fx = case_when(between(cic_perc_24,  0.00,  20.00001) ~ '00-20%',
#                                        between(cic_perc_24, 20.000011,  40.00001) ~ '20-40%',
#                                        between(cic_perc_24, 40.000011,  60.00001) ~ '40-60%',
#                                        between(cic_perc_24, 60.000011,  80.00001) ~ '60-80%',
#                                        between(cic_perc_24, 80.000011, 100.00001) ~ '80-100%',
#                                        TRUE ~ NA)) %>% 
#   group_by(perc_uso_ciclo_fx) %>% 
#   tally() %>% 
#   mutate(perc_tot = n / sum(n) * 100) %>% 
#   arrange(desc(perc_uso_ciclo_fx))
# 
# 
# # % de uso da rede cicloviária frente ao total (pares OD distantes pelo menos 500 m)
# hexagonos %>% 
#   st_drop_geometry() %>%
#   # filter(estudantes_totais > 0 & hexclas_24 != 'hex origem impossivel') %>%
#   filter(estudantes_totais > 0) %>% 
#   filter(hexclas_28X != 'hex origem impossivel' & hexclas_28X != 'viagens fora do tempo') %>% 
#   mutate(ext_viagens = ext_tot_28X / vgs_ok_28X) %>% 
#   filter(ext_viagens > 500) %>% 
#   select(id_hex, cic_perc_28X, hexclas_28X) %>% 
#   # Incluindo os decimais longos para classificar números redondos (ex. 60.00),
#   # que não estão sendo pegos pelo between(), embora deveriam
#   mutate(perc_uso_ciclo_fx = case_when(between(cic_perc_28X,  0.00,  20.00001) ~ '00-20%',
#                                        between(cic_perc_28X, 20.000011,  40.00001) ~ '20-40%',
#                                        between(cic_perc_28X, 40.000011,  60.00001) ~ '40-60%',
#                                        between(cic_perc_28X, 60.000011,  80.00001) ~ '60-80%',
#                                        between(cic_perc_28X, 80.000011, 100.00001) ~ '80-100%',
#                                        TRUE ~ NA)) %>% 
#   # filter(is.na(perc_uso_ciclo_fx) & hexclas_28X != 'hex origem impossivel')
#   group_by(perc_uso_ciclo_fx) %>% 
#   tally() %>% 
#   mutate(perc_tot = n / sum(n) * 100)  %>% 
#   arrange(desc(perc_uso_ciclo_fx))





# Os cic_perc_28X que estavam virando NA por serem números redondos:
# hexagonos %>% 
#   st_drop_geometry() %>%
#   filter(id_hex %in% c('89a81076db7ffff', '89a81009397ffff', '89a8100ce0bffff',
#                        '89a81015b6bffff', '89a8104692fffff', '89a8107109bffff', '89a81009eb7ffff')) %>% 
#   select(id_hex, vgs_tot_24, ext_cic_24, cic_vg_24, cic_perc_24,
#          ext_cic_28X, cic_vg_28X, cic_perc_28X) %>% 
#   mutate(cic_perc_28X = as.character(cic_perc_28X))


hexagonos %>% 
  st_drop_geometry() %>% 
  filter(estudantes_totais > 0) %>% 
  filter(estudantes_totais > 0 & vgs_ok_24 > 0) %>% 
  select(cic_perc_28X) %>% 
  summary()

hexagonos %>% 
  st_drop_geometry() %>%
  filter(estudantes_totais > 0 & vgs_ok_24 > 0) %>% 
  select(id_hex, vgs_tot_24, vgs_ok_24, 
         ext_tot_24, ext_cic_24, cic_vg_24, cic_perc_24,
         ext_tot_28X, ext_cic_28X, cic_vg_28X, cic_perc_28X)
