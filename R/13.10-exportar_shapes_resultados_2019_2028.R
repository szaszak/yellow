
# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')

# Definir ano de análise e limite máximo de tempo
ano1 <- '2019'; ano2 <- '2028'; tempo_max <- '15'

# Qual solução usar? A primeira considera população de interesse maior do que as
# matrículas disponíveis; a segunda ajusta a população para caber nas matrículas
versao_solucao <- 2

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_osm_sp     <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_dados <- sprintf("%s/02_dados_pop_mat", pasta_aop_optimum)
if (versao_solucao == 1) {
  pasta_aop_lpsolve <- sprintf("%s/03_lpSolve1_pop_maior_que_mat", pasta_aop_optimum)
} else if (versao_solucao == 2) {
  pasta_aop_lpsolve <- sprintf("%s/04_lpSolve2_pop_ajustada", pasta_aop_optimum)
}
pasta_opaop_ano1  <- sprintf("%s/%s", pasta_aop_lpsolve, ano1)
pasta_opaop_ano2  <- sprintf("%s/%s", pasta_aop_lpsolve, ano2)


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

# Resultados por hexágono do lpSolver para o ano 1
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


# Resultados por hexágono do lpSolver para o ano 2
hex_resultados_2 <- sprintf('%s/07_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano2, ano2, tempo_max)
hex_resultados_2 <- read_delim(hex_resultados_2, delim = ';', col_types = 'ccidddii')
hex_resultados_2 <- hex_resultados_2 %>% select(orig,
                                                hexclas_28 = class_hex_orig,
                                                vgs_ok_28_tempo = tempo_medio_vgs_a_tempo,
                                                vgs_ok_28  = viagens_a_tempo,
                                                vgs_nok_28 = viagens_fora_tempo,
                                                vgs_tot_28 = tot_viagens,
                                                ext_cic_28 = ext_ciclo_tot_m,
                                                perccic_28 = perc_part)
head(hex_resultados_2)

# Juntar aos resultados também as origens impossíveis
orig_impossiveis_2 <- sprintf('%s/06_lpsolve_hexagonos_origens_impossiveis_%s_%smin.gpkg', pasta_opaop_ano2, ano2, tempo_max)
orig_impossiveis_2 <- read_sf(orig_impossiveis_2) %>% st_drop_geometry() %>% select(orig = id_hex)
orig_impossiveis_2 <- orig_impossiveis_2 %>% mutate(hexclas_28 = 'hex origem impossivel',
                                                    vgs_ok_28_tempo = as.numeric(NA),
                                                    vgs_ok_28  = as.numeric(NA),
                                                    vgs_nok_28 = as.numeric(NA),
                                                    vgs_tot_28 = as.numeric(NA),
                                                    ext_cic_28 = as.numeric(NA),
                                                    perccic_28 = as.numeric(NA))

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
         hexclas_28 = ifelse(is.na(hexclas_28), 'hexag sem pop ou mat', hexclas_28)) %>% 
  # Quais as diferenças de viagens feitas dentro do tempo e extensão percorrida
  # em infraestrutura cicloviária dessas mesmas viagens entre os dois cenários?
  mutate(dif_vgs_ok = vgs_ok_28 - vgs_ok_19,
         dif_cic_ok = ext_cic_28 - ext_cic_19)


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
  relocate(c(perccic_19, perccic_28, geom), .after = last_col())

# Gravar resultados
out_hex <- sprintf('%s/hexagonos_sp_resultados_metodo_%s_res09_2019_2028.gpkg', pasta_aop_lpsolve, versao_solucao)
st_write(hexagonos, out_hex, driver = 'GPKG', append = FALSE, delete_layer = TRUE)



# ------------------------------------------------------------------------------
# Base geral - viário da cidade de SP
# ------------------------------------------------------------------------------

# Abrir cópia do viário de SP com osm_ids
viario_sp <- read_sf(sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp))
viario_sp <- viario_sp %>% select(osm_id) %>% st_transform(31983)
viario_sp <- viario_sp %>% mutate(ext = round(st_length(.), 4))
head(viario_sp)

# Infraestrutura cicloviária ano 1
ciclo_19 <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_alter, ano1)
ciclo_19 <- read_delim(ciclo_19, delim = ';', col_types = 'cc')
ciclo_19 <- ciclo_19 %>% rename(infra_ciclo_19 = infra_ciclo)
head(ciclo_19)

# Infraestrutura cicloviária ano 2
# # ATENÇÃO: Para a rede 2028, alguns osm_ids constantes no arquivo original
# tmp_infra_ciclo_%s.csv estão marcados como pertencentes à rede de referência, 
# mas na verdade possuem só um trecho que de fato tem estrutura cicloviária. 
# Isso foi percebido no momento do script 13.09 e um aviso foi colocado no 
# script 12.14 e no script 12.12, que é o que gera o arquivo. A correção se deu 
# no script 13.09, que gerou o  arquivo revisado tmp_infra_ciclo_%s_revisada.csv.
# Caso a correção seja implementada no 12.12, apagar esse aviso e voltar a 
# referência para a primeira linha abaixo, em vez da segunda:
# ciclo_28 <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_alter, ano2)
ciclo_28 <- sprintf('%s/tmp_infra_ciclo_%s_revisada.csv', pasta_aoprv_alter, ano2)
ciclo_28 <- read_delim(ciclo_28, delim = ';', col_types = 'cc')
ciclo_28 <- ciclo_28 %>% rename(infra_ciclo_28 = infra_ciclo)
head(ciclo_28)


# Resultados por osm_id do lpSolver para o ano 1
viario_resultados_1 <- sprintf('%s/08_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano1, ano1, tempo_max)
viario_resultados_1 <- read_delim(viario_resultados_1, delim = ';', col_types = 'cidd')
viario_resultados_1 <- viario_resultados_1 %>% rename(vgs_19  = viagens_tot,
                                                      cic_19  = ext_ciclo_tot_m,
                                                      perc_19 = perc_part)
head(viario_resultados_1)

# Resultados por hexágono do lpSolver para o ano 2
viario_resultados_2 <- sprintf('%s/08_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano2, ano2, tempo_max)
viario_resultados_2 <- read_delim(viario_resultados_2, delim = ';', col_types = 'cidd')
viario_resultados_2 <- viario_resultados_2 %>% rename(vgs_28  = viagens_tot,
                                                      cic_28  = ext_ciclo_tot_m,
                                                      perc_28 = perc_part)
head(viario_resultados_2)


# Juntar resultados ao shapefile de viário
viario_sp <- 
  viario_sp %>% 
  left_join(viario_resultados_1, by = c('osm_id' = 'osm_way_id')) %>%
  left_join(viario_resultados_2, by = c('osm_id' = 'osm_way_id')) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
  mutate(dif_vgs = vgs_28 - vgs_19,
         dif_cic = cic_28 - cic_19) %>% 
  left_join(ciclo_19, by = 'osm_id') %>%
  left_join(ciclo_28, by = 'osm_id') %>%
  relocate(c(perc_19, perc_28, geom), .after = last_col())

head(viario_sp)

# Gravar resultados
out_viario <- sprintf('%s/viario_osm_sp_resultados_metodo_%s_res09_2019_2028.gpkg', pasta_aop_lpsolve, versao_solucao)
st_write(viario_sp, out_viario, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Comparativo - Infraestruturas cicloviárias 2019-2028
# ------------------------------------------------------------------------------

# 3.286.964 metros de infra ciclo em 2028 após revisão dos osm_ids da rede de referência no script 13.09
infra_ciclo_2028 <- viario_sp %>% filter(!is.na(infra_ciclo_28)) %>% st_drop_geometry() %>% select(ext) %>% sum()
# 636.401,5 metros de infra ciclo em 2019
infra_ciclo_2019 <- viario_sp %>% filter(!is.na(infra_ciclo_19)) %>% st_drop_geometry() %>% select(ext) %>% sum()
# 2.650.563 metros de diferença entre as duas após revisão dos osm_ids da rede de referência no script 13.09
infra_ciclo_2028 - infra_ciclo_2019

# Sp tinha 474 km de ciclovias e ciclofaixas em 2019; meta era 1.800 km - em uma
# estimativa por extensão de linhas no QGIS:
#   474 km = 636401 m -> proporção de 1.342618
# 1.800 km = x
# x = 1800 * 636401 / 474 = 2.416.713 m
# 2.416.713 m - 636.401 m existentes -> faltam 1.780.312 metros a serem priorizados
# (1800 * 636401 / 474) - 636401 = 1.780.312
# ((1800 * 636401 / 474) - 636401) / 2 = 890.155,80 (metade da rede futura)

# Quanto da rede temos que priorizar na visualização do QGIS?
viario_sp %>% filter(dif_cic >= 15000) %>% st_drop_geometry() %>% select(ext) %>% sum()
# dif_cic > 15000 =   837.216,80 m
viario_sp %>% filter(dif_cic >= 15000 & is.na(infra_ciclo_19)) %>% st_drop_geometry() %>% select(ext) %>% sum()
# dif_cic >= 15000 & is.na(infra_ciclo_19) = 774.190,3
viario_sp %>% filter(dif_cic > 5000) %>% st_drop_geometry() %>% select(ext) %>% sum()
# dif_cic >  5000 = 1.303.798,00 m

viario_sp %>% filter(dif_cic > 15000) %>% mapview()
