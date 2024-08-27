# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_ssd         <- "/media/livre/SSD120GB/yellow"
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
dir.create(pasta_aoprv_alter, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Agregar totais de população e oportunidades aos hexágonos
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp_orig <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp_orig <- hex_sp_orig %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))

# Tratar como dataframe e selecionar somente colunas de interesse
hex_sp <- st_centroid(hex_sp_orig) %>% mutate(centroides = as.character(geom)) %>% st_drop_geometry()

# Oportunidades por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_landuse_2019_v2.csv', dados_originais)
dados_ipea <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea <- dados_ipea %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar só totais de oportunidades
dados_ipea <- dados_ipea %>% mutate(oportunidades = T001 + E001 + M001 + S001 + C001) %>% select(id_hex, oportunidades)
head(dados_ipea)

# População por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_population_2010_v2.csv', dados_originais)
dados_ipea_pop <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea_pop <- dados_ipea_pop %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar neste momento só dados totais da população
dados_ipea_pop <- dados_ipea_pop %>% select(id_hex, populacao = P001)
head(dados_ipea_pop)

# Juntar dados de oportunidades e população
hex_ipea <- 
  hex_sp %>% 
  left_join(dados_ipea, by = 'id_hex') %>% 
  left_join(dados_ipea_pop, by = 'id_hex')

# Hexágonos IPEA sem oportunidade ou sem população devem ser descartados
# ATENÇÃO: Este filtro não pode acontecer, porque vamos usar dados do CENSO e de
# matrículas adiante que vão estar em hexágonos diferentes dos do IPEA. Se esse
# filtro for feito aqui, alguns hexágonos vão sumir e não entrarão como par OD
# no routing
# hex_ipea <- hex_ipea %>% filter(oportunidades > 0 | populacao > 0)

# Limpar ambiente
rm(dados_ipea, dados_ipea_pop)


# ------------------------------------------------------------------------------
# Juntar com dados de população e escolas gerados posteriormente
# ------------------------------------------------------------------------------

# ATENÇÃO: os dados de população e de escolas usados aqui haviam sido gerados 
# só mais pra frente nos scripts (13.04 a 13.06). Como os dados diferem do IPEA,
# precisamos garantir que os hexágonos onde há população de interesse e os onde
# há escolas de interesse estejam contemplados nas contas

# Pasta onde vão estar esses dados
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)

# Demanda: quantidade de pessoas por hexágono
pop <- sprintf('%s/hex_grid_sp_res09_dados_censo_por_hexagono.csv', pasta_aop_optimum)
pop <- read_delim(pop, delim = ';', col_types = cols(.default = "c"))
# pop %>% filter(!id_hex %in% hex_sp$id_hex)
pop <- pop %>% mutate_at(2:ncol(.), as.numeric)
# Garantir que haja população
pop <- pop %>% filter(pop_res2_hex > 0)
pop <- pop %>% select(id_hex, pop = pop_res2_hex)
# Checar se algum id ficou duplicado por qualquer motivo
# pop %>% group_by(orig) %>% tally() %>% filter(n > 1) %>% nrow()
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.csv', pasta_aop_optimum)
mat <- read_delim(mat, delim = ';', col_types = cols(.default = "c"))
# mat %>% filter(!id_hex %in% hex_sp$id_hex)
mat <- mat %>% mutate_at(2:ncol(.), as.numeric)
# Garantir que haja matrículas
mat <- mat %>% filter(matriculas_ensino_medio > 0 | matriculas_idades_15_17 > 0)
# Checar se algum id ficou duplicado por qualquer motivo
# mat %>% group_by(id_hex) %>% tally() %>% filter(n > 1) %>% nrow()
head(mat)


# Puxar hexágonos de população e matrículas que não estão na grade do IPEA

# Hexágonos calculados com população e que ainda não estão nos hexágonos do IPEA
# ATENÇÃO: da forma como estava sendo feito antes, hexágonos que no IPEA possuíam
# oportunidades mas não população (ex. 89a810008b7ffff), mas que nos dados do
# CENSO possuíam população (proporcional, calculada para o hexágono) não iriam
# entrar pois já estão na base do IPEA. Com isso, não seriam consideradas como 
# origem. Lá na frente, no script do lpSolve, cerca de 3.200 hexágonos podem
# estar nessa situação
# hex_calc_pop <- hex_sp %>% filter(id_hex %in% pop$id_hex & !id_hex %in% hex_ipea$id_hex)
# hex_calc_pop <- pop %>% filter(!id_hex %in% hex_ipea$id_hex)

# # Dos hexágonos calculados, quais possuem população e oportunidades (são origens e destinos)
# hex_calc_pop_op <- rbind(hex_calc_pop, hex_calc_esc) %>% group_by(id_hex) %>% tally() %>% filter(n > 1)
# hex_calc_pop_op <- hex_sp %>% filter(id_hex %in% hex_calc_pop_op$id_hex)
# 
# # Dos hexágonos calculados, quais possuem somente população (são só origens)
# hex_calc_so_pop <- hex_calc_pop %>% filter(!id_hex %in% hex_calc_pop_op$id_hex)
# hex_calc_so_pop <- hex_sp %>% filter(id_hex %in% hex_calc_so_pop$id_hex)
# 
# # Dos hexágonos calculados, quais possuem somente escolas (são só destinos)
# hex_calc_so_op <- hex_calc_esc %>% filter(!id_hex %in% hex_calc_pop_op$id_hex)
# hex_calc_so_op <- hex_sp %>% filter(id_hex %in% hex_calc_so_op$id_hex)
# 
# 
# # Juntar com hexágonos do IPEA
# 
# # Hexágono contém população e oportunidades (são origens e destinos)
# hex_pop_op <- 
#   hex_ipea %>% 
#   filter(oportunidades > 0 & populacao > 0) %>% 
#   rbind(hex_calc_pop_op) %>% 
#   distinct()
# 
# # Hexágono contém somente população (são só origens)
# hex_so_pop <- 
#   hex_ipea %>% 
#   filter(oportunidades == 0 & populacao > 0) %>% 
#   rbind(hex_calc_so_pop) %>% 
#   distinct()
# 
# # Hexágono contém somente oportunidades (são só destinos)
# hex_so_op <- 
#   hex_ipea %>% 
#   filter(oportunidades > 0 & populacao == 0) %>% 
#   rbind(hex_calc_so_op) %>% 
#   distinct()
# 
# 
# # Limpar ambiente
# rm(hex_ipea, pop, mat, hex_calc_pop, hex_calc_esc, 
#    hex_calc_pop_op, hex_calc_so_pop, hex_calc_so_op,
#    pasta_aop_optimum)


# # Somar populações das duas fontes, só para demarcar hexágonos a serem considerados
# como origens
hex_ipea <- hex_ipea %>% left_join(pop, by = 'id_hex') %>% mutate(pop_marker = populacao + pop)

# Hexágonos calculados com escolas e que ainda não estão nos hexágonos do IPEA
# hex_calc_esc <- hex_sp %>% filter(id_hex %in% mat$id_hex & !id_hex %in% hex_ipea$id_hex)
# hex_calc_esc <- mat %>% filter(!id_hex %in% hex_ipea$id_hex)
hex_ipea <- 
  hex_ipea %>% 
  left_join(mat, by = 'id_hex') %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  # Somar oportunidades e matrículas, só para demarcar hexágonos a serem considerados
  # como destinos
  mutate(op_marker = oportunidades + matriculas_ensino_medio + matriculas_idades_15_17)

# Dos hexágonos calculados, quais possuem população e oportunidades (são origens e destinos)
hex_pop_op <- hex_ipea %>% filter(pop_marker > 0 & op_marker > 0)

# Dos hexágonos calculados, quais possuem somente população (são só origens)
hex_so_pop <- hex_ipea %>% filter(pop_marker > 0 & op_marker == 0)

# Dos hexágonos calculados, quais possuem somente escolas (são só destinos)
hex_so_op  <- hex_ipea %>% filter(pop_marker == 0 & op_marker > 0)

# Dos hexágonos calculados, quais não possuem oportunidades ou população?
hex_zero   <- hex_ipea %>% filter(pop_marker == 0 & op_marker == 0)

# nrow(hex_ipea) ==  nrow(hex_pop_op) + nrow(hex_so_pop) + nrow(hex_so_op) + nrow(hex_zero)


# Gravar hexágonos que possuem alguma população ou oportunidade
hex_sp_orig <- hex_sp_orig %>% left_join(hex_ipea, by = 'id_hex') %>% select(-centroides)
# hex_sp_orig <- hex_sp_orig %>% filter(id_hex %in% hex_pop_op$id_hex | id_hex %in% hex_so_pop$id_hex | id_hex %in% hex_so_op$id_hex)
out_hex_considerados <- sprintf('%s/tmp_sao_paulo_hexagonos_populacao_oportunidades.gpkg', pasta_aoprv_alter)
st_write(hex_sp_orig, out_hex_considerados, driver = 'GPKG', append = FALSE)

# Limpar ambiente
rm(hex_ipea, pop, mat, hex_zero, pasta_aop_optimum, hex_sp_orig, out_hex_considerados)


# ------------------------------------------------------------------------------
# Gerar latlong para as origens e destinos (centroides dos hexágonos)
# ------------------------------------------------------------------------------

# Separar coluna de centroides em latlon
hex_sp <-
  hex_sp %>%
  separate(centroides, '[c\\(\\), )]', into = c('x', 'y', 'lon', 'z', 'lat', 'u')) %>%
  select(id_hex, lat, lon)


# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/hex_spo_res09_26vizinhos.csv", pasta_aoprev_hex)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
head(hex_com_vizinhos)

# # Por que origem com 62 pessoas não chega a um hexágono do lado com 386 matrículas?
# hex_com_vizinhos %>% filter(id_hex_x == '89a81039c77ffff' & id_hex_y == '89a81039c63ffff')
# Origem está nos hexágonos só com população
# hex_so_pop %>% filter(id_hex == '89a81039c77ffff')
# Destino está nos hexágonos com população e oportunidades
# hex_pop_op %>% filter(id_hex == '89a81039c63ffff')
# # Por que 10 pessoas 'morando' no cemitério do Campo Grande e que não chegam 
# a 142 vagas do outro lado da rua?
# hex_com_vizinhos %>% filter(id_hex_x == '89a810008b7ffff' & id_hex_y == '89a81000d7bffff')

# Gerar hexágonos de origem e destino para routing, em que os hexágonos de origem
# necessariamente possuam população e os de destino possuam oportunidades
hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  # Origens precisam conter população, ou seja, precisam estar em hex_pop_op ou hex_so_pop
  filter(id_hex_x %in% hex_pop_op$id_hex | id_hex_x %in% hex_so_pop$id_hex) %>%
  # Destinos precisam conter oportunidades, ou seja, precisam estar em hex_pop_op ou hex_so_op
  filter(id_hex_y %in% hex_pop_op$id_hex | id_hex_y %in% hex_so_op$id_hex) %>%
  # Checar se par de hexágonos que havia 'sumido' permanece no filtro - ok. Erro
  # estava na linha acima, em que filtro era id_hex_x em vez de id_hex_y
  # filter(id_hex_x == '89a81039c77ffff' & id_hex_y == '89a81039c63ffff')
  # Juntar hexágonos de origem e destino às cordenadas latlong de seus centroides
  left_join(hex_sp, by = c('id_hex_x' = 'id_hex')) %>%
  left_join(hex_sp, by = c('id_hex_y' = 'id_hex'))
  
# Remover hexágonos vizinhos que estão fora do shape de São Paulo - em outras
# palavras, remover quaisquer linhas que possuem lat ou lon = NA
hex_com_vizinhos <- hex_com_vizinhos %>% drop_na()

# Método anterior, que considerava todos os hexágonos como origem e destino possíveis: 
# 10.911.566 / 1.267.988 = 8.6 vezes as queries com resolução 8
# Agora, incluindo mais uns 6 mil hexágonos: 13,694,545 rows remaining
nrow(hex_com_vizinhos)

# Limpar ambiente
rm(hex_sp, hex_pop_op, hex_so_pop, hex_so_op)


# ------------------------------------------------------------------------------
# Criar base para routing
# ------------------------------------------------------------------------------

# Criar coluna com URL para GET no GraphHopper
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id'

hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))


# Criar uma coluna de id
hex_com_vizinhos <- hex_com_vizinhos %>% mutate(id = str_c(id_hex_x, id_hex_y, sep = '-'), .before = 'id_hex_x')
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
head(hex_com_vizinhos)


# Guardar resultados - base integral
out_file <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
if (file.exists(out_file)) { file.rename(from = out_file, to = sprintf('%s_BKP_APAGAR', out_file)) }
write_delim(hex_com_vizinhos, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Dividir resultados para processamento - fazer para 2019 e, depois, 2028
# ------------------------------------------------------------------------------

# ano <- '2019'
ano <- '2028'

# # Criar base de ids já processados, caso ainda não exista
# 
# if (ano == '2019') {
#   pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_ssd)
#   arqs_resultados <- data.frame(arq = list.files(pasta_rts_aopt_19, recursive = FALSE, full.names = TRUE))
# } else if (ano == '2028') {
#   pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_ssd)
#   arqs_resultados <- data.frame(arq = list.files(pasta_rts_aopt_28, recursive = FALSE, full.names = TRUE))
# }
# 
# # Gerar arquivo com ids de rotas já rodadas
# detach("package:tidylog")
# for (arq_file in arqs_resultados$arq) {
#   # arq_file <- arqs_resultados$arq[1]
# 
#   # Abrir arquivos de resultados (rotas já processadas)
#   arq <- read_delim(arq_file, delim = ';', col_types = cols(.default = "c")) %>% distinct()
# 
#   # Regravar arquivo sem linhas repetidas, caso existam
#   write_delim(arq, arq_file, delim = ';')
# 
#   # Reconstituir hex_id longo
#   arq <-
#     arq %>%
#     select(hex_id) %>%
#     mutate(hex_id = str_replace(hex_id, '^([a-z0-9]{6})-([a-z0-9]{6})', '89a81\\1ffff-89a81\\2ffff'))
# 
#   # Gravar somente ids já processados
#   ids_processados <- sprintf('%s/tmp_00_ids_processados_%s.csv', pasta_ssd, ano)
#   if (file.exists(ids_processados)) {
#     write_delim(arq, ids_processados, delim = ';', append = TRUE)
#   } else {
#     write_delim(arq, ids_processados, delim = ';', append = FALSE)
#   }
# 
# }
# 
# # Limpar ambiente
# rm(arqs_resultados, arq_file, arq, ids_processados)


# Dividir resultados em arquivos menores, para processamento
hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
# nrow(hex_com_vizinhos) # 10.911.566; agora 13,851,772
head(hex_com_vizinhos)

# Hexágono que estava com problema agora está na base? Sim
# hex_com_vizinhos %>% filter(id == '89a81039c77ffff-89a81039c63ffff')


# Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
# do dataframe hex_com_vizinhos se houver
# library('tidylog')
ids_processados <- sprintf('%s/tmp_00_ids_processados_%s.csv', pasta_ssd, ano)
if (file.exists(ids_processados)) {
  ids_processados <- read_delim(ids_processados, delim = ';', col_types = 'c')
  # head(ids_processados)
  
  # Remover ids já processados
  hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% ids_processados$hex_id)
  
  # # Por que 10 pessoas 'morando' no cemitério do Campo Grande e que não chegam 
  # a 142 vagas do outro lado da rua?
  # hex_com_vizinhos %>% filter(id == '89a810008b7ffff-89a81000d7bffff')
}
rm(ids_processados, ano)



# Definir intervalo que custe pouca memória do future
intervalo <- 100000
# Puxar valor de nrow(hex_com_vizinhos)
max_value <- nrow(hex_com_vizinhos)
# max_value/ intervalo # 110 vezes

# Abrir base hex_com_vizinhos e selecionar quantidade de linhas
# de acordo com intervalo, checar ids que já rodaram, fazer
# routing com future, limpar memória e reiniciar até terminar
counter <- 1
for (i in seq(1, max_value, intervalo)) {
  # i <- 1;

  # Se valor máximo pelo intervalo for maior que dataframe,
  # considerar tamanho do dataframe como máximo
  interval_max <- i + intervalo - 1
  if (interval_max > max_value) { interval_max <- max_value }
  print(sprintf('%i - %i', i, interval_max))

  # Puxar só linhas correspondentes ao intervalo
  out_hex <- hex_com_vizinhos %>% slice(i:interval_max)

  # Gravar arquivo temporário
  out_file <- sprintf('%s/tmp_base_para_routing_res09_26vizinhos_%s.csv', pasta_ssd, str_pad(counter, 3, pad = '0'))
  write_delim(out_hex, out_file, delim = ';')
  
  # Atualizar counter
  counter <- counter + 1

}
