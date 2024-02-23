# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_ttmarix_2019 <- sprintf("%s/04_ttmatrix_rede_2019", pasta_graphhopper)
# pasta_gh_ttmarix_2028 <- sprintf("%s/06_ttmatrix_rede_2028", pasta_graphhopper)
pasta_oportunidades <- sprintf("%s/08_acesso_oportunidades", pasta_dados)
dir.create(pasta_oportunidades, recursive = TRUE, showWarnings = FALSE)


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


# IPEA - Acesso a Oportunidades - Modos Ativos
# https://ipeagit.github.io/aopdata/reference/read_access.html
ipea_aop <- sprintf('%s/aop_access_active_2019_v2.csv', dados_originais)
ipea_aop <- read_delim(ipea_aop, delim = ',', col_types = cols(.default = "c"))
ipea_aop <- ipea_aop %>% filter(abbrev_muni == 'spo') %>% select(-all_of(remove_cols))
# Selecionar colunas de acesso a oportunidades para 30 minutos
ipea_aop <- 
  ipea_aop %>% 
  filter(mode == 'bicycle') %>% 
  select(id_hex,
         # CMA - Cumulative opportunity measure (active)
         # TT - All jobs
         # ST - All healthcare facilities
         # ET - All public schools
         # MT - All school enrollments
         # CT - All Social Assistance Reference Centers (CRAS)
         CMATT30, CMAST30, CMAET30, CMAMT30, CMACT30,
         # CMP - Cumulative opportunity measure (passive)
         # PT - All population
         # PH - Men
         # PM - Women
         # PB - White population
         # PA - Asian-descendent population
         # PI - Indigenous population
         # PN - Back population
         CMPPT30, CMPPH30, CMPPM30, CMPPB30, CMPPA30, CMPPI30, CMPPN30
  )
head(ipea_aop)


# ------------------------------------------------------------------------------
# ttmatrix
# ------------------------------------------------------------------------------

# Matriz de tempos de deslocamento em bicicleta
ttmatrix <- sprintf('%s/ttmatrix_res09_resultados_2019.csv', pasta_gh_ttmarix_2019)
ttmatrix <- read_delim(ttmatrix, delim = ';', col_types = "ccdddd")
ttmatrix <- ttmatrix %>% select(id_hex_x, id_hex_y, time)
head(ttmatrix)


# Adicionar uma linha referente ao próprio hexágono para que as oportunidades
# presentes no hexágono de origem também sejam computadas
tmp <- ttmatrix %>% select(id_hex_x) %>% distinct() %>% mutate(id_hex_y = id_hex_x,
                                                               time     = 1)

ttmatrix <- rbind(ttmatrix, tmp)
rm(tmp)


# ------------------------------------------------------------------------------
# Juntar oportunidades acessadas
# ------------------------------------------------------------------------------

# Oportunidades acessadas a menos de 15 minutos (30 min = 1800 seg)
tempo_max <- 900
# Calcular acesso a oportunidades - IPEA está acessando tempos de 900 a 1050 seg
ttmatrix_15 <- 
  ttmatrix %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex_y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex_x) %>% 
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
ttmatrix_30 <- 
  ttmatrix %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex_y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex_x) %>% 
  summarise(CMATT30 = sum(T001, na.rm = TRUE),
            CMAST30 = sum(S001, na.rm = TRUE),
            CMAET30 = sum(E001, na.rm = TRUE),
            CMAMT30 = sum(M001, na.rm = TRUE),
            CMACT30 = sum(C001, na.rm = TRUE))


# Oportunidades acessadas a menos de 40 minutos (30 min = 1800 seg)
tempo_max <- 2400
# Calcular acesso a oportunidades - IPEA está acessando tempos de 900 a 1050 seg
ttmatrix_40 <- 
  ttmatrix %>% 
  filter(time <= tempo_max) %>%
  # Juntar oportunidades acessadas
  left_join(ipea_oport, by = c('id_hex_y' = 'id_hex')) %>% 
  # Agrupar a partir do hexágono de origem e somar as oportunidades
  group_by(id_hex_x) %>% 
  summarise(CMATT40 = sum(T001, na.rm = TRUE),
            CMAST40 = sum(S001, na.rm = TRUE),
            CMAET40 = sum(E001, na.rm = TRUE),
            CMAMT40 = sum(M001, na.rm = TRUE),
            CMACT40 = sum(C001, na.rm = TRUE))


# Juntar todos os acessos a oportunidades por faixa de tempo
ttmatrix_final <- 
  ttmatrix_15 %>% 
  left_join(ttmatrix_30, by = 'id_hex_x') %>% 
  left_join(ttmatrix_40, by = 'id_hex_x') %>% 
  rename(id_hex = id_hex_x)


# Juntar ao shape de hexágonos
hexagonos <- hexagonos %>% left_join(ttmatrix_final, by = 'id_hex')

# Jogar coluna de geom para a última
hexagonos <- hexagonos %>% select(-geom, geom)

# Substituir NAs por zeros
hexagonos <- hexagonos %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))

head(hexagonos)


# Gravar oportunidades SP
out_shape <- sprintf('%s/hex_agregado_oportunidades_res09_2019.gpkg', pasta_oportunidades)
st_write(hexagonos, out_shape, driver = 'GPKG', append = FALSE)








# hexagonos %>% filter(id_hex == '89a81074447ffff')
# ttmatrix %>% filter(id_hex_x == '89a8100eb47ffff' & id_hex_y == '89a8100c143ffff')
# 
# ipea_aop %>% filter(id_hex == '89a81074447ffff')
# ipea_pop %>% filter(id_hex == '89a81074447ffff')
# ipea_oport %>% filter(id_hex == '89a81074447ffff')









# calcular_oportunidades <- function(id_hex, tempo_max = 900) {
#   # id_hex <- '88a8100c33fffff'; tempo_max = 900
#   
#   oport <- 
#     ttmatrix %>% 
#     # Puxar somente os hexágonos com origem em id_hex
#     filter(id_hex_x == id_hex) %>% 
#     # Filtrar somente os com tempo de acesso a outros hexágonos dentro do limite
#     filter(time <= tempo_max) %>% # 3600, 1800, 900
#     # Adicionar uma linha referente ao próprio hexágono para que as oportunidades
#     # presentes no hexágono de origem também sejam computadas
#     add_row(id_hex_x = id_hex,
#             id_hex_y = id_hex,
#             distance = 1,
#             time     = 1,
#             speed    = 1) %>% 
#     # Juntar as oportunidades
#     left_join(oportunidades, by = c('id_hex_y' = 'id_hex')) %>% 
#     # Agrupar a partir do hexágono de origem e somar as oportunidades
#     group_by(id_hex_x) %>% 
#     summarise(empregos = sum(empregos_total, na.rm = TRUE),
#               saude    = sum(saude_total, na.rm = TRUE),
#               educacao = sum(edu_total, na.rm = TRUE),
#               ass_soc  = sum(cras_total, na.rm = TRUE))
#   
#   
# }
# 
# this <- ttmatrix %>% select(id_hex_x) %>% distinct()
# 
# detach("package:tidylog")
# lala <- lapply(this$id_hex_x, calcular_oportunidades)
# lala <- rbindlist(lala)
# 
# hex_out <- hex_sp %>% select(id_hex, geometry)
# hex_out <- hex_out %>% left_join(lala, by = c('id_hex' = 'id_hex_x')) %>% select(-geometry, geometry)
# 
# file_out <- sprintf('%s/hex_spo_res08_17vizinhos_oportunidades.gpkg', pasta_oportunidades)
# st_write(hex_out, file_out, driver = 'GPKG', append = FALSE)

# ttmatrix %>% 
#   filter(id_hex_x == '88a8100c33fffff') %>% 
#   filter(time <= 900) %>% # 3600, 1800, 900
#   add_row(id_hex_x = '88a8100c33fffff',
#           id_hex_y = '88a8100c33fffff',
#           distance = 0,
#           time     = 0,
#           speed    = 0) %>% 
#   left_join(oportunidades, by = c('id_hex_y' = 'id_hex')) %>% 
#   group_by(id_hex_x) %>% 
#   summarise(empregos = sum(empregos_total, na.rm = TRUE),
#             saude    = sum(saude_total, na.rm = TRUE),
#             educacao = sum(edu_total, na.rm = TRUE),
#             ass_soc  = sum(cras_total, na.rm = TRUE))
