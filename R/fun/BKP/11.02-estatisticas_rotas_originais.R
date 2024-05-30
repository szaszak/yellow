library('tidyverse')
library('tidylog')

# Pasta base de dados
pasta_dados <- sprintf('/Volumes/Expansion/BKP_Ciclocidade_4TB/BKP_Base_GtsRegionais/GitLab/yellow_dados')
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)

# Viagens consideradas no modelo
viagens <- sprintf('%s/10_detours/03_ttmatrix_viagens_originais.csv', pasta_dados)
viagens <- read_delim(viagens, delim = ';', col_types = "cccddddcc")
viagens <- viagens %>% select(trip_id)
head(viagens)


# ------------------------------------------------------------------------------
# Abrir e juntar bases de dados processadas
# ------------------------------------------------------------------------------

# Abrir arquivos processados
base_modelo <- list.files(path = pasta_base_agrup, 
                          pattern = '^\\d{6}_trechos_processados_todos.csv', 
                          recursive = FALSE, 
                          full.names = TRUE)

# arq_trechos_proc <- sprintf('%s/trechos_processados_todos.csv', pasta_base_agrup)
# base_modelo <- read_delim(arq_trechos_proc, delim = ';', col_types = 'cccicidddddddd')
base_modelo <- map_df(base_modelo, read_delim, delim = ';', col_types = 'ccciccdidddddddd')

# Retirar linhas que estão todas como NA (30 viagens cujo processamento falhou)
base_modelo <- base_modelo %>% filter(!is.na(trip_id))

# Puxar somente atributos de interesse
base_modelo <- 
  base_modelo %>% 
  select(trip_id, edges.way_id, qgis_id, elev_grad_rev, 
         linha_sent, elev_sent, qgisid_ext_m) %>% 
  distinct()


# Puxar somente viagens consideradas no modelo
modelo_vgs <- base_modelo %>% filter(trip_id %in% viagens$trip_id)
head(modelo_vgs)


# ------------------------------------------------------------------------------
# Atributos de viário
# ------------------------------------------------------------------------------

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
# TODO: Atenção: algumas linhas da base atrib_viario estavam duplicadas (70) - isso
# só foi percebido depois, quando o modelo e os resultados já estavam gerados! Por
# isso a linha abaixo está comentada, para gerar resultados reproduzíveis, mas o
# correto é eliminar as duplicatas
atrib_viario <- atrib_viario %>% distinct()
head(atrib_viario)


# Abrir arquivos com os marcações de vias expressas
vias_expressas <- sprintf('%s/B1_listagem_vias_expressas.csv', pasta_atrib_viario)
vias_expressas <- read_delim(vias_expressas, delim = ';', col_types = 'cc')
vias_expressas <- vias_expressas %>% mutate(via_expressa = 'sim') %>% select(-name)
head(vias_expressas)


# Abrir arquivos com os marcações de corredores de ônibus
vias_corredores <- sprintf('%s/B2_listagem_vias_corredores_onibus.csv', pasta_atrib_viario)
vias_corredores <- read_delim(vias_corredores, delim = ';', col_types = 'cccc')
vias_corredores <- vias_corredores %>% mutate(via_corredor = 'sim') %>% select(qgis_id, via_corredor)
head(vias_corredores)



# ------------------------------------------------------------------------------
# Unificar atributos de viário
# ------------------------------------------------------------------------------

# Selecionar colunas de interesse em atrib_viario
atrib_viario <- atrib_viario %>% select(osm_id, qgis_id, class_via, 
                                        infra_ciclo, via_restr, osm_highway, 
                                        osm_oneway, osm_lanes, osm_maxspeed)
head(atrib_viario)

# Incluir info de via expressa
atrib_viario <- atrib_viario %>% left_join(vias_expressas, by = 'osm_id') %>% select(-osm_id)

# Incluir info de vias com corredores
atrib_viario <- atrib_viario %>% left_join(vias_corredores, by = 'qgis_id')
head(atrib_viario)

# atrib_viario %>% filter(qgis_id %in% c('271157', '271158', '196042', '211446', '211447')) %>% select(qgis_id, length_m)

# Juntar tudo em modelo_vgs
modelo_vgs <- modelo_vgs %>% left_join(atrib_viario, by = 'qgis_id')
modelo_vgs <- modelo_vgs %>% mutate(infra_ciclo_grouped = ifelse(infra_ciclo == 'sem_infra_ciclo', 'sem_infra_ciclo', 'com_infra_ciclo'))
head(modelo_vgs)


# ------------------------------------------------------------------------------
# Estatísticas das rotas originais (consideradas as usadas no modelo)
# ------------------------------------------------------------------------------

# > names(modelo_vgs)
# [1] "trip_id"             "edges.way_id"        "qgis_id"             "elev_grad_rev"       "linha_sent"         
# [6] "elev_sent"           "qgisid_ext_m"        "class_via"           "infra_ciclo"         "via_restr"          
# [11] "osm_highway"         "osm_oneway"          "osm_lanes"           "osm_maxspeed"        "via_expressa"       
# [16] "via_corredor"        "infra_ciclo_grouped"

# Classificação viária CET
modelo_vgs %>% 
  group_by(class_via) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# class_via                ext prop_grupo prop_total
# <chr>                  <dbl>      <dbl>      <dbl>
# 1 arterial          115941489.      49.5       49.5 
# 2 coletora           72594347.      31         31   
# 3 local              35186419.      15.0       15.0 
# 4 ped_serv           10263535.       4.38       4.38
# 5 vias de pedestres    134918.       0.06       0.06
# 6 vtr                   39501.       0.02       0.02

# Classificação viária CET com infraestrutura cicloviária
modelo_vgs %>% 
  group_by(class_via, infra_ciclo_grouped) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# # Groups:   class_via [6]
# class_via         infra_ciclo_grouped        ext prop_grupo prop_total
# <chr>             <chr>                    <dbl>      <dbl>      <dbl>
# 1 arterial          com_infra_ciclo     70475652.       60.8       30.1 
# 2 arterial          sem_infra_ciclo     45465837.       39.2       19.4 
# 3 coletora          com_infra_ciclo     12749080.       17.6        5.44
# 4 coletora          sem_infra_ciclo     59845267.       82.4       25.6 
# 5 local             com_infra_ciclo      1992683.        5.66       0.85
# 6 local             sem_infra_ciclo     33193737.       94.3       14.2 
# 7 ped_serv          com_infra_ciclo       862538.        8.4        0.37
# 8 ped_serv          sem_infra_ciclo      9400997.       91.6        4.01
# 9 vias de pedestres com_infra_ciclo           39.4       0.03       0   
# 10 vias de pedestres sem_infra_ciclo       134878.      100.         0.06
# 11 vtr               com_infra_ciclo        21821.       55.2        0.01
# 12 vtr               sem_infra_ciclo        17680.       44.8        0.01

# modelo_vgs %>% 
#   filter(osm_highway == 'trunk' | class_via == 'vtr' | class_via == 'arterial') %>% 
#   group_by(class_via, osm_highway) %>% 
#   summarise(ext = sum(qgisid_ext_m)) %>% 
#   # Percentuais no grupo e no total
#   mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
#          prop_total = round(ext / sum(.$ext) * 100, 2)) %>% 
#   # arrange(-prop_total)
#   ungroup() %>% 
#   # slice(1:20)
#   tail(20)

# Relação da classificação CET para arteriais versus OSM trunk, com infraciclo
modelo_vgs %>% 
  # filter(osm_highway == 'trunk') %>% 
  filter(class_via == 'arterial') %>% 
  filter(!osm_highway %in% c('cycleway', 'footway', 'path', 'pedestrian', 'service', 'steps')) %>% 
  group_by(class_via, osm_highway, infra_ciclo_grouped) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2)) %>% 
  head(20)
# # Groups:   class_via, osm_highway [11]
# class_via osm_highway    infra_ciclo_grouped       ext prop_grupo prop_total
# <chr>     <chr>          <chr>                   <dbl>      <dbl>      <dbl>
# 1 arterial  motorway_link  sem_infra_ciclo         2527.     100          0   
# 2 arterial  primary        com_infra_ciclo     28566694.      58.2       25.4 
# 3 arterial  primary        sem_infra_ciclo     20520993.      41.8       18.3 
# 4 arterial  primary_link   com_infra_ciclo          273.       0.06       0   
# 5 arterial  primary_link   sem_infra_ciclo       470149.      99.9        0.42
# 6 arterial  residential    com_infra_ciclo        17987.       1.25       0.02
# 7 arterial  residential    sem_infra_ciclo      1421214.      98.8        1.27
# 8 arterial  secondary      com_infra_ciclo      1014883.      10.3        0.9 
# 9 arterial  secondary      sem_infra_ciclo      8831979.      89.7        7.87
# 10 arterial  secondary_link com_infra_ciclo          123.       0.18       0   
# 11 arterial  secondary_link sem_infra_ciclo        67011.      99.8        0.06
# 12 arterial  tertiary       com_infra_ciclo       472854.      13.4        0.42
# 13 arterial  tertiary       sem_infra_ciclo      3063653.      86.6        2.73
# 14 arterial  tertiary_link  sem_infra_ciclo         9497.     100          0.01
# 15 arterial  trunk          com_infra_ciclo     39412494.      84.1       35.1 
# 16 arterial  trunk          sem_infra_ciclo      7441492.      15.9        6.63
# 17 arterial  trunk_link     com_infra_ciclo        55164.       6.37       0.05
# 18 arterial  trunk_link     sem_infra_ciclo       811204.      93.6        0.72
# 19 arterial  unclassified   sem_infra_ciclo        47581.     100          0.04

# Vias com corredores de ônibus e infraciclo
modelo_vgs %>% 
  group_by(via_corredor, infra_ciclo_grouped) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# # Groups:   via_corredor [2]
# via_corredor infra_ciclo_grouped        ext prop_grupo prop_total
# <chr>        <chr>                    <dbl>      <dbl>      <dbl>
# 1 sim          com_infra_ciclo      20517662.       60.0       8.76
# 2 sim          sem_infra_ciclo      13669584.       40.0       5.84
# 3 NA           com_infra_ciclo      65584151.       32.8      28.0 
# 4 NA           sem_infra_ciclo     134388812.       67.2      57.4 

# Velocidades e infraciclo
modelo_vgs %>% 
  filter(!is.na(osm_maxspeed)) %>% 
  group_by(osm_maxspeed, infra_ciclo_grouped) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# # Groups:   osm_maxspeed [9]
# osm_maxspeed infra_ciclo_grouped       ext prop_grupo prop_total
# <chr>        <chr>                   <dbl>      <dbl>      <dbl>
# 1 10           sem_infra_ciclo        22285.     100          0.02
# 2 20           com_infra_ciclo         1327.       0.3        0   
# 3 20           sem_infra_ciclo       437515.      99.7        0.3 
# 4 30           com_infra_ciclo        57516.       0.53       0.04
# 5 30           sem_infra_ciclo     10879460.      99.5        7.48
# 6 40           com_infra_ciclo      2820055.      12.3        1.94
# 7 40           sem_infra_ciclo     20116177.      87.7       13.8 
# 8 50           com_infra_ciclo     68264745.      63.1       46.9 
# 9 50           sem_infra_ciclo     39859659.      36.9       27.4 
# 10 60           com_infra_ciclo         2147.       0.07       0   
# 11 60           sem_infra_ciclo      3057863.      99.9        2.1 
# 12 70           sem_infra_ciclo         9598.     100          0.01
# 13 80           sem_infra_ciclo          413.     100          0   
# 14 90           sem_infra_ciclo         8988.     100          0.01


# Altimetrias
modelo_vgs %>% 
  mutate(cat_grad = case_when(between(elev_grad_rev, -99, -9.0000000000001) ~ 'desc_ver', # vertiginosa
                              between(elev_grad_rev,  -9, -6.0000000000001) ~ 'desc_for',
                              between(elev_grad_rev,  -6, -4.0000000000001) ~ 'desc_med',
                              between(elev_grad_rev,  -4, -2.0000000000001) ~ 'desc_lev',
                              between(elev_grad_rev,  -2.0000000000000,  2) ~ 'plano',
                              between(elev_grad_rev,   2.0000000000001,  4) ~ 'subi_lev',
                              between(elev_grad_rev,   4.0000000000001,  6) ~ 'subi_med',
                              between(elev_grad_rev,   6.0000000000001,  9) ~ 'subi_for',
                              between(elev_grad_rev,   9.0000000000001, 99) ~ 'subi_ver')) %>%  # vertiginosa
  group_by(cat_grad) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# # A tibble: 10 × 4
# cat_grad        ext prop_grupo prop_total
# <chr>         <dbl>      <dbl>      <dbl>
# 1 desc_for   1175844.       0.5        0.5 
# 2 desc_lev  10772434.       4.6        4.6 
# 3 desc_med   2570694.       1.1        1.1 
# 4 desc_ver    333043.       0.14       0.14
# 5 plano    200718457.      85.7       85.7 
# 6 subi_for   1042954.       0.45       0.45
# 7 subi_lev  10780280.       4.6        4.6 
# 8 subi_med   2544639.       1.09       1.09
# 9 subi_ver    258283.       0.11       0.11
# 10 NA         3963582.       1.69       1.69


# Número de faixas
modelo_vgs %>% 
  filter(!is.na(osm_lanes)) %>% 
  group_by(osm_lanes, infra_ciclo_grouped) %>% 
  summarise(ext = sum(qgisid_ext_m)) %>% 
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))
# # Groups:   osm_lanes [7]
# osm_lanes infra_ciclo_grouped       ext prop_grupo prop_total
# <int> <chr>                   <dbl>      <dbl>      <dbl>
# 1         1 com_infra_ciclo       888882.       18.0       0.54
# 2         1 sem_infra_ciclo      4046622.       82.0       2.46
# 3         2 com_infra_ciclo      6389605.       15.3       3.89
# 4         2 sem_infra_ciclo     35375881.       84.7      21.5 
# 5         3 com_infra_ciclo     37963938.       57.3      23.1 
# 6         3 sem_infra_ciclo     28256244.       42.7      17.2 
# 7         4 com_infra_ciclo     29728810.       66.3      18.1 
# 8         4 sem_infra_ciclo     15109153.       33.7       9.19
# 9         5 com_infra_ciclo      1994378.       52.0       1.21
# 10         5 sem_infra_ciclo      1838119.       48.0       1.12
# 11         6 com_infra_ciclo      1952263.       82.7       1.19
# 12         6 sem_infra_ciclo       407440.       17.3       0.25
# 13         7 com_infra_ciclo       388878.       87.9       0.24
# 14         7 sem_infra_ciclo        53352.       12.1       0.03