library('tidyverse')
library('tidylog')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_detours      <- sprintf('%s/10_detours', pasta_dados)

# ------------------------------------------------------------------------------
# Abrir - viagens originais consideradas, map matching e atributos de viário
# ------------------------------------------------------------------------------

# Abrir viagens com latlon originais de origem e destino, já filtradas com
# somente as viagens a serem consideradas - queremos os trip-ids delas
vgs_detour <- sprintf('%s/04_viagens_com_distancias_e_detours.csv', pasta_detours)
vgs_detour <- read_delim(vgs_detour, delim = ';', col_types = 'cccdddd')
vgs_detour <- vgs_detour %>% select(trip_id, detour_mm, detour_gh)
head(vgs_detour)


# Abrir arquivos de viagens que foram consideradas nos modelos - aqui, vamos
# usar esta base só porque o que precisamos é o trip_id e a distância total
# da viagem, que constam nela e podem ser acessadas com distinct()
base_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')
base_modelo <- base_modelo %>% select(trip_id, dist_mapmatch = dist_total) %>% distinct()
head(base_modelo)


# Abrir arquivos processados agrupados, resultados do map matching (tem todas
# as viagens, com todos os trechos percorridos - precisa juntar com atributos)
result_mm <- list.files(path = pasta_base_agrup, 
                        pattern = '^\\d{6}_trechos_processados_todos.csv', 
                        recursive = FALSE, 
                        full.names = TRUE)

result_mm <- map_df(result_mm, read_delim, delim = ';', col_types = 'ccciccdidddddddd')
result_mm <- result_mm %>% select(trip_id, qgis_id, edges.length, qgisid_ext_m)
head(result_mm)

# Manter somente viagens originais consideradas (sem divisão, com distâncias 
# maior que zero, com distância em linha reta maior do que 200m, sem outliers
# extremos para fatores de detour nas rotas do map matching e modeladas)
result_mm <- result_mm %>% filter(trip_id %in% vgs_detour$trip_id)

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici') %>% distinct()
atrib_viario <- atrib_viario %>% select(qgis_id, 
                                        length_m, 
                                        elev_grad_sent_linha,
                                        semaforos,
                                        dens_lotes_100m,
                                        class_via,
                                        via_restr,
                                        infra_ciclo)
head(atrib_viario)

# Juntar atributos de viário aos trechos das rotas consideradas
result_mm <- result_mm %>% left_join(atrib_viario, by = 'qgis_id')
head(result_mm)

# ------------------------------------------------------------------------------
# Análises das rotas originais
# ------------------------------------------------------------------------------

# Quantidade de viagens
result_mm %>% select(trip_id) %>% distinct() %>% nrow()
# 118.367

# Criar grupo de uso de estruturas cicloviárias para cálculos
grupo_infraciclo <- 
  result_mm %>% 
  group_by(trip_id, infra_ciclo) %>% 
  summarise(ext_perc_length_m = sum(length_m),
            ext_perc_edges_len = sum(edges.length)) %>% 
  ungroup()

# Qual o percentual de viagens que usou algum tipo de infra cicloviária?
grupo_infraciclo %>% 
  filter(infra_ciclo != 'sem_infra_ciclo') %>% 
  select(trip_id) %>% 
  distinct() %>% 
  nrow()
# 75.054 (63.40788% do total)


# De viagens percorridas com infracicloviária, qual o fatour de detour?
grupo_infraciclo %>% 
  filter(infra_ciclo != 'sem_infra_ciclo') %>% 
  select(trip_id) %>% 
  distinct() %>% 
  left_join(vgs_detour, by = 'trip_id') %>% 
  select(detour_mm, detour_gh) %>% 
  summary()
# detour_mm        detour_gh     
# Min.   :0.7348   Min.   :0.8496  
# 1st Qu.:1.1518   1st Qu.:1.1746  
# Median :1.3011   Median :1.3133  
# Mean   :1.3597   Mean   :1.3799  
# 3rd Qu.:1.4832   3rd Qu.:1.4966  
# Max.   :2.6774   Max.   :2.7922 


# De quem usou estruturas cicloviárias, qual o percentual da viagem percorrido
# nessas estruturas
grupo_infraciclo %>% 
  filter(infra_ciclo != 'sem_infra_ciclo') %>% 
  group_by(trip_id) %>% 
  summarise(ext_perc_length_m  = sum(ext_perc_length_m),
            ext_perc_edges_len = sum(ext_perc_edges_len)) %>% 
  ungroup() %>% 
  left_join(base_modelo, by = 'trip_id') %>% 
  mutate(perc_length_m  = ext_perc_length_m  / dist_mapmatch * 100,
         perc_edges_len = ext_perc_edges_len / dist_mapmatch * 100) %>% 
  select(perc_length_m, perc_edges_len) %>% 
  summary()
# perc_length_m       perc_edges_len  
# Min.   :  0.05152   Min.   :  0.00  
# 1st Qu.: 23.54566   1st Qu.: 22.68  
# Median : 51.17531   Median : 50.98  
# Mean   : 51.11835   Mean   : 50.39  
# 3rd Qu.: 77.60542   3rd Qu.: 77.17  
# Max.   :310.03618   Max.   :891.61 