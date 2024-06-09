library('tidyverse')
library('tidylog')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)



# Abrir arquivos processados
# base_modelo <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
viagens <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
viagens <- read_delim(viagens, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')
viagens <- viagens %>% select(trip_id, qgis_id, dia_util, infra_ciclo, class_via, via_restr, cat_grad, dist_trecho_quadra, dist_total)
# names(viagens)
head(viagens)

# Qual o percentual de viagens que usou algum tipo de infra cicloviária?
viagens %>% select(infra_ciclo) %>% distinct()

# Quais os ids das viagens que usaram infra cicloviária
vgs_com_infraciclo <- 
  viagens %>% 
  group_by(trip_id, infra_ciclo) %>% 
  tally() %>% 
  ungroup() %>% 
  filter(infra_ciclo != 'sem_infra_ciclo') %>% 
  select(trip_id) %>% 
  distinct()

vgs_com_infraciclo <- viagens %>% filter(trip_id %in% vgs_com_infraciclo$trip_id)
n_vgs_com_infraciclo <- vgs_com_infraciclo %>% select(trip_id) %>% distinct() %>% nrow()

vgs_sem_infraciclo <- viagens %>% filter(!trip_id %in% vgs_com_infraciclo$trip_id)
n_vgs_sem_infraciclo <- vgs_sem_infraciclo %>% select(trip_id) %>% distinct() %>% nrow()

n_vgs_com_infraciclo / (n_vgs_com_infraciclo + n_vgs_sem_infraciclo) * 100

viagens %>% 
  filter(dia_util == 'util') %>% 
  group_by(class_via, infra_ciclo) %>% 
  summarise(ext = sum(dist_trecho_quadra))
  # Calcular proporcionais para o grupo e do todo


viagens %>% filter(trip_id == '000138_00') %>% select(qgis_id) %>% distinct() %>% pull() %>% paste(collapse = "', '")


# Abrir viagens que vieram do map matching
vgs2 <- sprintf('%s/201811_trechos_processados_todos.csv', pasta_base_agrup)
vgs2 <- read_delim(vgs2, delim = ';', col_types = 'ccciccdidddddddd')
vgs2 <- vgs2 %>% select(trip_id, qgis_id, cluster, qgisid_ext_m)
head(vgs2)


# Quais os últimos qgis_id de cada viagem
this <- 
  vgs2 %>% 
  group_by(trip_id) %>% 
  summarise(cluster = max(cluster)) %>% 
  ungroup() %>% 
  left_join(vgs2, by = c('trip_id', 'cluster'))

# Juntar os primeiros qgis_ids de cada viagem aos seus últimos
vgs2 <- 
  vgs2 %>% 
  filter(cluster == 1) %>%
  rbind(this) %>% 
  arrange(trip_id, cluster)

# TODO: pegar os centróides dessa viagem e calcular rota com as origens e destinos via graphhopper

vgs2 %>% filter(trip_id == '000138_00') %>% select(qgis_id) %>% distinct() %>% pull() %>% paste(collapse = "', '")
