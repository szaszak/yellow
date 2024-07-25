library('tidyverse')
library('tidylog')
library('sf')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
# pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
# pasta_osmids_aopt2 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_teste)
# pasta_rotas_aopt2  <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_teste)

osm_ids_19 <- sprintf('%s/04_osm_way_ids_aop_ciclo_2019.csv', pasta_aoprv_teste)
osm_ids_19 <- read_delim(osm_ids_19, delim = ';', col_types = 'cccdd')
head(osm_ids_19)

osm_ids_28 <- sprintf('%s/08_osm_way_ids_aop_ciclo_2028.csv', pasta_aoprv_teste)
osm_ids_28 <- read_delim(osm_ids_28, delim = ';', col_types = 'cccdd')
head(osm_ids_28)


# ------------------------------------------------------------------------------
# Cálculo por hexágono
# ------------------------------------------------------------------------------

osm_ids_19 <- osm_ids_19 %>% mutate(base_id = str_sub(hex_id_alt, 1, 6), .before = 'hex_id_alt')
resumo1 <- osm_ids_19 %>% group_by(base_id) %>% summarise(ext_ciclo_19 = sum(ext_rev))

osm_ids_28 <- osm_ids_28 %>% mutate(base_id = str_sub(hex_id_alt, 1, 6), .before = 'hex_id_alt')
resumo2 <- osm_ids_28 %>% group_by(base_id) %>% summarise(ext_ciclo_28 = sum(ext_rev))

resumo <- left_join(resumo1, resumo2, by = 'base_id')
resumo <- resumo %>% mutate(dif28_19_km = (ext_ciclo_28 - ext_ciclo_19) / 1000)
resumo

# ------------------------------------------------------------------------------
# Cálculo por osm_id
# ------------------------------------------------------------------------------

resumo_oi1 <- osm_ids_19 %>% group_by(osm_way_id) %>% summarise(ext_ciclo_19 = sum(ext_rev)) %>% ungroup()

resumo_oi2 <- osm_ids_28 %>% group_by(osm_way_id) %>% summarise(ext_ciclo_28 = sum(ext_rev)) %>% ungroup()

resumo_oi <- left_join(resumo_oi2, resumo_oi1, by = 'osm_way_id')
resumo_oi <- resumo_oi %>% mutate(ext_ciclo_19 = ifelse(is.na(ext_ciclo_19), 0, ext_ciclo_19))
resumo_oi <- resumo_oi %>% mutate(dif28_19_m = ext_ciclo_28 - ext_ciclo_19)
head(resumo_oi)