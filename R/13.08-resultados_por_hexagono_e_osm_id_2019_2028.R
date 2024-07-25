# 

# carregar bibliotecas
library('tidyverse')
library('tidylog')

# Definir ano de análise e limite máximo de tempo
ano <- '2019'; tempo_max <- '15'
# ano <- '2028'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ano   <- sprintf("%s/%s", pasta_aop_optimum, ano)


# Origens, destinos e quantidade de viagens
ods_vgs <- sprintf('%s/06_lpsolve_resultados_viagens_por_hexagono_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'cci')
head(ods_vgs)

# Osm_ids que faziam parte da ttmatrix utilizada
osm_ids <- sprintf('%s/02_osm_ids_ttmatrix_%s_res09_%smin.csv', pasta_opaop_ano, ano,tempo_max)
osm_ids <- read_delim(osm_ids, delim = ';', col_types = 'cccd')
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
# Quantidade de viagens e extensão percorrida em infra ciclo - por hexágono
# ------------------------------------------------------------------------------

# Qual o número total de viagens geradas por cada hexágono de origem?
# Qual a extensão percorrida em infra cicloviária quando somadas todas as rotas
# que saem de cada hexágono de origem?

# Visão por hexágonos de origem: quanto de distância (em metros) cada hexágono 
# percorreu em determinado ano base?
hex_resultados <- 
  osm_ids %>%
  # Agrupar: extensão percorrida em infra cicloviária para cada par OD
  group_by(orig, dest) %>% 
  summarise(ext_ciclo = sum(ext_percorrida)) %>% 
  # Juntar quantidade de viagens para cada par OD
  left_join(ods_vgs, by = c('orig', 'dest')) %>% 
  # Queremos somente rotas em que houve viagens
  filter(!is.na(viagens)) %>% 
  # Extensão percorrida (em metros) é a quantidade de viagens vezes a extensão 
  # percorrida por rota
  mutate(ext_ciclo_vgs = ext_ciclo * viagens) %>% 
  # Calcular total de viagens em cada hexágono de origem e total da extensão
  # (em metros) percorrida em infra cicloviária a partir daquela origem
  group_by(orig) %>% 
  summarise(viagens_tot     = sum(viagens),
            ext_ciclo_tot_m = sum(ext_ciclo_vgs))

# Calcular percentual de participação do todo
hex_resultados <- 
  hex_resultados %>% 
  mutate(perc_part = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)

# sum(hex_resultados$perc_part)

# Gravar resultados
hex_resultados_out <- sprintf('%s/08_resultados_por_hexagono_%s_res09_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(hex_resultados, hex_resultados_out, delim = ';')


# ------------------------------------------------------------------------------
# Quantidade de viagens e extensão percorrida em infra ciclo - por osm_id
# ------------------------------------------------------------------------------

# Qual o número total de viagens que passam por cada osm_id?
# Quanto cada osm_id é importante? Ou seja, qual a extensão em infra cicloviária 
# percorrida em cada osm_id quando somadas todas as rotas que passam por aquele
# osm_id?

# Visão por hexágonos de origem: quanto de distância (em metros) cada hexágono 
# percorreu em determinado ano base?
osm_id_resultados <- 
  osm_ids %>%
  # Juntar quantidade de viagens para cada par OD
  left_join(ods_vgs, by = c('orig', 'dest')) %>% 
  # Queremos somente rotas em que houve viagens
  filter(!is.na(viagens)) %>% 
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
  mutate(perc_part = ext_ciclo_tot_m / sum(ext_ciclo_tot_m) * 100)

# sum(osm_id_resultados$perc_part)

# Gravar resultados
osm_id_resultados_out <- sprintf('%s/09_resultados_por_osmid_%s_res09_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(osm_id_resultados, osm_id_resultados_out, delim = ';')

