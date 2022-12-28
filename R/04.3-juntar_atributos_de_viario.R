# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf("%s/04_atributos_viario", pasta_dados)

# -----------------------------------------------------------------------------
# Elevação, extensão e curvatura horizontal
# -----------------------------------------------------------------------------

# Arquivo com elevação por trecho de viário (qgis_id)
open_file1 <- sprintf('%s/A1_listagem_elevacao_por_trecho_viario.csv', pasta_atrib_viario)
elev  <- read_delim(open_file1, delim = ';', col_types = "ccdddddd")

# Isolar colunas de interesse
elev <- elev %>% select(osm_id, qgis_id, length_m, elev_grad_abs)
head(elev)


# -----------------------------------------------------------------------------
# Curvatura horizontal
# -----------------------------------------------------------------------------

# Arquivo com curvatura horizontal por trecho de viário (qgis_id)
open_file2 <- sprintf('%s/A2_listagem_curvatura_por_trecho_viario.csv', pasta_atrib_viario)
curv  <- read_delim(open_file2, delim = ';', col_types = "cd")
head(curv)

# -----------------------------------------------------------------------------
# Quantidade de lotes
# -----------------------------------------------------------------------------

# Arquivo com quantidade de lotes por trecho de viário (qgis_id)
open_file3A <- sprintf('%s/A3_A_listagem_lotes_por_trecho_de_viario.csv', pasta_atrib_viario)
lotesA      <- read_delim(open_file3A, delim = ';', col_types = "cc")

# Agrupar quantidade de lotes por qgis_id
lotesA <- lotesA %>% group_by(qgis_id) %>% summarise(lotes_tot = n())

# Agregar - qtd de lotes para cada 100 metros
lotesA <- 
  lotesA %>% 
  left_join(subset(elev, select = c(qgis_id, length_m)), by = 'qgis_id') %>% 
  mutate(dens_lotes_100m = lotes_tot / length_m * 100) %>% 
  select(-length_m)

head(lotesA)



# Arquivo com quantidade de lotes por trecho de viário (qgis_id) - a 15m de interseções
open_file3B <- sprintf('%s/A3_B_listagem_lotes_por_trecho_de_viario_15m.csv', pasta_atrib_viario)
lotesB      <- read_delim(open_file3B, delim = ';', col_types = "cdc")

# Agrupar quantidade de lotes por qgis_id
lotesB <- lotesB %>% group_by(qgis_id, length_m_15m) %>% summarise(lotes_15m = n())

# Agregar - qtd de lotes para cada 100 metros
lotesB <- lotesB %>% mutate(dens_lotes_100m_15m = lotes_15m / length_m_15m * 100)

# Reordernar colunas
lotesB <- lotesB %>% select(qgis_id, lotes_15m, length_m_15m, dens_lotes_100m_15m)

head(lotesB)


# Arquivo com quantidade de lotes por trecho de viário (qgis_id) - a 15m de interseções
open_file3C <- sprintf('%s/A3_C_listagem_lotes_por_trecho_de_viario_30m.csv', pasta_atrib_viario)
lotesC      <- read_delim(open_file3C, delim = ';', col_types = "cdc")

# Agrupar quantidade de lotes por qgis_id
lotesC <- lotesC %>% group_by(qgis_id, length_m_30m) %>% summarise(lotes_30m = n())

# Agregar - qtd de lotes para cada 100 metros
lotesC <- lotesC %>% mutate(dens_lotes_100m_30m = lotes_30m / length_m_30m * 100)

# Reordernar colunas
lotesC <- lotesC %>% select(qgis_id, lotes_30m, length_m_30m, dens_lotes_100m_30m)

head(lotesC)



# Finalizar dataframe de lotes
lotes <- 
  lotesA %>% 
  left_join(lotesB, by = 'qgis_id') %>% 
  left_join(lotesC, by = 'qgis_id')


head(lotes)
rm(lotesA, lotesB, lotesC)

# -----------------------------------------------------------------------------
# Classificação viária
# -----------------------------------------------------------------------------

# Arquivo classificação viária por trecho de viário (qgis_id)
open_file4   <- sprintf('%s/A4_listagem_tipologia_de_viario_por_trecho.csv', pasta_atrib_viario)
class_viaria <- read_delim(open_file4, delim = ';', col_types = "ccdc")

# Agrupar por qgis_id e cvc_dctipo, somar quantidade de pontos associados a uma
# classificação de viário e deixar somente a classificação com maior número de
# associações. Se houver empate (ex. duas classificações associadas a uma vtr
# e a uma arterial, a escolha será por ordem alfabética): arterial, coletora,
# local, rodovia, via de pedestres, vtr 
class_viaria <- 
  class_viaria %>% 
  group_by(qgis_id, cvc_dctipo) %>% 
  tally() %>% 
  # Filtrar e deixar somente a classificação viária com mais associações
  filter(n == max(n)) %>% 
  ungroup() %>% 
  # Ordenar por cvc_dctipo e manter somente o primeiro
  arrange(qgis_id, cvc_dctipo) %>% 
  distinct(qgis_id, .keep_all = TRUE) %>% 
  # Baixar caixa dos tipos de via e isolar colunas de interesse
  mutate(class_via = tolower(cvc_dctipo)) %>% 
  select(qgis_id, class_via)

head(class_viaria)


# -----------------------------------------------------------------------------
# Infraestrutura cicloviária
# -----------------------------------------------------------------------------

# Tipo de infra cicloviária por trecho de viário (osm_id)
open_file5  <- sprintf('%s/A5_listagem_vias_infra_cicloviaria.csv', pasta_atrib_viario)
infra_ciclo <- read_delim(open_file5, delim = ';', col_types = "cccc")

# Isolar colunas de interesse
infra_ciclo <- infra_ciclo %>% select(osm_id, infra_ciclo = tipo_2018)
head(infra_ciclo)

# infra_ciclo %>% select(infra_ciclo) %>% distinct()


# -----------------------------------------------------------------------------
# Vias restritas
# -----------------------------------------------------------------------------

# Arquivo com listagem de vias que estão em áreas com restrição de circulação
# por veículos motorizados (osm_id)
open_file6 <- sprintf('%s/A6_listagem_vias_em_areas_restritas.csv', pasta_atrib_viario)
vias_restr <- read_delim(open_file6, delim = ';', col_types = "cc")

# Criar marcação de via restrita, descartar coluna de nome
vias_restr <- vias_restr %>% mutate(via_restr = 'via restrita') %>% select(-name)
head(vias_restr)

# # Os arquivos das vias na USP e em parques já estão contemplados pelo anterior
# open_file6 <- sprintf('%s/listagem_vias_campus_usp.csv', pasta_atrib_viario)
# open_file6 <- sprintf('%s/listagem_vias_em_parques.csv', pasta_atrib_viario)
# vias_restr2 <- read_delim(open_file6, delim = ';', col_types = "cc")
# vias_restr2 %>% filter(!osm_id %in% vias_restr$osm_id)



# -----------------------------------------------------------------------------
# Juntar tudo e exportar
# -----------------------------------------------------------------------------

atrib_viario <- 
  elev %>% 
  left_join(curv, by = 'qgis_id') %>% 
  left_join(lotes, by = 'qgis_id') %>% 
  left_join(class_viaria, by = 'qgis_id') %>%
  left_join(infra_ciclo, by = 'osm_id') %>%
  left_join(vias_restr, by = 'osm_id')

# Substituir NAs por zeros nas colunas de lotes
atrib_viario <- atrib_viario %>% mutate(across(matches('lotes'), ~replace_na(.x, 0)))


head(atrib_viario)

# tail(atrib_viario)

# Salvar arquivo .csv
out_file <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
write_delim(atrib_viario, out_file, delim = ';')
