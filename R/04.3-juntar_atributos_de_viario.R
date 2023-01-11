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
elev  <- read_delim(open_file1, delim = ';', col_types = "ccddddddddddd")

# Isolar colunas de interesse - aqui temos a coluna elev_grad que se refere
# ao gradiente do viário no sentido do traçado da linha no QGIS. Ao associar
# com os resultados do map matching, esse sentido terá de ser considerado para
# avaliar se o sinal é o mesmo ou se será invertido
elev <- elev %>% select(osm_id, qgis_id, length_m, elev_grad_sent_linha = elev_grad)
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
class_viaria <- read_delim(open_file4, delim = ';', col_types = "ccci")


# # Quais osm_id não possuem nenhuma classificação viária? - Após checagem manual
# # desses ids no QGIS, a vasta maioria é de vias de serviço, dentro de parques,
# # na USP, dentro de lotes etc. Podemos classificar geral como vias de serviço
# cv_null <- class_viaria %>% filter(is.na(cvc_dctipo)) %>% select(osm_id) %>% distinct()
# cv_non_null <- class_viaria %>% filter(!is.na(cvc_dctipo)) %>% select(osm_id) %>% distinct()
# 
# cv_null_null <- cv_null %>% filter(osm_id %nin% cv_non_null$osm_id) %>% distinct()
# 
# # Guardar resultados para serem atualizados em seguida como vias de serviço
# class_viaria_null <- class_viaria %>% filter(osm_id %nin% cv_non_null$osm_id) %>% select(qgis_id) %>% distinct()
# 
# # Salvar arquivo .csv
# out_file <- sprintf('%s/AX_listagem_viario_sem_tipologia_atribuida.csv', pasta_atrib_viario)
# write_delim(cv_null_null, out_file, delim = ';')
# 
# rm(cv_null, cv_non_null, cv_null_null, out_file)


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


# # Quantos NA ainda temos no dataframe?
# colSums(is.na(class_viaria)) # 34442
# 
# # Classificar os ids que estão sem atribuição como vias de serviço
# class_viaria <- 
#   class_viaria %>% 
#   mutate(class_via = ifelse(qgis_id %in% class_viaria_null$qgis_id, 'serviço', class_via))
# 
# # Quantos NA ainda temos no dataframe?
# colSums(is.na(class_viaria)) # 15965
# 
# # Salvar arquivo .csv
# export <- class_viaria %>% filter(is.na(class_via)) %>% select(qgis_id) %>% distinct()
# out_file <- sprintf('%s/AY_listagem_viario_sem_tipologia_atribuida2.csv', pasta_atrib_viario)
# write_delim(export, out_file, delim = ';')


# Após checagem geral, podemos atribuir todos os qgis_id que estão sem 
# classificação viária atribuíada com o termo genérico "ped_serv", referente a
# vias de serviço ou vias de pedestres. Alguns viários como a ciclovia Pinheiros
# vão ficar com essa atribuição, e tudo bem - isso porque vamos depois definir
# essa ciclovia como "expressa" e vai ser bom tê-la com outra classificação de
# viário que não arterial ou coletora, já que ela é isolada dos veículos
class_viaria <-
  class_viaria %>%
  mutate(class_via = ifelse(is.na(class_via), 'ped_serv', class_via))

# Quantos NA ainda temos no dataframe?
colSums(is.na(class_viaria))

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
# Tags OSM
# -----------------------------------------------------------------------------

# Arquivo com listagem atributos do OSM referentes às vias (osm_id)
open_file7 <- sprintf('%s/A7_listagem_tags_osm_de_viario.csv', pasta_atrib_viario)
osm_tags <- read_delim(open_file7, delim = ';', col_types = "ccici")

# Renomear colunas para ficar claro que são tags do OSM
osm_tags <-  osm_tags %>% rename(osm_oneway = oneway,
                                 osm_lanes  = lanes,
                                 osm_surface  = surface,
                                 osm_maxspeed = maxspeed)

head(osm_tags)


# -----------------------------------------------------------------------------
# Juntar tudo, substituir NAs que ficaram e exportar
# -----------------------------------------------------------------------------

atrib_viario <- 
  elev %>% 
  left_join(curv, by = 'qgis_id') %>% 
  left_join(lotes, by = 'qgis_id') %>% 
  left_join(class_viaria, by = 'qgis_id') %>%
  left_join(infra_ciclo, by = 'osm_id') %>%
  left_join(vias_restr, by = 'osm_id') %>% 
  left_join(osm_tags, by = 'osm_id')


# Substituir NAs por zeros nas colunas de lotes
atrib_viario <- atrib_viario %>% mutate(across(matches('lotes'), ~replace_na(.x, 0)))

# Substituir NAs por zeros nas colunas de length_m de lotes
atrib_viario <- atrib_viario %>% mutate(across(matches('length_m_'), ~replace_na(.x, 0)))

# Substituir NAs nas colunas de infraestrutura cicloviária
atrib_viario <- atrib_viario %>% mutate(infra_ciclo = ifelse(is.na(infra_ciclo), 'sem_infra_ciclo', infra_ciclo))

# Substituir NAs nas colunas de vias restritas
atrib_viario <- atrib_viario %>% mutate(via_restr = ifelse(is.na(via_restr), 'via_comum', via_restr))

# Substituir NAs nas colunas de osm_oneway - assume-se que o que está como NA
# corresponde a vias que não são se mão única
atrib_viario <- atrib_viario %>% mutate(osm_oneway = ifelse(is.na(osm_oneway), 'no', osm_oneway))

# Quantos NA ainda temos no dataframe?
colSums(is.na(atrib_viario))

head(atrib_viario)
# tail(atrib_viario)

# Salvar arquivo .csv
out_file <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
write_delim(atrib_viario, out_file, delim = ';')
