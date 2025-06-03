# Antes de rodar esta parte, é preciso verificar como estão as estruturas
# cicloviárias no PBF original, em especial, quais são os osm_ids das ciclovias,
# ciclovias expressas (Marginal Pinheiros, Radial Leste e Parque Tietê) e das
# vias com ciclofaixas, que serão marcadas aqui como uma rede local (lcn). Juntar
# esses osm_ids em arquivos CSV próprios para serem usados aqui e garantir que
# essas vias serão lidas com estrutura cicloviária - checar no QGIS se está tudo 
# ok. Para simular redes futuras, é basicamente rodar o mesmo script, só que
# atualizando os osm_ids para incluir as vias que teriam as estruturas

# A rede para 2024 foi gerada no QGIS, comparando o mapa OSM que estávamos usando
# e incluindo as estruturas existentes até dezembro de 2024. Foi colocado na
# pasta 14_aop_2024_2028

# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados         <- "../../yellow_dados"
pasta_pbf_original  <- "../../yellow_src/valhalla_tiles_sp/pbf"
pasta_atrib_viario  <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_graphhopper   <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs       <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_aop_2024_2028 <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_redes_24_28   <- sprintf("%s/01_redes_2024_2028", pasta_aop_2024_2028)
dir.create(pasta_redes_24_28, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Consolidação redes cicloviárias 2024 e de referência 2028
# ------------------------------------------------------------------------------

# Vamos precisar das marcações feitas anteriormente para a rede 2019:

# Abrir arquivo com osm_ids de ciclovias expressas em 2019 - a classificação que
# está no dataframe de atributos do viário, acima, contém osm_ids das vias 
# próximas, já que o map matching foi feito no modo 'pedestrian' e ao passar
# por essas vias, o osm_id que ia ser considerado era o do viário
ciclo_expressas <- sprintf('%s/00_atributos_ciclovias_expressas_2019.csv', pasta_gh_pbfs)
ciclo_expressas <- read_delim(ciclo_expressas, delim = ';', col_types = 'c') %>% distinct()

# Abrir o arquivo com osm_ids de ciclovias comuns (não expressas) em 2019
ciclo_comuns <- sprintf('%s/01_atributos_ciclovias_comuns_2019.csv', pasta_gh_pbfs)
ciclo_comuns <- read_delim(ciclo_comuns, delim = ';', col_types = 'c') %>% distinct()

# Abrir arquivo de trechos de ciclovias comuns em que não há semáforos ou interseções
ciclo_ciclov_semsem <- sprintf('%s/03_atributos_ciclovias_comuns_sem_semaforo.csv', pasta_gh_pbfs)
ciclo_ciclov_semsem <- read_delim(ciclo_ciclov_semsem, delim = ';', col_types = 'c') %>% distinct()



# Rede cicloviária 2024 - a rede está marcada na coluna 'rede_cicloviaria_2024'
ciclo_2024 <- sprintf('%s/sao_paulo_osm_filtrado_com_qgis_id_redes_cicloviarias_2019_2024_2028_para_criar_pbfs.gpkg', pasta_redes_24_28)
ciclo_2024 <- read_sf(ciclo_2024)
head(ciclo_2024)

# Ciclovia Paulista: 358376530; Sumaré: 311571202, Faria Lima: 220294302
# ciclo_comuns %>% filter(osm_id %in% c('358376530', '311571202', '220294302'))
# ciclo_2024 %>% filter(osm_id %in% c('358376530', '311571202', '220294302')) %>% st_drop_geometry() %>% select(osm_id, name, matches('2024')) %>% distinct()

# Da rede de 2024, quais são as ciclovias? São as que estão marcadas como 'sim'
# no shape com as marcações de 2024 ou as que aparecem na seleção de osm_ids 
# em 01_atributos_ciclovias_comuns_2019.csv
ciclo_ciclov24 <- 
  ciclo_2024 %>% 
  # Primeiro, manter somente o que pertence à rede 2024
  filter(!is.na(rede_cicloviaria_2024)) %>% 
  filter(ciclovia_2024 == 'sim' | osm_id %in% ciclo_comuns$osm_id) %>%
  # Atualizar marcação de ciclovia ou ciclo_expressa
  mutate(ciclovia_2024 = ifelse(osm_id %in% ciclo_expressas$osm_id, 'ciclo_expressa', 'ciclovia_comum')) %>% 
  mutate(infra_ciclo = ciclovia_2024)

# Testar resultados
# ciclo_ciclov24 %>% filter(osm_id %in% c('358376530', '311571202', '220294302')) %>% st_drop_geometry() %>% select(osm_id, name, matches('2024')) %>% distinct()
# ciclo_ciclov24 %>% filter(osm_id %in% ciclo_comuns$osm_id) %>% mapview()
st_write(ciclo_ciclov24, sprintf('%s/rede_consolidada_ciclovias_2024.gpkg', pasta_redes_24_28), driver = 'GPKG', append = FALSE, delete_layer = TRUE)

# Ciclofaixas 2024 são as que estão marcadas na coluna rede_cicloviaria_2024 e
# que não são ciclovias, conforme definido no chunk acima
ciclo_ciclofx24 <- 
  ciclo_2024 %>% 
  # Primeiro, manter somente o que pertence à rede 2024
  filter(!is.na(rede_cicloviaria_2024)) %>% 
  filter(is.na(ciclovia_2024) & !osm_id %in% ciclo_ciclov24$osm_id) %>% 
  mutate(infra_ciclo = ifelse(is.na(ciclovia_2024), 'ciclofaixa', ciclovia_2024))

# Testar resultados
st_write(ciclo_ciclofx24, sprintf('%s/rede_consolidada_ciclofaixas_2024.gpkg', pasta_redes_24_28), driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# A rede de referência é o que resta da rede demarcada: ele não pertence à rede
# 2024 mas tem a marcação de 'referencia' na coluna rede_cicloviaria
ciclo_2028 <- 
  ciclo_2024 %>% 
  filter(is.na(rede_cicloviaria_2024) & rede_cicloviaria == 'referencia') %>% 
  # Alguns osm_ids, por serem maiores do que os qgis_ids, vão aparecer tanto
  # na rede de referência quanto na rede 2028 - vamos considerar estes casos 
  # como pertencentes à rede 2024, removendo-os da rede de referência
  filter(!osm_id %in% ciclo_ciclov24$osm_id & !osm_id %in% ciclo_ciclofx24$osm_id) %>% 
  mutate(infra_ciclo = ifelse(is.na(ciclovia_2024), 'ciclofaixa', ciclovia_2024))

# No shape de marcação das redes cicloviárias, há osm_ids que têm pequenas partes 
# marcadas como rede de referência quando o restante desses mesmos osm_ids não 
# possuem rede cicloviária. Um exemplo é o osm_id 264984110, que tem 8,06 metros 
# de rede de referência dentre os seus 818,49 metros de  viário. Vamos corrigir 
# essa classificação aqui.
ciclo_2024 %>% filter(osm_id == '264984110')
ciclo_2028 %>% filter(osm_id == '264984110')


# Puxar osm_ids com pelo menos 50% de extensão de rede cicloviária
ciclo_2024_ref_corrigida <- 
  ciclo_2024 %>% 
  st_drop_geometry() %>% 
  # filter(osm_id == '264984110') %>% 
  # filter(osm_id %in% c('264984110', '1001832992', '1001904094', '1001929998')) %>%  
  # Tudo o que for NA vai ser marcado como 'sem rede', enquanto as redes 2024 e
  # de referência vão ser marcadas ambas como de referência
  mutate(rede = ifelse(is.na(rede_cicloviaria), 'sem rede', 'referencia')) %>% 
  group_by(osm_id, rede) %>% 
  summarise(ext = sum(length_m)) %>% 
  mutate(perc = ext / sum(ext) * 100) %>% 
  ungroup() %>% 
  # Manter somente os osm_ids com mais do que 50% de extensão com rede cicloviária
  filter(rede == 'referencia' & perc > 50)

# Corrigir: nos osm_ids com infra cicloviária calculada percorrida pelo routing,
# tudo o que não estiver na rede de referência (2024 + 2028) e que tenha pelo
# menos x% de extensão com rede cicloviária, vai ser remarcado
ciclo_2028 <- 
  ciclo_2028 %>% 
  # filter(osm_id %in% c('264984110', '1001832992', '1001904094', '1001929998')) %>%  
  mutate(rede_cicloviaria = ifelse(osm_id %in% ciclo_2024_ref_corrigida$osm_id, rede_cicloviaria, as.character(NA))) %>% 
  filter(!is.na(rede_cicloviaria))


# Testar resultados
st_write(ciclo_2028, sprintf('%s/rede_consolidada_2028_sem_2024.gpkg', pasta_redes_24_28), driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# Limpar ambiente
rm(ciclo_expressas, ciclo_comuns, ciclo_2024)


# ----------------------------------------------------------------------------
# Editar arquivo PBF
# ----------------------------------------------------------------------------

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici') %>% distinct()


# Arquivo .pbf a ser utilizado
osm_file_orig <- sprintf("%s/20220216_sao_paulo_edited_20221223.osm.pbf", pasta_pbf_original)
osm_file_tmp  <- sprintf('%s/lala.opl', pasta_redes_24_28)

# Converter arquivo .pbf para o formato .opl, que pode ser lido como texto
message('\nConvertendo arquivo .pbf em .opl com o osmium.\n')
osmium_path <- sprintf("/usr/bin/osmium")
arg_o1 <- sprintf('cat "%s"', osm_file_orig)
arg_o2 <- sprintf('--overwrite')
arg_o3 <- sprintf('-o "%s"', osm_file_tmp)
system2(command = osmium_path, args = c(arg_o1, arg_o2, arg_o3))

# Limpar ambiente
rm(arg_o1, arg_o2, arg_o3)


# Gerar arquivo osm base para edições
# ----------------------------------------------------------------------------

# Abrir arquivo .opl para edição, sem linha de header
osm <- read_delim(osm_file_tmp, delim = ' ', col_types = cols(.default = "c"), col_names = FALSE)

# Renomear colunas, para facilitar edição
osm <- osm %>% select(obj_type     = X1, 
                      # obj_version  = X2,
                      # is_visible   = X3,
                      # last_edit    = X4,
                      # time_last_edit = X5,
                      # last_user_id   = X6,
                      # last_user_name = X7,
                      tags = X8, 
                      lon  = X9, 
                      lat  = X10)

gc(T)

# Criar coluna temporária de index, para merge posterior
osm <- osm %>% add_column(index_col = 1:nrow(osm))

# As tags com aspas duplas estão dando erro - remover as aspas
# osm <- osm %>% mutate(tags = str_replace_all(tags, '"', ''))

head(osm)



# Separar dataframe osm em dois (ways e não ways) e começar edições
# ----------------------------------------------------------------------------

# Separar o dataframe osm em dois:
# 1. Somente trechos de vias (w - ways)
osm_edit <- osm %>% filter(str_starts(obj_type, 'w'))
# 2. Somente nodes (n) e relações (r) - https://osmcode.org/opl-file-format/#encoding
osm <- osm %>% filter(!str_starts(obj_type, 'w'))


# Criar coluna de osm_id no dataframe osm_edit
osm_edit <- osm_edit %>% mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type')
# head(osm_edit)

# Criar uma coluna com as tags de highway. Se ficar como NA é porque não é viário
osm_edit <- 
  osm_edit %>% 
  # Isolar a tag de highway das demais (highway=motorway, highway=motorway_link,)
  mutate(tag_highway = str_extract(tags, 'highway=[a-zA-Z_]*[,]?'),
         # Remover o texto "highway=" e a vírgula usada como separador
         tag_highway = str_replace(tag_highway, 'highway=', ''),
         tag_highway = str_replace(tag_highway, ',', ''),
         .before = 'tags')

# head(osm_edit)

# osm_edit %>% select(tag_highway) %>% distinct() %>% arrange(tag_highway) %>% slice(21:33)
# 1 bus_stop              
# 2 construction          
# 3 corridor              
# 4 cycleway              
# 5 elevator              
# 6 emergency_access_point
# 7 emergency_bay         
# 8 footway               
# 9 living_street         
# 10 motorway              
# 11 motorway_link         
# 12 path                  
# 13 pedestrian            
# 14 platform              
# 15 primary               
# 16 primary_link          
# 17 proposed              
# 18 raceway               
# 19 residential           
# 20 rest_area  
# 21 secondary     
# 22 secondary_link
# 23 service       
# 24 services      
# 25 steps         
# 26 tertiary      
# 27 tertiary_link 
# 28 track         
# 29 trunk         
# 30 trunk_link    
# 31 unclassified  
# 32 yes           
# 33 NA   


# Todos os osm_ids das estruturas a serem analisadas estão em atrib_viario?
ciclo_ciclov24 %>% filter(!osm_id %in% atrib_viario$osm_id) %>% nrow()  # OK
ciclo_ciclofx24 %>% filter(!osm_id %in% atrib_viario$osm_id) %>% nrow() # OK
ciclo_2028 %>% filter(!osm_id %in% atrib_viario$osm_id) %>% nrow()      # OK

# Garantir que todos os osm_ids selecionados estejam no mapa osm final
osm_ids_1 <- ciclo_ciclov24 %>% st_drop_geometry() %>% select(osm_id) %>% distinct()
osm_ids_2 <- ciclo_ciclofx24 %>% st_drop_geometry() %>% select(osm_id) %>% distinct()
osm_ids_3 <- ciclo_2028 %>% st_drop_geometry() %>% select(osm_id) %>% distinct()

todos_osm_ids <- rbind(osm_ids_1, osm_ids_2, osm_ids_3) %>% distinct()
rm(osm_ids_1, osm_ids_3) # Vamos precisar do osm_ids_2 ao criar a lcn de 2024

# Puxar somente os links de viário que vamos considerar e editar - os demais vão
# voltar ao dataframe principal
tags_viario <- c('cycleway', 'living_street', 'footway', 
                 'motorway','motorway_link', 'path', 'pedestrian', 
                 'primary', 'primary_link', 'residential', 
                 'secondary', 'secondary_link', 'service', # 'steps',
                 'tertiary', 'tertiary_link', 'track', 
                 'trunk', 'trunk_link', 'unclassified'
                 )

# # Remover as linhas que não usaremos e juntá-las de volta ao dataframe principal
# osm_edit_tmp <- osm_edit %>% filter(!tag_highway %in% tags_viario)
# osm_edit_tmp <- osm_edit_tmp %>% select(-c(osm_id, tag_highway))
# # head(osm_edit_tmp)
# 
# # Fazer a junção dos dataframes e remover o temporário
# osm <- osm %>% rbind(osm_edit_tmp)
# # Limpar ambiente
# rm(osm_edit_tmp)


# Isolar somente as linhas de viário que serão editadas (terão as tags alteradas)
# osm_edit <- osm_edit %>% filter(tag_highway %in% tags_viario)
osm_edit <- osm_edit %>% filter(tag_highway %in% tags_viario | osm_id %in% todos_osm_ids$osm_id)

# Checagem - todos os osm_id que precisamos estão considerados?
# todos_osm_ids %>% filter(!osm_id %in% osm_edit$osm_id)

# O que é 'steps' vão ser removidos de qualquer forma
osm_edit <- osm_edit %>% filter(tag_highway != 'steps')

# Limpar ambiente
rm(tags_viario)
# head(osm_edit)


# # Por qualquer motivo, há 464 (0,32%) linhas de viário que não estão na listagem 
# # de vias com atributos - vamos desconsiderá-las e jogá-las de volta no df principal
# osm_edit_tmp <- osm_edit %>% filter(!osm_id %in% atrib_viario$osm_id)
# osm_edit_tmp <- osm_edit_tmp %>% select(-c(osm_id, tag_highway))
# # head(osm_edit_tmp)
# 
# # Fazer a junção dos dataframes
# osm <- osm %>% rbind(osm_edit_tmp)
# 
# # Limpar ambiente
# rm(osm_edit_tmp)

# Isolar somente as linhas de viário que serão editadas (terão as tags alteradas)
# e que estão na listagem de vias com os atributos
osm_edit <- osm_edit %>% filter(osm_id %in% atrib_viario$osm_id)
# head(osm_edit)


# ------------------------------------------------------------------------------
# Substituir tags de highway pela classificação viária da CET
# ------------------------------------------------------------------------------

# Simplificar dataframe atrib_viario para conter somente classificação viária
class_viaria <- atrib_viario %>% select(osm_id, length_m, class_via, osm_highway)
# head(class_viaria)

# Uma vez que um osm_id é composto por um ou mais qgis_id, puxar classificação 
# viária predominante no trecho - ela vai ser a com maior extensão
class_viaria <- 
  class_viaria %>% 
  group_by(osm_id, class_via) %>% 
  summarise(ext = sum(length_m)) %>% 
  arrange(osm_id, -ext) %>% 
  slice(1) %>% 
  ungroup()

# Remover coluna de extensão
class_viaria <- class_viaria %>% select(-ext)
head(class_viaria)

# class_viaria %>% select(class_via) %>% distinct()
# 1 arterial         
# 2 coletora         
# 3 local            
# 4 ped_serv         
# 5 rodovia          
# 6 vias de pedestres
# 7 vtr 


# Atualizar classificações viárias de acordo com o que será usado no custom model
# do graphhopper
class_viaria <- 
  class_viaria %>% 
  # Atualizar as tags do OSM para corresponder à classificação viária da CET
  mutate(new_tag = case_when(class_via == 'arterial' ~ 'primary',
                             class_via == 'coletora' ~ 'secondary',
                             class_via == 'local'    ~ 'residential',
                             class_via == 'ped_serv' ~ 'pedestrian',
                             class_via == 'rodovia'  ~ 'motorway',
                             class_via == 'vias de pedestres' ~ 'pedestrian',
                             class_via == 'vtr'      ~ 'trunk'
                             )) %>% 
  select(-class_via) %>% 
  distinct()

head(class_viaria)


# ------------------------------------------------------------------------------
# Garantir que vias de pedestres e serviço do OSM não vão ser sobrepostas pela CET
# ------------------------------------------------------------------------------

# Quais são as vias de serviço e de pedestres em osm_edit
tags_ped_serv  <- c('service', 'footway', 'living_street', 'pedestrian', 'track', 'path')
ped_serv_lines <- osm_edit %>% filter(tag_highway %in% tags_ped_serv)

# # Alguns exemplos
# osm %>% filter(obj_type == 'w283003802') # highway=footway
# class_viaria %>% filter(osm_id == '283003802') # residential
# osm %>% filter(obj_type == 'w832672014') # highway=footway
# class_viaria %>% filter(osm_id == '832672014') # primary
# osm_edit %>% filter(obj_type == 'w711425071') # highway=footway
# class_viaria %>% filter(osm_id == '711425071') # residential

# Se trecho era marcado como algo relacionado a pedestres, vamos manter
class_viaria <- 
  class_viaria %>% 
  mutate(new_tag = ifelse(osm_id %in% ped_serv_lines$osm_id, 'pedestrian', new_tag))


# ------------------------------------------------------------------------------
# Garantir que ciclovias expressas vão ser lidas como cycleways
# ------------------------------------------------------------------------------

# Puxar osm_ids ciclovias expressas
ciclo_expressas_rev <- 
  ciclo_ciclov24 %>% 
  st_drop_geometry() %>% 
  filter(ciclovia_2024 == 'ciclo_expressa') %>% 
  select(osm_id) %>% 
  distinct()

# Atualizar classificação viária para osm_ids que são ciclovias expressas - eles
# precisam ser lidos como 'cycleway' - garantir que isso seja feito
class_viaria <- 
  class_viaria %>% 
  mutate(new_tag = ifelse(osm_id %in% ciclo_expressas_rev$osm_id, 'cycleway', new_tag))


# ------------------------------------------------------------------------------
# Garantir que ciclovias comuns vão ser lidas como cycleways
# ------------------------------------------------------------------------------

# Puxar osm_ids ciclovias comuns
ciclo_comuns_rev <- 
  ciclo_ciclov24 %>% 
  st_drop_geometry() %>% 
  filter(ciclovia_2024 == 'ciclovia_comum') %>% 
  select(osm_id) %>% 
  distinct()

# Atualizar tag_highway de ciclovias para cycleway (inclui ciclovias e calçadas
# partilhadas/compartilhadas)
class_viaria <- 
  class_viaria %>% 
  mutate(new_tag = ifelse(osm_id %in% ciclo_comuns_rev$osm_id, 'cycleway', new_tag))


# Juntar a abertura da tag highway ao atributo atualizado
class_viaria <- 
  class_viaria %>% 
  mutate(tag1_highway = str_c('Thighway=', new_tag, sep = '')) %>% 
  select(-new_tag)

# class_viaria %>% select(tag1_highway) %>% distinct()
# 1 Thighway=cycleway   
# 2 Thighway=secondary  
# 3 Thighway=residential
# 4 Thighway=primary    
# 5 Thighway=pedestrian 
# 6 Thighway=motorway   
# 7 Thighway=trunk 

# Juntar a primeira tag, de highway, ao dataframe de vias a serem editadas
osm_edit <- osm_edit %>% left_join(class_viaria, by = 'osm_id')

head(osm_edit)

# Limpar ambiente
rm(class_viaria)


# ------------------------------------------------------------------------------
# Smoothness: Trechos com/sem semáforos, vias restritas e ciclovias expressas
# ------------------------------------------------------------------------------

# No graphhopper, vamos usar a tag de 'smoothness' para marcar vias com e sem
# semáforos, vias restritas (dentro de parques, USP) e como diferencial entre
# as ciclovias comuns e as expressas

# Agrupar por osm_id as informações sobre a existência de semáforos no trecho
smoothness <- 
  atrib_viario %>% 
  group_by(osm_id, semaforos) %>% 
  tally() %>% 
  ungroup() %>% 
  pivot_wider(id_cols = osm_id,
              names_from = semaforos,
              values_from = n)

smoothness <- 
  smoothness %>% 
  mutate(smoothness = case_when(
    # Se há marcação de semáforos no início e no fim, manter como inicio_fim
    !is.na(inicio_fim) ~ 'inicio_fim',
    # Se houver marcação só de início
    is.na(inicio_fim) & !is.na(inicio) ~ 'inicio',
    # Se houver marcação só de fim
    is.na(inicio_fim) & is.na(inicio) & !is.na(fim) ~ 'fim',
    # Se houver marcação de início e fim, mesmo que inicio_fim seja vazio,
    # considerar com semáforos no início e no fim do trecho
    is.na(inicio_fim) & !is.na(inicio) & !is.na(fim) ~ 'inicio_fim',
    TRUE ~ 'sem_semaforos'
  ))


# No graphhopper a marcação de semáforos será lida como "smoothness", atualizar
# no dataframe
smoothness <- 
  smoothness %>% 
  mutate(new_tag = case_when(smoothness == 'fim' ~          'horrible',
                             smoothness == 'inicio_fim' ~   'very_bad',
                             smoothness == 'inicio' ~       'bad',
                             smoothness == 'sem_semaforos'~ 'intermediate'
                             ))


# Isolar colunas de interesse
smoothness <- smoothness %>% select(osm_id, new_tag)


# O shape de semáforos é complexo - o da CET é bem ruim, o do OSM idem. Usamos o
# do OSM e ele não pega direito nas ciclovias - pega bem só no viário, por ser
# um nó do viário. Por isso, olhando o mapa podemos considerar os trechos quase
# todos de ciclovias comuns (não incluindo as expressas) como 'smoothness=very_bad',
# exceto os trechos que olhando no mapa não possuem semáforos ou interseções
# smoothness %>% filter(osm_id %in% ciclo_comuns$osm_id) %>% select(new_tag) %>% distinct()

# Se a estrutura estiver na lista de osm_ids sem semáforos ou interseções, ela
# vai ser marcada como 'intermediate', se não vai manter a tag atual
smoothness <- 
  smoothness %>% 
  # Se forem ciclovias comuns e a tag de smoothness estiver como 'intermediate',
  # atualizar para 'very_bad'
  mutate(new_tag = ifelse(osm_id %in% ciclo_comuns_rev$osm_id & new_tag == 'intermediate',
                          'very_bad',
                          new_tag)) %>% 
  # Se forem os trechos sem semáforos e interseções, atualizar como 'intermediate'
  mutate(new_tag = ifelse(osm_id %in% ciclo_ciclov_semsem$osm_id,
                          'intermediate',
                          new_tag))

# # Checar - ciclovias comuns (todas)
# smoothness %>% filter(osm_id %in% ciclo_expressas$osm_id) %>% select(new_tag) %>% distinct()
# # Checar - ciclovias comuns que não estão marcadas como sem semáforos e interseções
# smoothness %>% filter(osm_id %in% ciclo_expressas$osm_id & !osm_id %in% ciclo_ciclov_semsem$osm_id) %>% select(new_tag) %>% distinct()
# # Checar - ciclovias comuns marcadas como sem semáforos e interseções
# smoothness %>% filter(osm_id %in% ciclo_ciclov_semsem$osm_id) %>% select(new_tag) %>% distinct()


# Vias restritas não possuem semáforos e serão catalogadas como smoothness = good
vias_restritas <- atrib_viario %>% select(osm_id, via_restr)
vias_restritas <- vias_restritas %>% filter(via_restr == 'via_restrita') %>% distinct()
# vias_restritas %>% group_by(osm_id) %>% tally() %>% filter(n > 1)

# Atualizar no dataframe de smoothness
smoothness <- 
  smoothness %>% 
  mutate(new_tag = ifelse(osm_id %in% vias_restritas$osm_id, 'good', new_tag))


# Finalmente, se for uma ciclovia expressa, smoothness será 'excellent'
smoothness <- 
  smoothness %>% 
  mutate(new_tag = ifelse(osm_id %in% ciclo_expressas_rev$osm_id, 'excellent', new_tag))

# smoothness %>% filter(osm_id %in% ciclo_expressas_rev$osm_id) %>% select(new_tag) %>% distinct()


# Checar - Av Paulista
# smoothness %>% filter(osm_id == '358376530')
# Checar - Ciclovia Pinheiros
# smoothness %>% filter(osm_id == '297200000') 
# Checar - Ciclovia Faria Lima
# smoothness %>% filter(osm_id == '220294302') 

# Juntar a abertura da tag smoothness ao atributo atualizado
smoothness <- 
  smoothness %>% 
  mutate(tag2_smoothness = str_c(',smoothness=', new_tag, sep = '')) %>% 
  select(-new_tag)


# Juntar a tag de smoothness ao dataframe de vias a serem editadas
osm_edit <- osm_edit %>% left_join(smoothness, by = 'osm_id')


# osm_edit %>% filter(tag2_smoothness == ',smoothness=excellent') %>% select(tag_highway) %>% distinct()

# Limpar ambiente
rm(smoothness, vias_restritas, ciclo_ciclov_semsem)


# ------------------------------------------------------------------------------
# Surface: vias de mão dupla e de mão única
# ------------------------------------------------------------------------------

# Vamos usar a tag 'surface' para diferenciar no graphhopper vias de mão única
# de vias de mão dupla

# Isolar coluna de de mão dupla/única  -notar que ela se aplica também a osm_ids
# que são cycleways e para os quais essa tag não deverá ser usada
osm_oneway <- atrib_viario %>% select(osm_id, osm_oneway) %>% distinct()

# Mão dupla e única será lido no graphhopper com a marcação de 'surface', sendo
# 'compacted' para vias de mão dupla e 'asphalt' para vias de mão única - ainda
# assim, vamos manter a tag original de 'oneway' para ser considerada no roteamento
osm_oneway <- 
  osm_oneway %>% 
  # Por enquanto, vamos marcar todas as tags onde osm_oneway é 'no' e corrigir
  # as ciclovias a seguir
  mutate(new_tag = ifelse(osm_oneway == 'no', 'compacted,oneway=no', 'asphalt,oneway=yes')) %>% 
  # Juntar a abertura da tag smoothness ao atributo atualizado
  mutate(tag3_surface = str_c(',surface=', new_tag, sep = '')) %>% 
  select(osm_id, tag3_surface)


# Ciclovias não podem ser consideradas 'oneway=yes', garantir que isso não aconteça
# osm_oneway %>% filter(osm_id %in% ciclo_comuns_rev$osm_id) %>% select(tag3_surface) %>% distinct()
# osm_oneway %>% filter(osm_id %in% ciclo_expressas_rev$osm_id) %>% select(tag3_surface) %>% distinct()

# Corrigir as tags para ciclovias comuns e expressas - elas vão ter a tag de 
# 'surface' retirada e a tag de 'oneway' atualizada sempre para 'no'
osm_oneway <- 
  osm_oneway %>% 
  # Atualizar para ciclovias comuns
  mutate(tag3_surface = ifelse(osm_id %in% ciclo_comuns_rev$osm_id, 
                               str_replace(tag3_surface, ',surface=compacted,oneway=no|,surface=asphalt,oneway=yes', ',oneway=no'),
                               tag3_surface)) %>% 
  # Atualizar para ciclovias expressas
  mutate(tag3_surface = ifelse(osm_id %in% ciclo_expressas_rev$osm_id, 
                               str_replace(tag3_surface, ',surface=compacted,oneway=no|,surface=asphalt,oneway=yes', ',oneway=no'),
                               tag3_surface))


# Juntar a tag de surface ao dataframe de vias a serem editadas
osm_edit <- osm_edit %>% left_join(osm_oneway, by = 'osm_id')
# osm_edit %>% filter(str_detect(tag3_surface, 'asphalt')) %>% select(tags) %>% sample_n(10)

# Limpar ambiente
rm(osm_oneway, ciclo_comuns_rev, ciclo_expressas_rev)



# ------------------------------------------------------------------------------
# Criar rede local (lcn) de ciclofaixas - marcar osm_ids com tags
# ------------------------------------------------------------------------------

# As redes cicloviárias locais, regionais e nacionais (lcn, rcn e ncn) são lidas
# no OSM como relations (r) e não como ways (w). Aqui, vamos fazer a marcação
# apenas para consultas no mapa. No momento de rodar o GraphHopper, o que vai
# importar é a rede local lcn criada como uma linha de relations (r) a seguir.

# Criar uma coluna em osm_edit: osm_ids com ciclofaixas pertencem a uma rede
# local (lcn) - para fins de simplificação (e dada a rede cicloviária existente),
# vamos considerar todas as ciclofaixas como bidirecionais. É uma abordagem ok,
# já que nos locais onde as ciclofaixas são unidirecionais, elas sempre são
# acompanhadas por outra unidirecional no sentido oposto da via e isso vai ajudar
# a não ter que ficar selecionando todas uma a uma para ver quais são unidirecionais
osm_edit <-
  osm_edit %>%
  mutate(tag4_lcn = ifelse(osm_id %in% ciclo_ciclofx24$osm_id, 
                           ',route=bicycle,network=lcn,lcn=yes,cycleway:right=lane,cycleway:right:oneway=no', 
                           ''))


# ------------------------------------------------------------------------------
# Finalizar osm_edit - Agrupar novas tags em uma coluna única
# ------------------------------------------------------------------------------

# Atualizar a coluna de tags original, juntando todas as novas tags
osm_edit <- 
  osm_edit %>% 
  mutate(tags = str_c(tag1_highway, tag2_smoothness, tag3_surface, tag4_lcn, sep = '')) %>% 
  # Manter somente as colunas de interesse
  select(obj_type, tags, lon, lat, index_col)

# osm_edit %>% filter(str_detect(tags, 'lcn'))


# ------------------------------------------------------------------------------
# Redes (lcn, rcn, ncn) - Remover lcns existentes e criar rede de ciclofaixas
# ------------------------------------------------------------------------------

# O objetivo aqui é adicionar todos os osm_way_ids que possuem infraestrutura
# de ciclofaixas às redes local, regional ou nacional, para que possam receber
# as alterações de velocidades via essas tags. Como no graphhopper esse atributo
# é lido como uma relação (r - relation) em vez de uma tag de viário (w - way),
# na prática é preciso criar ou alterar uma rede existente para que esses
# trechos sejam considerados de forma diferente no cálculo de velocidade
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/OSMBikeNetworkTagParser.java

# https://wiki.openstreetmap.org/wiki/Cycle_routes
# osm %>% filter(obj_type == 'w323086593') %>% select(tags)
# osm %>% filter(obj_type == 'n8645160887') %>% select(lon)

# Tipicamente, uma lcn é composta pelas colunas de:
# 1. object_type começando com 'r': r5143346
# 2. tags contendo o nome da lcn e as tags network, route e type. Exemplo:
# Tname=Ciclovia%20%Braz%20%Leme,network=lcn,route=bicycle,type=route
# 3. a coluna de lon contendo a concatenação os osm_ids que fazem parte da rede. Exemplo:
# Mw323086590@,w323086594@,w846163664@,w846163665@,w323086591@,w323086593@
# 4. uma coluna de lat que é NA


# Primeiro, vamos puxar somente uma linha com uma lcn para editar (r5143346)
lcn <- osm %>% filter(obj_type == 'r5143346')

# Agora vamos remover todas as linhas de lcn da base osm
osm <- osm %>% filter(!str_detect(tags, 'lcn'))


# Agora, vamos editar a linha isolada de lcn para corresponder à rede de 
# ciclofaixas que queremos criar na cidade
lcn <- lcn %>% mutate(tags = 'Tname=Ciclofaixas2024,network=lcn,route=bicycle,type=route')


# Deixar os osm_ids no padrão que será lido como relation (r) - w1002086272@
lcn_ids <- osm_ids_2 %>% mutate(lcn_ids = str_c('w', osm_id, '@', sep = '')) %>% select(lcn_ids)

# Concatenar todas as linhas como uma única string, separada por vírgula - aqui,
# o objeto deixa de ser um dataframe para virar uma string
# https://stackoverflow.com/questions/13944078/concatenate-rows-of-a-data-frame
lcn_ids <- apply(lcn_ids, 2, paste, collapse = ',')

# Para finalizar, precisamos colocar um 'M' na frente dessa grande string
lcn_ids <- str_c('M', lcn_ids, sep = '')

# Inserir a string como a coluna de lon da nossa lcn
lcn <- lcn %>% mutate(lon = lcn_ids)


# Para finalizar, vamos inserir esta linha de volta ao dataframe osm
osm <- osm %>% rbind(lcn)


# Limpar ambiente
rm(lcn, lcn_ids, osm_ids_2)



# ------------------------------------------------------------------------------
# Finalizar dataframe osm - juntar linhas de viário (w) que passaram por edição
# ------------------------------------------------------------------------------

# Para finalizar, juntar ao dataframe osm
osm <- osm %>% rbind(osm_edit) %>% arrange(index_col) %>% select(-index_col)

# Limpar ambiente
rm(osm_edit)



# ------------------------------------------------------------------------------
# Gravar resultados como .opl e converter para .pbf em seguida
# ------------------------------------------------------------------------------

# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_redes_24_28)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/20220216_sao_paulo_edited_20250304_A_infraciclo_2024.osm.pbf', pasta_redes_24_28)

# Converter arquivo .opl de volta para o formato .pbf, para ser compilado
arg_o4 <- sprintf('cat "%s"', osm_file_out)
arg_o5 <- sprintf('--overwrite')
arg_o6 <- sprintf('-o "%s"', pbf_file_out)
# sprintf('%s %s %s %s', osmium_path, arg_o1, arg_o2, arg_o3)
system2(command = osmium_path, args = c(arg_o4, arg_o5, arg_o6))

# Limpar ambiente
rm(arg_o4, arg_o5, arg_o6)

# Apagar arquivos .opl temporários que haviam sido criados
# file.remove(osm_file_tmp)
file.remove(osm_file_out)
file.remove(osm_file_tmp)
