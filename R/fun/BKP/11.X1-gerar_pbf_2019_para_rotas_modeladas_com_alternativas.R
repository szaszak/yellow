# Simulação de LTS - Teste 1: Remover vias expressas sem estrutura cicloviária
# do arquivo .pbf para rodar nova ttmatrix

library('tidyverse')
library('tidylog')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_pbf_original <- "../../yellow_src/valhalla_tiles_sp/pbf"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_graphhopper  <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_pbfs      <- sprintf("%s/03_PBFs_SP_rede_2019", pasta_graphhopper)
pasta_orig_modalt  <- sprintf("%s/11_rotas_modeladas_com_alternativas", pasta_dados)
pasta_pbf_modalt   <- sprintf("%s/01_PBFs", pasta_orig_modalt)
dir.create(pasta_pbf_modalt, recursive = TRUE, showWarnings = FALSE)

# Arquivo PBF resultante da edição
pbf_file_out <- '20220216_edit20240418_SP_infraciclo_2019_infraciclo_2019.osm.pbf'
pbf_file_out  <- sprintf('%s/%s', pasta_pbf_modalt, pbf_file_out)


# ------------------------------------------------------------------------------
# Abrir atributos de viário
# ------------------------------------------------------------------------------

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
atrib_viario <- atrib_viario %>% distinct()

# Abrir arquivo com osm_ids de ciclovias expressas em 2019 - a classificação que
# está no dataframe de atributos do viário, acima, contém osm_ids das vias 
# próximas, já que o map matching foi feito no modo 'pedestrian' e ao passar
# por essas vias, o osm_id que ia ser considerado era o do viário
ciclo_expressas <- sprintf('%s/00_atributos_ciclovias_expressas_2019.csv', pasta_gh_pbfs)
ciclo_expressas <- read_delim(ciclo_expressas, delim = ';', col_types = 'c') %>% distinct()

# Abrir o arquivo com osm_ids de ciclovias comuns (não expressas) em 2019
ciclo_comuns <- sprintf('%s/01_atributos_ciclovias_comuns_2019.csv', pasta_gh_pbfs)
ciclo_comuns <- read_delim(ciclo_comuns, delim = ';', col_types = 'c') %>% distinct()

# Abrir o arquivo com osm_ids de vias com ciclofaixa em 2019
ciclo_ciclofx <- sprintf('%s/02_atributos_ciclofaixas_lcn.csv', pasta_gh_pbfs)
ciclo_ciclofx <- read_delim(ciclo_ciclofx, delim = ';', col_types = 'c') %>% distinct()

# Abrir arquivo de trechos de ciclovias comuns em que não há semáforos ou interseções
ciclo_ciclov_semsem <- sprintf('%s/03_atributos_ciclovias_comuns_sem_semaforo.csv', pasta_gh_pbfs)
ciclo_ciclov_semsem <- read_delim(ciclo_ciclov_semsem, delim = ';', col_types = 'c') %>% distinct()


# ----------------------------------------------------------------------------
# Editar arquivo PBF
# ----------------------------------------------------------------------------

# Arquivo .pbf a ser utilizado
osm_file_orig <- sprintf("%s/20220216_sao_paulo_edited_20221223.osm.pbf", pasta_pbf_original)
osm_file_tmp  <- sprintf('%s/lala.opl', pasta_pbf_modalt)

# Converter arquivo .pbf para o formato .opl, que pode ser lido como texto
message('\nConvertendo arquivo .pbf em .opl com o osmium.\n')
osmium_path <- sprintf("/usr/bin/osmium")
arg_o1 <- sprintf('cat "%s"', osm_file_orig)
arg_o2 <- sprintf('--overwrite')
arg_o3 <- sprintf('-o "%s"', osm_file_tmp)
system2(command = osmium_path, args = c(arg_o1, arg_o2, arg_o3))

# Limpar ambiente
rm(arg_o1, arg_o2, arg_o3)


# ------------------------------------------------------------------------------
# Gerar arquivo osm base para edições
# ------------------------------------------------------------------------------

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

# Criar coluna temporária de index, para merge posterior
osm <- osm %>% add_column(index_col = 1:nrow(osm))
head(osm)

# As tags com aspas duplas estão dando erro - remover as aspas
# osm <- osm %>% mutate(tags = str_replace_all(tags, '"', ''))

gc(T)


# ------------------------------------------------------------------------------
# # Separar osm em dois (ways e não ways) e isolar links de viário para edição
# ------------------------------------------------------------------------------

# 1. Somente trechos de vias (w - ways)
osm_edit <- osm %>% filter(str_starts(obj_type, 'w'))
# 2. Somente nodes (n) e relações (r) - https://osmcode.org/opl-file-format/#encoding
osm <- osm %>% filter(!str_starts(obj_type, 'w'))
gc(T)


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


# Puxar somente os links de viário que vamos considerar e mesmo editar
tags_viario <- c('cycleway', 'footway', 'living_street', 
                 'motorway','motorway_link', 'path', 'pedestrian', 
                 'primary', 'primary_link', 'residential', 
                 'secondary', 'secondary_link', 'service', # 'steps',
                 'tertiary', 'tertiary_link', 'track', 
                 'trunk', 'trunk_link'#, 'unclassified'
)

# Isolar somente as linhas de viário que serão editadas (terão as tags alteradas)
osm_edit <- osm_edit %>% filter(tag_highway %in% tags_viario)

# Limpar ambiente
rm(tags_viario)
# head(osm_edit)



# ------------------------------------------------------------------------------
# Preparar base com classificação viária da CET
# ------------------------------------------------------------------------------

# Simplificar dataframe atrib_viario para conter somente classificação viária
class_viaria <- atrib_viario %>% select(osm_id, length_m, class_via, osm_highway, infra_ciclo)
# head(class_viaria)

# # Isolar classificação das infraestruturas cicloviárias
# infra_ciclo <- class_viaria %>% select(osm_id, infra_ciclo) %>% distinct()
# # Testar se ficam com osm_ids repetidos - não ficam
# # infra_ciclo %>% group_by(osm_id) %>% tally() %>% filter(n > 1)

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

# # Juntar infraestrutura cicloviária
# class_viaria <- class_viaria %>% left_join(infra_ciclo, by = 'osm_id') 

# head(class_viaria)

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
  distinct()

# head(class_viaria)


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

# Limpar ambiente
rm(tags_ped_serv, ped_serv_lines)
# head(class_viaria)

# Garantir que ciclovias expressas vão ser lidas como cycleways
# ------------------------------------------------------------------------------

# Atualizar classificação viária para osm_ids que são ciclovias expressas - eles
# precisam ser lidos como 'cycleway' - garantir que isso seja feito
class_viaria <- 
  class_viaria %>% 
  mutate(new_tag = ifelse(osm_id %in% ciclo_expressas$osm_id, 'cycleway', new_tag))


# Garantir que ciclovias comuns vão ser lidas como cycleways
# ------------------------------------------------------------------------------

# class_viaria %>% filter(osm_id %in% ciclo_comuns$osm_id) %>% select(osm_id, new_tag) %>% distinct()

# Atualizar tag_highway de ciclovias para cycleway (inclui ciclovias e calçadas
# partilhadas/compartilhadas)
class_viaria <- 
  class_viaria %>% 
  mutate(new_tag = ifelse(osm_id %in% ciclo_comuns$osm_id, 'cycleway', new_tag))


# Juntar a abertura da tag highway ao atributo atualizado em 'new_tag'
class_viaria <- 
  class_viaria %>% 
  mutate(tag1_highway = str_c('Thighway=', new_tag, sep = '')) %>% 
  # Remover coluna temporária
  select(-new_tag)
# head(class_viaria)

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
# head(osm_edit)


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
  mutate(new_tag = ifelse(osm_id %in% ciclo_comuns$osm_id & new_tag == 'intermediate',
                          'very_bad',
                          new_tag)) %>% 
  # Se forem os trechos sem semáforos e interseções, atualizar como 'intermediate'
  mutate(new_tag = ifelse(osm_id %in% ciclo_ciclov_semsem$osm_id,
                          'intermediate',
                          new_tag))

# # Checar - ciclovias comuns (todas)
# smoothness %>% filter(osm_id %in% ciclo_comuns$osm_id) %>% select(new_tag) %>% distinct()
# # Checar - ciclovias comuns que não estão marcadas como sem semáforos e interseções
# smoothness %>% filter(osm_id %in% ciclo_comuns$osm_id & !osm_id %in% ciclo_ciclov_semsem$osm_id) %>% select(new_tag) %>% distinct()
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
  mutate(new_tag = ifelse(osm_id %in% ciclo_expressas$osm_id, 'excellent', new_tag))

# smoothness %>% filter(osm_id %in% ciclo_expressas$osm_id) %>% select(new_tag) %>% distinct()


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
# head(osm_edit)

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
# osm_oneway %>% filter(osm_id %in% ciclo_comuns$osm_id) %>% select(tag3_surface) %>% distinct()
# osm_oneway %>% filter(osm_id %in% ciclo_expressas$osm_id) %>% select(tag3_surface) %>% distinct()

# Corrigir as tags para ciclovias comuns e expressas - elas vão ter a tag de 
# 'surface' retirada e a tag de 'oneway' atualizada sempre para 'no'
# ATENÇÃO: ao remover as tags de 'surface' e 'oneway' (por exemplo) ciclovias 
# comuns ficam menos atrativas do que vias comuns de mão única, o que é estranho. 
# Por exemplo, a Paulista passa a ser despriorizada frente à Alameda Santos, 
# mesmo com a ciclovia. Por este motivo, a velocidade das cycleways será ajustada
# diretamente no arquivo JSON puxado pelo config-example.yml do GraphHopper. A
# intenção é ter um certo equilíbrio de que as ciclovias passem a ser priorizadas
# frente à própria via em que estão e em relação a eventuais paralelas próximas,
# mas ao mesmo tempo evitar que a velocidade seja aumentada demais e de forma
# desproporcional.
osm_oneway <- 
  osm_oneway %>% 
  # Atualizar para ciclovias comuns
  mutate(tag3_surface = ifelse(osm_id %in% ciclo_comuns$osm_id, 
                               str_replace(tag3_surface, ',surface=compacted,oneway=no|,surface=asphalt,oneway=yes', ',oneway=no'),
                               tag3_surface)) %>% 
  # Atualizar para ciclovias expressas
  mutate(tag3_surface = ifelse(osm_id %in% ciclo_expressas$osm_id, 
                               str_replace(tag3_surface, ',surface=compacted,oneway=no|,surface=asphalt,oneway=yes', ',oneway=no'),
                               tag3_surface))


# Juntar a tag de surface ao dataframe de vias a serem editadas
osm_edit <- osm_edit %>% left_join(osm_oneway, by = 'osm_id')
# osm_edit %>% filter(str_detect(tag3_surface, 'asphalt')) %>% select(tags) %>% sample_n(10)

# Limpar ambiente
rm(osm_oneway, ciclo_comuns, ciclo_expressas)



# ------------------------------------------------------------------------------
# Criar rede local (lcn) de ciclofaixas - marcar osm_ids com tags
# ------------------------------------------------------------------------------

# As redes cicloviárias locais, regionais e nacionais (lcn, rcn e ncn) são lidas
# no OSM como relations (r) e não como ways (w). Aqui, vamos fazer a marcação
# apenas para consultas no mapa. No momento de rodar o GraphHopper, o que vai
# importar é a rede local lcn criada como uma linha de relations (r) a seguir.


# Puxar todos os osm_ids que estão marcados como sendo ciclofaixas
# ciclofaixas <- infra_ciclo %>% filter(infra_ciclo == 'ciclofaixa')
# lcn_ids <- ciclofaixas %>% select(osm_id)

# Criar uma coluna em osm_edit: osm_ids com ciclofaixas pertencem a uma rede
# local (lcn) - para fins de simplificação (e dada a rede cicloviária existente),
# vamos considerar todas as ciclofaixas como bidirecionais. É uma abordagem ok,
# já que nos locais onde as ciclofaixas são unidirecionais, elas sempre são
# acompanhadas por outra unidirecional no sentido oposto da via e isso vai ajudar
# a não ter que ficar selecionando todas uma a uma para ver quais são unidirecionais
osm_edit <-
  osm_edit %>%
  mutate(tag4_lcn = ifelse(osm_id %in% ciclo_ciclofx$osm_id, 
                           ',route=bicycle,network=lcn,lcn=yes,cycleway:right=lane,cycleway:right:oneway=no', 
                           ''))

# # Criar uma coluna com as tags de permissão de bicicleta - queremos 
# # especificamente a tag 'bicycle=no '
# osm_edit <- 
#   osm_edit %>% 
#   # Isolar a tag de highway das demais (highway=motorway, highway=motorway_link,)
#   mutate(tag_bike = str_extract(tags, 'bicycle=[a-zA-Z_]*[,]?'),
#          # Remover o texto "highway=" e a vírgula usada como separador
#          # tag_bike = str_replace(tag_bike, 'highway=', ''),
#          tag_bike = str_replace(tag_bike, ',', ''),
#          .before = 'tags')
# 
# # tag_bike            
# # <chr>               
# #   1 NA                  
# # 2 bicycle=use_sidepath
# # 3 bicycle=yes         
# # 4 bicycle=designated  
# # 5 bicycle=no          
# # 6 bicycle=permissive  
# # 7 bicycle=permit      
# # 8 bicycle=private     
# # 9 bicycle=dismount    
# # 10 bicycle=destination 
# # 11 bicycle=  
# 
# # Demarcar, de acordo com a info original do OSM, onde as bicicletas não podem
# # circular (tag 'bicycle=no'). Isso deve retirar as bicicletas de vias expressas,
# # rodovias, viadutos etc.
# osm_edit <- 
#   osm_edit %>% 
#   mutate(tag4_lcn = ifelse(tag_bike != 'bicycle=no' | is.na(tag_bike) | str_detect(tag4_lcn, 'network=lcn') | str_detect(tag1_highway, 'Thighway=cycleway'),
#                            tag4_lcn,
#                            str_c(tag4_lcn, ',bicycle=no', sep = '')))
# # head(osm_edit)
# 
# # Testar se deu certo nos diferentes cenários
# # osm_edit %>% filter(is.na(tag_bike)) %>% sample_n(20) %>% select(tag_bike, tag4_lcn)
# # osm_edit %>% filter(!is.na(tag_bike)) %>% sample_n(20) %>% select(tag_bike, tag4_lcn)
# # osm_edit %>% filter(tag_bike == 'bicycle=no' & str_detect(tag4_lcn, 'network=lcn')) %>% select(tag4_lcn) %>% 
# #   mutate(tag4_lcn = str_replace(tag4_lcn, 'route=bicycle,network=lcn,lcn=yes,cycleway:right=lane,', '')) %>% 
# #   sample_n(20)



# ------------------------------------------------------------------------------
# Finalizar osm_edit - Agrupar novas tags em uma coluna única
# ------------------------------------------------------------------------------

# Atualizar a coluna de tags original, juntando todas as novas tags
osm_edit <- osm_edit %>% mutate(tags = str_c(tag1_highway, tag2_smoothness, tag3_surface, tag4_lcn, 
                                             sep = ''))

osm_edit <- osm_edit %>% select(-c(tag1_highway, tag2_smoothness, tag3_surface, tag4_lcn))
# head(osm_edit)



# ------------------------------------------------------------------------------
# TRATAMENTO LTS: REMOVER VIAS EXPRESSAS SEM INFRA CICLOVIÁRIA
# ------------------------------------------------------------------------------

# TODO: Por enquanto, as limitações de escolha de rota vão ser feitas direto no
# arquivo JSON puxado pelo config-example.yml do GraphHopper em vez de excluir
# diretamente vias com LTS alto. Se o resultado for bom, manter desta forma; se
# não, a edição ocorre aqui.


# # Estruturas de ciclofaixa estão em ciclo_ciclofx - marcá-las em osm_edit - depois
# # voltaremos a elas
# osm_edit <- osm_edit %>% mutate(ciclo = ifelse(osm_id %in% ciclo_ciclofx$osm_id, 
#                                                'sim', 
#                                                'não'),
#                                 .before = 'class_via')
# # head(osm_edit)
# 
# # Selecionar osm_ids que serão removidos
# remove_tags <- c('motorway', 'motorway_link', 'trunk', 'trunk_link')
# remove_ids <- 
#   osm_edit %>% 
#   filter((class_via == 'vtr' | tag_highway %in% remove_tags) & ciclo == 'não') %>% 
#   select(osm_id) %>% 
#   distinct()
# 
# # Remover osm_ids que vão ficar de fora no teste do LTS
# osm_edit <- osm_edit %>% filter(!osm_id %in% remove_ids$osm_id)
# # head(osm_edit)


# Manter somente as colunas de interesse
osm_edit <- osm_edit %>% select(obj_type, tags, lon, lat, index_col)
# head(osm_edit)

# Limpar ambiente
rm(class_viaria, remove_tags, remove_ids)
gc(T)
# head(osm_edit)



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
lcn <- lcn %>% mutate(tags = 'Tname=Ciclofaixas2019,network=lcn,route=bicycle,type=route')


# Deixar os osm_ids no padrão que será lido como relation (r) - w1002086272@
lcn_ids <- ciclo_ciclofx %>% mutate(lcn_ids = str_c('w', osm_id, '@', sep = '')) %>% select(lcn_ids)

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
rm(lcn, lcn_ids, ciclo_ciclofx)



# ------------------------------------------------------------------------------
# Finalizar dataframe osm - juntar linhas de viário (w) que passaram por edição
# ------------------------------------------------------------------------------

# Para finalizar, juntar ao dataframe osm
osm <- osm %>% rbind(osm_edit) %>% arrange(index_col) %>% select(-index_col)

# Limpar ambiente
# rm(osm_edit)



# ------------------------------------------------------------------------------
# Gravar resultados como .opl e converter para .pbf em seguida
# ------------------------------------------------------------------------------

# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_pbf_modalt)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm, osm_file_out, delim = ' ', col_names = FALSE, na = '')


# Converter arquivo .opl de volta para o formato .pbf, para ser compilado
# pbf_file_out está declarado ao início do script
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


# Para testar, no terminal:
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/11_rotas_modeladas_com_alternativas/PBFs/20220216_edit20240418_SP_infraciclo_2019_infraciclo_2019.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml

# E abrir no navegador:
# http://localhost:8989/

# Testar polyline:
# https://valhalla.github.io/demos/polyline/?unescape=true&polyline6=false#%0A
