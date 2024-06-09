# Edita atributos (tags) da área de testes para ser usada no GraphHopper

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_testes   <- sprintf("%s/01_testes", pasta_graphhopper)
# dir.create(pasta_gh_testes, recursive = TRUE, showWarnings = FALSE)

# Arquivo .pbf a ser utilizado
osm_file_orig <- sprintf("%s/entorno_praca.pbf", pasta_gh_testes)
osm_file_tmp  <- sprintf('%s/lala.opl', pasta_gh_testes)

# Converter arquivo .pbf para o formato .opl, que pode ser lido como texto
message('\nConvertendo arquivo .pbf em .opl com o osmium.\n')
osmium_path <- sprintf("/usr/bin/osmium")
arg_o1 <- sprintf('cat "%s"', osm_file_orig)
arg_o2 <- sprintf('--overwrite')
arg_o3 <- sprintf('-o "%s"', osm_file_tmp)
system2(command = osmium_path, args = c(arg_o1, arg_o2, arg_o3))

# Limpar ambiente
rm(arg_o1, arg_o2, arg_o3)


# ----------------------------------------------------------------------------
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

# Criar coluna temporária de index, para merge posterior
osm <- osm %>% add_column(index_col = 1:nrow(osm))

# As tags com aspas duplas estão dando erro - remover as aspas
# osm <- osm %>% mutate(tags = str_replace_all(tags, '"', ''))


# ----------------------------------------------------------------------------
# Ver arquivos de referência
# ----------------------------------------------------------------------------

# BikeCommonAverageSpeedParser.java
# BikeCommonPriorityParser.java
# 
# DefaultWeightingFactory.java
# PriorityWeughting.java

# [PASSOU POR EDIÇÃO] - Mudar custom profile para bike, apontar para novo perfil
# criado em json, habilitar graph.encoded_values (para conter surface, toll e trach_type), 
# habilitar altimetrias
# /graphhopper/config-example.yml

# Arquivos principais a serem modificados
# [PASSOU POR EDIÇÃO] - valores de velocidade todos agora apontam para o BASE_SPEED
# criado, de 10.0. O valor correto do modelo seria 10.241, mas em algum lugar, este
# valor é arredondado para 10 - a correção vai se dar no custom model, adicionando
# os 0.2408 restantes (ver abaixo)
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/BikeCommonAverageSpeedParser.java
# [PASSOU POR EDIÇÃO] - valores de weightToPrioMap mudados para weightToPrioMap.put(100d, UNCHANGED.getValue());
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/BikeCommonPriorityParser.java
# [NÃO PASSOU POR EDIÇÃO]
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/BikePriorityParser.java

# [NÃO PASSOU POR EDIÇÃO]
# PriorityCode - códigos de preferência de rota, influenciam o priority - é neste arquivo
# que é feita a conta do "value" resultante / 10 para gerar um fator de priority
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/PriorityCode.java

# Possui a lista de avoidHighwayTags
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/BikePriorityParser.java

# [NÃO PASSARAM POR EDIÇÃO]
# TagParser - possui a seleção de tags marcadas como INTENDED e ONEWAYS
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/AbstractAverageSpeedParser.java
# Possui a seleção de tags marcadas como FERRIES
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/AbstractAccessParser.java

# Mudei o DEFAULT_HEADING_PENALTY de 300 para 0 no arquivo:
# /graphhopper/web-api/src/main/java/com/graphhopper/util/Parameters.java


# [PASSOU POR EDIÇÃO]
# Definição da função multiply_by
# /graphhopper/web-api/src/main/java/com/graphhopper/json/Statement.java
# Mudar linhas
# return "value *= " + value;
# return "value += " + value;

# return new MinMax(minMax1.min * minMax2.min, minMax1.max * minMax2.max);
# return new MinMax(minMax1.min + minMax2.min, minMax1.max + minMax2.max);

# Base speed: 7
# road_class: [ ped_serv ]
# 

{
  "speed": [
    {
      "if": "road_environment == ROAD || road_environment == BRIDGE || road_environment == TUNNEL",
      "multiply_by": "1.2621"
    },
    {
      "if": "road_class == PEDESTRIAN",
      "multiply_by": "0"
    },
    {
      "if": "road_class == RESIDENTIAL || road_class == TERTIARY",
      "multiply_by": "0.6637"
    },
    {
      "if": "road_class == SECONDARY",
      "multiply_by": "1.0425"
    },
    {
      "if": "road_class == PRIMARY",
      "multiply_by": "0.9589"
    },
    {
      "if": "road_class == CYCLEWAY",
      "multiply_by": "0.3394"
    },
    {
      "if": "bike_network == LOCAL",
      "multiply_by": "0.3325"
    },
    {
      "if": "smoothness == HORRIBLE",
      "multiply_by": "0"
    },
    {
      "if": "smoothness == VERY_BAD",
      "multiply_by": "0.0341"
    },
    {
      "if": "smoothness == BAD",
      "multiply_by": "0.1835"
    },
    {
      "if": "smoothness == INTERMEDIATE",
      "multiply_by": "0.2981"
    },
    {
      "if": "smoothness == GOOD",
      "multiply_by": "0.7907"
    },
    {
      "if": "smoothness == EXCELLENT",
      "multiply_by": "2.5258"
    },
    {
      "if": "surface == COMPACTED",
      "multiply_by": "0"
    },
    {
      "if": "surface == ASPHALT",
      "multiply_by": "0.1664"
    },
    {
      "if": "average_slope >= 6",
      "multiply_by": "0"
    },
    {
      "if": "average_slope >= 4 && average_slope < 6",
      "multiply_by": "1.2933"
    },
    {
      "if": "average_slope >= 2 && average_slope < 4",
      "multiply_by": "1.5643"
    },
    {
      "if": "average_slope >= 0 && average_slope < 2",
      "multiply_by": "2.9149"
    },
    {
      "if": "average_slope >= -2 && average_slope < 0",
      "multiply_by": "3.5982"
    },
    {
      "if": "average_slope >= -4 && average_slope < -2",
      "multiply_by": "3.6272"
    },
    {
      "if": "average_slope >= -6 && average_slope < -4",
      "multiply_by": "4.4806"
    },
    {
      "if": "average_slope < -6",
      "multiply_by": "4.5495"
    }
  ]
}


# Lista de road_class aceitas:
# /graphhopper/core/src/main/java/com/graphhopper/routing/ev/RoadClass.java
# OTHER("other"), MOTORWAY("motorway"),
# TRUNK("trunk"), PRIMARY("primary"), SECONDARY("secondary"),
# TERTIARY("tertiary"), RESIDENTIAL("residential"), UNCLASSIFIED("unclassified"),
# SERVICE("service"), ROAD("road"), TRACK("track"),
# BRIDLEWAY("bridleway"), STEPS("steps"), CYCLEWAY("cycleway"),
# PATH("path"), LIVING_STREET("living_street"), FOOTWAY("footway"),
# PEDESTRIAN("pedestrian"), PLATFORM("platform"), CORRIDOR("corridor");




# cd graphhopper/ && ../apache-maven-3.8.7/bin/mvn clean install -DskipTests && cd -
# clear && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca_editado_0_todas_vias_residential.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml

# clear && grep -RIlnw graphhopper/ -e "PriorityCode"


# ----------------------------------------------------------------------------
# Teste 0 - deixar todas as vias do entorno da praça como 'residential'
# ----------------------------------------------------------------------------

# Criar um dataframe próprio para fazer as edições
ids_to_edit <- c('277290438', '595630234', '172869010', '39046029', '39046028', '436649363', '59515635',
                 '59515643' # Trecho da semaneiros, para tirar tags de ciclovias
)

# Alterar informações de velocidades somente nas vias de interesse
osm_edit1 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit)

osm_edit1 <-
  osm_edit1 %>%
  # Limpar todas as tags, depois inserir as de highway=residential
  mutate(tags_new = 'Thighway=residential'
  ) %>%
  select(index_col, tags_new)

# Puxar as demais vias, para serem agrupadas com as editadas e formar novamente o .pbf
osm_edit2 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # # relações (r) - https://osmcode.org/opl-file-format/#encoding
  # filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>%
  select(-obj_type) %>%
  filter(!osm_id %in% ids_to_edit) %>% 
  select(index_col, tags_new = tags)

# Juntar os dois dataframes temporários antes de atualizar ao original
osm_edit <- rbind(osm_edit1, osm_edit2)


# Gerar dataframe com velocidades alteradas para ser usado nos testes
osm_out <- 
  osm %>% 
  left_join(osm_edit, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% 
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_0_todas_vias_residential.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml




# ----------------------------------------------------------------------------
# Teste 1 - criar tags de 'arterial' e 'coletora' nas vias no entorno do parque
# ----------------------------------------------------------------------------

# RESULTADO: O MAPA NÃO RECONHECE AS TAGS COMO VÁLIDAS PARA TRAÇAR ROTA

# graphhopper/core/src/main/java/com/graphhopper/routing/ev/RoadClass.java

# Criar um dataframe próprio para fazer as edições - vias no entorno do parque
ids_to_edit_coletoras <- c('277290438', '595630234', '172869010', '39046029', '39046028', '436649363', '59515635')

# Criar um dataframe próprio para fazer as edições - Av. dos Semaneiros
ids_to_edit_arteriais <- c('59515643', '490208499', '490208501')

# Isolar as vias a serem editadas (coletoras)
osm_edit1 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit_coletoras)

# Limpar todas as tags, depois inserir as de highway=coletora
osm_edit1 <- osm_edit1 %>% mutate(tags_new = 'Thighway=coletora') %>% select(index_col, tags_new)


# Isolar as vias a serem editadas (arteriais)
osm_edit2 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit_arteriais)

# Limpar todas as tags, depois inserir as de highway=arterial
osm_edit2 <- osm_edit2 %>% mutate(tags_new = 'Thighway=arterial') %>% select(index_col, tags_new)



# Puxar as demais vias, para serem agrupadas com as editadas e formar novamente o .pbf
osm_edit3 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # # relações (r) - https://osmcode.org/opl-file-format/#encoding
  # filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>%
  select(-obj_type) %>%
  filter(!osm_id %in% ids_to_edit_coletoras & !osm_id %in% ids_to_edit_arteriais) %>% 
  select(index_col, tags_new = tags)

# Juntar os dois dataframes temporários antes de atualizar ao original
osm_edit <- rbind(osm_edit1, osm_edit2, osm_edit3)


# Gerar dataframe com velocidades alteradas para ser usado nos testes
osm_out <- 
  osm %>% 
  left_join(osm_edit, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% 
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_1_vias_coletoras_arteriais.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml




# ----------------------------------------------------------------------------
# Teste 2 - alterar classificações de viários e tags para teste de velocidades
# ----------------------------------------------------------------------------

# 39046029 - highway residential, sem outras tags
# 932653138 - highway secondary, sem outras tags
# 277290438, 595630234 - highway secondary, tags de cycleway:left e cycleway:designated
# 59515643 - highway residential, tags de cycleway:left e cycleway:designated
# 490208501 - highway residential, tag de cycleway:left (sem cycleway:designated)
# 846163664, 846163663, 846163665, 323086591 - highway cycleway
# 323086593 - highway cycleway, tag criada de "express cycleway"
# 14053851, 936496308, 936495816, 936496950 - highway tertiary
# 925995971, 16258132 - highway residential, com tag criada de semáforo
# 39046028 - highway residential, com tag oneway = no

# Função para trocar as tags de determinado grupo de estruturas
atualizar_tags <- function(tmp_df, edit_osm_ids, new_tag_string) {
  
  tmp_df <- 
    tmp_df %>% 
    filter(osm_id %in% edit_osm_ids) %>% 
    mutate(tags_new = new_tag_string) %>% 
    select(-tags)
}

# Marcar quais ids de trechos vão passar por alterações manuais
ids_to_edit <- c('39046029', '932653138', '277290438', '595630234', '59515643', 
                 '490208501', '846163664', '846163663', '846163665', '323086591',
                 '323086593', '14053851', '936496308', '936495816', '936496950',
                 '925995971', '16258132', '39046028')

# Fazer edição das tags por trecho, de acordo com a listagem acima
osm_edit <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) 

edit_ids  <- c('39046029')
edit_tags <- 'Thighway=residential'
osm_edit1 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit1

edit_ids  <- c('932653138')
edit_tags <- 'Thighway=secondary'
osm_edit2 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit2

edit_ids  <- c('277290438', '595630234')
# edit_tags <- 'Thighway=secondary,cycleway:left=lane,bicycle=designated'
# edit_tags <- 'Thighway=secondary,smoothness=good'
edit_tags <- 'Thighway=secondary,route=bicycle,network=lcn,lcn=yes'
osm_edit3 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit3

# osm %>% filter(str_detect(obj_type, '59515643')) %>% select(tags)
# osm %>% filter(str_detect(obj_type, 'n292424864'))
# osm %>% filter(str_detect(tags, 'lcn'))

edit_ids  <- c('59515643')
edit_tags <- 'Thighway=residential,cycleway:left=lane,bicycle=designated'
osm_edit4 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit4

edit_ids  <- c('490208501')
edit_tags <- 'Thighway=residential,cycleway:left=lane'
osm_edit5 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit5

edit_ids  <- c('846163664', '846163663', '846163665', '323086591')
edit_tags <- 'Thighway=cycleway'
osm_edit6 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit6

edit_ids  <- c('323086593')
edit_tags <- 'Thighway=cycleway,bicycle=express'
osm_edit7 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit7

edit_ids  <- c('14053851', '936496308', '936495816', '936496950')
edit_tags <- 'Thighway=tertiary'
osm_edit8 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit8

edit_ids  <- c('925995971', '16258132')
edit_tags <- 'Thighway=residential,traffic_lights=yes'
osm_edit9 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit9

edit_ids  <- c('39046028')
edit_tags <- 'Thighway=residential,oneway=no'
osm_edit10 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit10

# Juntar os dataframes temporários
osm_edit_group <- rbind(osm_edit1, osm_edit2, osm_edit3, osm_edit4, osm_edit5,
                        osm_edit6, osm_edit7, osm_edit8, osm_edit9, osm_edit10)

# Limpar ambiente
rm(osm_edit1, osm_edit2, osm_edit3, osm_edit4, osm_edit5,
   osm_edit6, osm_edit7, osm_edit8, osm_edit9, osm_edit10)


# Puxar demais vias para serem modificadas todas para residential sem outras tags
osm_edit_group2 <- osm_edit %>% filter(!osm_id %in% ids_to_edit & str_detect(tags, 'highway='))

# Para simplificar, todas vão ser marcadas como 'residential'
edit_ids  <- osm_edit_group2 %>% select(osm_id) %>% distinct()
edit_tags <- 'Thighway=residential'
osm_edit_group2 <- atualizar_tags(osm_edit_group2, edit_ids$osm_id, edit_tags)
head(osm_edit_group2)


# Puxar demais entradas, que não foram e não serão editadas (ex. buildings, access)
osm_not_edited <- 
  osm_edit %>% 
  filter(!osm_id %in% ids_to_edit & !str_detect(tags, 'highway=')) %>% 
  rename(tags_new = tags)

# Juntar todos os dataframes editados
osm_edit_out <- rbind(osm_edit_group, osm_edit_group2, osm_not_edited)

# Limpar ambiente
rm(osm_edit_group, osm_edit_group2, osm_not_edited, osm_edit, ids_to_edit, edit_ids, edit_tags)


# Gerar dataframe de saída, com partes que não foram editadas e as que foram
osm_out <- 
  osm %>% 
  left_join(osm_edit_out, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% 
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col, osm_id))

head(osm_out)

# osm_out %>% filter(str_detect(obj_type, '925995971'))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_2_teste_highways_e_tags.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml




# ----------------------------------------------------------------------------
# Teste 3 - alterar clas. de viários e tags mantendo algumas tags originais
# ----------------------------------------------------------------------------

# 39046029 - highway residential, sem outras tags
# 932653138 - highway secondary, sem outras tags
# 277290438, 595630234 - highway secondary, tags de cycleway:left e lcn
# 59515643 - highway residential, tags de cycleway:left e cycleway:designated
# 490208501 - highway residential, tag de cycleway:left (sem cycleway:designated)
# 846163664, 846163663, 846163665, 323086591 - highway cycleway
# 323086593 - highway cycleway, tag criada de "express cycleway"
# 14053851, 936496308, 936495816, 936496950 - highway tertiary
# 925995971, 16258132 - highway residential, com tag criada de semáforo
# 39046028 - highway residential, com tag oneway = no

# Função para limpar todas as tags, exceto as informadas ao chamar a função
limpar_tags <- function(tmp_df, edit_osm_ids, new_tag_string) {
  
  tmp_df <- 
    tmp_df %>% 
    filter(osm_id %in% edit_osm_ids) %>% 
    mutate(tags_new = new_tag_string) %>% 
    select(-tags)
}


# Função para trocar as tags de determinado grupo de estruturas
atualizar_tags <- function(df, edit_osm_ids, insert_tag_string) {
  # df <- osm_edit; edit_osm_ids <- edit_ids; insert_tag_string <- edit_tags
  
  # Criar dataframe de saída, para armazenar todas as mudanças
  df_out <- data.frame()
  
  # Rodar para cada id a ser editado
  for (id_to_edit in edit_osm_ids) {
    
    # id_to_edit <- "277290438"
    print(id_to_edit)
    
    # Isolar 
    tmp_df <- df %>% filter(osm_id == id_to_edit)
    
    # Guardar tags de interesse para compor conjunto final, inserindo vírgula
    # após cada tag, para junção a seguir
    tags <- tmp_df %>% select(tags)
    highway_tag <- tags %>% str_extract('highway=[A-Za-z]+')
    oneway_tag  <- tags %>% str_extract('oneway=[A-Za-z]+')
    bicycle_tag <- tags %>% str_extract('bicycle=[A-Za-z]+')
    cyclane_tag <- tags %>% str_extract('cycleway:right=[A-Za-z]+|cycleway:left=[A-Za-z]+')
    cyclane_oneway_tag1 <- tags %>% str_extract('cycleway:right:oneway=[A-Za-z]+|cycleway:left:oneway=[A-Za-z]+')
    cyclane_oneway_tag2 <- tags %>% str_extract('oneway:bicycle=[A-Za-z]+')
    surface_tag <- tags %>% str_extract('surface=[A-Za-z]+')
    # lala_tag <- tags %>% str_extract('lala=[A-Za-z]+') %>% str_c(',')
    
    # Usar somente uma das tags de oneway:bicycle - se a construção oneway:bicycle
    # existir, usá-la
    if (is.na(cyclane_oneway_tag2)) {
      cyclane_oneway_tag <- cyclane_oneway_tag1
    } else {
      cyclane_oneway_tag <- cyclane_oneway_tag2
    }
    
    # Se valor de oneway não existir para ciclovias, assumir 'no'
    if (highway_tag == 'highway=cycleway' & is.na(oneway_tag)) { oneway_tag <- 'oneway=no'}
    
    # Se via possui ciclofaixas e bicycle_tag for 'no', ajustar para 'yes'
    if (!is.na(cyclane_tag) & bicycle_tag == 'bicycle=no') { bicycle_tag <- 'bicycle=yes'}
    
    # Atribuições de "smoothness" para a combinação entre existência de 
    # ciclofaixa e vias de uma ou duas mãos:
    # via sem ciclofaixa e duas mãos: very_bad
    # via sem ciclofaixa e uma mão: bad
    # via com ciclofaixa e duas mãos: intermediate
    # via com ciclofaixa e uma mão: good
    # Deve ser marcada manualmente depois, para os casos específicos:
    # via com ciclovia e marcação de expressa (oneway será ignorado): excellent
    if (highway_tag != 'highway=cycleway') {
      
      if (is.na(cyclane_tag) & oneway_tag == 'oneway=no') {
        smoothness_tag <- 'smoothness=very_bad'
      } else if (is.na(cyclane_tag) & oneway_tag == 'oneway=yes') {
        smoothness_tag <- 'smoothness=bad'
      } else if (!is.na(cyclane_tag) & oneway_tag == 'oneway=no') {
        smoothness_tag <- 'smoothness=intermediate'
      } else if (!is.na(cyclane_tag) & oneway_tag == 'oneway=yes') {
        smoothness_tag <- 'smoothness=good'
      } else {
        smoothness_tag <- NA
      }
    } else {
      smoothness_tag <- NA
    }
    
    orig_tags <- c(#highway_tag,
                   oneway_tag, 
                   bicycle_tag, 
                   cyclane_tag,
                   # cyclane_oneway_tag,
                   cyclane_oneway_tag1,
                   cyclane_oneway_tag2,
                   surface_tag,
                   smoothness_tag)
    
    
    # Juntar tags originais às tags inseridas
    insert_tag_string <- edit_tags
    
    # Juntar somente as tags detectadas (não são NA)
    for (orig_tag in orig_tags) {
      if (!is.na(orig_tag)) {
        insert_tag_string <- str_c(insert_tag_string, orig_tag, sep = ',')
      }
    }
    
    # Atualizar as tags em tmp_df
    tmp_df <- tmp_df %>% mutate(tags_new = insert_tag_string)
    
    # Juntar resultados ao df_out
    df_out <- df_out %>% rbind(tmp_df)
    
  }

  # Remover coluna de tags
  df_out <- df_out %>% select(-tags)
  

}

# Marcar quais ids de trechos vão passar por alterações manuais
ids_to_edit <- c('39046029', '932653138', '277290438', '595630234', '59515643', 
                 '490208501', '846163664', '846163663', '846163665', '323086591',
                 '323086593', '14053851', '936496308', '936495816', '936496950',
                 '925995971', '16258132', '39046028')

# Fazer edição das tags por trecho, de acordo com a listagem acima
osm_edit <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) 

edit_ids  <- c('39046029')
edit_tags <- 'Thighway=residential'
osm_edit1 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit1

edit_ids  <- c('932653138')
edit_tags <- 'Thighway=secondary'
osm_edit2 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit2

edit_ids  <- c('277290438', '595630234')
# edit_tags <- 'Thighway=secondary,cycleway:left=lane,bicycle=designated'
edit_tags <- 'Thighway=secondary,lcn=yes,name=Ciclovias%20%SP,route=bicycle,type=route'
# edit_tags <- 'Thighway=secondary,cycle_network=BR,network=lcn'
osm_edit3 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit3

edit_ids  <- c('59515643')
edit_tags <- 'Thighway=residential'
osm_edit4 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit4

edit_ids  <- c('490208501')
edit_tags <- 'Thighway=residential'
osm_edit5 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit5

edit_ids  <- c('846163664', '846163663', '846163665', '323086591')
edit_tags <- 'Thighway=cycleway'
osm_edit6 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit6

edit_ids  <- c('323086593')
edit_tags <- 'Thighway=cycleway,smoothness=excellent'
osm_edit7 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit7

edit_ids  <- c('14053851', '936496308', '936495816', '936496950')
edit_tags <- 'Thighway=tertiary'
osm_edit8 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit8

edit_ids  <- c('925995971', '16258132')
edit_tags <- 'Thighway=residential,surface=salt'
osm_edit9 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit9

edit_ids  <- c('39046028')
edit_tags <- 'Thighway=residential'
osm_edit10 <- atualizar_tags(osm_edit, edit_ids, edit_tags)
osm_edit10


# Limpar ambiente
rm(highway_tag,
   oneway_tag, 
   bicycle_tag, 
   cyclane_tag,
   cyclane_oneway_tag,
   cyclane_oneway_tag1,
   cyclane_oneway_tag2,
   smoothness_tag,
   orig_tags,
   orig_tag,
   tmp_df,
   tags,
   insert_tag_string,
   edit_tags,
   tags,
   df, 
   edit_osm_ids,
   edit_ids,
   df_out,
   id_to_edit
)


# -----------------------------------
# Redes locais - lcn, rcn, ncn
# -----------------------------------

# O objetivo aqui é adicionar todos os osm_way_ids que possuem infraestrutura
# cicloviária às redes local, regional ou nacional, para que possam receber
# as alterações de velocidades via essas tags. Como no graphhopper esse atributo
# é lido como uma relação (r - relation) em vez de uma tag de viário (w - way),
# na prática é preciso criar ou alterar uma rede existente para que esses
# trechos sejam considerados de forma diferente no cálculo de velocidade
# /graphhopper/core/src/main/java/com/graphhopper/routing/util/parsers/OSMBikeNetworkTagParser.java

# https://wiki.openstreetmap.org/wiki/Cycle_routes
# osm %>% filter(obj_type == 'w323086593') %>% select(tags)
# osm %>% filter(obj_type == 'n8645160887') %>% select(lon)

# Ciclovia Pedroso pertence a uma rede local lcn
osm %>% filter(str_detect(tags, 'lcn'))
# Tags: Tlcn=yes,name=Ciclovia%20%da%20%Avenida%20%Pedroso%20%de%20%Morais,route=bicycle,type=route
osm %>% filter(str_detect(tags, 'lcn')) %>% select(tags)
# A coluna de lon se refere aos way_ids (osm_ids relacionados a ela)
# Mw323086590@,w323086594@,w846163664@,w846163665@,w323086591@,w323086593@
osm %>% filter(str_detect(tags, 'lcn')) %>% select(lon)
# O object_type é uma relação: r9132886
osm %>% filter(str_detect(tags, 'lcn')) %>% select(obj_type)


# Tentar alterar essa rede para contemplar as vias da Semaneiros e da Diógenes
former_lon <- osm %>% filter(obj_type == 'r9132886') %>% select(lon)
new_lon <- str_c(former_lon, 'w277290438@', 'w595630234@', sep = ',')
osm <- osm %>% mutate(lon = ifelse(obj_type == 'r9132886', new_lon, lon))
osm %>% filter(obj_type == 'r9132886') %>% select(lon)

# tags <- 
#   osm_edit %>% 
#   filter(osm_id %in% edit_ids) %>% 
#   select(tags)
# 
# # Tags de interesse: highway, oneway
# tags %>% str_extract('highway=[A-Za-z]+')
# tags %>% str_extract('oneway=[A-Za-z]+')
# tags %>% str_extract('bicycle=[A-Za-z]+')
# tags %>% str_extract('cycleway:right=[A-Za-z]+|cycleway:left=[A-Za-z]+')
# tags %>% str_extract('cycleway:right:oneway=[A-Za-z]+|cycleway:left:oneway=[A-Za-z]+')
# tags %>% str_extract('oneway:bicycle=[A-Za-z]+')
#   
# 
# as.character(tags[[1]])



# Juntar os dataframes temporários
osm_edit_group <- rbind(osm_edit1, osm_edit2, osm_edit3, osm_edit4, osm_edit5,
                        osm_edit6, osm_edit7, osm_edit8, osm_edit9, osm_edit10)

# Limpar ambiente
rm(osm_edit1, osm_edit2, osm_edit3, osm_edit4, osm_edit5,
   osm_edit6, osm_edit7, osm_edit8, osm_edit9, osm_edit10)


# Puxar demais vias para serem modificadas todas para residential sem outras tags
osm_edit_group2 <- osm_edit %>% filter(!osm_id %in% ids_to_edit & str_detect(tags, 'highway='))

# Para simplificar, todas vão ser marcadas como 'residential'
edit_ids  <- osm_edit_group2 %>% select(osm_id) %>% distinct()
edit_tags <- 'Thighway=residential'
osm_edit_group2 <- limpar_tags(osm_edit_group2, edit_ids$osm_id, edit_tags)
head(osm_edit_group2)


# Puxar demais entradas, que não foram e não serão editadas (ex. buildings, access)
osm_not_edited <- 
  osm_edit %>% 
  filter(!osm_id %in% ids_to_edit & !str_detect(tags, 'highway=')) %>% 
  rename(tags_new = tags)

# Juntar todos os dataframes editados
osm_edit_out <- rbind(osm_edit_group, osm_edit_group2, osm_not_edited)

# Limpar ambiente
rm(osm_edit_group, osm_edit_group2, osm_not_edited, osm_edit, ids_to_edit, edit_ids, edit_tags)


# Gerar dataframe de saída, com partes que não foram editadas e as que foram
osm_out <- 
  osm %>% 
  left_join(osm_edit_out, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% 
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col, osm_id))

head(osm_out)

# osm_out %>% filter(str_detect(obj_type, '925995971'))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_3_teste_highways_e_tags_avancado.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml
















# ----------------------------------------------------------------------------
# Teste 1 - alterar maxspeed e retirar menções a maxspeed:backward
# ----------------------------------------------------------------------------

# Criar um dataframe próprio para fazer as edições
ids_to_edit <- c('277290438', '595630234', '172869010', '39046029', '39046028', '436649363', '59515635')

# Alterar informações de velocidades somente nas vias de interesse
osm_edit1 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit)

osm_edit1 <-
  osm_edit1 %>%
  # Retirar todas as menções existentes sobre velocidades máximas - tags maxspeed
  # e maxspeed:backward  -https://rdrr.io/cran/stringi/man/about_search_regex.html
  mutate(tags = str_replace_all(tags, ',maxspeed=[\\d]+,', ','),
         tags = str_replace_all(tags, ',maxspeed:backward=[\\d]+,', ','),
         # Declarar novas velocidades
         speed1 = 'maxspeed=1,',
         # Separar letra inicial das tags...
         tags_start = str_sub(tags, start = 1, end = 1),
         # ...e tudo o que vem depois da letra inicial
         tags_middle = str_sub(tags, start = 2, end = -1),
         # Remontar tags, com velocidades como primeira tag
         tags_new = str_c(tags_start, speed1, tags_middle, sep = '')
  ) %>%
  select(index_col, tags_new)

# Nas demais vias, limpar todas as tags (inclusive as de 'highway'), para que 
# não sejam consideradas como opção
osm_edit2 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(!osm_id %in% ids_to_edit)

# Retirar todas as menções de tags existentes
osm_edit2 <- osm_edit2 %>% mutate(tags_new = 'T') %>% select(index_col, tags_new)

# Juntar os dois dataframes temporários antes de atualizar ao original
osm_edit <- rbind(osm_edit1, osm_edit2)


# Gerar dataframe com velocidades alteradas para ser usado nos testes
osm_out <- 
  osm %>% 
  left_join(osm_edit, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% 
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col))
  

# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_1.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml
























# ----------------------------------------------------------------------------
# Teste 2 - deixar maxspeed e maxspeed:backward iguais para todos os trechos
# ----------------------------------------------------------------------------

# Criar um dataframe próprio para fazer as edições
ids_to_edit <- c('277290438', '595630234', '172869010', '39046029', '39046028', '436649363', '59515635')

# Alterar informações de velocidades somente nas vias de interesse
osm_edit1 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit)

osm_edit1 <-
  osm_edit1 %>%
  # Retirar todas as menções existentes sobre velocidades máximas - tags maxspeed
  # e maxspeed:backward - https://rdrr.io/cran/stringi/man/about_search_regex.html
  mutate(tags = str_replace_all(tags, ',maxspeed=[\\d]+,', ','),
         tags = str_replace_all(tags, ',maxspeed:backward=[\\d]+,', ','),
         # Declarar novas velocidades
         speed1 = 'maxspeed=1,',
         speed2 = 'maxspeed:backward=1,',
         # Separar letra inicial das tags...
         tags_start = str_sub(tags, start = 1, end = 1),
         # ...e tudo o que vem depois da letra inicial
         tags_middle = str_sub(tags, start = 2, end = -1),
         # Remontar tags, com velocidades como primeira tag
         tags_new = str_c(tags_start, speed1, speed2, tags_middle, sep = '')
  ) %>%
  select(index_col, tags_new)

# Nas demais vias, limpar todas as tags (inclusive as de 'highway'), para que 
# não sejam consideradas como opção
osm_edit2 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(!osm_id %in% ids_to_edit)

# Retirar todas as menções de tags existentes
osm_edit2 <- osm_edit2 %>% mutate(tags_new = 'T') %>% select(index_col, tags_new)

# Juntar os dois dataframes temporários antes de atualizar ao original
osm_edit <- rbind(osm_edit1, osm_edit2)


# Gerar dataframe com velocidades alteradas para ser usado nos testes
osm_out <- 
  osm %>% 
  left_join(osm_edit, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% select(index_col, tags_new)
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_2.pbf', pasta_gh_testes)

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

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml


# ----------------------------------------------------------------------------
# Teste 3 - deixar maxspeed iguais e maxspeed:backward diferentes entre trechos
# ----------------------------------------------------------------------------

# Criar um dataframe próprio para fazer as edições
ids_to_edit <- c('277290438', '595630234', '172869010', '39046029', '39046028', '436649363', '59515635')

# Alterar informações de velocidades somente nas vias de interesse
osm_edit1 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(osm_id %in% ids_to_edit)

osm_edit1 <-
  osm_edit1 %>%
  # Retirar todas as menções existentes sobre velocidades máximas - tags maxspeed
  # e maxspeed:backward - https://rdrr.io/cran/stringi/man/about_search_regex.html
  mutate(tags = str_replace_all(tags, ',maxspeed=[\\d]+,', ','),
         tags = str_replace_all(tags, ',maxspeed:backward=[\\d]+,', ','),
         # Declarar novas velocidades
         speed1 = 'maxspeed=1,',
         speed2 = 'maxspeed:backward=100,',
         # Separar letra inicial das tags...
         tags_start = str_sub(tags, start = 1, end = 1),
         # ...e tudo o que vem depois da letra inicial
         tags_middle = str_sub(tags, start = 2, end = -1),
         # Remontar tags, com velocidades como primeira tag
         tags_new = str_c(tags_start, speed1, speed2, tags_middle, sep = '')
  ) %>%
  select(index_col, tags_new)

# Remover info de ciclofaixa na via
osm_edit1 <- 
  osm_edit1 %>% 
  mutate(tags_new = ifelse(index_col == '27778', 
                           'Tmaxspeed=1,maxspeed:backward=1,highway=secondary',
                           tags_new))


# Nas demais vias, limpar todas as tags (inclusive as de 'highway'), para que 
# não sejam consideradas como opção
osm_edit2 <- 
  osm %>% 
  select(-c(lon, lat)) %>% 
  # Selectionar somente os trechos de vias (w - ways), descartando nodes (n) e 
  # relações (r) - https://osmcode.org/opl-file-format/#encoding
  filter(str_starts(obj_type, 'w')) %>%
  # Criar coluna de osm_id
  mutate(osm_id = str_sub(obj_type, start = 2, end = -1), .after = 'obj_type') %>% 
  select(-obj_type) %>% 
  filter(!osm_id %in% ids_to_edit)

# Retirar todas as menções de tags existentes
osm_edit2 <- osm_edit2 %>% mutate(tags_new = 'T') %>% select(index_col, tags_new)

# Juntar os dois dataframes temporários antes de atualizar ao original
osm_edit <- rbind(osm_edit1, osm_edit2)


# Gerar dataframe com velocidades alteradas para ser usado nos testes
osm_out <- 
  osm %>% 
  left_join(osm_edit, by = 'index_col') %>% 
  # filter(!is.na(tags_new)) %>% select(index_col, tags_new)
  mutate(tags = ifelse(is.na(tags_new), tags, tags_new)) %>% 
  select(-c(tags_new, index_col))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_gh_testes)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm_out, osm_file_out, delim = ' ', col_names = FALSE, na = '')

# Nomear arquivo de testes
pbf_file_out  <- sprintf('%s/entorno_praca_editado_3.pbf', pasta_gh_testes)

# Converter arquivo .opl de volta para o formato .pbf, para ser compilado
arg_o4 <- sprintf('cat "%s"', osm_file_out)
arg_o5 <- sprintf('--overwrite')
arg_o6 <- sprintf('-o "%s"', pbf_file_out)
# sprintf('%s %s %s %s', osmium_path, arg_o1, arg_o2, arg_o3)
system2(command = osmium_path, args = c(arg_o4, arg_o5, arg_o6))

# Limpar ambiente
rm(arg_o4, arg_o5, arg_o6)

# Apagar arquivos .opl temporários que haviam sido criados
file.remove(osm_file_tmp)
file.remove(osm_file_out)

# Testar o GraphHopper - remover a pasta existente 'graph-cache' e abrir o 
# terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml
