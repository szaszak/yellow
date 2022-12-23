# Edita o mapa do OSM offline - o objetivo é mudar algumas tags que não permitem
# a passagem de pedestres em ciclovias, para que o map matching usando o modo 
# pedestre possa ser mais fiel

# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas e arquivos
pasta_geral_tiles    <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/valhalla_tiles_sp"
pasta_valhalla_pbf   <- sprintf("%s/pbf", pasta_geral_tiles)

osm_file_orig <- sprintf('%s/20220216_sao_paulo.osm.pbf', pasta_valhalla_pbf)
osm_file_tmp  <- sprintf('%s/lala.opl', pasta_valhalla_pbf)
pbf_file_out  <- sprintf('%s/20220216_sao_paulo_edited_20221223.osm.pbf', pasta_valhalla_pbf)


# Converter arquivo .pbf para o formato .opl, que pode ser lido como texto
message('\nConvertendo arquivo .pbf em .opl com o osmium.\n')
osmium_path <- sprintf("/usr/bin/osmium")
arg_o1 <- sprintf('cat "%s"', osm_file_orig)
arg_o2 <- sprintf('--overwrite')
arg_o3 <- sprintf('-o "%s"', osm_file_tmp)
system2(command = osmium_path, args = c(arg_o1, arg_o2, arg_o3))


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


# ------------------------------------------------------------------------------
# Ciclovia do Rio Pinheiros
# ------------------------------------------------------------------------------

# Simplificar dataframe filtrando por string constante na coluna de tags
osm_cycle <- osm %>% filter(str_detect(tags, 'cycleway'))

# Selecionar os objetos (osm_id) que vão precisar de edição - queremos a Ciclovia
# do Rio Pinheiros, onde está marcado como "foot=no"
objetos_a_editar <- 
  osm_cycle %>% 
  filter(str_detect(tags, 'Ciclovia%20%do%20%Rio%20%Pinheiros')) %>% 
  filter(str_detect(tags, 'highway=cycleway')) %>% 
  filter(str_detect(tags, 'foot=no')) %>% 
  select(obj_type)


# ------------------------------------------------------------------------------
# Pontes em geral
# ------------------------------------------------------------------------------

# Selecionar os objetos (osm_id) de pontes que têm as tags 'foot' ou 'bicycle'
objetos_a_editar2 <- 
  osm %>% 
  filter(str_detect(tags, 'bridge') & (str_detect(tags, 'foot') | str_detect(tags, 'bicycle'))) %>% 
  select(obj_type)

# # Teste de remoção - tags de 'foot=no' das linhas (objetos) selecionadas
# osm_cycle %>% 
#   mutate(tags = case_when(obj_type %in% objetos_a_editar$obj_type ~ str_replace(tags, 'foot=no', 'foot=yes'),
#                           TRUE ~tags)) %>%
#   filter(obj_type %in% objetos_a_editar$obj_type) %>% 
#   select(tags)

objetos_a_editar <- objetos_a_editar %>% rbind(objetos_a_editar2)


# ------------------------------------------------------------------------------
# Túnel 9 de Julho
# ------------------------------------------------------------------------------

# Túneis em que bicicletas e pedestres não são permitidos, mas que não são tipos
# específicos, como de trem/metrô, passagens de água etc.
objetos_a_editar3 <- 
  osm %>% 
  filter(
    str_detect(tags, 'tunnel') & 
      !str_detect(tags, 'electrified') & 
      !str_detect(tags, 'waterway') &
      !str_detect(tags, 'highway=trunk') &
      !str_detect(tags, 'maxspeed=100') &
      !str_detect(tags, 'access=private') &
      !str_detect(tags, 'access=customers') &
      # Este é o túnel da Rebouças com a Faria Lima
      !str_detect(tags, 'name=Túnel%20%Jornalista%20%Fernando%20%Vieira%20%de%20%Melo') &
      # Se é um túnel e é via de serviço ou de pedestres, é uma
      # passagem de transporte público (conforme checagem manual)
      !str_detect(tags, 'highway=service') &
      !str_detect(tags, 'highway=footway') &
      (str_detect(tags, 'foot=no') | str_detect(tags, 'bicycle=no'))
    ) %>% 
  select(obj_type)

# Checar resultados
# objetos_a_editar3 %>% mutate(obj_type = str_replace(obj_type, 'w', '')) %>% as.list()

objetos_a_editar <- objetos_a_editar %>% rbind(objetos_a_editar3)



# ------------------------------------------------------------------------------
# Executar a edição
# ------------------------------------------------------------------------------

# Remover as tags de 'foot=no' / 'bicycle=no' das linhas (objetos) selecionadas
osm <- 
  osm %>% 
  mutate(tags = case_when(obj_type %in% objetos_a_editar$obj_type ~ str_replace(tags, 'foot=no', 'foot=yes'),
                          TRUE ~tags)) %>% 
  mutate(tags = case_when(obj_type %in% objetos_a_editar$obj_type ~ str_replace(tags, 'bicycle=no', 'bicycle=yes'),
                          TRUE ~tags))

# As tags com aspas duplas estão dando erro - remover as aspas
osm <- osm %>% mutate(tags = str_replace_all(tags, '"', ''))


# Salvar arquivo resultante .opl - atenção para o delimitador, que é um espaço,
# retirar nomes de colunas e para a exportação de NAs como ''
osm_file_out <- sprintf('%s/lala_altered.opl', pasta_valhalla_pbf)
# write_delim(boo, osm_file_out, delim = ' ', col_names = FALSE)
write_delim(osm, osm_file_out, delim = ' ', col_names = FALSE, na = '')


# Converter arquivo .opl de volta para o formato .pbf, para ser compilado
arg_o4 <- sprintf('cat "%s"', osm_file_out)
arg_o5 <- sprintf('--overwrite')
arg_o6 <- sprintf('-o "%s"', pbf_file_out)
# sprintf('%s %s %s %s', osmium_path, arg_o1, arg_o2, arg_o3)
system2(command = osmium_path, args = c(arg_o4, arg_o5, arg_o6))


# Apagar arquivos .opl temporários que haviam sido criados
file.remove(osm_file_tmp)
file.remove(osm_file_out)




# # Erro é sempre na linha seguinte à apesentada, ex: osm %>% slice(15273142)
# # OPL error: Duplicate attribute: nodes (N) on line 15273141 column 0
# com_problemas <- c('w4273875', 'w4273889')
# osm <- osm %>% filter(!obj_type %in% com_problemas)
# 
# boo <- osm %>% head(68475)