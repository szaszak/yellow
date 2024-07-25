# Comando para rodar o GraphHopper no terminal - atenção para o PBF a ser carregado:
# PBF da Rede 2019; custom model ajustado (LTS)
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example_LTS.yml

# carregar bibliotecas
source('fun/setup.R')
library('httr')
library('jsonlite')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
dir.create(pasta_osmids_aopt, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_rotas_aopt, recursive = TRUE, showWarnings = FALSE)



# ------------------------------------------------------------------------------
# Routing a partir de dois pontos com rotas alternativas (até 3 por par OD)
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe,
# com rotas até 3 alternativas por par OD - aceita um dataframe de uma linha como
# entrada
gh_route_alt <- function(hex_id) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=true&calc_points=true&algorithm=alternative_route&details=osm_way_id&details=time&details=road_class&details=road_environment&details=surface&details=bike_network&details=smoothness&details=average_slope&details=distance'
  # url <- ods_vgs %>% slice(1) %>% select(url) %>% pull()
  # Estação Vila Madalena: -23.546258,-46.690898
  # IME: -23.559007,-46.73208
  # CCSP: -23.571498,-46.639806
  
  # hex_id <- '89a81046b2fffff-89a81044d8fffff'
  # hex_id <- hex_com_vizinhos %>% head(1) %>% select(id) %>% pull()
  df_line <- hex_com_vizinhos %>% filter(id == hex_id)
  # Encurtar hex_id - todos aqui são '89a81' + 6 caracteres de dígito ou letra = 'ffff'
  hex_id_short <- str_replace(hex_id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2')
  hex_id_base  <- str_sub(hex_id_short, 1, 6)
  
  # Fazer a GET de roteamento no Grahphopper
  # print(df_line$url)
  gh_response <- GET(df_line$url)
  
  # Mensagem tem que ser "Success: (200) OK"
  if (http_status(gh_response)$message == 'Success: (200) OK') {
    
    # Resposta da query, já colapsada e transformada em dataframe
    # Remover aviso de 'No encoding supplied: defaulting to UTF-8' na linha fromJSON()
    suppressMessages(
      response_text <- 
        # Ignorar aviso 'argument is not an atomic vector; coercing'
        suppressWarnings(str_c(content(gh_response, 'text'), collapse = ", ")) %>% 
        # Concatenar toda a string de resultados
        str_c("[", ., "]") %>% 
        # Transformar em dataframe
        fromJSON() %>% 
        as.data.frame()
    )
    
    # Nos interessa a coluna de 'paths', como um novo dataframe
    paths <- response_text$paths %>% as.data.frame()
    
    # Puxar osm_way_ids dos resultados de cada alternativa e gravar em pasta separada
    for (i in seq(1, length(paths$details$osm_way_id))) {
      # i <- 1
      osm_ways <- paths$details$osm_way_id[i] %>% as.data.frame()
      
      if (nrow(osm_ways) > 0) {
        # Manter somente osm_ids e inserir número da rota alternativa
        osm_ways <- osm_ways %>% select(osm_way_id = X3) %>% mutate(hex_id = hex_id_short,
                                                                    alt = i,
                                                                    index = row_number(),
                                                                    .before = 'osm_way_id')
        # Gravar resultados agrupados por hex_id_short de origem
        osm_way_out <- sprintf('%s/%s.csv', pasta_osmids_aopt, hex_id_base)
        if (file.exists(osm_way_out)) {
          write_delim(osm_ways, osm_way_out, delim = ';', append = TRUE)
        } else {
          write_delim(osm_ways, osm_way_out, delim = ';', append = FALSE)
        }
        
      } else {
        # Se não há osm_way_ids, é porque os pontos estão muito próximos uns dos
        # outros, mesmo que seja um osm_way_id diferente entre a origem e o 
        # destino. A distância e a velocidade calculadas vão ser zero também - 
        # pular este registro, que vai ser vazio
        return(sprintf('Pulando: %s não tem osm_way_ids (provavelmente tem distância = 0)', hex_id))
      }
      
    }
    
    
    # Isolar colunas de interesse
    paths <- 
      paths %>% 
      # Calcular tempo em segundos e velocidade média
      mutate(time = time / 1000,
             speed = distance / time * 3.6) %>% 
      # Descartar colunas extras - a coluna poly é o shape da rota traçada
      select(distance, weight, time, speed, poly = points)
    
    # Testar polyline:
    # https://valhalla.github.io/demos/polyline/?unescape=true&polyline6=false#%0A
    
    # Adicionar colunas de informação vindas do dataframe original
    paths <- 
      paths %>% 
      mutate(hex_id    = hex_id_short,
             alt       = row_number(),
             # id_hex.x  = df_line$id_hex_x,
             # id_hex.y  = df_line$id_hex_y,
             .before = 'distance') #%>% 
    # mutate(lon.x     = df_line$lon.x,
    #        lat.x     = df_line$lat.x,
    #        lon.y     = df_line$lon.y,
    #        lat.y     = df_line$lat.y,
    #        .after = 'poly')
    
  } else {
    
    # Se a query no GraphHopper não deu resultados, guardar como dataframe vazio
    paths <- data.frame(hex_id    = hex_id_short,
                        alt       = NA,
                        # id_hex.x  = df_line$id_hex_x,
                        # id_hex.y  = df_line$id_hex_y,
                        distance  = NA,
                        weight    = NA,
                        time      = NA,
                        speed     = NA,
                        poly      = NA
                        # lon.x     = df_line$lon.x,
                        # lat.x     = df_line$lat.x,
                        # lon.y     = df_line$lon.y,
                        # lat.y     = df_line$lat.y
    )
    
  }
  
  # Guardar resultados temporários agrupados por hex_id_short de origem
  tmp_file <- sprintf('%s/%s_modalt.csv', pasta_rotas_aopt, hex_id_base)
  if (file.exists(tmp_file)) {
    write_delim(paths, tmp_file, delim = ';', append = TRUE)
  } else {
    write_delim(paths, tmp_file, delim = ';', append = FALSE)
  }
  
  # Guardar ids já processados em arquivo próprio
  df_line <- df_line %>% select(id)
  ids_processados <- sprintf('%s/tmp_00_ids_processados_2019.csv', pasta_aoprv_teste)
  
  if (file.exists(ids_processados)) {
    write_delim(df_line, ids_processados, delim = ';', append = TRUE)
  } else {
    write_delim(df_line, ids_processados, delim = ';', append = FALSE)
  }
  
}


# ------------------------------------------------------------------------------
# Agregar totais de população e oportunidades aos hexágonos
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))

# Tratar como dataframe e selecionar somente colunas de interesse
hex_sp <- st_centroid(hex_sp) %>% mutate(centroides = as.character(geom)) %>% st_drop_geometry()

# Oportunidades por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_landuse_2019_v2.csv', dados_originais)
dados_ipea <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea <- dados_ipea %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar só totais de oportunidades
dados_ipea <- dados_ipea %>% mutate(oportunidades = T001 + E001 + M001 + S001 + C001) %>% select(id_hex, oportunidades)
head(dados_ipea)

# População por hexágono
# https://ipeagit.github.io/aopdata/reference/read_landuse.html
open_file <- sprintf('%s/aop_population_2010_v2.csv', dados_originais)
dados_ipea_pop <- read_delim(open_file, delim = ',', col_types = "cccccddddddddddddddddd")
dados_ipea_pop <- dados_ipea_pop %>% filter(abbrev_muni == 'spo') %>% select(-c(year, abbrev_muni, name_muni, code_muni))
# Deixar neste momento só dados totais da população
dados_ipea_pop <- dados_ipea_pop %>% select(id_hex, populacao = P001)
head(dados_ipea_pop)

# Juntar dados de oportunidades e população
hex_sp <- 
  hex_sp %>% 
  left_join(dados_ipea, by = 'id_hex') %>% 
  left_join(dados_ipea_pop, by = 'id_hex')

# Hexágonos sem oportunidade e sem população devem ser descartados
hex_sp <- hex_sp %>% filter(oportunidades > 0 & populacao > 0)


# ------------------------------------------------------------------------------
# Gerar latlong para as origens e destinos (centroides dos hexágonos)
# ------------------------------------------------------------------------------

# Separar coluna de centroides em latlon
hex_sp <-
  hex_sp %>%
  separate(centroides, '[c\\(\\), )]', into = c('x', 'y', 'lon', 'z', 'lat', 'u')) %>%
  select(id_hex, lat, lon)

# hex_sp %>% filter(is.na(lat) | is.na(lon))

# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/hex_spo_res09_26vizinhos.csv", pasta_aoprev_hex)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))

# Juntar hexágonos de origem e destino às cordenadas latlong de seus centroides
hex_com_vizinhos <-
  hex_com_vizinhos %>%
  left_join(hex_sp, by = c('id_hex_x' = 'id_hex')) %>%
  left_join(hex_sp, by = c('id_hex_y' = 'id_hex'))

# Remover hexágonos vizinhos que estão fora do shape de São Paulo
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.y) & !is.na(lon.y))
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!is.na(lat.x) & !is.na(lon.x))
# 10911566 / 1267988 = 8.6 vezes as queries com resolução 8
nrow(hex_com_vizinhos)

# Limpar ambiente
rm(hex_sp, dados_ipea, dados_ipea_pop)


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

# Para o primeiro teste, queremos apenas dois hexágonos da zona leste
# filter: removed 10,909,452 rows (>99%), 2,114 rows remaining
hex_com_vizinhos <- hex_com_vizinhos %>% filter(id_hex_x == '89a81046b2fffff' | id_hex_x == '89a81044d93ffff')
# hex_com_vizinhos %>% filter(id_hex_x == id_hex_y)
# head(hex_com_vizinhos)

# Criar coluna com URL para GET no GraphHopper
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id'

hex_com_vizinhos <- 
  hex_com_vizinhos %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))


# Criar uma coluna de id
hex_com_vizinhos <- hex_com_vizinhos %>% mutate(id = str_c(id_hex_x, id_hex_y, sep = '-'), .before = 'id_hex_x')
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
head(hex_com_vizinhos)


# Guardar resultados - base integral
out_file <- sprintf('%s/00_base_para_teste_routing_res09_26vizinhos.csv', pasta_aoprv_teste)
write_delim(hex_com_vizinhos, out_file, delim = ';')


# Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
# do dataframe hex_com_vizinhos se houver
# library('tidylog')
arqs_resultados <- data.frame(arq = list.files(pasta_rotas_aopt, recursive = FALSE, full.names = FALSE))
arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_replace(arq, '_modalt.csv', ''))
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$hex_id)
rm(arqs_resultados)


# Para cada linha de origem e destino, gerar rotas modeladas com alternativas
detach("package:tidylog")

# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# se forem só as rotas originais, é ok rodar no RStudio
for (id in hex_com_vizinhos$id) { gh_route_alt(id) }



# ------------------------------------------------------------------------------
# Fazer arquivos identificados que ficaram de fora
# ------------------------------------------------------------------------------

# Esta parte do script é para rodar de novo somente com os arquivos que foram 
# detectados que ficaram de fora ao processar os resultados e tentar gerar a
# ttmatrix para rotas com e sem infraestrutura cicloviária. Devem ser poucos
# arquivos que por qualquer motivo não ficaram salvos nas bases - talvez porque
# o future tenha feito um multi-processamento que tentou escrever ao mesmo tempo
# no mesmo arquivo e não conseguiu. Para rodar de novo, é só rodar esse script
# até a declaração das funções e pular todo o resto. Comentar as linhas abaixo
# e rodar a partir delas

# # Abrir hexágonos para SP combinados com vizinhos
# hex_com_vizinhos <- sprintf("%s/00_base_para_teste_routing_res09_26vizinhos.csv", pasta_aoprv_teste)
# hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
# head(hex_com_vizinhos)
# 
# # Arquivos identificados que faltam rodar (scripts pós routing com graphhopper)
# arq_faltam <- sprintf('%s/xxx_faltam_processar.csv', pasta_aoprv_teste)
# faltam <- read_delim(arq_faltam, delim = ';', col_types = 'c')
# 
# hex_com_vizinhos <- hex_com_vizinhos %>% filter(id %in% faltam$id)
# 
# # Para cada linha de origem e destino, gerar rotas modeladas com alternativas
# detach("package:tidylog")
# 
# # Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# # se forem só as rotas originais, é ok rodar no RStudio
# for (id in hex_com_vizinhos$id) { gh_route_alt(id) }
# 
# file.remove(arq_faltam)
