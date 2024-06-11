# Comando para rodar o GraphHopper no terminal - atenção para o PBF a ser carregado:
# PBF da Rede 2019; custom model ajustado (LTS)
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example_LTS.yml

# carregar bibliotecas
source('fun/setup.R')
library('httr')
library('jsonlite')
library('arrow')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
# dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
# pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
dir.create(pasta_ids_aopt_19, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_rts_aopt_19, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos com rotas alternativas (até 3 por par OD)
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe,
# com rotas até 3 alternativas por par OD - aceita um dataframe de uma linha como
# entrada
gh_route_alt <- function(hex_id, route_options) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  # url <- ods_vgs %>% slice(1) %>% select(url) %>% pull()
  # Estação Vila Madalena: -23.546258,-46.690898
  # IME: -23.559007,-46.73208
  # CCSP: -23.571498,-46.639806
  
  # df_line$url <- paste('http://localhost:8989/route/?point=', 
  # '-23.541504', '%2C', '-46.685779', '&point=', 
  # '-23.571498', '%2C', '-46.639806', route_options,
  # sep = '')
  
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
        osm_way_out <- sprintf('%s/%s.csv', pasta_ids_aopt_19, hex_id_base)
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
  tmp_file <- sprintf('%s/%s_modalt.csv', pasta_rts_aopt_19, hex_id_base)
  if (file.exists(tmp_file)) {
    write_delim(paths, tmp_file, delim = ';', append = TRUE)
  } else {
    write_delim(paths, tmp_file, delim = ';', append = FALSE)
  }
  
  # Guardar ids já processados em arquivo próprio
  df_line <- df_line %>% select(id)
  ids_processados <- sprintf('%s/tmp_00_ids_processados_2019.csv', pasta_aoprv_alter)
  
  if (file.exists(ids_processados)) {
    write_delim(df_line, ids_processados, delim = ';', append = TRUE)
  } else {
    write_delim(df_line, ids_processados, delim = ';', append = FALSE)
  }
  
}


# ------------------------------------------------------------------------------
# Calcular matriz de distâncias - RStudio
# ------------------------------------------------------------------------------

# Abrir base para routing com 26 vizinhos, manter só id e url
hex_com_vizinhos <- sprintf("%s/00_base_para_routing_res09_26vizinhos.csv", pasta_aoprv_alter)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
head(hex_com_vizinhos)



# Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
# do dataframe hex_com_vizinhos se houver
# library('tidylog')
arqs_resultados <- data.frame(arq = list.files(pasta_rts_aopt_19, recursive = FALSE, full.names = FALSE))
arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_replace(arq, '_modalt.csv', ''))
hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$hex_id)
rm(arqs_resultados)


# Para cada linha de origem e destino, gerar rotas modeladas com alternativas
detach("package:tidylog")

# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# se forem só as rotas originais, é ok rodar no RStudio
for (id in hex_com_vizinhos$id) { gh_route_alt(id, route_options = route_options) }


# ------------------------------------------------------------------------------
# Para rodar no Jupyter com future
# ------------------------------------------------------------------------------

detach("package:tidylog")

# Arquivos a processar
arqs <- list.files(pasta_aoprv_alter, pattern = '^tmp_base_para_routing_res09_26vizinhos_[0-9]{3}.csv', full.names = TRUE)
# Arquivos já processados
ids_processados <- sprintf('%s/tmp_00_ids_processados_2019.csv', pasta_aoprv_alter)

for (arq in arqs) {
  # arq <- arqs[1]
  print(arq)
  
  # Abrir base para routing com 26 vizinhos, manter só id e url  
  hex_com_vizinhos <- read_delim(arq, delim = ';', col_types = cols(.default = "c"))
  
  # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
  # do dataframe hex_com_vizinhos se houver
  # library('tidylog')
  if (file.exists(ids_processados)) {
    arqs_resultados <- read_delim(ids_processados, delim = ';', col_types = "c")
    hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$id)
    rm(arqs_resultados)
    print(nrow(hex_com_vizinhos))
  }
  
  # Se arquivo já foi completamente processado, removê-lo e passar para o seguinte
  if (nrow(hex_com_vizinhos) == 0) { 
    file.remove(arq) 
    next
  }
  
  # Limpar memória
  gc(T)
  
  # Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
  # se forem só as rotas originais, é ok rodar no RStudio
  # for (id in hex_com_vizinhos$id) { gh_route_alt(id, route_options = route_options) }
  # Rodar função para todos os arquivos- multi thread (Jupyter)
  (start = Sys.time())
  future::plan(future::multicore)
  invisible(future.apply::future_lapply(X   = hex_com_vizinhos$id,
                                        FUN = gh_route_alt,
                                        future.seed = TRUE))
  Sys.time()
  Sys.time() - start
  
  # Remover arquivo inteiramente processado
  file.remove(arq) 
  
  # Limpar memória
  rm(hex_com_vizinhos)  
  gc(T)
  
}


# ------------------------------------------------------------------------------
# Checar se todos rodaram
# ------------------------------------------------------------------------------

# Abrir base para routing com 26 vizinhos, manter só id e url
hex_com_vizinhos <- sprintf("%s/00_base_para_routing_res09_26vizinhos.csv", pasta_aoprv_alter)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
head(hex_com_vizinhos)

ids_ok <- hex_com_vizinhos %>% separate(id, into = c('id1', 'id2'), sep = '-', remove = TRUE)
ids_ok <- ids_ok %>% select(id = id1) %>% distinct()
head(ids_ok)
gc(T)

# arqs_resultados <- data.frame(arq = list.files(pasta_rts_aopt_19, recursive = FALSE, full.names = FALSE))
# arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_replace(arq, '_modalt.csv', ''))
arqs_resultados <- data.frame(arq = list.files(pasta_ids_aopt_19, recursive = FALSE, full.names = FALSE))
arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_replace(arq, '.csv', ''))
arqs_resultados <- arqs_resultados %>% select(hex_id)
arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_c('89a81', hex_id, 'ffff'))

# Os dois ids que não possuem resultados foram checados no QGIS: 89a81044b57ffff 
# e 89a81038bafffff. Ambos estão nos limites do mapa hexagonal e possuem acesso
# ruim em termos de viário às demais vias. Possivelmente, são osm_ids de vias de
# serviço ou algo do tipo que impossibilitam o routing, dando NAs como resultados
ids_ok %>% filter(!id %in% arqs_resultados$hex_id)


# hex_com_vizinhos %>% filter(str_detect(id, '^89a81([a-z0-9]{6})ffff'))
# hex_com_vizinhos %>% head() %>% mutate(id = str_replace(id, '^89a81([a-z0-9]{6})ffff', '\\1') )