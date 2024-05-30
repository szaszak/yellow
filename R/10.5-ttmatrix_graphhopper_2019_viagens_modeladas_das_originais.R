library('tidyverse')
library('tidylog')
library('httr')
library('jsonlite')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)
pasta_orig_way_ids <- sprintf("%s/01_osm_way_ids_rotas_modeladas", pasta_orig_vs_mod)
dir.create(pasta_orig_way_ids, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Routing a partir de dois pontos com rotas alternativas (até 3 por par OD)
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe,
# com rotas até 3 alternativas por par OD - aceita um dataframe de uma linha como
# entrada
gh_route <- function(df_line, out_file, route_options) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  # url <- ods_vgs %>% slice(1) %>% select(url) %>% pull()
  # Estação Vila Madalena: -23.546258,-46.690898
  # IME: -23.559007,-46.73208
  # CCSP: -23.571498,-46.639806
  
  # df_line$url <- paste('http://localhost:8989/route/?point=', 
  # '-23.541504', '%2C', '-46.685779', '&point=', 
  # '-23.571498', '%2C', '-46.639806', route_options,
  # sep = '')
  
  # df_line <- ods_vgs %>% slice(1)
  # df_line <- tmp_df
  
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
    
    # Puxar osm_way_ids dos resultados 
    osm_ways <- paths$details$osm_way_id[1] %>% as.data.frame()
      
    if (nrow(osm_ways) > 0) {
      osm_ways <- osm_ways %>% mutate(index = 1:nrow(.)) %>% select(index, osm_way_id = X3)
      
      # Gravar resultados
      osm_way_out <- sprintf('%s/%s.csv', pasta_orig_way_ids, df_line$trip_id)
      write_delim(osm_ways, osm_way_out, delim = ';')
      
    } else {
      # Se não há osm_way_ids, é porque os pontos estão muito próximos uns dos
      # outros, mesmo que seja um osm_way_id diferente entre a origem e o 
      # destino. A distância e a velocidade calculadas vão ser zero também - 
      # pular este registro, que vai ser vazio
      return(sprintf('Pulando: %s não tem osm_way_ids (provavelmente tem distância = 0)', df_line$trip_id))
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
      mutate(trip_id   = df_line$trip_id,
             qgis_id.x = df_line$qgis_id.x,
             qgis_id.y = df_line$qgis_id.y,
             .before = 'distance') %>% 
      mutate(lon.x     = df_line$lon.x,
             lat.x     = df_line$lat.x,
             lon.y     = df_line$lon.y,
             lat.y     = df_line$lat.y,
             .after = 'poly')
    
  } else {
    
    # Se a query no GraphHopper não deu resultados, guardar como dataframe vazio
    paths <- data.frame(trip_id   = df_line$trip_id,
                        qgis_id.x = df_line$qgis_id.x,
                        qgis_id.y = df_line$qgis_id.y,
                        distance  = NA,
                        weight    = NA,
                        time      = NA,
                        speed     = NA,
                        poly      = NA,
                        lon.x     = df_line$lon.x,
                        lat.x     = df_line$lat.x,
                        lon.y     = df_line$lon.y,
                        lat.y     = df_line$lat.y
    )
    
  }
  
  # Guardar resultados temporários
  if (file.exists(out_file)) {
    write_delim(paths, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(paths, out_file, delim = ';', append = FALSE)
  }
  
}



# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

# Abrir origens e destinos das rotas iniciais da Yellow - são consideradas aqui
# (pelos scripts anteriores) as rotas que (a) tiveram algum trecho considerado
# no modelo; (b) que não foram divididas em trechos menores; e (c) em que o
# trecho único considerado é o inicial (possui trip_id com _00)
ods_vgs <- sprintf('%s/03_rotas_originais_infraciclo_detour_carac_viagens.csv', pasta_orig_vs_mod)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'cccdddddccccdddddddd')
# Remover viagens com origem e destino no mesmo qgis_id
ods_vgs <- ods_vgs %>% filter(qgis_id.x != qgis_id.y)
ods_vgs <- ods_vgs %>% select(trip_id, qgis_id.x, qgis_id.y, lon.x, lat.x, lon.y, lat.y)
head(ods_vgs)

# Criar coluna com URL para GET no GraphHopper
route_options <- '&profile=bike&instructions=false&calc_points=true&details=osm_way_id'

ods_vgs <- 
  ods_vgs %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))

head(ods_vgs)


# Arquivo de saída
out_file <- sprintf('%s/04_ttmatrix_viagens_modeladas_a_partir_das_originais.csv', pasta_orig_vs_mod)

# Se arquivo de saída existe, desconsiderar viagens que já foram roteadas
if (file.exists(out_file)) {
  this <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
  this <- this %>% select(trip_id) %>% distinct()
  
  # Remover viagens que já foram roteadas
  ods_vgs <- ods_vgs %>% filter(!trip_id %in% this$trip_id)
  rm(this)
  
}


# No terminal, iniciar GraphHopper com .pbf da rede 2019 e config.yml sem alterações de LTS
# (base) livre@laika:/media/livre/HDINT1TB/Base_GtsRegionais$ clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml

# Para cada linha de origem e destino, gerar rotas modeladas com alternativas
detach("package:tidylog")

# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# se forem só as rotas originais, é ok rodar no RStudio
for (line in seq(1, nrow(ods_vgs))) {
  # Isolar linha do dataframe
  # line <- nrow(ods_vgs)
  tmp_df <- ods_vgs %>% slice(line)
  gh_route(tmp_df, out_file, route_options)
}

