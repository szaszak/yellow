# Para rodar este script, inicialmente o teste seria o de gerar um novo .pbf para
# 2019 com as características do LTS, removendo algumas vias a partir das tags
# de bicycle=no (ver script (11.X1-gerar_pbf_2019_para_rotas_modeladas_com_alternativas.R)
# nos arquivos. Deu bem errado pois essas tags estão marcadas em locais onde não
# deveriam estar e isso alterava muito os resultados. A solução foi criar um
# modelo alterado de velocidades (20230521_modelo_velocidades_SP_LTS.json) no
# qual as vias com LTS alto - rodovia,s VTR - passaram a ter um limite máximo
# de velocidades. O mesmo aconteceu com trechos de subidas muito íngremes. Na
# prática, portanto, o .pbf resultante ficou praticamente idêntico ao da pasta
# yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019.
# 
# Para rodar esse scripts, poderia ser qualquer um dos .PBFs, mas o arquivo que 
# é usado junto com o graphhopper tem que ser o config-example_LTS.yml, que por
# sua vez aponta para o modelo de velocidades 20230521_modelo_velocidades_SP_LTS.json.
# 
# No testar, no terminal:
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example_LTS.yml
# 
# E abrir no navegador:
# http://localhost:8989/


library('tidyverse')
library('tidylog')
library('httr')
library('jsonlite')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)
pasta_orig_modalt  <- sprintf("%s/11_rotas_modeladas_com_alternativas", pasta_dados)
pasta_osm_way_ids  <- sprintf("%s/01_osm_way_ids_rotas_modeladas", pasta_orig_modalt)
pasta_rotas_modalt <- sprintf("%s/02_rotas_modeladas_alternatives", pasta_orig_modalt)
dir.create(pasta_osm_way_ids, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_rotas_modalt, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos com rotas alternativas (até 3 por par OD)
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe,
# com rotas até 3 alternativas por par OD - aceita um dataframe de uma linha como
# entrada
gh_route_alt <- function(tripid, out_file, route_options) {
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
  df_line <- ods_vgs %>% filter(trip_id == tripid)
  
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
      osm_ways <- paths$details$osm_way_id[i] %>% as.data.frame()
      
      if (nrow(osm_ways) > 0) {
        # TODO: Inserir número da rota alternativa aqui
        osm_ways <- osm_ways %>% mutate(index = row_number()) %>% select(index, osm_way_id = X3)
        
        # Gravar resultados
        osm_way_out <- sprintf('%s/%s_%i.csv', pasta_osm_way_ids, df_line$trip_id, i)
        write_delim(osm_ways, osm_way_out, delim = ';')
        
      } else {
        # Se não há osm_way_ids, é porque os pontos estão muito próximos uns dos
        # outros, mesmo que seja um osm_way_id diferente entre a origem e o 
        # destino. A distância e a velocidade calculadas vão ser zero também - 
        # pular este registro, que vai ser vazio
        return(sprintf('Pulando: %s não tem osm_way_ids (provavelmente tem distância = 0)', tripid))
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
  tmp_file <- sprintf('%s/%s_%i_modalt.csv', pasta_rotas_modalt, df_line$trip_id, i)
  write_delim(paths, tmp_file, delim = ';')
  
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
# head(ods_vgs)

# Criar coluna com URL para GET no GraphHopper
route_options <- '&profile=bike&instructions=false&calc_points=true&algorithm=alternative_route&details=osm_way_id'

ods_vgs <- 
  ods_vgs %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, route_options,
                     sep = ''))

head(ods_vgs)



# Para cada linha de origem e destino, gerar rotas modeladas com alternativas
detach("package:tidylog")

# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# se forem só as rotas originais, é ok rodar no RStudio
for (tripid in ods_vgs$trip_id) {
  gh_route_alt(tripid)
}
# for (line in seq(1, nrow(ods_vgs))) {
#   # Isolar linha do dataframe
#   # line <- nrow(ods_vgs)
#   tmp_df <- ods_vgs %>% slice(line)
#   gh_route_alt(tmp_df, out_file, route_options)
# }


# ------------------------------------------------------------------------------
# Juntar todos os resultados
# ------------------------------------------------------------------------------

# Arquivo de saída
out_file <- sprintf('%s/01_ttmatrix_rotas_modeladas_de_viagens_originais_com_alternativas.csv', pasta_orig_modalt)

# Listar todos os arquivos de resultados em um dataframe único
arqs_resultados <- data.frame(arq = list.files(pasta_rotas_modalt, recursive = FALSE, full.names = TRUE))

for (arq in arqs_resultados$arq) {
  # Abrir arquivo de resultados
  arq <- read_delim(arq, delim = ';', col_types = cols(.default = "c"))
  
  # Guardar resultados 
  if (file.exists(out_file)) {
    write_delim(arq, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(arq, out_file, delim = ';', append = FALSE)
  }
  
}


# Abrir resultados ordenar por trip_od e weight
resultados_final <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
resultados_final <- resultados_final %>% arrange(trip_id, weight)
head(resultados_final)

write_delim(resultados_final, out_file, delim = ';')
