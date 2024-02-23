library('tidyverse')
library('tidylog')
library('httr')
library('jsonlite')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_detours      <- sprintf('%s/10_detours', pasta_dados)


# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe
gh_route <- function(url, out_file) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  # url <- ods_vgs %>% slice(1) %>% select(url) %>% pull()
  
  # Fazer a GET de roteamento no Grahphopper
  # print(url)
  gh_response <- GET(url)
  
  # Encurtar url para guardar no dataframe de resultado
  url <- 
    url %>% 
    str_replace('http:\\/\\/localhost:8989\\/route\\/\\?point=', '') %>% 
    str_replace('&point=', ';') %>%
    str_replace_all('%2C', ',') %>% 
    str_replace('&profile=bike&instructions=false&calc_points=true&details=average_speed', '')
  
  # Mensagem tem que ser "Success: (200) OK"
  if (http_status(gh_response)$message == 'Success: (200) OK') {
    
    # Resposta da query, já colapsada e transformada em dataframe
    # Remover aviso de 'No encoding supplied: defaulting to UTF-8' na linha fromJSON()
    suppressMessages(
      response_text <- 
        # Ignorar aviso 'argument is not an atomic vector; coercing'
        suppressWarnings(str_c(gh_response, collapse = ", ")) %>% 
        # Concatenar toda a string de resultados
        str_c("[", ., "]") %>% 
        # Transformar em dataframe
        fromJSON() %>% 
        as.data.frame()
    )
    
    # Nos interessa a coluna de 'paths', como um novo dataframe
    paths <- response_text$paths %>% as.data.frame()
    
    # Isolar colunas de interesse
    paths <- 
      paths %>% 
      # Calcular tempo em segundos e velocidade média
      mutate(time = time / 1000,
             speed = distance / time * 3.6) %>% 
      # Descartar colunas extras - a coluna poly é o shape da rota traçada
      select(distance, weight, time, speed, poly = points) %>% 
      mutate(url = url)
    
  } else {
    
    # Se a query no GraphHopper não deu resultados, guardar como dataframe vazio
    paths <- data.frame(distance = NA,
                        weight   = NA,
                        time     = NA,
                        speed    = NA,
                        poly     = NA,
                        url      = url
    )
    
  }
  
  # Guardar resultados temporários
  if (file.exists(out_file)) {
    write_delim(paths, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(paths, out_file, delim = ';', append = FALSE)
  }
  
}


juntar_ods <- function(df, out_file) {
  # df <- df_part
  
  # Abrir arquivo resultante da geração de rotas
  tmp_file <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
  
  # Selecionar colunas de interesse no df original, com origens e destinos
  df <- df %>% select(trip_id, qgis_id.x, qgis_id.y)
  
  # Juntar os dois dataframes
  if (nrow(tmp_file) == nrow(df)) { 
    df <- cbind(df, tmp_file) 
  } else {
      print('Quantidades de linhas entre arquivo gerado pelo GraphHopper e dataframe diferem. Checar!')
    }
  
}

# ------------------------------------------------------------------------------
# Routing a partir de dois pontos
# ------------------------------------------------------------------------------

# Abrir origens e destinos das rotas iniciais da Yellow
ods_vgs <- sprintf('%s/02_origens_e_destinos_com_latlon.csv', pasta_detours)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = cols(.default = "c"))
head(ods_vgs)

# Criar coluna com URL para GET no GraphHopper
ods_vgs <- 
  ods_vgs %>% 
  mutate(url = paste('http://localhost:8989/route/?point=', 
                     lat.x, '%2C', lon.x, '&point=', 
                     lat.y, '%2C', lon.y, '&profile=bike&instructions=false&calc_points=true&details=average_speed',
                     sep = ''))

head(ods_vgs)

# # Testar routing com GraphHopper
# out_file1 <- sprintf('%s/03_ttmatrix_teste_tmp.csv', pasta_detours)
# out_file2 <- sprintf('%s/03_ttmatrix_teste.csv', pasta_detours)
# df_part <- ods_vgs %>% slice(1:10)
# lapply(df_part$url, gh_route, out_file1)
# 
# df_part <- juntar_ods(df_part, out_file1)
# write_delim(df_part, out_file2, delim = ';')
# file.remove(out_file1, out_file2)
# rm(df_part, out_file1, out_file2)


# Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter
detach("package:tidylog")
out_file1 <- sprintf('%s/03_ttmatrix_viagens_originais_tmp.csv', pasta_detours)
out_file2 <- sprintf('%s/03_ttmatrix_viagens_originais.csv', pasta_detours)
lapply(ods_vgs$url, gh_route, out_file1)

ods_vgs <- juntar_ods(ods_vgs, out_file1)
head(ods_vgs)
write_delim(ods_vgs, out_file2, delim = ';')
# file.remove(out_file1)
