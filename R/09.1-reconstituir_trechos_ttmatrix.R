library('tidyverse')
library('tidylog')
library('sf')
library('data.table')
library('mapview')
library('googlePolylines')
# Mostra valores sem notação científica
options(scipen = 999)


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_ttmatrix_2019  <- sprintf("%s/04_ttmatrix_rede_2019", pasta_graphhopper)
pasta_gh_ttmatrix_2028  <- sprintf("%s/06_ttmatrix_rede_2028", pasta_graphhopper)
pasta_rotas_ttmatrix   <- sprintf("%s/09_rotas_ttmatrix/01_trechos_agrupados", pasta_dados)
dir.create(pasta_rotas_ttmatrix, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Funções para processar e agrupar os trechos percorridos ao gerar a ttmatrix
# ------------------------------------------------------------------------------

# Segmenta a base em intervalos para processar group_by() de pontos de origem e destino
agrupar_arcos_ttmatrix <- function(arq, ano_base, time_min = 2400, interval = 25000) {
  
  # arq <- arqs_ttmatrix_2019[1]; ano_base <- '2019'
  # time_min <- 2400; interval <- 25000;
  print(arq)
  
  # Abrir arquivo resultante da ttmatrix e renomear colunas
  rotas <- read_delim(arq, delim = ';', col_types = 'ddddcc', col_names = FALSE)
  names(rotas) <- c('distance', 'weight', 'time', 'speed', 'poly', 'url')
  # head(rotas)
  
  # Manter somente rotas a menos de X segundos (2400 = 40 minutos)
  rotas <- rotas %>% filter(time <= time_min)
  # Vamos precisar somente do trajeto
  rotas <- rotas %>% select(poly) # %>% add_column(index_col = 1:nrow(.))
  # head(rotas)
  
  # Definir arquivo de saída, onde processamento será argupado, e rodar
  file_basename <- str_split(arq, '/')[[1]][[length(str_split(arq, '/')[[1]])]]
  file_basename <- file_basename %>% str_replace('.csv', '')
  out_file <- sprintf('%s/%s_%s_rotas_agrupadas_tmp.csv', pasta_rotas_ttmatrix, file_basename, ano_base)
  
  
  # Processar quantidade de linhas conforme definido por 'interval' (padrão 25000)
  for (i in seq(1, nrow(rotas), interval)) {
    # i <- 675001; lala <- sprintf('%s - %s', 1, 1 + interval - 1)
    lala <- sprintf('%s - %s', i, i + interval - 1)
    print(lala)
    
    # Segmentar dataframe para rodar mais rápido
    this <- rotas %>% slice(i:(i + interval - 1))
    # head(this)
    
    # Transformar polyline em linhas de latlon
    this <- decode(as.character(this$poly))
    
    # Juntar tudo em um dataframe único
    this <- this %>%
      map(mutate,
          # Corrigir eventuais problemas nas strings de latlon
          lat = str_replace(lat, '\\.', ''),
          lon = str_replace(lon, '\\.', ''),
          lat = str_replace(lat, '-23', '-23.'),
          lon = str_replace(lon, '-46', '-46.')) %>%
      map(mutate,
          # Simplificar colunas de lat e lon para conterem só 9 caracteres
          lat = str_sub(lat, 1, 9),
          lon = str_sub(lon, 1, 9),
          # Criar colunas de latlon referentes ao destino
          lat_to = shift(lat, type = 'lead'),
          lon_to = shift(lon, type = 'lead')) %>%
      bind_rows() %>%
      # Eliminar as últimas linhas de cada trajeto, que ficaram só com as origens
      filter(!is.na(lat_to))
    
    # Agrupar linhas iguais, mantendo a quantidade de vezes em que cada uma apareceu
    this <- this %>% group_by(lat, lon, lat_to, lon_to) %>% tally() %>% ungroup()
    
    # Gravar resultados
    if (file.exists(out_file)) {
      write_delim(this, out_file, delim = ';', append = TRUE)
    } else {
      write_delim(this, out_file, delim = ';', append = FALSE)
    }
    
  }
  
}


# Agrupa todos os resultados da função anterior por grupo de latlon e latlon_to
finalizar_agrupamento_arcos <- function(f_pattern) {
  
  # Abrir todos os arquivos temporários resultantes do processo anterior
  arqs_temp <- list.files(pasta_rotas_ttmatrix, 
                          pattern = f_pattern, 
                          recursive = FALSE, 
                          full.names = TRUE)
  
  # Abrir todos os arquivos e juntar em um único dataframe
  rotas <- 
    lapply(X = arqs_temp, 
           FUN = read_delim, 
           delim = ';', 
           col_types = 'cccci') %>% 
    rbindlist(fill = TRUE)
  
  # head(rotas)
  
  # Agrupar linhas iguais, mantendo a quantidade de vezes em que cada uma apareceu
  rotas <- 
    rotas %>% 
    group_by(lat, lon, lat_to, lon_to) %>% 
    summarize(n = sum(n), .groups = 'keep') %>% 
    ungroup()
  
}


# Transforma dataframe com colunas de latlon e latlon_to em geopackage e salva
rotas_para_sf <- function(df) {
  
  # Transformar cada linha do dataframe em uma linha de shapefile
  # https://stackoverflow.com/questions/51918536/r-create-linestring-from-two-points-in-same-row-in-dataframe
  df$geom = sprintf("LINESTRING(%s %s, %s %s)", df$lon, df$lat, df$lon_to, df$lat_to)
  df <- df %>% st_as_sf(wkt = 'geom', crs = 4326) %>% st_transform(crs = 31983)
  # head(df)
  # df %>% mapview()
  # df %>% slice(1) %>% mapview()
  
  # Criar coluna de index e manter só as de interesse
  df <- df %>% select(n) %>% add_column(index_col = 1:nrow(.), .before = 'n')
  # head(df)
  
}


# ------------------------------------------------------------------------------
# Processar ttmatrixes de 2019 e 2028
# ------------------------------------------------------------------------------

detach("package:tidylog")

# Listar arquivos resultantes da ttmatrix da rede de 2019
f_pattern <- '^ttmatrix_res09_0[1-7].csv'
arqs_ttmatrix_2019 <- list.files(pasta_gh_ttmatrix_2019, 
                                 pattern = f_pattern, 
                                 recursive = FALSE, 
                                 full.names = TRUE)

# Agrupar todos os trechos nos quais houve passagem - rede 2019
lapply(arqs_ttmatrix_2019, agrupar_arcos_ttmatrix, ano_base = '2019', time_min = 2400)


# Listar arquivos resultantes da ttmatrix da rede de 2028
f_pattern <- '^ttmatrix_rede2028_res09_0[1-7].csv'
arqs_ttmatrix_2028 <- list.files(pasta_gh_ttmatrix_2028, 
                                 pattern = f_pattern, 
                                 recursive = FALSE, 
                                 full.names = TRUE)

# Agrupar todos os trechos nos quais houve passagem - rede 2028
lapply(arqs_ttmatrix_2028, agrupar_arcos_ttmatrix, ano_base = '2028', time_min = 2400)

# Limpar ambiente
gc(T)


# ------------------------------------------------------------------------------
# Agrupar resultados do processamento - ttmatrixes de 2019
# ------------------------------------------------------------------------------

# Listar arquivos resultantes da ttmatrix da rede de 2028
ano_base   <- '2019'
f_pattern  <- sprintf('^ttmatrix_res09_0[1-7]_%s_rotas_agrupadas_tmp.csv', ano_base)
rotas_2019 <- finalizar_agrupamento_arcos(f_pattern)
head(rotas_2019)

# Gravar resultados
out_arq <- sprintf('%s/01_ttmatrix_res09_%s_rotas_agrupadas.csv', pasta_rotas_ttmatrix, ano_base)
write_delim(rotas_2019, out_arq, delim = ';')


# Gerar gpkg dos resultados
rotas_2019 <- rotas_para_sf(rotas_2019)
head(rotas_2019)

# Gravar resultados
out_arq <- sprintf('%s/02_ttmatrix_res09_%s_rotas_agrupadas.gpkg', pasta_rotas_ttmatrix, ano_base)
st_write(rotas_2019, out_arq, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Agrupar resultados do processamento - ttmatrixes de 2028
# ------------------------------------------------------------------------------

# Listar arquivos resultantes da ttmatrix da rede de 2028
ano_base   <- '2028'
f_pattern  <- sprintf('^ttmatrix_rede2028_res09_0[1-7]_%s_rotas_agrupadas_tmp.csv', ano_base)
rotas_2028 <- finalizar_agrupamento_arcos(f_pattern)
head(rotas_2028)

# Gravar resultados
out_arq <- sprintf('%s/01_ttmatrix_res09_%s_rotas_agrupadas.csv', pasta_rotas_ttmatrix, ano_base)
write_delim(rotas_2028, out_arq, delim = ';')


# Gerar gpkg dos resultados
rotas_2028 <- rotas_para_sf(rotas_2028)
head(rotas_2028)

# Gravar resultados
out_arq <- sprintf('%s/02_ttmatrix_res09_%s_rotas_agrupadas.gpkg', pasta_rotas_ttmatrix, ano_base)
st_write(rotas_2028, out_arq, driver = 'GPKG', append = FALSE)



