# Cria um shape de semáforos, a partir do original da CET, para as vias de SP.
# Os semáforos são buffers a partir dos pontos originais, de forma a cobrir
# as intersecções como um todo - o buffer escolhido pode influenciar algumas
# vias menores (com semáforo) que chegam a maiores (sem semáforo), mas é preciso
# com que sejam grandes senão os que estão nas avenidas (um ponto só para todas
# as direções) não cobrem todas as possibilidades

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
dados_originais  <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_semaforos  <- sprintf("%s/04_semaforos_sp", pasta_dados)
dir.create(pasta_semaforos, recursive = TRUE, showWarnings = FALSE)


# Abrir dados originais de semáforos 
open_file <- sprintf('%s/202101_SemaforosSP_ListagemCET_1971-2020.csv', dados_originais)
semaforos <- read_delim(open_file, delim = ';', col_types = cols(.default = "c"))

# Simplificar nomes das colunas
names(semaforos) <- c('end', 'data_inicio', 'lat', 'lon')

# Corrigir grafia de latlong, que apresenta problemas com a separação decimal
semaforos <- semaforos %>% mutate(lat = str_replace(lat, '\\.', ''),
                                  lon = str_replace(lon, '\\.', ''),
                                  lat = as.double(str_replace(lat, '-23', '-23.')),
                                  lon = as.double(str_replace(lon, '-46', '-46.')))

# Criar a filtrar por ano de início de operação
semaforos <- 
  semaforos %>% 
  mutate(ano = as.numeric(str_sub(data_inicio, start = 7, end = 10))) %>% 
  filter(ano < 2019)


# Transformar em sf e criar um buffer a partir do ponto original
semaforos <- 
  semaforos %>% 
  # Criar shape em WGS84
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  # Transformar para SIRGAS para o buffer
  st_transform(31983) %>% 
  # Aplicar buffer de 25 m - alguns semáforos estão muito mal posicionados,
  # e só um buffer desse tamanho para fazê-los cobrir a intersecção toda
  st_buffer(25) %>% 
  # Transformar de volta para WGS84
  st_transform(4326)

# semaforos %>% mapview()


# Salvar arquivo
out_file <- sprintf('%s/semaforos_buffer25_sp.gpkg', pasta_semaforos)
st_write(semaforos, out_file, driver = 'GPKG', append = FALSE)


# Uma vez salvo, o arquivo vai passar pelo QGIS - o processo em R para fazer
# o "dissolve" dos buffers sobrepostos está a seguir, mas como demora muito e
# no QGIS vai rápido, fiz por lá. O processo no QGIS:
# 1. Abrir o arquivo e dar uma leve revisada na posição dos semáforos;
# 2. Vector > Geoprocessing Tools > Dissolve
# 3. Vector > Geometry Tools > Multiparts to Singleparts
# 4. Retirar colunas gerais para simplificar o shape e exportar como
# "semaforos_buffer25_sp_dissolved.gpkg"



# Processo no R (demora horrores para rodar)

# # Alguns semáforos muito próximos estão sobrepostos - unir em um buffer único:
# # https://gis.stackexchange.com/questions/323038/dissolve-only-overlapping-polygons-in-r-using-sf
# # 1. Criar um shape de contorno das bolas de buffer - demora MUITO para rodar -
# # como o resultado do st_union() é um MULTIPOLYGON, precisa ser separado em POLYGON
# contornos <- st_union(semaforos) %>% st_cast('POLYGON')
# 
# contornos <- contorno
# # 2. Definir a quais contornos cada buffer está relacionado
# clusters <- st_intersects(semaforos, contornos) %>% unlist()
# 
# # 3. "Dissolver" usando o group_by() e o summarize()
# semaforos_dissolved <-
#   # Inserir coluna relacionando semaforos aos clusters
#   cbind(semaforos, clusters) %>%
#   # Agrupar por cluster e dissolver
#   group_by(clusters) %>%
#   summarize(box = paste(box, collapse = ", "))
