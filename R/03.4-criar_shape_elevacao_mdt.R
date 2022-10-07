# Pega o arquivo de viário do OpenStreetMap que passou pelo processo de associação
# com o raster de MDT de SP e puxa os dados de elevação para uma coluna única


# Para rodar este script, vamos precisar do shapefile com o viário do OSM para
# São Paulo e do mapa MDT mais recente da cidade, feito a partir do levantamento
# de LiDAR em 2017. O processo de associar as duas bases é feito no QGIS:
# 
# 1. Abrir o raster do MDT e o viário do OSM no QGIS;
# 2. Rodar Processing > Processing Toolbox > Drape sem mexer nos parâmetros padrão
# 3. O resultado é o shapefile de viário com os dados de elevação, que é o shape
# sao_paulo_osm_filtrado_com_elevacao_mdt.gpkg


# # Links úteis para este processo
# Digital Terrain and Surface Models of São Paulo (Fernando Gomes)
# 50cm resolution models from LiDAR 3D survey in 2017
# https://www.kaggle.com/datasets/andasampa/dtm-dsm-sao-paulo/code?resource=download
# 
# Quando usar o Modelo Digital de Superfície (MDS) e Modelo Digital de Terreno (MDT)
# (queremos o MDT)
# https://www.linkedin.com/pulse/quando-usar-o-modelo-digital-de-superf%C3%ADcie-mds-e-mdt-geotecnologias?articleId=6704108970862292993
# https://youtu.be/jlExQjFI4_Y
# 
# Obtaining elevation data for points using QGIS
# https://gis.stackexchange.com/questions/344092/obtaining-elevation-data-for-points-using-qgis
# 
# How do I bring the Z value to attribute table?
# z($geometry)# 
# https://gis.stackexchange.com/questions/321517/how-do-i-bring-the-z-value-to-attribute-table



# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados        <- "../yellow_dados"
pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
dir.create(pasta_elevacao, recursive = TRUE, showWarnings = FALSE)


# Abrir shape com viário do OpenStreetMap com valoz Z incluído, vindo do MDT de SP
pontos_elevacao <- sprintf('%s/sao_paulo_osm_filtrado_com_elevacao_mdt.gpkg', pasta_osm_sp)
pontos_elevacao <- read_sf(pontos_elevacao)

# Transformar linhas em pontos para serem usados como nearest neighbor no momento
# do map matching
pontos_elevacao <- pontos_elevacao %>% st_cast('POINT') 


# Puxar dado de elevação (Z, vindo do MDT)
pontos_elevacao <- 
  pontos_elevacao %>% 
  # Separar a altimetria (atributo Z) e deixá-la em uma coluna única
  separate(geom, into = c('lon', 'lat', 'elev_mdt'), sep = ' ', remove = FALSE) %>% 
  # Descartar colunas que não serão usadas
  select(-c(lon, lat, name, highway, infra_ciclo)) %>% 
  # Limpar caracteres de string e transformar coluna de elevação em double. Vamos 
  # também arredondar para 3 casas decimais pois não precisaremos mais que isso
  mutate(elev_mdt = str_replace(elev_mdt, '\\)', ''),
         elev_mdt = round(as.double(elev_mdt), 3)) %>% 
  # Retirar pontos duplicados (cerca de mil pontos)
  distinct()



# Salvar arquivo com todas as curvas
out_file1 <- sprintf('%s/viario_osm_com_elevacao_mdt.gpkg', pasta_elevacao)
# write_rds(curvas_sp, sprintf('%s.rds', out_file), compress = 'gz')
st_write(pontos_elevacao, out_file1, driver = 'GPKG', append = FALSE)


# Retirar dimensão Z da geometria, pois não será usada no momento do map matching
pontos_elevacao <- pontos_elevacao %>% st_zm(drop = TRUE)

# Salvar arquivo com todas as curvas
out_file2 <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z.gpkg', pasta_elevacao)
# write_rds(curvas_sp, sprintf('%s.rds', out_file), compress = 'gz')
st_write(pontos_elevacao, out_file2, driver = 'GPKG', append = FALSE)