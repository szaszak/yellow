#

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
pasta_osm_sp     <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao   <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)


# Abrir arquivo com as vias de SP do OSM
open_file1 <- sprintf('%s/sao_paulo_osm_filtrado.gpkg', pasta_osm_sp)
vias_sp    <- read_sf(open_file1)


# Abrir arquivo com as linhas de curva para SP
open_file2 <- sprintf('%s/geosampa_curvas_de_elevacao.rds', pasta_elevacao)
curvas_sp  <- read_rds(open_file2) %>% select(ISOVALOR)
head(curvas_sp)



# Henrique Schaumann
via_teste <- vias_sp %>% filter(str_detect(name, 'Schaumann')) %>% st_buffer(dist = 3) %>% st_union()
via_teste %>% mapview()



# Amostra de vias de teste - Indianópolis/Moema
# amostra_vias <- vias_sp %>% slice(3050:3100) %>% st_centroid()
amostra_vias <- vias_sp %>% slice(3090:3100) #%>% st_centroid()
amostra_vias %>% mapview()

centroides_vias <- amostra_vias %>% st_centroid()
centroides_vias %>% mapview()


idx_cmp <- st_nearest_feature(centroides_vias, curvas_sp)
curvas_sp %>% slice(idx_cmp)


curvas_point <- curvas_sp %>% st_cast('POINT') %>% distinct()



amostra_buffer <- amostra_vias %>% st_buffer(dist = 10) %>% st_union()
amostra_buffer %>% mapview()

curvas_moema <- curvas_sp %>% filter(DISTRITO == 'MOEMA') %>% st_cast('POINT') %>% distinct()
# curvas_moema %>% st_cast('LINESTRING') %>% mapview()

# Recortar as curvas do shape do OSM que estão dentro do buffer
curvas_recortadas <- curvas_moema %>% filter(st_intersects(curvas_moema, amostra_buffer, sparse = FALSE))
curvas_recortadas %>% mapview()


centroides_vias <- amostra_vias %>% st_centroid()
centroides_vias %>% mapview()


# Pegar a curva (feature) mais próxima ao centróide do hexágono - o
# resultado é o index da feature mais próxima
idx_cmp <- st_nearest_feature(centroides_vias, curvas_recortadas)

curvas_recortadas %>% slice(idx_cmp)
curvas_recortadas %>% slice(idx_cmp) %>% mapview()



















# Amostra de vias de teste - Indianópolis/Moema
# amostra_vias <- vias_sp %>% slice(3050:3100) %>% st_centroid()
amostra_vias <- vias_sp %>% slice(3090:3100) #%>% st_centroid()
amostra_vias %>% mapview()

amostra_buffer <- amostra_vias %>% st_buffer(dist = 10) %>% st_union()
amostra_buffer %>% mapview()

curvas_moema <- curvas_sp %>% filter(DISTRITO == 'MOEMA') %>% st_cast('POINT') %>% distinct()
# curvas_moema %>% st_cast('LINESTRING') %>% mapview()

# Recortar as curvas do shape do OSM que estão dentro do buffer
curvas_recortadas <- curvas_moema %>% filter(st_intersects(curvas_moema, amostra_buffer, sparse = FALSE))
curvas_recortadas %>% mapview()


centroides_vias <- amostra_vias %>% st_centroid()
centroides_vias %>% mapview()


# Pegar a curva (feature) mais próxima ao centróide do hexágono - o
# resultado é o index da feature mais próxima
idx_cmp <- st_nearest_feature(centroides_vias, curvas_recortadas)

curvas_recortadas %>% slice(idx_cmp)
curvas_recortadas %>% slice(idx_cmp) %>% mapview()



