# Agrega as matrículas de escolas aos hexágonos h3 res 09

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_dados <- sprintf("%s/02_dados_pop_mat", pasta_aop_optimum)


# IPEA - Shape de grid hexagonal (resolução 9)
hexagonos <- sprintf('%s/aop_hex_grid_v2.gpkg', pasta_ipea)
hexagonos <- read_sf(hexagonos) %>% filter(abbrev_muni == 'spo') %>% select(id_hex)
# mapview(hexagonos)

# Censo escolar 2019 com matrículas
escolas <- sprintf('%s/matriculas_censo_escolar_2019_georref.gpkg', pasta_opaop_dados)
escolas <- st_read(escolas)
escolas <- escolas %>% select(CO_ENTIDADE, QT_MAT_MED, QT_MAT_BAS_15_17, geom)
head(escolas)


# Caso projeções entre os dados do Censo Escolar e do grid de hexágonos sejam 
# diferentes, reprojetar para o CRS do primeiro
if (st_crs(hexagonos) != st_crs(escolas)) {
  hexagonos <- hexagonos %>% st_transform(crs = st_crs(escolas))
}
head(hexagonos)


# Fazer interseção entre as escolas e os hexágonos - RODAR NO JUPYTER
escolas_hex <- st_intersection(escolas, hexagonos)
head(escolas_hex)

# # Gravar resultados
# out_file <- sprintf('%s/matriculas_censo_escolar_2019_hex_grid.gpkg', pasta_opaop_dados)
# st_write(escolas_hex, out_file, driver = 'GPKG', append = FALSE)


# Agrupar matrículas por hexágono
escolas_hex <- 
  escolas_hex %>% 
  st_drop_geometry() %>% 
  group_by(id_hex) %>% 
  summarise(matriculas_ensino_medio = sum(QT_MAT_MED),
            matriculas_idades_15_17 = sum(QT_MAT_BAS_15_17))

head(escolas_hex)


# Conferência de valores
sum(escolas$QT_MAT_MED)
sum(escolas$QT_MAT_BAS_15_17)
sum(escolas_hex$matriculas_ensino_medio)
sum(escolas_hex$matriculas_idades_15_17)


# Gravar resultados
out_file <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.csv', pasta_opaop_dados)
write_delim(escolas_hex, out_file, delim = ';')


# Juntar ao grid de hexágonos para exportação
hex_mat <- 
  left_join(hex_mat, escolas_hex, by = 'id_hex') %>% 
  replace(is.na(.), 0) %>% 
  relocate(geom, .after = last_col())

head(hex_mat)

# Gravar resultados
out_file2 <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.gpkg', pasta_opaop_dados)
st_write(hex_mat, out_file2, driver = 'GPKG', append = FALSE)