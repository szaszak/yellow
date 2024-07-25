# Gerar áreas proporcionais dos setores censitários de acordo com suas interseções
# com os hexágonos h3 - resolução 09

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_censo       <- sprintf("%s/CENSO", dados_originais)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)


# ------------------------------------------------------------------------------
# Setore censitários - calcular área por setor
# ------------------------------------------------------------------------------

# Código do município de SP no Censo
cod_muni <- '3550308'

# Abrir shapefile de setores censitários
setores <- sprintf('%s/setores_censitarios_SP.shp', pasta_censo)

# Filtrar somente setores para a cidade de interesse
setores <- read_sf(setores) %>% filter(CD_GEOCODM == cod_muni) %>% select(cod_setor = CD_GEOCODI)

# Garantir que shapefile não apresente erros
setores <- setores %>% st_make_valid()

# Recalcular área do setor, a partir da projeção utilizada
setores <- setores %>% mutate(area_orig_setor_m2 = unclass(st_area(.)), .before = 'geometry')
# mapview(setores)
head(setores)


# ------------------------------------------------------------------------------
# Distribuir dados do Censo por hexágono do IPEA - RODAR NO JUPYTER
# ------------------------------------------------------------------------------

# ATENÇÃO: O st_intersection() vai demorar muitas horas no RStudio, além de usar
# toda a RAM e quase todo o swap estendido. No Jupyter, ele roda em alguns
# segundos, quase não usa a RAM nem o processador

# IPEA - Shape de grid hexagonal (resolução 9)
hexagonos <- sprintf('%s/aop_hex_grid_v2.gpkg', pasta_ipea)
hexagonos <- read_sf(hexagonos) %>% filter(abbrev_muni == 'spo') %>% select(id_hex)
# mapview(hexagonos)

# Caso projeções entre os dados do Censo e do grid de hexágonos sejam 
# diferentes, reprojetar para o CRS do primeiro
if (st_crs(hexagonos) != st_crs(setores)) {
  hexagonos <- hexagonos %>% st_transform(crs = st_crs(setores))
}
head(hexagonos)

# Fazer interseção entre setores do censo de hexágonos
setores_hex <- st_intersection(setores, hexagonos)

# Calcular área proporcional à interseção com o hexágonos
setores_hex <- setores_hex %>% mutate(area_setor_hex_m2 = as.numeric(st_area(.)), 
                                      area_prop = area_setor_hex_m2 / area_orig_setor_m2,
                                      .before = 'geometry')

# Ordenar para exportar
setores_hex <- setores_hex %>% arrange(cod_setor, id_hex)
head(setores_hex, 15)

# Gravar resultados
out_file <- sprintf('%s/hex_grid_sp_res09_areas_setores_censitarios.gpkg', pasta_aop_optimum)
st_write(setores_hex, out_file, driver = 'GPKG', append = FALSE)


# ------------------------------------------------------------------------------
# Checar resultados
# ------------------------------------------------------------------------------

# # TESTE DE INTERSEÇÃO DOS SETORES POR HEXÁGONO
# hex_selecionados <- c('89a8100eb0bffff', '89a8100eb43ffff', '89a8100eb47ffff',
#                       '89a8100eb4fffff', '89a8100eb57ffff', '89a8100eb73ffff',
#                       '89a8100eb7bffff')
# hex_sel <- hexagonos %>% filter(id_hex %in% hex_selecionados)
# 
# this <- st_intersection(setores, hex_sel)
# mapview(this)
# 
# # Calcular área proporcional à interseção com o hexágonos
# this <- this %>% mutate(area_setor_hex_m2 = as.numeric(st_area(.)), 
#                         area_prop = area_setor_hex_m2 / area_orig_setor_m2,
#                         .before = 'geometry')
# this %>% arrange(cod_setor, id_hex) %>% head(16)
# 
# 
# this %>% st_drop_geometry() %>% group_by(cod_setor) %>% tally() %>% filter(n > 1)
# this %>% filter(cod_setor == '355030860000035') %>% mapview() + mapview(this)
# 
# this %>%
#   filter(cod_setor == '355030860000035') %>%
#   select(1, 2, 5, 12:14) %>%
#   mutate(area_pos = as.numeric(st_area(.)), .before = 'geometry') %>%
#   st_drop_geometry() %>%
#   mutate(prop = area_pos / area_m2) %>%
#   mutate(pop_prop = round(prop * pop_res2))
# 
# # Calcular população por fração do setor censitário (conforme área de interseção
# # com os hexágonos)
# that <-
#   this %>%
#   # filter(cod_setor == '355030860000035') %>%
#   select(1, 2, 5, 12:14) %>%
#   mutate(area_pos = as.numeric(st_area(.)), .before = 'geometry') %>%
#   st_drop_geometry() %>%
#   mutate(prop = area_pos / area_m2) %>%
#   mutate(pop_prop = round(prop * pop_res2))
# 
# # Calcular população por hexágono
# that %>%
#   select(id_hex, matches('_prop')) %>%
#   group_by(id_hex) %>%
#   summarise_all(sum, na.rm = TRUE)