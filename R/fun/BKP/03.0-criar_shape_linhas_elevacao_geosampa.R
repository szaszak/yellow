# Junta todos os arquivos de linhas de curva intermediárias e mestras do Geosampa,
# distribuídos por distritos, em um único dataframe para a cidade de SP inteira

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_geosampa     <- "/home/livre/Desktop/Base_GtsRegionais/Arquivos_Geosampa/MEIOFISICO"
pasta_curva_interm <- sprintf("%s/SIRGAS_SHP_Curva_Intermediaria", pasta_geosampa)
pasta_curva_mestra <- sprintf("%s/SIRGAS_SHP_Curva_Mestra", pasta_geosampa)
pasta_dados        <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_temporaria   <- sprintf("%s/tmp_elevacao", pasta_elevacao)
dir.create(pasta_temporaria, recursive = TRUE, showWarnings = FALSE)


# ----------------------------------------
# 1. Descomprimir os arquivos Geosampa
# ----------------------------------------
# Deszipar os arquivos do Geosampa usando o 7z - o 7z é melhor para esta função
# pois não cai em problemas de caracteres com encodings bizarros
descomprimir_arquivos <- function(x, base_folder) {
  sevenz_path <- sprintf("/usr/bin/7z")
  arg1 <- sprintf('-y x %s/%s', base_folder, x)
  arg2 <- sprintf('-o%s/', pasta_temporaria)
  system2(command = sevenz_path, args = c(arg1, arg2))
}

# Descomprimir linhas de curvas intermediárias
zip_files_curvas_inter <- list.files(pasta_curva_interm, pattern = ".zip")
walk(zip_files_curvas_inter, descomprimir_arquivos, pasta_curva_interm)

# Descomprimir linhas mestras
zip_files_curvas_mestras <- list.files(pasta_curva_mestra, pattern = ".zip")
walk(zip_files_curvas_mestras, descomprimir_arquivos, pasta_curva_mestra)


# ----------------------------------------
# 2. Abrir arquivos de linhas de curva
# ----------------------------------------
# Abrir shapes de linhas de curva intermediária
curvas_intermediarias <- list.files(pasta_temporaria, 
                                    pattern = "^SIRGAS_SHP_curvaintermediaria_linha_(.+).shp", 
                                    recursive = TRUE, 
                                    full.names = TRUE)

curvas_inter <- curvas_intermediarias %>% map(read_sf)

# Transformar todos em MULTILINESTRING (alguns não estão)
curvas_inter <- curvas_inter %>% map(st_cast, to = 'MULTILINESTRING')

# Juntar tudo em um dataframe único
curvas_inter <- curvas_inter %>% rbindlist() %>% st_as_sf() %>% rename(DISTRITO = distrito)



# Abrir shapes de linhas de curva mestra
curvas_mestras <- list.files(pasta_temporaria,
                             pattern = "^SIRGAS_SHP_curvamestra_linha_(.+).shp", 
                             recursive = TRUE, 
                             full.names = TRUE)

curvas_mestr <- curvas_mestras %>% map(read_sf)

# Transformar todos em MULTILINESTRING (alguns não estão)
curvas_mestr <- curvas_mestr %>% map(st_cast, to = 'MULTILINESTRING')

# Juntar tudo em um dataframe único
curvas_mestr <- curvas_mestr %>% rbindlist() %>% st_as_sf()



# ----------------------------------------
# 3. Juntar, salvar e apagar temporários
# ----------------------------------------

# Juntar arquivos de linhas de curvas mestras e intermediárias em um único
curvas_sp <- rbind(curvas_inter, curvas_mestr)

# Averiguar quais são as curvas cujos geometries são ou não válidos
curvas_filter <- st_is_valid(curvas_sp)

# Retirar curvas com geometries não válidos
curvas_sp <- curvas_sp %>% cbind(curvas_filter)
curvas_sp <- curvas_sp %>% filter(curvas_filter == TRUE) %>% select(-c(ESCALA, SCM, SETFIS, curvas_filter))


# Transformar projeção de SIRGAS para WGS84
curvas_sp <- curvas_sp %>% st_sf(crs = 31983) %>% st_transform(4326)



# Salvar arquivo com todas as curvas
out_file <- sprintf('%s/geosampa_curvas_de_elevacao', pasta_elevacao)
write_rds(curvas_sp, sprintf('%s.rds', out_file), compress = 'gz')
st_write(curvas_sp, sprintf('%s.gpkg', out_file), driver = 'GPKG', append = FALSE)


# Remover pasta com os arquivos temporários
unlink(pasta_temporaria, recursive = TRUE)


# ------------------------------------------------------------------------------
# Com dados do Geosampa - este processo foi substituído pelo seguinte
# ------------------------------------------------------------------------------

# Este é um arquivo de placeholder só para descrever o processo pelo qual o
# arquivo 'geosampa_curvas_de_elevacao.gpkg', resultante do script anterior,
# passou para o passo seguinte.

# O que precisamos é simplificar essa base de dados das curvas de elevação. Para
# isso, queremos somente os pontos de intersecção entre essas curvas e o viário
# de SP do OSM. O problema é que o R faria isso somente com o sf_intersection(),
# que demora MUITO para rodar, tornando-o inviável neste caso.

# Um post no stackoverflow sufere usar o pacote sfnetworks para fazer isso. Não
# cheguei a testar mas ficam os links de registro:
# https://gis.stackexchange.com/questions/414910/split-linestrings-at-pairwise-intersections
# https://luukvdmeer.github.io/sfnetworks/reference/spatial_morphers.html
# https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html#subdivide-edges
# 
# O código relevante seria o seguinte:
# network <- as_sfnetwork(linestrings) %>%
#   activate(edges) %>%
#   arrange(edge_length()) %>%
#   convert(to_spatial_subdivision)

# O que no R demoraria muito, no QGIS é uma ação trivial: usei as duas camadas
# (de curvas de nível e de viário do OSM) e rodei o Vector > Analysis Tools >
# Line Intersecions. Em questão de minutos rodou tudo, gerando o shapefile
# resultante com os pontos dos locais onde as linhas de curva interseccionam
# com o viário, com os dados de altimetria.
