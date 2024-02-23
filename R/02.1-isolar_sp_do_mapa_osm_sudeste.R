# Recorta a cidade de São Paulo do mapa Sudeste do OpenStreetMap

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_geral_tiles    <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/valhalla_tiles_sp"
pasta_valhalla_conf  <- sprintf("%s/conf", pasta_geral_tiles)
pasta_valhalla_pbf   <- sprintf("%s/pbf", pasta_geral_tiles)
pasta_valhalla_elev  <- sprintf("%s/elevation_tiles", pasta_geral_tiles)
pasta_valhalla_tiles <- sprintf("%s/valhalla_tiles", pasta_geral_tiles)
dir.create(pasta_valhalla_conf, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_valhalla_pbf, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_valhalla_elev, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_valhalla_tiles, recursive = TRUE, showWarnings = FALSE)



# ----------------------------------------
# 1. Baixar arquivo PBF e criar o .poly
# ----------------------------------------

# Baixar o arquivo PBF da região Sudeste do Brasil manualmente do site 
# http://download.geofabrik.de/south-america/brazil/sudeste.html e 
# salvar na pasta pasta 'valhalla_pbf'. Renomear para que fique clara a data
# da última modificação do arquivo, no formato '20220216_sudeste-latest.osm.pbf'

# Também será preciso o polyline da cidade de SP. Abaixo seguem links para 
# gerá-lo. Uma vez criado, deve ser colocado na mesma pasta 'valhalla_pbf'
# com o nome de 'SAO_PAULO.poly'.

# Links úteis e passo a passo:
# Sobre o formato .poly para ser usado: 
# https://wiki.openstreetmap.org/wiki/Osmconvert, 
# https://wiki.openstreetmap.org/wiki/Osmosis/Polygon_Filter_File_Format

#  Para usar o osmosis, é preciso criar um polígono no formato .poly de são 
#  paulo. O modo mais fácil é:
#  1..Abrir o mapa dos limites administrativos de SP do Geosampa no QGIS;
#  2. Instalar um plugin chamado "Export OSM Poly, que aparece como "osmpoly_export";
#  3. No botão do plugin que aparece no menu, exportar a camada no formato .poly;
#  4. Por conveniência, renomear o polígono "SAO PAULO.poly" para "SAO_PAULO.poly".


# Atualizar os nomes dos arquivos aqui antes de continuar:
poly_file    <- sprintf('%s/SAO_PAULO.poly', pasta_valhalla_pbf)
# pbf_file     <- sprintf('%s/20220216_sudeste-latest.osm.pbf', pasta_valhalla_pbf)
# out_pbf_file <- sprintf('%s/20220216_sao_paulo.osm.pbf', pasta_valhalla_pbf)
# Pegar última versão dos arquivos, com base no nome do arquivo
pbf_file     <- list.files(pasta_valhalla_pbf, pattern = "20220216_sudeste-latest.osm.pbf")[-1]
pbf_file     <- sprintf('%s/%s', pasta_valhalla_pbf, pbf_file)
out_pbf_file <- list.files(pasta_valhalla_pbf, pattern = "20220216_sao_paulo.osm.pbf")[-1]
out_pbf_file <- sprintf('%s/%s', pasta_valhalla_pbf, out_pbf_file)


# ----------------------------------------
# 2. Rodar o osmosis
# ----------------------------------------

# Uma vez baixado o arquivo PBF, é preciso isolar a cidade de SP dele. Para isso, 
# é preciso usar um programa chamado Osmosis (ver passo a passo a seguir):
# Sobre o Osmosis: https://github.com/openstreetmap/osmosis (tem pacote para o Debian, instalar via apt)
# Sobre como usar o osmosis para recortar o polígono: https://github.com/eqasim-org/sao_paulo/blob/master/docs/howto.md
# Exemplos de uso do osmosis: https://wiki.openstreetmap.org/wiki/Osmosis/Examples


# Uma vez com o (a) arquivo do sudeste do Brasil no formato OSM e (b) o 
# polígono de São Paulo no formato .poly, rodar o osmosis no terminal:

message('\nIniciando o osmosis - este passo deve demorar cerca de 70 minutos para rodar.\n')
osmosis_path <- sprintf("/usr/bin/osmosis")
arg_o1 <- sprintf('--read-pbf file="%s"', pbf_file)
arg_o2 <- sprintf('--bounding-polygon file="%s"', poly_file)
arg_o3 <- sprintf('--write-pbf file="%s"', out_pbf_file)
system2(command = osmosis_path, args = c(arg_o1, arg_o2, arg_o3))



