# Recorta uma área de testes de um arquivo .pbf do OpenStreetMap

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_gh_testes   <- sprintf("%s/01_testes", pasta_graphhopper)
dir.create(pasta_gh_testes, recursive = TRUE, showWarnings = FALSE)

# Arquivo .pbf a ser utilizado
pasta_geral_tiles  <- "/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/valhalla_tiles_sp"
pasta_valhalla_pbf <- sprintf("%s/pbf", pasta_geral_tiles)


# ----------------------------------------------------------------------------
# 1. Criar o arquivo .poly da área de testes
# ----------------------------------------------------------------------------

# A exemplo do script 2.1, precisaremos criar um arquivo .poly da área de
# testes para o GraphHopper (entorno da Praça do Pôr do Sol, cujas vias são
# bidirecionais e estão sem limites de velocidade estabelecidos)

# 1. Instalar um plugin chamado "Export OSM Poly, que aparece com o nome de
# "osmpoly_export"

# 2. Criar um polígono no entorno da área de testes:

# Layer > Create Layer > New temporary scratch layer
# Layer name: New scratch layer
# Geometry type: MultiPolygon
# Projection: WGS 84 (tanto az aqui, vamos ter que reprojetar depois)

# Layer > Create Layer > New GeoPackage Layer
# Database: viario_praca_por_do_sol_poligono.gpkg
# Table name: viario_praca_por_do_sol_poligono (preenchimento automático)
# Geometry type: Polygon
# Projection: WGS 84 (tanto az aqui, vamos ter que reprojetar depois)

# New Field:
# Name: name
# Type: Text
# Langth: 0
# Clicar em "Add to Fields List" e em "OK".


# 3. Criar o polígono no entorno das ruas de interesse e salvar as mudanças.
# Como atributo de nome, coloquei "entorno_praca"


# 4. Exportar o arquivo .poly em Vector > Export OSM Poly > Export OSM Poly
# Nome de saída: entorno_praca.poly
# /home/livre/Área de trabalho/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.poly



# Atualizar os nomes dos arquivos aqui antes de continuar:
poly_file    <- sprintf('%s/entorno_praca.poly', pasta_gh_testes)

# Pegar última versão dos arquivos, com base no nome do arquivo
pbf_file     <- sprintf('%s/20220216_sao_paulo_edited_20221223.osm.pbf', pasta_valhalla_pbf)
out_pbf_file <- sprintf('%s/entorno_praca.pbf', pasta_gh_testes)


# ----------------------------------------------------------------------------
# 2. Rodar o osmosis para recortar a área
# ----------------------------------------------------------------------------

# Uma vez baixado o arquivo PBF, é preciso isolar a área selecionada dele. Para 
# isso, é preciso usar um programa chamado Osmosis (ver passo a passo a seguir):
# Sobre o Osmosis: https://github.com/openstreetmap/osmosis (tem pacote para o Debian, instalar via apt)
# Sobre como usar o osmosis para recortar o polígono: https://github.com/eqasim-org/sao_paulo/blob/master/docs/howto.md
# Exemplos de uso do osmosis: https://wiki.openstreetmap.org/wiki/Osmosis/Examples


# Uma vez com o (a) arquivo da cidade de SP no formato OSM e (b) o 
# polígono de interesse no formato .poly, rodar o osmosis no terminal:

# O processo leva cerca de 70 minutos para a cidade toda, mas menos de 1 minuto
# para uma área de interesse pequena
message('\nIniciando o osmosis...\n')
osmosis_path <- sprintf("/usr/bin/osmosis")
arg_o1 <- sprintf('--read-pbf file="%s"', pbf_file)
arg_o2 <- sprintf('--bounding-polygon file="%s"', poly_file)
arg_o3 <- sprintf('--write-pbf file="%s"', out_pbf_file)
system2(command = osmosis_path, args = c(arg_o1, arg_o2, arg_o3))


# Testar o GraphHopper - abrir o terminal na pasta acima da "graphhopper"
# (base) livre@laika:~/Downloads/graphhopper/graphhopper_maxspeed$ java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/01_testes/entorno_praca.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example.yml
