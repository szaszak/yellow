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