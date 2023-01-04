# ------------------------------------------------------------------------------
# Curvatura horizontal por trecho de viário (qgis_id)
# ------------------------------------------------------------------------------

# Arquivos a serem usados:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg

# Field Calculator
# Create a new field
# Output field name: curv_h
# Output field type: Decimal number (real)
# Expression: round( abs( (straight_distance_2d( $geometry ) / $length) - 1 ), 3)


# Exportar como CSV:
# - Selecionar somente as colunas qgis_id e curv_h
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / A2_listagem_curvatura_por_trecho_viario.csv



# ------------------------------------------------------------------------------
# Quantidade de lotes por trecho de viário (qgis_id)
# ------------------------------------------------------------------------------

# # Arquivos a serem usados:
# 1. Todos os lotes do Geosampa (Geosampa > Cadastro > Lotes), unidos em uma única 
# camada com Vector > Data Management Tools > Merge Layers, em SIRGAS:
# Base_GtsRegionais/Arquivos_Geosampa/CADASTRO/LOTES/202201_SIRGAS_Geosampa_Lotes
# 
# 2. A camada de viário do OSM já com a subdivisão que contempla os QGIS_ID, em SIRGAS:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg


# A. Fazer buffer na camada de viário
# 
# Vector > Geoprocessing Tools > Buffer
# Input layer: sao_paulo_osm_filtrado_com_qgis_id.gpkg
# Distance: 12 metros
# Segments: 5 (não mexer)
# End cap style: Flat
# Join style: Bevel
# Miter limit: 2 (não mexer)
# 
# Resultado: camada "Buffered".



# B. Fazer sobreposição do buffer com a camada de lotes
# 
# Na camada 202201_SIRGAS_Geosampa_Lotes, filtrar os tipos de lote. Na coluna "lo_tp_lote":
#   F = "Fiscal" no Geosampa; -> escolher somente este
#   M = "Espaço livre" no Geosampa;
#   V = "Via de Acesso" no Geosampa
# 
# Em seguida:
# Vector > Geoprocessing Tools > Intersection
# Input layer: "Buffered" (resultante da etapa anterior)
# Overlay layer: 202201_SIRGAS_Geosampa_Lotes
# Input fields to keep: qgis_id
# 
# Resultado: camada "Intersection".
# 
# A quantidade de lotes por quadra é a quantidade de vezes (linhas) que um mesmo 
# qgis_id se repete na camada Intersection.



# Exportar como CSV:
# - Selecionar somente as colunas qgis_id e lo_tp_lote
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / A3_A_listagem_lotes_por_trecho_de_viario.csv



# ------------------------------------------------------------------------------
# Quantidade de lotes por trecho de viário (qgis_id) a 15m das esquinas
# ------------------------------------------------------------------------------

# # Arquivos a serem usados:
# 1. Todos os lotes do Geosampa (Geosampa > Cadastro > Lotes), unidos em uma única 
# camada com Vector > Data Management Tools > Merge Layers, em SIRGAS:
# Base_GtsRegionais/Arquivos_Geosampa/CADASTRO/LOTES/202201_SIRGAS_Geosampa_Lotes
# 
# 2. A camada de viário do OSM já com a subdivisão que contempla os QGIS_ID, em SIRGAS:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg


# A. Retirar footway da camada de viário
# sao_paulo_osm_filtrado_com_qgis_id.gpkg > Filter > "highway" != 'footway'


# B. Gerar pontos de intersecção
# 
# Vector > Analysis Tools > Line intersections
# Input layer: sao_paulo_osm_filtrado_com_qgis_id.gpkg (filtrado)
# Intersect layer: sao_paulo_osm_filtrado_com_qgis_id.gpkg (filtrado)
# 
# Resultado: camada "Intersections".


# C. Fazer buffer nas interseções (15m)
# 
# Vector > Geoprocessing Tools > Buffer
# Input layer: Intersections
# Distance: 15 metros
# Segments: 5 (não mexer)
# End cap style: Round
# Join style: Round
# Miter limit: 2 (não mexer)
# 
# Resultado: camada "Buffered". Renomear para "Buffered - Intersections 15m"


# D. Retirar filtro da camada de viário
# sao_paulo_osm_filtrado_com_qgis_id.gpkg > Filter > Clear


# E. Fazer difference para isolar trechos de viário a 15m das interseções
# [demora uns 10-12 minutos]
# 
# Vector > Geoprocessing Tools > Difference
# Input layer: sao_paulo_osm_filtrado_com_qgis_id.gpkg (sem filtro)
# Overlay layer: "Buffered - Intersections 15m"
# 
# Resultado: camada "Difference". Renomear para "Difference 15m"


# F. Criar nova extensão do trecho
# "Difference 15m" > Field Calculator
# Create a new field
# Output field name: length_m_15m
# Output field type: Decimal number (real)
# Expression: $length


# G. Fazer buffer na camada de viário com recorte de 15m das interseções
# 
# Vector > Geoprocessing Tools > Buffer
# Input layer: "Difference 15m"
# Distance: 12 metros
# Segments: 5 (não mexer)
# End cap style: Flat
# Join style: Bevel
# Miter limit: 2 (não mexer)
# 
# Resultado: camada "Buffered". Renomear para "Buffered - Difference 15m"



# H. Fazer sobreposição do buffer com a camada de lotes
# [demora uns 10-12 minutos]
# 
# Na camada 202201_SIRGAS_Geosampa_Lotes, filtrar os tipos de lote. Na coluna "lo_tp_lote":
#   F = "Fiscal" no Geosampa; -> escolher somente este
#   M = "Espaço livre" no Geosampa;
#   V = "Via de Acesso" no Geosampa
# 
# Em seguida:
# Vector > Geoprocessing Tools > Intersection
# Input layer: "Buffered - Difference 15m" (resultante da etapa anterior)
# Overlay layer: 202201_SIRGAS_Geosampa_Lotes
# Input fields to keep: qgis_id, length_m_15m
# 
# Resultado: camada "Intersection".
# 
# A quantidade de lotes por quadra é a quantidade de vezes (linhas) que um mesmo 
# qgis_id se repete na camada Intersection.



# Exportar como CSV:
# - Selecionar somente as colunas qgis_id, length_m_15m e lo_tp_lote
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / A3_B_listagem_lotes_por_trecho_de_viario_15m.csv


# Repetir para 30 metros e exportar como
# Exportar como:
# 04_atributos_viario / A3_C_listagem_lotes_por_trecho_de_viario_30m.csv

# ------------------------------------------------------------------------------
# Tipologia de viário por trecho de viário (qgis_id)
# ------------------------------------------------------------------------------

# # Arquivos a serem usados:
# 1. Camada de classificação viária da CET pelo Geosampa, de 2019, em SIRGAS:
# Base_GtsRegionais/Arquivos_Geosampa/SISTEMAVIARIO/SIRGAS_SHP_SISTEMAVIARIO_Classe_Viaria_CET/201905/SIRGAS_SHP_classeviariacet_line.shp
# 
# 2. A camada de viário do OSM já com a subdivisão que contempla os QGIS_ID, em SIRGAS:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg


# A. Criar pontos ao longo das linhas de viário (fazer 2 vezes)
# A ideia é ter pontos ao longo das duas linhas (CET e OSM) para depois pegar o 
# nearest neighbor de cada ponto. Para isso:
#   
# Processing Toolbox > Vector geometry > Points along geometry
# Input Layer: camadas sao_paulo_osm_filtrado_com_qgis_id.gpkg e SIRGAS_SHP_classeviariacet_line.shp
# Distance: 10 meters
# Start offset: 0 (não mexer)
# End offset: 0 (não mexer)
# 
# Resultado: camadas "Interpolated points". Renomear para "Interpolated points OSM" e "Interpolated points CET".
# 
# 
# B. Juntar atributos de classificação viária
# Uma vez que as camadas estão como pontos, vamos juntar os dados de classificação 
# viária usando o nearest neighbor.
# 
# Processing Toolbox > Vector General > Join attributes by nearest
# Input Layer 1: Interpolated points OSM
# Input Layer 2: Interpolated points CET
# Layer 2 fields to copy: cvc_dctipo
# Maximum nearest neighbors: 1
# Maximum distance: 15 meters (esta distância foi o melhor custo benefício, pois 
# em alguns bairros o pessoal traçou calçadas e isso zoava a proximidade do 
# vizinho com o atributo de viário)
# 
# Resultado: "Joined Layer". Poderíamos filtrar o resultado pelo n = 1, porque
# há vários casos em que há mais de 1 vizinho mais próximo. Porém, como vamos
# agrupar depois pela quantidade de ids (o maior o número de ids define e 
# classificação viária), talvez seja melhor deixar tudo.


# Exportar como CSV:
# - Selecionar somente as colunas osm_id, qgis_id, cvc_dctipo e n
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / A4_listagem_tipologia_de_viario_por_trecho.csv



# ------------------------------------------------------------------------------
# Infra cicloviária e trechos em áreas restritas
# ------------------------------------------------------------------------------

# Estes arquivos haviam sido gerados nos momentos dos scripts 02.X. Os arquivos
# foram copiados como:
# 04_atributos_viario / A5_listagem_vias_infra_cicloviaria.csv
# 04_atributos_viario / A6_listagem_vias_em_areas_restritas.csv



# ------------------------------------------------------------------------------
# Tags do OSM relacionadas ao viário
# ------------------------------------------------------------------------------

# Abrir arquivo .pbf no QGIS para extrair as tags de interesse:

# Processing > Toolbox > Explode HStore Field
# Input layer: 20220216_sao_paulo_edited_20221223.osm — lines
# HStore field: "other_tags"
# Expected list of fields separated by a comma [optional]: oneway,lanes,maxspeed,surface
# (não pode ter espaço após a vírgula)

# Exportar como CSV:
# - Selecionar somente as colunas osm_id e as das tags extraídas acima
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / A7_listagem_tags_osm_de_viario.csv