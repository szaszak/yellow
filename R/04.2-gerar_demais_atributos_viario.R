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
# 04_atributos_viario / A3_listagem_lotes_por_trecho_de_viario.csv



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
# Distance: 25 meters
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
# Maximum distance: 15 meters (esta distância foi o melhor custo benefício, pois em alguns bairros o pessoal traçou calçadas e isso zoava a proximidade do vizinho com o atributo de viário)
# 
# Resultado: "Joined Layer". Filtrar o resultado pelo n = 1.


# Exportar como CSV:
# - Selecionar somente as colunas qgis_id, cvc_dctipo e length_m
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