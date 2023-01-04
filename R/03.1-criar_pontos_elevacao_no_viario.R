# Estes processos serão todos realizados no QGIS - este arquivo é uma guia de como fazê-los


# ------------------------------------------------------------------------------
# Criar pontos no viário a cada 2m
# ------------------------------------------------------------------------------

# A ideia é ter pontos ao longo das duas linhas (CET e OSM) para depois pegar o 
# nearest neighbor de cada ponto. Para isso:

# Arquivos a serem usados:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg

# Processing Toolbox > Vector geometry > Points along geometry
# Input Layer: sao_paulo_osm_filtrado_com_qgis_id.gpkg
# Distance: 2 meters
# Start offset: 0 (não mexer)
# End offset: 0 (não mexer)

# Resultado: camadas "Interpolated points".


# Exportar para liberar memória RAM:
# - Deixar como SIRGAS23S
# - Selecionar somente as colunas osm_id, qgis_id, distance e angle
# - Descartar colunas fid, length_m, name e highway
# Exportar como:
# 03_curva_elevacao_sp / tmp_pontos_2m.gpkg


# ------------------------------------------------------------------------------
# Associar pontos no viário a cada 2m com dados de elevação MDT
# ------------------------------------------------------------------------------

# Baixar os arquivos de MDT e MDS gerados pelo LiDAR pela SMDU em 2017
# Digital Terrain and Surface Models of São Paulo (Fernando Gomes)
# 50cm resolution models from LiDAR 3D survey in 2017
# https://www.kaggle.com/datasets/andasampa/dtm-dsm-sao-paulo/code?resource=download
# 
# Quando usar o Modelo Digital de Superfície (MDS) e Modelo Digital de Terreno (MDT)
# (queremos o MDT)
# https://www.linkedin.com/pulse/quando-usar-o-modelo-digital-de-superf%C3%ADcie-mds-e-mdt-geotecnologias?articleId=6704108970862292993


# Este processo é lento (demora 3h) )mas curiosamente gasta muito pouca memória do computador.

# Arquivos a serem usados:
# 03_curva_elevacao_sp / tmp_pontos_2m.gpkg
# Arquivo MDT_sampa-ZSTD (camada de MDT da SMDU / Fernando Gomes)

# Processing Toolbox > Vector Geometry > Drape (set Z value from raster)
# Input Layer: tmp_pontos_2m.gpkg
# Raster Layer: MDT_sampa-ZSTD
# Band number: Band 1: SP_MDT_las (Gray) (preenchimento vai ser automático)
# Value for nodata: 0, não mexer
# Scale factor: 1, não mexer

# Resultado: Camada "draped".


# ------------------------------------------------------------------------------
# Puxar o valor Z como um dos atributos
# ------------------------------------------------------------------------------

# Na camada "draped" resultante, abrir:

# Field Calculator
# Create a new field
# Output field name: elev_mdt
# Output field type: Decimal number (real)
# Expression: z($geometry)

# Salvar essas alterações vai levar uns 30-35 min


# Ao exportar:
# - Deixar como WGS84
# - Selecionar somente as colunas osm_id, qgis_id, distance e elev_mdt
# - Remover o eixo Z em Geometry > Geometry type = Point (desabilitar "Include z-dimension")
# Exportar como:
# 03_curva_elevacao_sp / viario_osmid_qgisid_pontos_2m_draped.gpkg

# [NÃO FAZER] Para filtrar somente um ponto a cada dois, na prática diminuindo a resolução de um 
# ponto a cada 2m para um ponto a cada 4m, filtrar a camada e usar a expressão:
# "fid" % 2 = 0 (linhas pares)
# "fid" % 2 <> 0 (linhas ímpares)

# https://groups.google.com/g/australian-qgis-user-group/c/FEYhD7aJ5Ds
# RowNumber % 2 = 0
# RowNumber % 2 <>0



