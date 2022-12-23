# ------------------------------------------------------------------------------
# Gerar qgis_id, ou um id para cada quadra/trecho do viário
# ------------------------------------------------------------------------------

# 1. Segmentar viário nas intersecções

# O viário original do OSM está segmentado segundo seus "osm_id", podendo ter várias quadras - queremos quadra por quadra.
# https://grass.osgeo.org/grass82/manuals/v.clean.html
# https://gis.stackexchange.com/questions/216204/splitting-where-lines-cross-each-other-using-qgis

# Arquivos a serem usados:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado.gpkg


# Processing toolbox > v.clean
# Layer to clean: sao_paulo_osm_filtrado.gpkg
# Cleaning tool: break,rmdangle
# Threshold: [ vazio ]

# Resultado: camadas "Cleaned" e "Error". A camada "cleaned" tem o viário segmentado por quadra.


# 2. Criar coluna de "qgis_id", complementar à "osm_id"
# https://gis.stackexchange.com/questions/405041/adding-leading-zeros-to-string-value-in-qgis

# Na camada "Cleaned":

# Field calculator > Create a new field
# Output field name: qgis_id
# Output field type: string
# Output field length: 10
# Fórmula: lpad("fid", 6, '0')
# Fórmula alternativa [não usar]: lpad(@row_number, 6, '0')


# Field calculator > Create a new field
# Output field name: length_m
# Output field type: Decimal number (real)
# Expression: round($length, 3)


# Ao exportar:
# - Deixar como SIRGAS23S
# - Selecionar somente as colunas osm_id, qgis_id, name e highway
# - Descartar colunas fid, cat, infra_ciclo
# Exportar como:
# 02_osm_simplificado_sp / sao_paulo_osm_filtrado_com_qgis_id.gpkg
