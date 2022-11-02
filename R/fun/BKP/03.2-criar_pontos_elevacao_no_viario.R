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



# ------------------------------------------------------------------------------
# Com raster do MDT de São Paulo de 2017
# ------------------------------------------------------------------------------

# 1. Baixar os arquivos de MDT e MDS gerados pelo LiDAR pela SMDU em 2017
# Digital Terrain and Surface Models of São Paulo (Fernando Gomes)
# 50cm resolution models from LiDAR 3D survey in 2017
# https://www.kaggle.com/datasets/andasampa/dtm-dsm-sao-paulo/code?resource=download
# 
# Quando usar o Modelo Digital de Superfície (MDS) e Modelo Digital de Terreno (MDT)
# (queremos o MDT)
# https://www.linkedin.com/pulse/quando-usar-o-modelo-digital-de-superf%C3%ADcie-mds-e-mdt-geotecnologias?articleId=6704108970862292993
# 
# 2. Abrir o raster no QGIS junto com o arquivo de viário do OpenStreetMap e usar 
# o "Processing toolbox > Drape" para adicionar as altimetrias às linhas de viário.
# Este processo demora umas boas horas para terminar.
# Obtaining elevation data for points using QGIS
# https://gis.stackexchange.com/questions/344092/obtaining-elevation-data-for-points-using-qgis
# 
# 3. Uma vez adicionado o valor Z, puxá-lo como um atributo da camada, para ficar
# visível. Salvar esta coluna como "elev_mdt".
# How do I bring the Z value to attribute table?
# https://gis.stackexchange.com/questions/321517/how-do-i-bring-the-z-value-to-attribute-table
# z($geometry)
# 
# 4. Exportar o arquivo como:
#   A. Com Z como uma das colunas: viario_osm_com_elevacao_mdt.gpkg
#   B. Sem Z como uma das colunas: viario_osm_com_elevacao_mdt_sem_z.gpkg
# 
#   É este último arquivo que será usado no processo a seguir.



# ------------------------------------------------------------------------------
# Gerar shape com dados de altimetria por intersecção de quadra no QGIS
# ------------------------------------------------------------------------------

# Criar declividade nos arcos da rede, segmentados quadra-a-quadra.
# 
# ------------------------------------------------------------------------------
# 0. Abrir as seguintes camadas:
# A. Viário do OSM que será utilizado no processamento de map matching.
# Neste caso, usaremos a camada "sao_paulo_osm_filtrado_com_elevacao_mdt.gpkg" 
# que está na pasta "yellow_dados > 02_osm_simplificado_sp".
# B. Camada de pontos de elevação do viário do OSM com a elevação do MDT.
# Usaremos a camada "viario_osm_com_elevacao_mdt_sem_z.gpkg" que está na pasta 
# "yellow_dados > 03_curva_elevacao_sp".
# 
# Checar se ambas as camadas estão em SIRGAS. Se não, converter.
# 
# ------------------------------------------------------------------------------
# 1. Segmentar viário nas intersecções
# O viário original do OSM está segmentado segundo seus "osm_id", podendo ter 
# várias quadras - queremos quadra por quadra.
# https://grass.osgeo.org/grass82/manuals/v.clean.html
# https://gis.stackexchange.com/questions/216204/splitting-where-lines-cross-each-other-using-qgis
# 
# Processing toolbox > v.clean
# Layer to clean: sao_paulo_osm_filtrado_com_elevacao_mdt
# Cleaning tool: break,rmdangle
# Threshold: [ vazio ]
# 
# Resultado: camadas "Cleaned" e "Error". A camada "cleaned" tem o viário segmentado por quadra.
# 
# ------------------------------------------------------------------------------
# 2. Extrair vértices (pontos das esquinas)
# Queremos agora os pontos para cada intersecção.
# https://gis.stackexchange.com/questions/306190/generating-start-and-end-points-for-linestrings
# 
# Processing toolbox > Extract specific vertices
# Input layer: "Cleaned" (resultante do processo anterior)
# Vertex indices: 0,-1
# Obs. Os valores 0 e -1 se referem ao primeiro e último ponto da linha
# 
# Resultado: camada "Vertices", com vários pontos sobrepostos. Há 1 ponto para 
# cada linha que se sobrepõe, ou seja, em uma esquina em "+" há 4 pontos sobrepostos.
# 
# ------------------------------------------------------------------------------
# 3. Retirar pontos de vértices sobrepostos
# Como estamos interessados só na localização estes pontos, podemos ficar com 
# só um deles por intersecção.
# https://gis.stackexchange.com/questions/374806/delete-duplicate-points-in-qgis
# 
# Processing toolbox > Delete duplicate geometries
# Input layer: "Vertices" (resultante do processo 2)
# 
# Resultado: camada "Cleaned", com 1 ponto por intersecção. Para não confundir 
# com a anterior, renomear para "Cleaned 2".
# 
# ------------------------------------------------------------------------------
# 4. Puxar dados de elevação
# Vamos pegar os dados de elevação dos pontos de viário gerados pelo cruzamento 
# do raster do MDT de SP com o viário do OSM. Aqui, poderíamos refazer o 
# processo do Drape, mas talvez seja mais rápido usar o nearest neighbor, descrito aqui.
# 
# Processing toolbox > Join attributes by nearest
# Input layer: "Cleaned 2" (resultante do processo 3)
# Input layer 2: "viario_osm_com_elevacao_mdt_sem_z"
# Layer 2 fields to copy: selecionar coluna "elev_mdt"
# Habilitar a opção "Discard records which could not be joined"
# Maximum nearest neighbors: 1
# Maximum distance: 0.5 meters
# 
# Resultado: camada "Joined layer", com todos os nearest neighbors encontrados 
# para cada ponto de intersecção.
# 
# Como, novamente, precisamos somente de um dado único da altimetria, filtrar a 
# camada resultante com "Filter > n = 1" para restar somente um ponto único por 
# intersecção.
# 
# ------------------------------------------------------------------------------
# 5. Juntar dados de elevação às linhas de quadra
# O processo 1 nos gerou as linhas de viário separadas por quadra. Vamos associar 
# as altimetrias registradas em cada esquina a essas linhas.
# 
# Processing toolbox > Join attributes by nearest
# Input layer: "Cleaned" (resultante do processo 1)
# Input layer 2: "Joined layer" (resultante do processo 4)
# Layer 2 fields to copy: selecionar coluna "elev_mdt"
# Habilitar a opção "Discard records which could not be joined"
# Maximum nearest neighbors: 1
# Maximum distance: [Not set]
# 
# Resultado: camada "Joined layer", com mais do que o dobro de itens da camada 
# original "Cleaned". Isso porque temos pelo menos 2 pontos associados a cada 
# linha - há uns 350 casos com 3 pontos e outros 2 com 4 pontos. Isso terá de 
# ser tratado no R depois.
# 
# Para não confundir com a anterior, renomear para "Viário com elevação".
# 
# ------------------------------------------------------------------------------
# 6. Ajustar camada para exportação
# Vamos simplificar esta camada e deixar só o essencial para exportação:
#   
# 1. Deixar somente as colunas "fid", "osm_id", "name", "length_m" e "elev_mdt". 
# Remover as demais.
# 
# 2. Atualizar o cálculo para "length_m" usando "Field calculator > Update 
# existing field > length_m > Expression: $length"
# 
# 3. Criar coluna de "qgis_id", complementar à "osm_id". Em:
#   https://gis.stackexchange.com/questions/405041/adding-leading-zeros-to-string-value-in-qgis
# 
# Field calculator > Create a new field
# Output field name: qgis_id
# Output field type: string
# Output field length: 10
# Fórmula: lpad("fid", 6, '0')
# Fórmula alternativa [não usar]: lpad(@row_number, 6, '0')
# 
# Exportar como "viario_osm_com_elevacao_mdt_sem_z_por_quadra.gpkg" na pasta 
# "yellow_dados > 03_curva_elevacao_sp".
# 
# Tratar no R: a coluna "elev_mdt" vai ter que ser desmembrada em quatro, 
# "elev_mdt1" (...) "elev_mdt4" com pivot_wider() para que fiquem duas colunas 
# de altimetria e, junto com a distância, possa ser calculada a aclividade. Este 
# valor poderá ser juntado aos segmentos de viário gerados no processo 1 via 
# um id único formado pelo "osm_id + qgis_id"



# ------------------------------------------------------------------------------
# Tratamento de declividades no viário
# ------------------------------------------------------------------------------

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)

# Abrir shape com viário do OpenStreetMap com valoz Z incluído, vindo do MDT de SP
elevacao_viario <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z_por_quadra.gpkg', pasta_elevacao)
elevacao_viario <- read_sf(elevacao_viario)


# Guardar coluna de geometry para merge posterior
geom_viario <- elevacao_viario %>% select(osm_id, qgis_id) %>% distinct()


# Criar dataframe para cálculo das declividades
declividades <- 
  elevacao_viario %>% 
  st_drop_geometry() %>%
  # A coluna de 'n' servirá como base para o pivot_wider() - para isso, alterar
  # redação dos n para virar elev_1, elev_2, elev_3 e elev_4
  mutate(n = sprintf('elev_%s', n)) %>% 
  # Puxar elevações para colunas
  pivot_wider(id_cols = c(osm_id, qgis_id, name, length_m),
              names_from = 'n',
              values_from = 'elev_mdt')

# Calcular as declividades por trecho de viário
declividades <- 
  declividades %>% 
  # Cálculos do mutate() devem considerar as linhas, por isso o rowwise()
  rowwise() %>%
  # Calcular elevações máximas e mínimas, extrair variação e calcular gradação
  mutate(elev_max = max(c(elev_1, elev_2, elev_3, elev_4), na.rm = TRUE),
         elev_min = min(c(elev_1, elev_2, elev_3, elev_4), na.rm = TRUE),
         elev_var = elev_max - elev_min,
         # Calcular aclividade (m): Δy / Δx
         # https://www.calculator.net/slope-calculator.html
         aclividade = elev_var / length_m,
         # Calcular ângulo de inclinação (θ): arctan(Δy / Δx) - como a função
         # atan() traz o resultado em radianos, é preciso converter para graus
         # https://r-lang.com/how-to-convert-radians-to-degrees-in-r/
         angulo_inc = atan(aclividade) * 180 / pi)


# declividades %>% select(length_m, aclividade, angulo_inc) %>% summary()
# length_m          aclividade         angulo_inc     
# Min.   :   0.007   Min.   :0.000000   Min.   : 0.0000  
# 1st Qu.:  20.892   1st Qu.:0.008076   1st Qu.: 0.4627  
# Median :  51.714   Median :0.025412   Median : 1.4557  
# Mean   :  70.295   Mean   :0.042632   Mean   : 2.4273  
# 3rd Qu.:  93.458   3rd Qu.:0.060792   3rd Qu.: 3.4789  
# Max.   :9396.688   Max.   :2.143325   Max.   :64.9879 

# Juntar novamente geometria do viário aos dados de declividades
declividades_out <- declividades %>% left_join(geom_viario, by = c('osm_id', 'qgis_id'))

# Guardar resultados
out_file <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z_por_quadra_com_aclividades.gpkg', pasta_elevacao)
st_write(declividades_out, out_file, driver = 'GPKG', append = FALSE)
