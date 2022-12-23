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
# 2. Criar coluna de "qgis_id", complementar à "osm_id"
# https://gis.stackexchange.com/questions/405041/adding-leading-zeros-to-string-value-in-qgis
# 
# Field calculator > Create a new field
# Output field name: qgis_id
# Output field type: string
# Output field length: 10
# Fórmula: lpad("fid", 6, '0')
# Fórmula alternativa [não usar]: lpad(@row_number, 6, '0')
# 
# ------------------------------------------------------------------------------
# 3. Extrair vértices (pontos das esquinas)
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
# 4. Puxar dados de elevação
# Vamos pegar os dados de elevação dos pontos de viário gerados pelo cruzamento 
# do raster do MDT de SP com o viário do OSM. Aqui, poderíamos refazer o processo 
# do Drape, mas talvez seja mais rápido usar o nearest neighbor, descrito aqui.
# 
# Processing toolbox > Join attributes by nearest
# Input layer: "Cleaned 2" (resultante do processo 3)Input layer: "Vertices" (resultante do processo 2)
# Input layer 2: "viario_osm_com_elevacao_mdt_sem_z"
# Layer 2 fields to copy: selecionar coluna "elev_mdt"
# Habilitar a opção "Discard records which could not be joined"
# Maximum nearest neighbors: 1
# Maximum distance: 0.5 meters
# 
# Resultado: camada "Joined layer", com todos os nearest neighbors encontrados 
# para cada ponto de intersecção. 
# 
# Temos 2 pontos para cada QGIS_ID. Como, novamente, precisamos somente de um 
# dado único da altimetria, filtrar a camada resultante com "Filter > n = 1" 
# para restar somente um ponto único por intersecção.
# 
# ------------------------------------------------------------------------------
# 5. Juntar dados de elevação às linhas de quadra
# O processo 1 nos gerou as linhas de viário separadas por quadra. Vamos associar 
# as altimetrias registradas em cada esquina a essas linhas.
# 
# Processing toolbox > Join attributes by nearest
# Input layer: "Cleaned" (resultante do processo 1)
# Input layer 2: "Joined layer" (resultante do processo 4)
# Layer 2 fields to copy: selecionar as colunas "elev_mdt", "name" e "qgis_id" (criada na etapa 3)
# Habilitar a opção "Discard records which could not be joined"
# Maximum nearest neighbors: 1
# Maximum distance: [Not set]
# 
# Resultado: camada "Joined layer". Para não confundir com a anterior, renomear 
# para "Viário com elevação".
# Haverá uma entrada para cada ligação deste trecho do viário com seus trechos 
# vizinhos e duas ele com ele mesmo (uma para cada altimetria). Isso terá de ser 
# tratado no R depois.
# 
# ------------------------------------------------------------------------------
# 6. Ajustar camada para exportação
# Vamos simplificar esta camada e deixar só o essencial para exportação:
# 
# 1. Retirar as colunas de "cat", "distance", "feature_x", "feature_y", "nearest_x" e "nearest_y". 
# 
# 2. Vão ficar somente as colunas "fid", "osm_id", "name", "qgis_id", "qgis_id2", "elev_mdt" e "n". 
# 
# 3. Criar cálculo para "length_m" usando "Field calculator > Update existing field > length_m > Expression: $length"
# 
# 
# Exportar como "viario_osm_com_elevacao_mdt_sem_z_por_quadra.gpkg" na pasta 
# "yellow_dados > 03_curva_elevacao_sp" (retirar coluna de fid antes ao exportar).
# 
# Tratar no R.



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
geom_viario <- elevacao_viario %>% select(osm_id, qgis_id, qgis_id_2) %>% distinct()


# Criar dataframe para cálculo das declividades
declividades <- 
  elevacao_viario %>% 
  st_drop_geometry() %>%
  # A coluna de 'n' servirá como base para o pivot_wider() - para isso, alterar
  # redação dos n para virar elev_1, elev_2, elev_3 e elev_4
  mutate(n = sprintf('elev_%s', n)) %>% 
  # Puxar elevações para colunas
  pivot_wider(id_cols = c(osm_id, qgis_id, qgis_id_2, name, name_2, length_m),
              names_from = 'n',
              values_from = 'elev_mdt')



# Calcular declividades por arco do viário
declividades_arcos <- 
  declividades %>% 
  # Selecionar somente os próprios arcos, sem suas relações com os arcos vizinhos
  filter(qgis_id == qgis_id_2) %>% 
  # Cálculos do mutate() devem considerar as linhas, por isso o rowwise()
  rowwise() %>%
  # Calcular elevações máximas e mínimas, extrair variação e calcular gradação
  mutate(elev_min = across(starts_with('elev_')) %>% min(na.rm = TRUE),
         elev_max = across(starts_with('elev_')) %>% max(na.rm = TRUE),
         elev_var = elev_max - elev_min,
         # Calcular elev_grad (m): Δy / Δx
         # https://www.calculator.net/slope-calculator.html
         elev_grad = elev_var / length_m,
         # Calcular ângulo de inclinação (θ): arctan(Δy / Δx) - como a função
         # atan() traz o resultado em radianos, é preciso converter para graus
         # https://r-lang.com/how-to-convert-radians-to-degrees-in-r/
         elev_ang_inc = atan(elev_grad) * 180 / pi) %>% 
  # Simplificar dataframe
  select(osm_id, qgis_id, qgis_id_2, name, name_2, length_m, 
         elev_min, elev_max, elev_var, elev_grad, elev_ang_inc)

# Exemplo de resultados
# declividades_arcos %>% filter(qgis_id %in% c('000258', '000002', '000016'))


# Deixar um dataframe simples para o left_join() que vem no bloco a seguir
declividades_arcos2 <- declividades_arcos %>% select(osm_id, qgis_id, matches('^elev_'))

# Calcular os gradientes de declividade por direção do arco
declividades_direcionais <- 
  declividades %>%
  # Selecionar somente as relações dos arcos com seus arcos vizinhos
  filter(qgis_id != qgis_id_2) %>% 
  arrange(qgis_id, qgis_id_2) %>% 
  # Cálculos do mutate() devem considerar as linhas, por isso o rowwise()
  rowwise() %>%
  # Isolar valor único de altimetria - este valor equivale ao ponto de interseção
  # entre a ligação entre qgis_id e qgis_id_2
  mutate(elev_val = across(starts_with('elev_')) %>% max(na.rm = TRUE)) %>% 
  # Excluir colunas gerais de elevação
  select(-matches('^elev_[0-9]')) %>% 
  # Juntar dados de declividades no arco principal, calculados antes
  left_join(declividades_arcos2, by = c('osm_id', 'qgis_id')) %>% 
  # Ajustar sinais positivo/negativo para declividades conforme a direção - 
  # quando elev_val é igual ao valor mínimo de elevação (elev_min), a direção 
  # é de uma descida; caso contrário, é de uma subida. Isso influencia nos
  # valores de elev_var, elev_grad e elev_ang_inc
  mutate(elev_var = case_when(elev_val == elev_min ~ elev_var * -1,
                              TRUE ~ elev_var),
         elev_grad = case_when(elev_val == elev_min ~ elev_grad * -1,
                               TRUE ~ elev_grad),
         elev_ang_inc = case_when(elev_val == elev_min ~ elev_ang_inc * -1,
                                  TRUE ~ elev_ang_inc))

# Juntar os dois resultados em um dataframe único
declividades <- 
  declividades_arcos %>% 
  # Criar coluna vazia elev_val, que será calculada somente para os direcionais
  mutate(elev_val = 'NA', .after = 'length_m') %>%
  # Juntar dataframe de resultados direcionais
  rbind(declividades_direcionais) %>% 
  # Remover rowwise() para conseguir ordenar os resultados
  ungroup() %>% 
  arrange(qgis_id, qgis_id_2)


# declividades %>% select(length_m, aclividade, angulo_inc) %>% summary()
# length_m          aclividade         angulo_inc     
# Min.   :   0.007   Min.   :0.000000   Min.   : 0.0000  
# 1st Qu.:  20.892   1st Qu.:0.008076   1st Qu.: 0.4627  
# Median :  51.714   Median :0.025412   Median : 1.4557  
# Mean   :  70.295   Mean   :0.042632   Mean   : 2.4273  
# 3rd Qu.:  93.458   3rd Qu.:0.060792   3rd Qu.: 3.4789  
# Max.   :9396.688   Max.   :2.143325   Max.   :64.9879 

# Juntar novamente geometria do viário aos dados de declividades
declividades_out <- 
  declividades %>% 
  left_join(geom_viario, by = c('osm_id', 'qgis_id', 'qgis_id_2')) %>% 
  st_as_sf(crs = 31983)

# Avaliar resultados
# declividades_out %>% filter(qgis_id == qgis_id_2) %>% mapview(zcol = 'elev_grad')

# Guardar resultados
out_file <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z_por_quadra_com_aclividades.gpkg', pasta_elevacao)
st_write(declividades_out, out_file, driver = 'GPKG', append = FALSE)
