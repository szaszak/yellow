# Este processo é realizado primeiro no QGIS, entrando no R só depois


# ------------------------------------------------------------------------------
# Criar pontos no viário a cada 2m
# ------------------------------------------------------------------------------

# A ideia é ter pontos ao longo das linhas (OSM) para depois pegar o nearest 
# neighbor de cada ponto e associar ao qgis_id. Para isso:

# Arquivos a serem usados:
# 09_rotas_ttmatrix/01_trechos_agrupados/02_ttmatrix_res09_2019_rotas_agrupadas.gpkg
# 09_rotas_ttmatrix/01_trechos_agrupados/02_ttmatrix_res09_2028_rotas_agrupadas.gpkg

# Processing Toolbox > Vector geometry > Points along geometry
# Input Layer: 02_ttmatrix_res09_2019_rotas_agrupadas.gpkg
# Distance: 2 meters
# Start offset: 0 (não mexer)
# End offset: 0 (não mexer)

# Resultado: camadas "Interpolated points".


# Exportar para liberar memória RAM:
# - Deixar como SIRGAS23S
# - Selecionar somente a coluna index_col
# Exportar como:
# 09_rotas_ttmatrix/02_associacao_qgis_id/01_interpolated_points_2019.gpkg
# (repetir para 2028)
# 09_rotas_ttmatrix/02_associacao_qgis_id/01_interpolated_points_2028.gpkg



# ------------------------------------------------------------------------------
# Tipologia de viário por trecho de viário (qgis_id)
# ------------------------------------------------------------------------------

# # Arquivos a serem usados:
# 1.  A camada de viário do OSM já com a subdivisão que contempla os QGIS_ID, em SIRGAS e em pontos:
# 03_curva_elevacao_sp/viario_osmid_qgisid_pontos_2m_draped.gpkg
# 
# 2. A camada de pontos resultantes do processo anterior, para associação (rodar uma vez para cada)
# 09_rotas_ttmatrix/02_associacao_qgis_id/interpolated_points_2019.gpkg
# 09_rotas_ttmatrix/02_associacao_qgis_id/interpolated_points_2028.gpkg


# Juntar atributos de classificação viária
# Uma vez que as camadas estão como pontos, vamos juntar os dados de qgis_id
# viária usando o nearest neighbor.
# 
# Processing Toolbox > Vector General > Join attributes by nearest
# Input Layer 1: Interpolated points - interpolated_points_2019
# Input Layer 2: Interpolated points - viario_osmid_qgisid_pontos_2m_draped
# Layer 2 fields to copy: osm_id, qgis_id
# Maximum nearest neighbors: 1
# Maximum distance: 15 meters (esta distância foi o melhor custo benefício, pois 
# em alguns bairros o pessoal traçou calçadas e isso zoava a proximidade do 
# vizinho com o atributo de viário)
# Joined layer [optional] - Salvar como gpkg: 
# 09_rotas_ttmatrix/02_interpolated_points_2019_joined_qgis_id.gpkg


# Destes arquivos, abrir no R (abaixo), selecionar colunas e exportar em CSV.


# ------------------------------------------------------------------------------
# Definir qgis_id predominante em cada trecho
# ------------------------------------------------------------------------------

library('tidyverse')
library('tidylog')
library('sf')
# library('mapview')


# Estrutura de pastas
pasta_dados           <- "../../yellow_dados"
pasta_atrib_viario    <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_ttmatrix_qgisid <- sprintf("%s/09_rotas_ttmatrix/02_associacao_qgis_id", pasta_dados)
# dir.create(pasta_ttmatrix_qgisid, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Reduzir tamanhos dos arquivos a serem trabalhados
# ------------------------------------------------------------------------------

# # Ler arquivo, isolar colunas de interesse e exportar como dataframe .CSV
# open_file <- sprintf('%s/02_interpolated_points_2019_joined_qgis_id.gpkg', pasta_ttmatrix_qgisid)
# open_file <- sprintf('%s/02_interpolated_points_2028_joined_qgis_id.gpkg', pasta_ttmatrix_qgisid)
# 
# rotas <- read_sf(open_file) %>% st_drop_geometry()
# rotas <- rotas %>% select(index_col, n, osm_id, qgis_id)
# head(rotas)
# 
# out_file <- str_replace(open_file, '.gpkg', '.csv')
# write_delim(rotas, out_file, delim = ';')



# ------------------------------------------------------------------------------
# Reduzir tamanhos dos arquivos a serem trabalhados
# ------------------------------------------------------------------------------

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
atrib_viario <- atrib_viario %>% select(qgis_id, length_m, infra_ciclo, via_restr) %>% distinct()
head(atrib_viario)

# Ler arquivo, isolar colunas de interesse e exportar como dataframe .CSV
open_file <- sprintf('%s/02_interpolated_points_2019_joined_qgis_id.csv', pasta_ttmatrix_qgisid)
# open_file <- sprintf('%s/02_interpolated_points_2028_joined_qgis_id.csv', pasta_ttmatrix_qgisid)

rotas <- read_delim(open_file, delim = ';', col_types = 'iccc')
head(rotas)


# Agrupar por qgis_id - somar quantidade de pontos associados a um trecho e
# considerar somente o id predominante, com maior número de associações
rotas_grouped <- 
  rotas %>% 
  group_by(index_col, n, qgis_id) %>% 
  tally() %>% 
  # Filtrar e deixar somente a classificação viária com mais associações
  filter(nn == max(nn)) %>% 
  ungroup()

# O que está acontecendo aqui é que alguns trechos ficaram com duas linhas no
# momento em que geramos os arquivos que reconstituíam as rotas:
# 09_rotas_ttmatrix/01_trechos_agrupados/02_ttmatrix_res09_2028_rotas_agrupadas.gpkg
# Provavelmente, isso se deu devido a dois cenários:
# 1. Havia dois sentidos, de ida e de volta, em vias bidirecionais, resultando 
# em duas linhas diferentes;
# 2. Haviam linhas de chegada nos arcos que ficaram com pontos de chegada 
# levemente diferentes devido à simplificação da precisão do latlong (ex. Rua 
# Teodoro Sampaio, quase chegando à Av. Dr. Arnaldo)
# rotas_grouped %>% group_by(qgis_id) %>% tally() %>% filter(n > 1)

# Isso vai resultar em qgis_ids associados a mais de um trecho (que no fundo é o
# mesmo trecho, só que com mais de uma linha de rota passando por ele). É ok 
# desde que ao somar as distâncias percorridas dentro e fora de infraestruturas
# cicloviárias, seja feito um agrupamento por qgis_id das quantidades de viagem
# por trecho
head(rotas_grouped)


# Alguns trechos acabaram sendo associados a mais de um qgis_id. Como os pontos
# para associação foram gerados a cada 2m, provavelmente é o caso de trechos
# muito pequenos, que resultaram em poucos pontos próximos a mais de um arco
# da rede (a mais de um qgis_id). Vamos checar:
ids_problema <- rotas_grouped %>% group_by(index_col) %>% tally() %>% filter(n > 1)
ids_problema <- ids_problema %>% left_join(rotas_grouped, by = 'index_col')
ids_problema <- ids_problema %>% select(qgis_id, index_col) %>% distinct()
ids_problema <- ids_problema %>% left_join(atrib_viario, by = 'qgis_id')
ids_problema %>% select(length_m) %>% summary()
# 2019
# length_m       
# Min.   :   0.024  
# 1st Qu.:   7.698  
# Median :  27.847  
# Mean   :  45.543  
# 3rd Qu.:  62.110  
# Max.   :1514.292 

# 2028
# length_m       
# Min.   :   0.023  
# 1st Qu.:   7.564  
# Median :  25.021  
# Mean   :  44.528  
# 3rd Qu.:  61.200  
# Max.   :1514.292 

# Como não há muito o que fazer e os trechos são em geral pequenos (o maior é a
# ciclovia Pinheiros, cujo qgis_id está associado a vários outros index_col),
# vamos removê-los do cálculo (é 2,55% da base em 2019; 2,68% em 2028)
rotas_grouped <- rotas_grouped %>% filter(!index_col %in% ids_problema$index_col)


# Associar aos atributos de viário
rotas_grouped <- rotas_grouped %>% left_join(atrib_viario, by = 'qgis_id')
# rotas_grouped %>% select(index_col) %>% distinct() %>% nrow() == nrow(rotas_grouped)
head(rotas_grouped)


# Gerar resultados sobre as rotas
rotas_grouped %>% 
  # Multiplicar a extensão do arco pela quantidade de vezes que foi percorrido
  mutate(n   = as.integer(n),
         ext = n * length_m) %>% 
  # Agrupar por deslocamentos em infra cicloviária e áreas restritas
  group_by(infra_ciclo, via_restr) %>% 
  summarise(ext = sum(ext)) %>% 
  # Calcular proporcionais para o grupo e do todo
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))

# Dados para 2019
# infra_ciclo     via_restr             ext prop_grupo prop_total
# <chr>           <chr>               <dbl>      <dbl>      <dbl>
# 1 ciclofaixa      via_comum     3096705463.      99.8        6.36
# 2 ciclofaixa      via_restrita     4588618.       0.15       0.01
# 3 ciclovia        via_comum     1411504512.      92.6        2.9 
# 4 ciclovia        via_restrita   112970842.       7.41       0.23
# 5 expressa        via_comum      845894610.     100          1.74
# 6 sem_infra_ciclo via_comum    42710349443.      98.8       87.7 
# 7 sem_infra_ciclo via_restrita   516289951.       1.19       1.06


# Dados para 2028
# infra_ciclo     via_restr             ext prop_grupo prop_total
# <chr>           <chr>               <dbl>      <dbl>      <dbl>
# 1 ciclofaixa      via_comum     3180387003.      99.9        5.91
# 2 ciclofaixa      via_restrita     4306269.       0.14       0.01
# 3 ciclovia        via_comum     1637787472.      93.7        3.04
# 4 ciclovia        via_restrita   109924865.       6.29       0.2 
# 5 expressa        via_comum     1078306008.     100          2   
# 6 sem_infra_ciclo via_comum    47165773089.      98.7       87.7 
# 7 sem_infra_ciclo via_restrita   615324664.       1.29       1.14



# Gerar resultados sobre as rotas
rotas_grouped %>% 
  # Multiplicar a extensão do arco pela quantidade de vezes que foi percorrido
  mutate(n   = as.integer(n),
         ext = n * length_m) %>% 
  # Agrupar por deslocamentos em infra cicloviária
  group_by(infra_ciclo) %>% 
  summarise(ext = sum(ext)) %>% 
  # Calcular proporcionais para o grupo e do todo
  mutate(prop_grupo = round(ext / sum(ext) * 100, 2),
         prop_total = round(ext / sum(.$ext) * 100, 2))

  

# Dados para 2019
# infra_ciclo              ext prop_grupo prop_total
# <chr>                  <dbl>      <dbl>      <dbl>
# 1 ciclofaixa       3101294081.       6.37       6.37
# 2 ciclovia         1524475354.       3.13       3.13
# 3 expressa          845894610.       1.74       1.74
# 4 sem_infra_ciclo 43226639394.      88.8       88.8 

# Dados para 2028
# infra_ciclo              ext prop_grupo prop_total
# <chr>                  <dbl>      <dbl>      <dbl>
# 1 ciclofaixa       3184693271.       5.92       5.92
# 2 ciclovia         1747712337.       3.25       3.25
# 3 expressa         1078306008.       2          2   
# 4 sem_infra_ciclo 47781097753.      88.8       88.8 


rotas_grouped %>% 
  # Multiplicar a extensão do arco pela quantidade de vezes que foi percorrido
  mutate(n   = as.integer(n),
         ext = n * length_m) %>% 
  select(ext) %>% 
  sum()

# Total extensão 2019: 48.698.303.440
# Total extensão 2028: 53.791.809.369