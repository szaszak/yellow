# Isola os ids e os nomes das vias que possuem algum tipo de infraestrutura
# cicloviária e exporta como arquivos .csv. O shape usado como base para a
# extração dessas ruas é o de viário simplificado do OSM, resultante do script
# 02.2 e que passou por um processo de revisão manual no QGIS, descrito abaixo:
# 
# Primeiro, o mapa do OSM relacionado à infraestrutura cicloviária de SP foi
# revisado em sua completude, tendo como base o shape de infraestrutura 
# cicloviária existente na cidade (GeoCET/Geosampa). Após este processo, o
# mapa OSM foi baixado e passou pelos scripts 02.X, que o simplificam. Em seguida,
# foi aberto no QGIS de forma a poder comparar com um shape da rede cicloviária
# existente em 2018 (fonte Geosampa) para marcar quais vias possuíam as estruturas
# naquele ano. Uma vez que as ciclovias e as tipologias de calçadas compartilhadas 
# e partilhadas haviam sido revisadas como vias segregadas no OSM, foi preciso 
# associar quais viários estavam relacionados a esses novos caminhos desenhados
# e marcá-los como contendo essas estruturas. No OSM, ciclofaixas compõem um 
# atributo do viário, sendo possível identificar o viário diretamente pelo seu
# id. Já ciclovias e calçadas compartilhadas tiveram os viários pelos quais 
# passam selecionados e marcados para que possamos usar também esse id (do viário, 
# além do do id do caminho traçado) como referência para os trechos identificados
# no map matching. Isso porque, ao usar o perfil de pedestres, pode ser que a
# escolha de rota não considere a ciclovia ou a calçada partilhada/compartilhada, 
# usando o id do viário. Precisamos resgatar essa informação. Como última etapa
# do processo, ciclovias expressas (Marginal Pinheiros e Radial Leste) receberam
# esta demarcação na base de dados; ruas que têm ciclovias, calçadas partilhadas
# e calçadas compartilhadas receberam, assim como os próprios caminhos traçados
# no OSM referentes a essas estruturas, receberam a demarcação geral de 'ciclovia';
# ciclofaixas mantiveram a mesma demarcação geral de 'ciclofaixa', independente-
# mente da sua tipologia.


# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
pasta_osm_sp <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)

# Abrir shapefile de ruas do OSM simplificado, com dados de infra cicloviária
open_file <- sprintf('%s/sao_paulo_osm_filtrado_ids_infracicloviaria_20220220.gpkg', pasta_osm_sp)
viario_sp <- read_sf(open_file)

# Filtrar linhas que possuem algum tipo de infra cicloviária
# viario_sp %>% filter(existente_2018 == 'sim') %>% mapview()
viario_sp <- 
  viario_sp %>% 
  st_drop_geometry() %>% 
  filter(existente_2018 == 'sim') %>% 
  select(-c(infra_ciclo, existente_2018))


# Salvar arquivos
write_delim(viario_sp, sprintf('%s/listagem_vias_infra_cicloviaria.csv', pasta_osm_sp), delim = ';')
