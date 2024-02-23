# ------------------------------------------------------------------------------
# Processamento no QGIS
# ------------------------------------------------------------------------------

# No R poderia ser feito o processamento via st_intersects(), mas ele vai
# demorar para essa quantidade de pontos - ver esta referência:
# https://stackoverflow.com/questions/48650274/spatial-efficient-way-of-finding-all-points-within-x-meters-of-a-point)
# 
# Por isso, é mais simples e rápido fazer esse processamento no QGIS:

# Arquivos a serem usados:
# 04_atributos_viario / tmp_pontos_viario_inicio.gpkg
# 04_atributos_viario / tmp_pontos_viario_fim.gpkg
# 04_atributos_viario / tmp_buffer_2m_semaforos.gpkg


# 1. Juntar semáforos aos pontos de início

# Vector > Geoprocessing Tools > Clip (o Clip é bem mais rápido do que o Intersection)
# Input layer: tmp_pontos_viario_inicio
# Overlay layer: tmp_buffer_2m_semaforos

# Resultado: camada "Clipped". Renomear para "Clipped início"

# Exportar como CSV:
# - Selecionar somente as colunas osm_id e qgis_id
# - Separador é ponto e vírgula
# Exportar como:
# 04_atributos_viario / tmp_pontos_viario_inicio.csv


# 2. Fazer o mesmo método para pontos de fim de viário, exportar como tmp_pontos_viario_fim.csv



# ------------------------------------------------------------------------------
# Fazer marcação nos qgis_id dos trechos com semáforos
# ------------------------------------------------------------------------------

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf("%s/04_atributos_viario", pasta_dados)

# Abrir shape temporário de pontos de início de trecho do viário
pontos_viario_inicio <- sprintf('%s/tmp_pontos_viario_inicio.csv', pasta_atrib_viario)
pontos_viario_inicio <- read_delim(pontos_viario_inicio, delim = ';', col_types = cols(.default = "c"))

# Abrir shape temporário de pontos de fim de trecho do viário
pontos_viario_fim <- sprintf('%s/tmp_pontos_viario_fim.csv', pasta_atrib_viario)
pontos_viario_fim <- read_delim(pontos_viario_fim, delim = ';', col_types = cols(.default = "c"))

# Juntar todos os osm_id e qgis_id
trechos_viario <- 
  pontos_viario_inicio %>% 
  rbind(pontos_viario_fim) %>% 
  distinct()

# Inserir marcações de semáforos ao início, fim ou em ambos os pontos do trecho
trechos_viario <- 
  trechos_viario %>% 
  arrange(qgis_id) %>% 
  mutate(semaforos = case_when(
    qgis_id %in% pontos_viario_inicio$qgis_id  & qgis_id %in% pontos_viario_fim$qgis_id  ~ 'inicio_fim',
    qgis_id %in% pontos_viario_inicio$qgis_id  & qgis_id %nin% pontos_viario_fim$qgis_id ~ 'inicio',
    qgis_id %nin% pontos_viario_inicio$qgis_id & qgis_id %in% pontos_viario_fim$qgis_id  ~ 'fim')
    )



# Gravar resultados
out_file <- sprintf('%s/A2_semaforos_por_trecho_de_viario.csv', pasta_atrib_viario)
write_delim(trechos_viario, out_file, delim = ';')
