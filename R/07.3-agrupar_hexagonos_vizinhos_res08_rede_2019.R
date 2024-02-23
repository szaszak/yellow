# carregar bibliotecas
source('fun/setup.R')
library('h3jsr')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_graphhopper <- sprintf("%s/07_graphhopper", pasta_dados)
pasta_hexagonos   <- sprintf("%s/02_hexagonos", pasta_graphhopper)
dir.create(pasta_hexagonos, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Criar hexágonos h3 para o município
# ------------------------------------------------------------------------------

# Aqui, vamos aproveitar o material já gerado para o Índice de Acesso à Cidade
# (https://github.com/Multiplicidademobilidade/indice_acesso_cidade/tree/main/R)
# do Instituto Multiplicidade Mobilidade, em que os hexágonos já foram gerados.
# Em especial, ver os scripts 01.01, que baixa o shape dos municípios, e 02.01,
# que cria um buffer em volta desse shape para detectar todos os hexágonos que
# se relacionam ao município e selecioná-los. O resultado é o shape que vamos
# importar aqui

# Abrir hexágonos para SP à resolução 8, com distância de ~1km entre os vértices
hex_sp <- read_rds(sprintf("%s/hex_spo_08_2019.rds", dados_originais))
hex_sp <- hex_sp %>% select(-c(h3_resolution, sigla_muni))


# ------------------------------------------------------------------------------
# Agrupar hexágonos vizinhos aos hexágonos de origem
# ------------------------------------------------------------------------------

# A velocidade máxima calculada pelo modelo seria de ~16 km/h - isso para uma mega
# descida, em ciclovia expressa etc. A média fica em torno de 11 km/h. Assim,
# a expectativa para um cálculo de acesso a oportunidades considerando até 60min,
# tendo como base essa velocidade máxima, podemos puxar os 17 vizinhos de cada 
# hexágono, o que significaria uma distância em linha reta de ~15km. Usando os
# dados do modelo, deverá ser impossível alguém percorrer toda essa distância
# no intervalo de 1 hora, pois teria de estar sempre descendo, fazer uma beeline
# e estar em uma ciclovia expressa o tempo todo

# id_hex <- '88a8100c33fffff'; n_vizinhos <- 17
# # Puxar grupo de hexágonos vizinhos
# grupo_hexagonos <- get_disk(id_hex, n_vizinhos) %>% data.frame() %>% setNames('id_hex_y')
# # Remover o hexágono de origem do grupo
# grupo_hexagonos <- grupo_hexagonos %>% filter(id_hex_y != id_hex)
# hex_vizinhos <- hex_sp %>% filter(id_hex %in% grupo_hexagonos$id_hex_y)
# mapview(hex_vizinhos)

# Usando o h3jsr, é possível puxar os hexágonos vizinhos:
# https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html

# Puxar todos os hexágonos do shape
hexagonos <- hex_sp %>% st_drop_geometry() %>% select(id_hex, centroides_muni)


# Pegar um hexágono e agrupar todos os seus n vizinhos (menos o hexágono original) em um dataframe
agregar_vizinhos <- function(id_hex, n_vizinhos) {
  # id_hex <- '88a8100c33fffff'; n_vizinhos <- 17
  
  # Puxar grupo de hexágonos vizinhos
  grupo_hexagonos <- get_disk(id_hex, n_vizinhos) %>% data.frame() %>% setNames('id_hex_y')
  # Remover o hexágono de origem do grupo
  grupo_hexagonos <- grupo_hexagonos %>% filter(id_hex_y != id_hex)
  
  # Combinar id_hex de origem com grupo de hexágonos vizinhos
  grupo_hexagonos <- expand.grid(id_hex, grupo_hexagonos$id_hex_y) %>% setNames(c('id_hex_x', 'id_hex_y'))

}


# Para cada hexágono, puxar seus vizinhos
hex_com_vizinhos <- lapply(hexagonos$id_hex, agregar_vizinhos, n_vizinhos = 17)
# Como o resultado é uma lista de vários dataframes, juntar tudo em uma coisa só
hex_com_vizinhos <- rbindlist(hex_com_vizinhos)

nrow(hex_com_vizinhos) # 2085696


# Guardar resultados
out_file <- sprintf('%s/hex_spo_res08_17vizinhos.csv', pasta_hexagonos)
write_delim(hex_com_vizinhos, out_file, delim = ';')
