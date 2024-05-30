# carregar bibliotecas
source('fun/setup.R')
library('h3jsr')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprev_hex  <- sprintf("%s/01_hexagonos_26_vizinhos", pasta_aop_rev)
dir.create(pasta_aoprev_hex, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Criar hexágonos h3 para o município
# ------------------------------------------------------------------------------

# Aqui, vamos aproveitar o material já gerado pelo IPEA - Acesso a Oportunidades
# (https://www.ipea.gov.br/acessooportunidades/dados/), em que os hexágonos já 
# foram gerados para a resolução 09 do h3.

# Abrir hexágonos para SP à resolução 9, com distância de ~350m entre os vértices
hex_sp <- read_sf(sprintf("%s/aop_hex_grid_v2.gpkg", dados_originais))
hex_sp <- hex_sp %>% filter(abbrev_muni == 'spo') %>% select(-c(abbrev_muni, name_muni, code_muni))
head(hex_sp)

# ------------------------------------------------------------------------------
# Agrupar hexágonos vizinhos aos hexágonos de origem
# ------------------------------------------------------------------------------

# A velocidade máxima calculada pelo modelo seria de ~16 km/h - isso para uma mega
# descida, em ciclovia expressa etc. A média fica em torno de 11 km/h. Assim,
# a expectativa para um cálculo de acesso a oportunidades considerando até 60min,
# tendo como base essa velocidade máxima, podemos puxar os 23 vizinhos de cada 
# hexágono, o que significaria uma distância em linha reta de ~8.050 m. Usando os
# dados descritivos, vemos que essa distância, a 12 km/h, equivale a 40 vizinhos
# e é maior do que 95% dos deslocamentos (percentil 95% = 7.710 m).
# 
# Se puxarmos 26 vizinhos, garantimos um cálculo de AOP para 45 minutos, ~9.100 m

# id_hex <- '89a81009a8bffff'; n_vizinhos <- 43
# # Puxar grupo de hexágonos vizinhos
# grupo_hexagonos <- get_disk(id_hex, n_vizinhos) %>% data.frame() %>% setNames('id_hex_y')
# # Remover o hexágono de origem do grupo
# grupo_hexagonos <- grupo_hexagonos %>% filter(id_hex_y != id_hex)
# hex_vizinhos <- hex_sp %>% filter(id_hex %in% grupo_hexagonos$id_hex_y)
# mapview(hex_vizinhos, color = 'blue')

# Usando o h3jsr, é possível puxar os hexágonos vizinhos:
# https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html

# Puxar todos os centróides dos hexágonos do shape
hexagonos <- st_centroid(hex_sp) %>% mutate(centroides = as.character(geom)) %>% st_drop_geometry()


# Pegar um hexágono e agrupar todos os seus n vizinhos (menos o hexágono original) em um dataframe
agregar_vizinhos <- function(id_hex, n_vizinhos) {
  # id_hex <- '88a8100c33fffff'; n_vizinhos <- 26
  
  # Puxar grupo de hexágonos vizinhos
  grupo_hexagonos <- get_disk(id_hex, n_vizinhos) %>% data.frame() %>% setNames('id_hex_y')
  # Remover o hexágono de origem do grupo
  grupo_hexagonos <- grupo_hexagonos %>% filter(id_hex_y != id_hex)
  
  # Combinar id_hex de origem com grupo de hexágonos vizinhos
  grupo_hexagonos <- expand.grid(id_hex, grupo_hexagonos$id_hex_y) %>% setNames(c('id_hex_x', 'id_hex_y'))
  
}


# Para cada hexágono, puxar seus vizinhos
# Se puxarmos 26 vizinhos, garantimos um cálculo de AOP para 45 minutos, ~9.100 m
hex_com_vizinhos <- lapply(hexagonos$id_hex, agregar_vizinhos, n_vizinhos = 26)
# # Como o resultado é uma lista de vários dataframes, juntar tudo em uma coisa só
# hex_com_vizinhos <- rbindlist(hex_com_vizinhos)

# res 8 = 2085696 (3 horas de processamento); 
# res 9 com 43 vizinhos = 85474884 (40x)
# res 9 com 22 vizinhos = 22859562 (11x)
# res 9 com 23 vizinhos = 24937704 (12x)
# res 9 com 26 vizinhos = 31714254 (15x)
nrow(hex_com_vizinhos) 


# Guardar resultados
out_file <- sprintf('%s/hex_spo_res09_26vizinhos.csv', pasta_aoprev_hex)
write_delim(hex_com_vizinhos, out_file, delim = ';')
