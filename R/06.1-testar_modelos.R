# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_base         <- sprintf("%s/05_testes_viagens_20181111", pasta_dados)

open_file <- sprintf('%s/viagens_processadas_todas.csv', pasta_base)
resultados <- read_delim(open_file, delim = ';', col_types = 'cciidddddiddiiiiiiicc')

head(resultados)

# Checar rota 112251 - parece ter sido feita de carro, como resolver?

resultados %>% 
  filter(veloc < 30) %>% 
  ggplot(aes(x = veloc, y = dist), size = 1) +
  geom_point()

resultados %>% 
  select(-c(qtd_quebras, qtd_it_outliers, prop_centr_150, vg_inicio, vg_termino)) %>% 
  # filter(veloc > 30) %>% 
  # mutate(dist_in = dist * prop_centr_100 / 100, .before = 'tempo') %>%
  mutate(prop_disttot_distedges = dist / dist_edges, .after = 'dist') %>% 
  arrange(-veloc) %>% 
  head(20)

resultados %>% 
  select(dist, prop_centr_100) %>% 
  # head(10) %>% 
  mutate(dist_in = dist * prop_centr_100 / 100) %>% 
  filter(dist_in <= 400) %>% 
  arrange(-dist_in)

