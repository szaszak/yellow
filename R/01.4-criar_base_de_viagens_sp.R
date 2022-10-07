# A partir da base de dados original e da listagem de viagens fora de SP,
# isola as viagens dentro da cidade de SP em um único dataframe. Ao final,
# cria um gráfico para demonstrar a distribuição de viagens por dia ao longo
# do período de operação da Yellow

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../../yellow_dados"
dados_originais  <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_viagens_sp <- sprintf("%s/01_viagens_em_sp", pasta_dados)



# ----------------------------------------------------------
# Juntar todas as viagens fora de SP
# ----------------------------------------------------------

# Ler todos os arquivos com viagens fora da cidade de SP
open_files <- list.files(path       = pasta_viagens_sp, 
                         pattern    = '^viagens_yellow_fora_de_sp_(.+).csv', 
                         recursive  = FALSE, 
                         full.names = TRUE)

# Aqui, a função read_delim() é aplicada sem o () devido a lapply
viagens_fora <- open_files %>% map(read_delim, delim = ';', col_types = cols(.default = "c"))

# Juntar todas as listas em um dataframe único, removendo viagens repetidas (ou
# seja, viagens que começaram na última noite do mês e entraram no primeiro dia
# do mês seguinte)
viagens_fora <- viagens_fora %>% rbindlist() %>% distinct()



# ----------------------------------------------------------
# Separar viagens dentro de SP da base integral
# ----------------------------------------------------------

# Abrir a base integral original de viagens da Yellow
open_file <- sprintf('%s/yellow_bike_original.rds', dados_originais)
yellow    <- read_rds(open_file) %>% select(-bikeid)

# Substituir tripid por versão simplificada, a partir do dicionário
open_dic_file  <- sprintf('%s/yellow_dicionario_tripid.csv', dados_originais)
dicionario_ids <- read_delim(open_dic_file, delim = ';', col_types = cols(.default = "c"))
dicionario_ids <- dicionario_ids %>% select(-bike_id)

# Fazer substituição do tripid pelo trip_id, simplificado. A coluna n_points
# traz a quantidade de pontos GPS por viagem
yellow <- yellow %>% left_join(dicionario_ids, by = 'tripid')
yellow <- yellow %>% select(trip_id, n_points, timestamps, lats, longs)

# Filtrar tudo o que não está fora de SP - das 427.026 viagens, serão removidas
# 70.780 e ficarão 356.246 viagens, por enquanto independentemente de quantos
# pontos GPS há em cada viagem
yellow_sp <- yellow %>% filter(trip_id %nin% viagens_fora$trip_id)

# Liberar espaço de memória
rm(yellow)


# Guardar base em formato compato
out_file1 <- sprintf("%s/sp_viagens_yellow_compacto.rds", pasta_viagens_sp)
write_rds(yellow_sp, out_file1, compress = 'gz')



# ----------------------------------------------------------
# Transformar base para que cada ponto fique em uma linha
# ----------------------------------------------------------

# Colunas de latlong estão como [-23.594451904296875,-23.5944525] - precisamos
# transformá-las em colunas e linhas de latlong de fato
yellow_sp <- 
  yellow_sp %>% 
  # Retirar colchetes nas colunas timestamps, lats e longs
  mutate(timestamps = str_replace_all(timestamps, '[\\[\\]]', ''),
         lats       = str_replace_all(lats, '[\\[\\]]', ''),
         longs      = str_replace_all(longs, '[\\[\\]]', '')) %>% 
  # Transformar colunas timestamps, lats e longs em listas,
  # a partir do separador vírgula presente em cada uma delas
  mutate(timestamps = str_split(timestamps, ', '),
         lats       = str_split(lats, ', '),
         longs      = str_split(longs, ', '))

# Expandir dataframe, em que as colunas de timestamps, lats e longs sejam
# separadas de acordo com suas vírgulas e se tornem novas linhas - com isso,
# cada linha se tornará um ponto registrado GPS
yellow_sp <- yellow_sp %>% separate_rows(timestamps, lats, longs, sep = ',', convert = TRUE)

# Transformar base em sf
# yellow_sp <- yellow_sp %>% st_as_sf(coords = c("longs", "lats"), crs = 4326)


# Gravar arquivo
out_file2 <- sprintf("%s/sp_viagens_yellow.rds", pasta_viagens_sp)
write_rds(yellow_sp, out_file2, compress = 'gz')



# ----------------------------------------------------------
# Criar gráfico com viagens por dia para a cidade de SP
# ----------------------------------------------------------

# Simplificar dataframe, selecionando somente viagens com mais de 10 pontos GPS
viagens_dia <- 
  yellow_sp %>% 
  mutate(n_points = as.numeric(n_points)) %>%
  filter(n_points > 10) %>% 
  select(trip_id, timestamps)

# Pegar só a primeira ocorrência do timestamp
viagens_dia <- viagens_dia %>% group_by(trip_id) %>% summarise(ts = first(timestamps))

# Criar coluna de data para ver distribuição de viagens
viagens_dia <- viagens_dia %>% mutate(time = as.POSIXlt(.$ts, origin = '1970-01-01'))
viagens_dia <- viagens_dia %>% select(-ts) 
viagens_dia <- viagens_dia %>% mutate(data = str_sub(.$time, start = 1, end = 10))


# Agrupar viagens por data, para gráfico 1 (todas as viagens)
viagens_chart1 <- viagens_dia %>% select(-time) %>% group_by(data) %>% tally() %>% arrange(data)

# Criar gráfico
viagens_chart1 %>% 
  rename(dia = data, viagens = n) %>% 
  ggplot(aes(x = dia, y = viagens), size = 1) +
  theme(axis.text.x = element_text(angle = 90, size = 4)) +
  geom_point()

ggsave(sprintf('%s/sp_viagens_yellow1.png', pasta_viagens_sp))


# Agrupar viagens por data, para gráfico 2 (viagens antes de 2018-10-25)
viagens_chart2 <- viagens_dia %>% filter(time < '2018-10-25')
viagens_chart2 <- viagens_chart2 %>% select(-time) %>% group_by(data) %>% tally() %>% arrange(data)

# Criar gráfico
viagens_chart2 %>% 
  rename(dia = data, viagens = n) %>% 
  ggplot(aes(x = dia, y = viagens), size = 1) +
  theme(axis.text.x = element_text(angle = 90, size = 4)) +
  geom_point()

ggsave(sprintf('%s/sp_viagens_yellow2.png', pasta_viagens_sp))


# Agrupar viagens por data, para gráfico 3 (viagens após 2018-10-25)
viagens_chart3 <- viagens_dia %>% filter(time >= '2018-10-25')
viagens_chart3 <- viagens_chart3 %>% select(-time) %>% group_by(data) %>% tally() %>% arrange(data)

# Criar gráfico
viagens_chart3 %>% 
  rename(dia = data, viagens = n) %>% 
  ggplot(aes(x = dia, y = viagens), size = 1) +
  theme(axis.text.x = element_text(angle = 90, size = 4)) +
  geom_point()

ggsave(sprintf('%s/sp_viagens_yellow3.png', pasta_viagens_sp))
