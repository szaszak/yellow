# Faz o tratamento inicial na base da Yellow, descompactando as colunas de
# timestamps, lats e longs de modo que cada ponto GPS se torne uma linha - o
# resultado é um dataframe temporário, que será usado para filtrar as viagens
# realizadas somente em São Paulo. Também cria dicionários de dados relativos 
# aos ids de viagens e bicicletas, com o objetivo de simplificar o dataframe 
# com um id único de viagem menor do que o hash da base original.

# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados      <- "../yellow_dados"
dados_originais  <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_viagens_sp <- sprintf("%s/01_viagens_em_sp", pasta_dados)
dir.create(pasta_viagens_sp, recursive = TRUE, showWarnings = FALSE)


# Abrir a base integral original de viagens da Yellow
open_file <- sprintf('%s/yellow_bike_original.rds', dados_originais)
yellow    <- read_rds(open_file)

# Criar coluna com a quantidade de pontos por viagem - como
# a vírgula é um separador, contá-las nos dá o total de pontos
# na viagem além do primeiro
yellow <- yellow %>% mutate(n_points = str_count(lats, ',') + 1)



# ----------------------------------------------------------
# Criar dicionário de bikeid e tripid
# ----------------------------------------------------------

# Temos 2.706 bicicletas registradas
df_bici <- yellow %>% select(bikeid) %>% distinct()

# Criar nova coluna de índice, bike_id
df_bici <- 
  df_bici %>% 
  # Criar nova coluna de índice, bike_id
  add_column(bike_id = 1:nrow(df_bici), .before = 'bikeid') %>% 
  # Transformar coluna em string com zfill()
  mutate(bike_id = sprintf("%04d", .$bike_id))

# Salvar dicionário de dados de bicicletas
out_file <- sprintf('%s/yellow_dicionario_bikeid.csv', pasta_dados)
write_delim(df_bici, out_file, delim = ';')



# Temos 427.026 viagens registradas
df_vgs <- yellow %>% select(tripid, bikeid, n_points) %>% distinct()

# Criar nova coluna de índice, trip_id
df_vgs <- 
  df_vgs %>% 
  # Criar nova coluna de índice, trip_id
  add_column(trip_id = 1:nrow(df_vgs), .before = 'tripid') %>% 
  # Transformar coluna em string com zfill()
  mutate(trip_id = sprintf("%06d", .$trip_id))

# Atualizar bikeid para versão resumida
df_vgs <- df_vgs %>% left_join(df_bici, by = 'bikeid') %>% select(trip_id, tripid, bike_id, n_points)

# Salvar dicionário de dados de viages
out_file <- sprintf('%s/yellow_dicionario_tripid.csv', pasta_dados)
write_delim(df_vgs, out_file, delim = ';')



# ----------------------------------------------------------
# Substituir por novos ids de viagens na base
# ----------------------------------------------------------

# Fazer o left_join() para associar os ids
yellow <- yellow %>% left_join(df_vgs, by = 'tripid')

# Descartar ids antigos e reordenar colunas - uma vez que o bike_id não faz
# diferença real e poderá ser acessado via o dicionário de dados, vamos
# descartar esta coluna também
yellow <- yellow %>% select(trip_id, timestamps, lats, longs)

# Abrir espaço na memória
rm(df_bici, df_vgs)



# ----------------------------------------------------------
# Trabalhar colunas de lat long
# ----------------------------------------------------------
# Colunas de latlong estão como [-23.594451904296875,-23.5944525] - precisamos
# transformá-las em colunas e linhas de latlong de fato
yellow <- 
  yellow %>% 
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
yellow <- yellow %>% separate_rows(timestamps, lats, longs, sep = ',', convert = TRUE)


# Transformar base em sf
yellow <- yellow %>% st_as_sf(coords = c("longs", "lats"), crs = 4326)


# Gravar arquivo
out_file <- sprintf("%s/tmp_viagens_yellow_formato_longo.rds", pasta_viagens_sp)
write_rds(yellow, out_file, compress = 'gz')


