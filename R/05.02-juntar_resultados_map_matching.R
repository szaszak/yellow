# Junta todos os arquivos de log de resultado (resumo das rotas completas) do 
# map matching em um arquivo único para cada mês processado (rodar no Jupyter)

# carregar bibliotecas
source('fun/setup.R')

# Dados a atualizar de acordo com o mês a rodar
ano_mes <- '201811'
ano_mes <- '201812'
ano_mes <- '201901'

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_mes_mapmatch <- sprintf("%s/%s", pasta_map_matching, ano_mes)
# pasta_viagens_csv <- sprintf("%s/viagens_processadas_csv", pasta_mes_mapmatch)
pasta_viagens_log <- sprintf("%s/viagens_processadas_log", pasta_mes_mapmatch)

# ----------------------------------------------------------
# Juntar e salvar resultados em arquivo único
# ----------------------------------------------------------

# Pegar nomes de todos os .csvs de resultado
result_files <- list.files(pasta_viagens_log, pattern = '*.csv', full.names = TRUE)

# Criar um dataframe único para todos os registros daquele dia
# https://stackoverflow.com/questions/46299777/add-filename-column-to-table-as-multiple-files-are-read-and-bound
result_files <- data.frame(n_seq = result_files)

# Dataframe temporário para processar uma certa quantidade de linhas
tmp_dia <-
  result_files %>%
  # Abrir cada arquivo e extrair conteúdo em nova coluna
  mutate(reg = lapply(n_seq, read_delim, delim = ';', col_types = cols(.default = "c"))) %>% 
  # Transformar cada linha de conteúdo em uma linha nova no tibble (linhas com
  # nomes de arquivos vão se repetir para cada linha de conteúdo)
  unnest(reg, keep_empty = TRUE) %>% 
  # Pegar a primeira linha e usar como nomes de colunas, depois descartá-la
  # setNames(slice(., 1)) %>% 
  # Nomes das colunas vieram junto - retirar essas linhas (todas repetidas)
  # filter(trip_id != 'trip_id') %>% 
  # Retirar primeira coluna, com referência aos endereços do arquivo
  select(2:12)


head(tmp_dia)

# Definir arquivo de saída
out_file <- sprintf('%s/%s/%s_map_matching_rotas_completas.csv', pasta_map_matching, ano_mes, ano_mes)
write_delim(tmp_dia, out_file, delim = ';')


# ----------------------------------------------------------
# Avaliar resultados
# ----------------------------------------------------------

# Pegar nomes de todos os .csvs de resultado
open_file <- sprintf('%s/%s/%s_map_matching_rotas_completas.csv', pasta_map_matching, ano_mes, ano_mes)
resultado <- read_delim(open_file, delim = ';', col_types = cols(.default = "c"))

head(resultado)

resultado %>% select(trip_id) %>% distinct() %>% nrow()
# ano_mes <- '201811' - 46060
# ano_mes <- '201812' - 79108
# ano_mes <- '201901' - 50768

resultado %>% 
  mutate(cod_proc = str_sub(cod_proc, 1, 2),
         id_trecho = str_c(trip_id, cod_proc, sep = '_')) %>% 
  select(id_trecho) %>% 
  distinct() %>% 
  nrow()
# ano_mes <- '201811' - 51590
# ano_mes <- '201812' - 90599
# ano_mes <- '201901' - 58510


# # Pegar nomes de todos os .csvs de resultado
# open_file2 <- sprintf('%s/%s/viagens_processadas_log.csv', pasta_map_matching, ano_mes, ano_mes)
# resultado2 <- read_delim(open_file2, delim = ';', col_types = cols(.default = "c"))
# 
# head(resultado2)
# 
# resultado2 %>% select(trip_id) %>% distinct() %>% nrow()
# # ano_mes <- '201811' - 53473
# # ano_mes <- '201812' - 95161
# # ano_mes <- '201901' - 61411



search_folder <- sprintf('%s/%s/viagens_processadas_csv', pasta_map_matching, ano_mes)
result_files  <- list.files(search_folder, pattern = '\\d{6}_\\d{2}.csv', recursive = TRUE, full.names = TRUE)
result_files <- data.frame(f_path = result_files)

result_files %>% 
  mutate(this = str_sub(f_path, -13, -8)) %>% 
  select(this) %>% 
  distinct() %>% 
  nrow()
