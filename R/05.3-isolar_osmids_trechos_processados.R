# carregar bibliotecas
source('fun/setup.R')

# -----------------------------------------------------------------------------
# Isolar osm_ids de trechos processados no map matching
# -----------------------------------------------------------------------------

# Abre todos os arquivos de mapmatching processados e isola seus osm_ids (edges.way_id)
isolar_osmids_processados <- function(ano_mes) {
  # Abrir todos trechos processados - queremos selecionar somente os osm_id para
  # associação com o viário
  search_folder <- sprintf('%s/%s/viagens_processadas_csv', pasta_map_matching, ano_mes)
  result_files  <- list.files(search_folder, pattern = '\\d{6}_\\d{2}.csv', recursive = TRUE, full.names = TRUE)
  
  # Criar um dataframe único para todos os registros daquele dia
  # https://stackoverflow.com/questions/46299777/add-filename-column-to-table-as-multiple-files-are-read-and-bound
  result_files <- data.frame(f_path = result_files)
  
  # Dataframe temporário para processar uma certa quantidade de linhas
  sel_osm_ids <-
    result_files %>%
    # Abrir cada arquivo e extrair conteúdo em nova coluna
    mutate(reg = lapply(f_path, read_delim, delim = ';', col_names = FALSE, col_types = cols(.default = "c"))) %>% 
    # Transformar cada linha de conteúdo em uma linha nova no tibble (linhas com
    # nomes de arquivos vão se repetir para cada linha de conteúdo)
    unnest(reg, keep_empty = TRUE) %>% 
    # Pegar a primeira linha e usar como nomes de colunas, depois descartá-la
    setNames(slice(., 1)) %>% 
    # Nomes das colunas vieram junto - retirar essas linhas (todas repetidas)
    filter(trip_id != 'trip_id') %>% 
    # Retirar primeira coluna, com referência aos endereços do arquivo
    select(edges.way_id) %>% 
    distinct()
  
  # Definir arquivo de saída
  out_file <- sprintf('%s/%s_osmids_selecionados.csv', pasta_map_matching, ano_mes)
  write_delim(sel_osm_ids, out_file, delim = ';')
}


# -----------------------------------------------------------------------------
# Estrutura de pastas e arquivos
# -----------------------------------------------------------------------------

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
meses_proc <- c('201811', '201812', '201901')

lapply(meses_proc, isolar_osmids_processados)
