# Junta todos os arquivos de resultado do map matching

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_viagens_csv <- sprintf("%s/viagens_processadas_csv", pasta_map_matching)
pasta_viagens_log <- sprintf("%s/viagens_processadas_log", pasta_map_matching)

# ----------------------------------------------------------
# Juntar e salvar resultados em arquivo único
# ----------------------------------------------------------

# Pegar nomes de todos os .csvs de resultado
result_files <- list.files(pasta_viagens_log, pattern = '*.csv', full.names = TRUE)

# Definir arquivo de saída
out_file <- sprintf('%s/resultados_map_matching.csv', pasta_map_matching)

# Abrir cada arquivo de resultado e gravar no arquivo de saída, um por um
for (rf in seq(1, length(result_files))) {
  # Abrir arquivo de resultado
  # tmp_file <- read_delim(result_files[rf], delim = ';', col_types = 'ccdddiddiiiiiiiccdd')
  tmp_file <- read_delim(result_files[rf], delim = ';', col_types = cols(.default = "c"))
  
  # Se for o primeiro, gravar com header; se não for, anexar sem header
  if (rf != 1) {
    write_delim(tmp_file, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(tmp_file, out_file, delim = ';', append = FALSE)
  }
  
}


# # Avaliar resultados
# result_files <- list.files(pasta_viagens_csv, pattern = '*.csv', full.names = TRUE)
# resultados <- result_files %>% map(read_delim, delim = ';', col_types = 'ccdddiddiiiiiiiccdd') %>% bind_rows()
# # Salvar arquivo .csv
# out_file <- sprintf('%s/resultados_200_viagens_clusters_trechos.csv', pasta_map_matching)
# write_delim(resultados, out_file, delim = ';')