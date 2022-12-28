# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_trechos_proc <- sprintf('%s/A_trechos_processados', pasta_modelos)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
dir.create(pasta_base_agrup, recursive = TRUE, showWarnings = FALSE)



# ----------------------------------------------------------
# Juntar e salvar resultados em arquivo único
# ----------------------------------------------------------

# Pegar nomes de todos os .csvs de resultado
result_files <- list.files(pasta_trechos_proc, 
                           pattern = '^\\d{6}_\\d{2}_agrupado.csv', 
                           recursive = TRUE, 
                           full.names = TRUE)

# Definir arquivo de saída
out_file <- sprintf('%s/trechos_processados_todos.csv', pasta_base_agrup)

# Abrir cada arquivo de resultado e gravar no arquivo de saída, um por um
for (rf in seq(1, length(result_files))) {
  # Abrir arquivo de resultado
  # tmp_file <- read_delim(result_files[rf], delim = ';', col_types = 'ccdddiddiiiiiiiccdd')
  # print(result_files[rf])
  print(rf)
  tmp_file <- read_delim(result_files[rf], delim = ';', col_types = cols(.default = "c"))
  
  # Se for o primeiro, gravar com header; se não for, anexar sem header
  if (rf != 1) {
    write_delim(tmp_file, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(tmp_file, out_file, delim = ';', append = FALSE)
  }
  
}
