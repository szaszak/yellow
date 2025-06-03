# Puxa resultados do routing para viagens até 20 minutos que estão em sua pasta
# respectiva e junta em um arquivo único na pasta principal

library('tidyverse')
# library('tidylog')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)
# pasta_ids_aopt_24 <- sprintf("%s/A_2024_osm_way_ids_aop", pasta_ttmatrix_24_28)
pasta_rts_aopt_24 <- sprintf("%s/B_2024_rotas_modeladas_alternatives", pasta_ttmatrix_24_28)
# pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_ttmatrix_24_28)
pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_ttmatrix_24_28)


# Definir ano para rodar o script: 2024 ou 2028. Não mudar o tempo aqui, deixar
# o tempo máximo a ser considerado
# ano <- 2024; min_thres <- 20 ; sec_thres <- min_thres * 60
ano <- 2028; min_thres <- 20 ; sec_thres <- min_thres * 60


# ------------------------------------------------------------------------------
# Puxar resultados do routing - viagens até 20 minutos (demora 3 min para rodar)
# ------------------------------------------------------------------------------

if (ano == '2024') {
  # Definir arquivo de saída
  out_base_name <- sprintf('04_base_alternatives_%s_res09_20min', ano)
  out_file <- sprintf('%s/%s.csv', pasta_ttmatrix_24_28, out_base_name)
  
  # Garantir que arquivo existente não será sobrescrito
  if (file.exists(out_file)) { file.rename(from = out_file, to = sprintf('%s_BKP_APAGAR', out_file)) }
  
  # Arquivos com as rotas resultantes do routing pelo graphhopper
  alternatives <- data.frame(arq = list.files(pasta_rts_aopt_24, 
                                              pattern = '_modalt.csv', 
                                              recursive = FALSE, 
                                              full.names = TRUE))
} else if (ano == '2028') {
  # Definir arquivo de saída
  out_base_name <- sprintf('05_base_alternatives_%s_res09_20min', ano)
  out_file <- sprintf('%s/%s.csv', pasta_ttmatrix_24_28, out_base_name)
  
  # Garantir que arquivo existente não será sobrescrito
  if (file.exists(out_file)) { file.rename(from = out_file, to = sprintf('%s_BKP_APAGAR', out_file)) }
  
  # Arquivos com as rotas resultantes do routing pelo graphhopper
  alternatives <- data.frame(arq = list.files(pasta_rts_aopt_28, 
                                              pattern = '_modalt.csv', 
                                              recursive = FALSE, 
                                              full.names = TRUE))
}


# Puxar as viagens com tempo menor ou igual ao limite
# detach("package:tidylog")
for (alter in alternatives$arq) {
  # alter <- alternatives$arq[1]
  trips_aop <- read_delim(alter, delim = ';', col_types = 'ciddddc')
  trips_aop <- trips_aop %>% filter(time <= sec_thres)
  
  if (file.exists(out_file)) {
    write_delim(trips_aop, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(trips_aop, out_file, delim = ';', append = FALSE)
  }
}

rm(trips_aop, alter, alternatives)


# Finalizar - tirar possíveis linhas duplicadas
library('tidylog')
trips_aop <- read_delim(out_file, delim = ';', col_types = cols(.default = "c"))
trips_aop <- trips_aop %>% distinct()
write_delim(trips_aop, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Transformar base de .CSV em .parquet
# ------------------------------------------------------------------------------

# library('arrow)

# that <- read_delim(out_file, delim = ';', col_types = 'ciddddc')
# head(that)
# 
# out_file2 <- sprintf('%s/%s.parquet', pasta_ttmatrix_24_28, out_base_name)
# write_parquet(that, out_file2)