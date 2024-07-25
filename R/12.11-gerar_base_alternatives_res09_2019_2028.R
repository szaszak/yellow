# Puxa resultados do routing para viagens até 40 minutos que estão em sua pasta
# respectiva e junta em um arquivo único na pasta principal

library('tidyverse')
# library('tidylog')


# Estrutura de pastas
pasta_ssd         <- "/media/livre/SSD120GB/yellow"
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_ssd)

# Definir ano para rodar o script: 2019 ou 2028
ano <- 2019; min_thres <- 40 ; sec_thres <- min_thres * 60


# ------------------------------------------------------------------------------
# Puxar resultados do routing - viagens até 40 minutos
# ------------------------------------------------------------------------------

if (ano == '2019') {
  # Definir arquivo de saída
  out_base_name <- sprintf('01_base_alternatives_%s_res09_40min', ano)
  out_file <- sprintf('%s/%s.csv', pasta_aoprv_alter, out_base_name)
  
  # Arquivos com as rotas resultantes do routing pelo graphhopper
  alternatives <- data.frame(arq = list.files(pasta_rts_aopt_19, 
                                              pattern = '_modalt.csv', 
                                              recursive = FALSE, 
                                              full.names = TRUE))
} else if (ano == '2028') {
  # Definir arquivo de saída
  out_base_name <- sprintf('04_base_alternatives_%s_res09_40min', ano)
  out_file <- sprintf('%s/%s.csv', pasta_aoprv_alter, out_base_name)
  
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
# out_file2 <- sprintf('%s/%s.parquet', pasta_aoprv_alter, out_base_name)
# write_parquet(that, out_file2)
