source('fun/setup.R')
library('arrow')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/IPEA", pasta_dados)
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_teste <- sprintf("%s/02_teste_aop_alternatives", pasta_aop_rev)
pasta_osmids_aopt <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_teste)
pasta_rotas_aopt  <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_teste)
pasta_osmids_aopt2 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_teste)
pasta_rotas_aopt2  <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_teste)

# Definir ano para rodar o script: 2019 ou 2028
ano <- 2028


# ------------------------------------------------------------------------------
# Puxar resultados do routing - viagens até 40 minutos
# ------------------------------------------------------------------------------

if (ano == '2019') {
  # Definir arquivo de saída
  out_base_name <- sprintf('01_base_teste_alternatives_%s_res09_40min', ano)
  out_file1 <- sprintf('%s/%s.csv', pasta_aoprv_teste, out_base_name)
  
  # Arquivos com as rotas resultantes do routing pelo graphhopper
  alternatives <- data.frame(arq = list.files(pasta_rotas_aopt, 
                                              pattern = '_modalt.csv', 
                                              recursive = FALSE, 
                                              full.names = TRUE))
} else if (ano == '2028') {
  # Definir arquivo de saída
  out_base_name <- sprintf('05_base_teste_alternatives_%s_res09_40min', ano)
  out_file1 <- sprintf('%s/%s.csv', pasta_aoprv_teste, out_base_name)
  
  # Arquivos com as rotas resultantes do routing pelo graphhopper
  alternatives <- data.frame(arq = list.files(pasta_rotas_aopt2, 
                                              pattern = '_modalt.csv', 
                                              recursive = FALSE, 
                                              full.names = TRUE))
}



# Puxar as viagens com tempo menor ou igual ao limite
# detach("package:tidylog")
for (alter in alternatives$arq) {
  # alter <- alternatives$arq[1]
  trips_aop <- read_delim(alter, delim = ';', col_types = 'ciddddc')
  trips_aop <- trips_aop %>% filter(time <= 2400)
  
  if (file.exists(out_file1)) {
    write_delim(trips_aop, out_file1, delim = ';', append = TRUE)
  } else {
    write_delim(trips_aop, out_file1, delim = ';', append = FALSE)
  }
}

rm(trips_aop, alter, alternatives)


# ------------------------------------------------------------------------------
# Transformar base de .CSV em .parquet
# ------------------------------------------------------------------------------

that <- read_delim(out_file1, delim = ';', col_types = 'ciddddc')
# head(that)
# 
out_file2 <- sprintf('%s/%s.parquet', pasta_aoprv_teste, out_base_name)
write_parquet(that, out_file2)
