library('tidyverse')
# library('tidylog')
library('arrow')

# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
# pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_alter)
pasta_rts_aopt_28 <- sprintf("%s/D_2028_rotas_modeladas_alternatives", pasta_aoprv_alter)

# Definir arquivo de saída
out_file <- sprintf('%s/02_base_alternatives_2028_res09_40min.csv', pasta_aoprv_alter)
# out_file <- sprintf('/media/livre/SSD120GB/01_base_alternatives_2019_res09_40min.csv')


# ------------------------------------------------------------------------------
# Puxar resultados do routing - viagens até 40 minutos
# ------------------------------------------------------------------------------

# Arquivos com as rotas resultantes do routing pelo graphhopper
alternatives <- data.frame(arq = list.files(pasta_rts_aopt_28, 
                                            pattern = '_modalt.csv', 
                                            recursive = FALSE, 
                                            full.names = TRUE))


# Puxar as viagens com tempo menor ou igual ao limite
# detach("package:tidylog")
for (alter in alternatives$arq) {
  # alter <- alternatives$arq[1]
  trips_aop <- read_delim(alter, delim = ';', col_types = 'ciddddc')
  trips_aop <- trips_aop %>% filter(time <= 2400)
  
  if (file.exists(out_file)) {
    write_delim(trips_aop, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(trips_aop, out_file, delim = ';', append = FALSE)
  }
}

rm(trips_aop, alter, alternatives)




# ------------------------------------------------------------------------------
# Transformar base de .CSV em .parquet
# ------------------------------------------------------------------------------

that <- read_delim(out_file, delim = ';', col_types = 'ciddddc')
# head(that)
# 
out_file2 <- sprintf('%s/02_base_alternatives_2028_res09_40min.parquet', pasta_aoprv_alter)
# out_file2 <- '/media/livre/SSD120GB/01_base_alternatives_2019_res09_40min.parquet'
write_parquet(that, out_file2)
