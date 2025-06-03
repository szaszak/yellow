# Uma vez que temos as ttmatrix finais, podemos puxar agora os osm_way_ids das
# rotas selecionadas que passaram por infraestrutura cicloviária para conseguir
# gerar um comparativo antes-e-depois nos scripts seguintes

# carregar bibliotecas
library('tidyverse')
library('tidylog')

# Definir ano e limites de tempo para ttmatrix final
# ano <- '2024'; min_thres <- 15;
ano <- '2028'; min_thres <- 15;

# Estrutura de pastas
pasta_dados          <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)

if (ano == '2024') {
  pasta_tmp_osmids <- sprintf("%s/E_%s_osm_way_ids_tmp_%s_min", pasta_ttmatrix_24_28, ano, min_thres)
  ttmatrix_final   <- sprintf('%s/10_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
} else if (ano == '2028') {
  pasta_tmp_osmids <- sprintf("%s/F_%s_osm_way_ids_tmp_%s_min", pasta_ttmatrix_24_28, ano, min_thres)
  ttmatrix_final   <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
}


# ------------------------------------------------------------------------------
# Selecionar osm_ids que aparecem na ttmatrix final
# ------------------------------------------------------------------------------

# Abrir ttmatrix final
ttmatrix_final <- read_delim(ttmatrix_final, 
                             delim = ';', 
                             col_select = c('hex_id', 'alt', 'infra_ciclo'), 
                             col_types = 'ccd')

# Manter somente rotas que passaram por infra cicloviária
ttmatrix_final <- ttmatrix_final %>% filter(infra_ciclo > 0)

# Recriar hex_id_alt (ids de origem e destino encurtados + alt) para filtragem
ids_ttmatrix_final <- 
  ttmatrix_final %>%
  mutate(hex_id_alt = str_c(str_sub(hex_id, 6, 11), str_sub(hex_id, 22, 27), alt, sep = '-')) %>% 
  select(hex_id_alt)


# Abrir arquivo com todos os osm_id com infra_ciclo com suas extensões percorridas
osmid_files <- data.frame(arqs = list.files(pasta_tmp_osmids, recursive = FALSE, full.names = TRUE))

# Gravar resultados
if (ano == '2024') {
  out_file <- sprintf('%s/12_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
} else if (ano == '2028') {
  out_file <- sprintf('%s/13_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, min_thres)
}

# Como a pasta de arquivos para 2028 é enorme, abrir um por um e filtrar - ficam
# somente os osm_ids que fazem parte das rotas presentes na ttmatrix_final
detach("package:tidylog")
for (osmid_file in osmid_files$arqs) {
  # osmid_file <- osmid_files$arqs[1]
  tmp_file <- read_delim(osmid_file, delim = ';', col_types = 'cccdd')
  
  # Dos arquivos com todos os ids, manter somente os que ficaram na ttmatrix final
  tmp_file <- tmp_file %>% filter(hex_id_alt %in% ids_ttmatrix_final$hex_id_alt)
  
  # Preparar para exportar - das colunas de extensão, precisamos somente da ext_rev,
  # que corresponde ao cálculo da distância percorrida no shapefile, corrigida com
  # base na extensão total da rota 
  # (ver script 12.14-gerar_ttmatrix_inicial_infraciclo_2019_2028.R)
  tmp_file <- tmp_file %>% select(hex_id_alt, osm_way_id, infra_ciclo, ext_percorrida = ext_rev)
  
  if (file.exists(out_file)) {
    write_delim(tmp_file, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(tmp_file, out_file, delim = ';', append = FALSE)
  }
  
}


# # Checagem: números de hex_id_alt devem ser os mesmos entre ids_ttmatrix_final e
# # ids_osmid_files
# library('tidylog')
# osmids_infraciclo <- read_delim(out_file, delim = ';', col_select = 'hex_id_alt', col_types = 'c') %>% distinct()
# ids_osmid_files <- ids_ttmatrix_final %>% select(hex_id_alt) %>% distinct()
# nrow(ids_ttmatrix_final) == nrow(osmids_infraciclo)
