library('tidyverse')
library('tidylog')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_orig_modalt  <- sprintf("%s/11_rotas_modeladas_com_alternativas", pasta_dados)
pasta_osm_way_ids  <- sprintf("%s/02_osm_way_ids_rotas_modeladas", pasta_orig_modalt)
pasta_marcparques  <- sprintf("%s/03_osm_way_ids_marcacao_parques", pasta_orig_modalt)
dir.create(pasta_marcparques, recursive = TRUE, showWarnings = FALSE)


# Arquivo de osm_ids em áreas verdes (parque e usp) - queremos só parques
parques <- sprintf('%s/A7_listagem_vias_em_areas_restritas.csv', pasta_atrib_viario)
parques <- read_delim(parques, delim = ';', col_types = 'ccc')
parques <- parques %>% filter(class == 'parques') %>% select(osm_way_id = osm_id, class)
head(parques)

# Arquivo de resultados das rotas modeladas
resultados <- sprintf('%s/02_ttmatrix_rotas_modeladas_de_viagens_originais_com_alternativas_e_infraciclo.csv', pasta_orig_modalt)
resultados <- read_delim(resultados, delim = ';', col_types = 'ciccddddddiiddddddcdddd')
head(resultados)

# Arquivos CSV com osm_ids resultantes das rotas modeladas
resultados_csv <- list.files(pasta_osm_way_ids, recursive = FALSE, full.names = TRUE)


# Isolar somente polyline de resultado com ids necessários para junção posterior
resultados_stripped <- resultados %>% select(trip_id, alt)
head(resultados_stripped)


# ------------------------------------------------------------------------------
# Gerar arquivo temporário de marcação de rotas modeladas inteiramente em parques
# ------------------------------------------------------------------------------

# Arquivo de saída final
out_file <- sprintf('%s/rotas_modeladas_de_viagens_originais_marcacao_de_parques.csv', pasta_marcparques)

# Agregar info de cada rota modelada e suas alternativas se a viagem aconteceu
# inteiramente dentro de uma área restrita de parque
detach("package:tidylog")
for (i in seq(1, nrow(resultados_stripped))) {
  # this <- resultados_stripped %>% slice(1)
  this <- resultados_stripped %>% slice(i)
  
  # Abrir arquivo da viagem modelada com alternativas
  vg_modelada <- sprintf('%s/%s_%i.csv', pasta_osm_way_ids, this$trip_id, this$alt)
  vg_modelada <- read_delim(vg_modelada, delim = ';', col_types = 'ic')
  
  # Juntar com dataframe de parques para bater os osm_ids dos dois
  vg_modelada <- vg_modelada %>% left_join(parques, by = 'osm_way_id')
  
  # Se todos os osm_ids forem dentro de parque, marcar a viagem como 'somente parque'
  if (nrow(vg_modelada) == nrow(vg_modelada %>% filter(!is.na(class)))) {
    this <- this %>% add_column(so_parque = 'sim')
  } else {
    this <- this %>% add_column(so_parque = 'não')
  }
  
  
  # Guardar resultados
  if (file.exists(out_file)) {
    write_delim(this, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(this, out_file, delim = ';', append = FALSE)
  }
}


# ------------------------------------------------------------------------------
# Juntar resultados totais à base de dados de resultados da viagens modeladas
# ------------------------------------------------------------------------------

# Puxar resultados de viagens, para características de cada uma - ex. com 
# contramão, com parques, com loop etc.
arq_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
arq_modelo <- read_delim(arq_modelo, delim = ';', col_types = "ccccdddddccddccdccdddccccccccccccc")
arq_modelo <- arq_modelo %>% select(trip_id, vg_contramao, vg_loop, vg_exper, vg_parques, dist_total) %>% distinct()
head(arq_modelo)

# Agregar características gerais das viagens aos resultados
resultados <- resultados %>% left_join(arq_modelo, by = 'trip_id')
rm(arq_modelo)


# Abrir resultados da marcação de viagens modeladas integralmente em parques
marc_parques <- read_delim(out_file, delim = ';', col_types = 'cic')
# marc_parques %>% filter(so_parque == 'sim')
head(marc_parques)


# Juntar marcações ao dataframe geral das viagens modeladas com alternativas
resultados_out <- left_join(resultados, marc_parques, by = c('trip_id', 'alt'))
resultados_out <- resultados_out %>% relocate(c(vg_contramao, vg_loop, vg_exper, vg_parques, so_parque, dist_total), 
                                              .before = 'poly')

# Arquivo de saída final
out_file2 <- sprintf('%s/03_ttmatrix_rotas_modeladas_de_viagens_originais_com_carac_viagens.csv', pasta_orig_modalt)
write_delim(resultados_out, out_file2, delim = ';')
