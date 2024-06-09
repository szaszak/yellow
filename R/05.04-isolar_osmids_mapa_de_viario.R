# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)


# -----------------------------------------------------------------------------
# Isolar osm_ids únicos para serem associados aos atributos de viário
# -----------------------------------------------------------------------------

# Abrir arquivos com seleção de osm_ids
result_files  <- list.files(pasta_map_matching, 
                            pattern = '^\\d{6}_osmids_selecionados.csv', 
                            recursive = FALSE, 
                            full.names = TRUE)

# Juntá-los em um dataframe único
sel_osm_ids <- data.frame()
for (f in result_files) {
  tmp_osm_ids <- read_delim(f, delim = ';', col_types = cols(.default = "c"))
  sel_osm_ids <- rbind(sel_osm_ids, tmp_osm_ids)
  rm(tmp_osm_ids, f)
}

# Retirar ids duplicados
sel_osm_ids <- sel_osm_ids %>% distinct()


# -----------------------------------------------------------------------------
# Simplificar shape de viário, a partir dos osm_ids únicos
# -----------------------------------------------------------------------------

# Abrir centroides de viário para associação entre osm_id (edges.way_id) e qgis_id
pontos_viario <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped.gpkg', pasta_elevacao)
pontos_viario <- read_sf(pontos_viario)

pontos_viario <- pontos_viario %>% filter(osm_id %in% sel_osm_ids$edges.way_id)

# Guardar resultado
out_file <- sprintf('%s/viario_osmid_qgisid_pontos_2m_draped_selecionados.gpkg', pasta_map_matching)
st_write(pontos_viario, out_file, driver = 'GPKG', append = FALSE)
