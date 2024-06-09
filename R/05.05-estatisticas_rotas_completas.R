# Gera algumas estatísticas relacionadas às rotas resultantes do map matching

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
# pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
# pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)

# distâncias percorridas
# velocidade média com base nessas distâncias


# ------------------------------------------------------------------------------
# Abrir e juntar bases de dados processadas
# ------------------------------------------------------------------------------

# Abrir arquivos processados
base_modelo <- list.files(path = pasta_base_agrup, 
                          pattern = '^\\d{6}_trechos_processados_todos.csv', 
                          recursive = FALSE, 
                          full.names = TRUE)

# arq_trechos_proc <- sprintf('%s/trechos_processados_todos.csv', pasta_base_agrup)
# base_modelo <- read_delim(arq_trechos_proc, delim = ';', col_types = 'cccicidddddddd')
base_modelo <- 
  lapply(X = base_modelo, FUN = read_delim, delim = ';', col_types = 'cccicciidddddddd') %>% 
  rbindlist(fill = TRUE)

# Retirar linhas que estão todas como NA (29 viagens cujo processamento falhou)
base_modelo <- base_modelo %>% filter(!is.na(trip_id))

head(base_modelo)

# Checar quantidade de viagens restantes na base
base_modelo %>%
  # head() %>%
  select(trip_id) %>%
  mutate(trip_id = str_sub(trip_id, 1, 6)) %>%
  distinct() %>% 
  nrow()

# Checar quantidade de trechos restantes na base
base_modelo %>%
  select(trip_id) %>%
  distinct() %>% 
  nrow()


# ------------------------------------------------------------------------------
# Abrir e juntar bases com resumo das viagens inteiras (resultado map matching)
# ------------------------------------------------------------------------------

abrir_resumo_rotas <- function(ano_mes) {
  # ano_mes <- '201811'
  open_file <- sprintf('%s/%s/%s_map_matching_rotas_completas.csv', pasta_map_matching, ano_mes, ano_mes)
  tmp_df    <- read_delim(open_file, delim = ';', col_types = 'cciiiddddcc')
  
}

meses_proc <- c('201811', '201812', '201901')
rotas_completas <- lapply(meses_proc, abrir_resumo_rotas)
rotas_completas <- rbindlist(rotas_completas)

# Preparar base para análise
rotas_completas <- 
  rotas_completas %>% 
  # Criar id de trecho de viagem, para igualar ao dataframe base_modelo
  mutate(trip_id = str_c(trip_id, cod_proc, sep = '_'),
         trip_id = str_sub(trip_id, 1, 9)) %>% 
  # Descartar colunas que não vão ser usadas
  select(-c(cod_proc, vg_inicio, vg_termino)) %>% 
  # Filtrar somente trip_ids que estão em base_modelo
  filter(trip_id %in% base_modelo$trip_id) %>% 
  # Retirar trip_ids que estão duplicados (são 8)
  distinct()

rotas_completas %>% summary()
