library('tidyverse')
library('tidylog')
library('data.table')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_trechos_proc <- sprintf("%s/A_trechos_processados", pasta_modelos)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)
dir.create(pasta_orig_vs_mod, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Pegar ids e distâncias de viagens consideradas no modelo (sem corte na viagem)
# ------------------------------------------------------------------------------

# Abrir arquivos de viagens que foram consideradas nos modelos
base_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')

# Daqui, nos interessam as viagens e as distâncias totais
base_modelo <- 
  base_modelo %>% 
  select(trip_id, dist_total) %>% 
  distinct() %>% 
  separate(trip_id, into = c('trip_id', 'divisor'), sep = '_', remove = TRUE)

# Vamos considerar somente as viagens que não foram divididas em trechos...
vgs_sem_divisao <- base_modelo %>% group_by(trip_id) %>% tally() %>% filter(n == 1)
base_modelo <- base_modelo %>% filter(trip_id %in% vgs_sem_divisao$trip_id)
rm(vgs_sem_divisao)

# ... e às quais o trecho único considerado é o trecho inicial (_00)
base_modelo <- base_modelo %>% filter(divisor == '00') %>% select(-divisor)
head(base_modelo, 20)


# ------------------------------------------------------------------------------
# Pegar qgis_id originais de início e fim dessas mesmas viagens
# ------------------------------------------------------------------------------

# Puxar todos os arquivos resultantes do map matching com trecho de viagem _00
arqs_od <- list.files(pasta_trechos_proc, 
                      pattern = '^[0-9]{6}_00_agrupado.csv', 
                      full.names = TRUE, 
                      recursive = TRUE)

# Ao puxarmos, trouxemos tanto os arquivos com resultados do map matching quanto
# os arquivos de log - queremos só os primeiros
arqs_od <- arqs_od %>% as.data.frame() 
sample_n(arqs_od, 6)

# Criar coluna com trip_id, vinda do nome do arquivo
arqs_od <- arqs_od %>% mutate(trip_id = str_sub(., 70, 75))
head(arqs_od)

# Filtrar somente arquivos que serão considerados, vindos da base_modelo
arqs_od <- arqs_od %>% filter(trip_id %in% base_modelo$trip_id)

# Um arquivo é considerado duas vezes (trip_id == '323082') - remover duplicata
# arqs_od %>% group_by(trip_id) %>% tally() %>% filter(n > 1)
# arqs_od %>% filter(trip_id == '323082')
arqs_od <- arqs_od %>% distinct(trip_id, .keep_all = TRUE)


# Gerar arquivo de saída, para ir abrigando os resultados
out_file <- sprintf('%s/01_origens_e_destinos_viagens_consideradas.csv', pasta_orig_vs_mod)

# Abrir todos os arquivos de interesse e puxar somente origens e destinos
detach("package:tidylog")
for (arq_line in arqs_od$.) {
  # arq_line <- arqs_od %>% slice(1) %>% select('.') %>% pull()
  # print(arq_line)
  
  # Ler arquivo e selecionar colunas de interesse  
  this <- read_delim(arq_line, 
                     delim = ';', 
                     col_types = cols(.default = "c"),
                     col_select = c(trip_id, qgis_id))
  
  # Manter somente a primeira e a última linhas
  this <- this %>% slice(1, nrow(this))
  
  # Criar colunas de qgis_id referentes ao destino e manter só primeira linha
  this <- this %>% mutate(qgis_id_to = shift(qgis_id, type = 'lead')) %>% head(1)
  
  
  # Gravar resultados
  if (file.exists(out_file)) {
    write_delim(this, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(this, out_file, delim = ';', append = FALSE)
  }
  
}

