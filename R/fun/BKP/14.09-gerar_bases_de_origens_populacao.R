# Isolar dados dos domicílios do CNEFE 2022 georreferenciados (dasimetria) só 
# para a cidade de SP. O processo de dasimetria foi feito junto ao projeto GeoReDUS, 
# do Centro de Estudos da Metrópole - CEM e da Frente Nacional de Prefeitos - FNP

source('fun/setup.R')
library('arrow')

# Estrutura de pastas
pasta_dados_cnefe    <- "../../georedus/Dados/01_CNEFE_GEO/2022"
pasta_dados_cnefe_ag <- "../../georedus/Dados/00_TMP_APAGAR/03_TMP_CNEFE_INT/06_CNEFE_URBANOS_2022"
pasta_dados_univ  <- "../../georedus/Dados/02_UNIVERSO/2022"
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/CENSO", pasta_dados)


# ------------------------------------------------------------------------------
# CNEFE 2022 - Domicílios particulares e coletivos para a cidade de SP (3550308)
# ------------------------------------------------------------------------------

# CNEFE - Todos os domicílios com latlon original
cnefe_sp <- sprintf('%s/35_CNEFE_2022.parquet', pasta_dados_cnefe)
cnefe_sp <- open_dataset(cnefe_sp)

# Puxar somente domicílios do município de SP
cnefe_sp <-
  cnefe_sp %>%
  # Filtrar domicílios particulares e coletivos
  filter(COD_ESPECIE %in% c('1', '2')) %>%
  # Município de São Paulo: 3550308
  filter(str_starts(CD_GEO, '3550308')) %>% 
  select(CD_GEO, COD_ESPECIE, lat, lon) %>%
  collect()

# Guardar cópia na pasta de Dados Originais
out_file <- sprintf('%s/CNEFE_SP_2022.parquet', dados_originais)
write_parquet(cnefe_sp, out_file)


# CNEFE - Domicílios agrupados por hexágono
cnefe_sp_ag <- sprintf('%s/cnefe_setor_hexagono_urbanos_agrupados_2022.parquet', pasta_dados_cnefe_ag)
cnefe_sp_ag <- open_dataset(cnefe_sp_ag)

cnefe_sp_ag <- cnefe_sp_ag %>% filter(str_starts(CD_SETOR, '3550308')) %>% collect()

# Guardar cópia na pasta de Dados Originais
out_file <- sprintf('%s/CNEFE_SP_2022_Setor_Hexagonos_Urbanos_Agrupados.parquet', dados_originais)
write_parquet(cnefe_sp_ag, out_file)


# ------------------------------------------------------------------------------
# CENSO 2022 - Básico, domicílios, pessoas isolados para a cidade de SP (3550308)
# ------------------------------------------------------------------------------

# Arquivo Básico
basico <- sprintf('%s/01_basico.parquet', pasta_dados_univ)
basico <- open_dataset(basico)
basico <- basico %>% filter(str_starts(cd_setor, '3550308')) %>% collect()
# Guardar cópia na pasta de Dados Originais
out_file <- sprintf('%s/CENSO_SP_2022_Basico.parquet', dados_originais)
write_parquet(basico, out_file)

# Domicílio
domicilio <- sprintf('%s/02_domicilio.parquet', pasta_dados_univ)
domicilio <- open_dataset(domicilio)
domicilio <- domicilio %>% filter(str_starts(cd_setor, '3550308')) %>% collect()
# Guardar cópia na pasta de Dados Originais
out_file <- sprintf('%s/CENSO_SP_2022_Domicilio.parquet', dados_originais)
write_parquet(domicilio, out_file)

# Pessoas
pessoas <- sprintf('%s/03_pessoas.parquet', pasta_dados_univ)
pessoas <- open_dataset(pessoas)
pessoas <- pessoas %>% filter(str_starts(cd_setor, '3550308')) %>% collect()
# Guardar cópia na pasta de Dados Originais
out_file <- sprintf('%s/CENSO_SP_2022_Pessoas.parquet', dados_originais)
write_parquet(pessoas, out_file)



# Dados Censo possuem pessoas só na faixa de 15-19 anos
pessoas15_19 <- pessoas %>% select(cd_setor, 
                                   masc_15_19 = V01012, 
                                   fem_15_19 = V01023, 
                                   tot_15_19 = V01034)



length(unique(basico$cd_setor))
length(unique(cnefe_sp_ag$CD_SETOR))

basico %>% 
  filter(!cd_setor %in% cnefe_sp_ag$CD_SETOR) %>% 
  filter(V0001 > 0) %>% 
  select(cd_setor, matches('^V0*'))




media_moradores <- basico %>% select(cd_setor, matches('^V0*'))

# Separar as variáveis de média e de valores absolutos
vars_med <- colnames(basico) %>% str_subset("V0005")
vars_abs <- colnames(basico) %>% discard( . %in% c(vars_med, "cd_setor")) %>% keep(str_starts(., 'V0'))

# Cálculo proporcional da dasimetria
cnefe_sp_ag <- cnefe_sp_ag %>% mutate(CD_SETOR = str_extract(CD_SETOR, '\\d+'))

this <- 
  cnefe_sp_ag %>% 
  group_by(CD_SETOR) %>%
  mutate(den_setor = sum(dom_urbanos_validos, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(h3_address) %>%
  mutate(den_hex = sum(dom_urbanos_validos, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(media_moradores, by = c('CD_SETOR' = 'cd_setor')) %>%
  mutate_at(vars_med, ~ (p = . * dom_urbanos_validos / den_hex)) %>%
  mutate_at(vars_abs, ~ (p = . * dom_urbanos_validos / den_setor)) %>%
  group_by(h3_address) %>%
  summarise_at(c(vars_med, vars_abs), ~ (p = sum(., na.rm = TRUE)))

this

# Pessoas = 11100736
sum(this$V0001)

cnefe_sp_ag %>% 
  group_by(CD_SETOR) %>%
  summarise(dom_validos = sum(dom_urbanos_validos, na.rm = TRUE))