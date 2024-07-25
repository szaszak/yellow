# Juntar bases do CENSO 2010 para a cidade de São Paulo

# Fontes dos dados - Malha Censitária 2010
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?edicao=26589&t=sobre

# Fontes dos dados - Resultados do Censo 2010
# Censos > Censo Demográfico 2010 > Resultados do Universo > Agregados por Setores Censitários
# https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html

# Censo 2022: População das cidades - Acessar o link e baixar a camada
# População > População residente - Municípios 2022
# https://censo2022.ibge.gov.br/apps/pgi/#/home


# Basico_SP1.csv
# V001 = Domicílios particulares permanentes ou pessoas responsáveis por
# domicílios particulares permanentes
# V002 = Moradores em domicílios particulares permanentes ou população
# residente em domicílios particulares permanentes# 
# V009 = Valor do rendimento nominal médio mensal das pessoas d??? ou mais de 
# idade (com e sem rendimento)

# Pessoa03_SP1.csv
# V001 - Pessoas Residentes
# V002 - Pessoas Residentes e cor ou raça - branca
# V003 - Pessoas Residentes e cor ou raça - preta
# V004 - Pessoas Residentes e cor ou raça - amarela
# V005 - Pessoas Residentes e cor ou raça - parda
# V006 - Pessoas Residentes e cor ou raça - indígena
# Mulheres negras - soma das colunas...
# V168, V170, V173, V175, V178, V180, V183, V185, V198, V200, V203, V205, V208, V210,
# V213, V215, V218, V220, V223, V225, V228, V230, V233, V235, V238, V240, V243, V245
# ... e das colunas V007 e V009 da tabela Pessoa5
# Pessoa05_SP1.csv

# Pessoa13_SP1.csv
# V040, V041, V042, V043, V044 - Pessoas com 6 a 10 anos de idade (ensino fundamental 1)
# V045, V046, V047, V048 - Pessoas com 11 a 14 anos de idade (ensino fundamental 2)
# V049, V050, V051 - Pessoas com 15 a 17 anos de idade (ensino médio)

# Domicilio01_SP1.csv
# V001 - Domicílios particulares e domicílios coletivos
# V002 - Domicílios particulares permanentes
# V003 - Domicílios particulares permanentes do tipo casa
# V004 - Domicílios particulares permanentes do tipo casa de vila ou em condomínio
# V005 - Domicílios particulares permanentes do tipo apartamento

# DomicilioRenda_SP1.csv
# V001 - Total de domicílios particulares improvisados
# V002 - Total do rendimento nominal mensal dos domicílios particulares
# V003 - Total do rendimento nominal mensal dos domicílios particulares permanentes
# V004 - Total do rendimento nominal mensal dos domicílios particulares improvisados
# Domicílios particulares com rendimento nominal mensal domiciliar per capita...
# V005 -  de até 1/8 salário mínimo
# V006 -  de mais de 1/8 a 1/4 salário mínimo
# V007 -  de mais de 1/4 a 1/2 salário mínimo
# V008 -  de mais de 1/2 a 1 salário mínimo
# V009 -  de mais de 1 a 2 salários mínimos
# V010 -  de mais de 2 a 3 salários mínimos
# V011 -  de mais de 3 a 5 salários mínimos
# V012 -  de mais de 5 a 10 salários mínimos
# V013 -  de mais de 10 salários mínimos
# V014 - Domicílios particulares sem rendimento nominal mensal domiciliar per capita

# ResponsavelRenda_SP1.csv
# V001 - Pessoas responsáveis com rendimento nominal mensal de até 1/2 salário mínimo
# V002 - Pessoas responsáveis com rendimento nominal mensal de mais de 1/2 a 1 salário mínimo
# V010 - Pessoas responsáveis sem rendimento nominal mensal
# V045 - Pessoas responsáveis com rendimento nominal mensal de até 1⁄2 salário mínimo, do sexo feminino
# V046 - Pessoas responsáveis com rendimento nominal mensal de mais de 1/2 a 1 salário mínimo, do sexo feminino
# V047 - Pessoas responsáveis com rendimento nominal mensal de mais de 1 a 2 salários mínimos, do sexo feminino
# V054 - Pessoas responsáveis sem rendimento nominal mensal, do sexo feminino

# PessoaRenda_SP1.csv
# V001 - Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de até 1/2 salário mínimo
# V002 - Pessoas de 10 anos ou mais de idade com rendimento nominal mensal de mais de 1/2 a 1 salário mínimo
# V010 - Pessoas de 10 anos ou mais de idade sem rendimento nominal mensal
# V045 - Mulheres de 10 anos ou mais de idade com rendimento nominal mensal de até 1/2 salário mínimo
# V046 - Mulheres de 10 anos ou mais de idade com rendimento nominal mensal de mais de 1/2 a 1 salário mínimo
# V047 - Mulheres de 10 anos ou mais de idade com rendimento nominal mensal de mais de 1 a 2 salários mínimos
# V054 - Mulheres de 10 anos ou mais de idade sem rendimento nominal mensal


# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')
# library('data.table')

# Mostra valores sem notação científica
options(scipen = 999)

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_censo       <- sprintf("%s/CENSO", dados_originais)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)


# ------------------------------------------------------------------------------
# Função principal - abrir arquivos e selecionar variáveis
# ------------------------------------------------------------------------------

# Consolida todos os arquivos em um único dataframe
abrir_e_selecionar <- function(open_files, selected_cols) {
  
  # Criar um dataframe temporário de armazenamento
  tmp <- data.frame()
  
  # Rodar para todos os arquivos
  for (arq in open_files) {
    # arq <- basico$arqs[2]
    # arq <- pessoas3$arqs[27]
    print(arq)
    
    # # Arquivo com os municípios de SP (exceto capital) é o único que está em UTF8
    # if (str_detect(arq, 'SP2') | str_detect(arq, '_ES')) {
    #   this <- read_delim(arq, delim = ';', locale = locale(encoding = 'utf-8'), col_types = cols(.default = 'c'))
    # } else {
    #   this <- read_delim(arq, delim = ';', locale = locale(encoding = 'iso_8859-1'), col_types = cols(.default = 'c'))
    # }
    this <- read_delim(arq, delim = ';', locale = locale(encoding = 'iso_8859-1'), col_types = cols(.default = 'c'))
    
    # Selecionar colunas de interesse
    this <- this %>% select(all_of(selected_cols))
    # names(this)
    
    linhas_problema <- this %>% filter(str_detect(this$Cod_setor, ','))
    if (nrow(linhas_problema) > 0) { print('ATENÇÃO, PROBLEMA \n') }
    
    # Juntar ao dataframe temporário
    tmp <- rbind(tmp, this)
  }
  
  return(tmp)
}



# ------------------------------------------------------------------------------
# Arquivo Básico - população residente, domicílios
# ------------------------------------------------------------------------------

# Listar arquivos baixados
basico <- data.frame(arqs = list.files(pasta_censo, pattern = '^Basico_(.)+.csv', recursive = TRUE, full.names = TRUE))

# Colunas de interesse
sel_cols <- c('Cod_setor', 'Cod_municipio', 'Cod_UF', 'Nome_do_municipio', 'Nome_da_UF ',
              'Situacao_setor', #'Cod_distrito', 'Nome_do_distrito',
              'V001', 'V002', 'V009')

# Consolidar todos os arquivos em um único dataframe
basico <- abrir_e_selecionar(basico$arqs, sel_cols)
# Remover trailing space dos nomes das colunas
names(basico) <- trimws(names(basico))
# basico %>% select(Nome_do_municipio) %>% sample_n(20)
# basico %>% filter(str_starts(Nome_do_municipio, 'IBIRAÇ'))

# Converter e renomear variáveis
basico <-
  basico %>%
  mutate(V009 = str_replace(V009, ',', '.')) %>% 
  mutate_at(vars(matches('V([0-9]){3}')), as.numeric) %>%
  rename(domicilios = V001,
         pop_res    = V002,
         renda_med  = V009)

# Limpar ambiente
rm(tmp_basico, arq, sel_cols)

# basico %>% select(Nome_do_municipio) %>% distinct() %>% tail(20)


# ------------------------------------------------------------------------------
# Pessoa3
# ------------------------------------------------------------------------------

# Listar arquivos baixados
pessoas3 <- data.frame(arqs = list.files(pasta_censo, pattern = '^[Pp]essoa03_(.)+.csv', recursive = TRUE, full.names = TRUE))

# Colunas de interesse
sel_cols <- c('Cod_setor', 'V001', 'V002', 'V003', 'V004', 'V005', 'V006')
              # 'V168', 'V170', 'V173', 'V175', 'V178', 'V180', 'V183', 'V185', 'V198',
              # 'V200', 'V203', 'V205', 'V208', 'V210', 'V213', 'V215', 'V218', 'V220',
              # 'V223', 'V225', 'V228', 'V230', 'V233', 'V235', 'V238', 'V240', 'V243',
              # 'V245'

# Consolidar todos os arquivos em um único dataframe
pessoas3 <- abrir_e_selecionar(pessoas3$arqs, sel_cols)

# Atualizar dataframe, filtrando somente municípios de interesse
pessoas3 <- pessoas3 %>% filter(Cod_setor %in% basico$Cod_setor)
pessoas <- pessoas3

# ------------------------------------------------------------------------------
# Pessoa5
# ------------------------------------------------------------------------------
# 
# # Listar arquivos baixados
# pessoas5 <- data.frame(arqs = list.files(pasta_censo, pattern = '^[Pp]essoa05_(.)+.csv', recursive = TRUE, full.names = TRUE))
# 
# # Colunas de interesse
# sel_cols <- c('Cod_setor', 'V007', 'V009')
# 
# # Consolidar todos os arquivos em um único dataframe
# pessoas5 <- abrir_e_selecionar(pessoas5$arqs, sel_cols)
# 
# # Atualizar dataframe, filtrando somente municípios de interesse
# pessoas5 <- pessoas5 %>% filter(Cod_setor %in% basico$Cod_setor)


# ------------------------------------------------------------------------------
# Junção Pessoa3 e Pessoa5
# ------------------------------------------------------------------------------

# pessoas <- pessoas3 %>% anti_join(pessoas5, by = 'Cod_setor')
# pessoas3 %>% filter(!Cod_setor %in% pessoas5$Cod_setor)
# pessoas5 %>% filter(!Cod_setor %in% pessoas3$Cod_setor)

# Unir bases de pessoas, para cálculo geral
# pessoas <- pessoas3 %>% left_join(pessoas5, by = 'Cod_setor')
# names(pessoas)

# Converter variáveis para número
pessoas <- pessoas %>% mutate_at(vars(matches('V([0-9]){3}')), as.numeric)

# Remover NAs
pessoas <- pessoas %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# # Criar variáveis de raça e simplificar dataframe
# pessoas <-
#   pessoas %>%
#   # Aqui, estamos considerando o recorte do ITDP e Multiplicidade, em que
#   # pessoas brancas incluem as brancas, amarelas e indígenas
#   mutate(pop_res2        = V001,
#          pessoas_negras  = V003 + V005,
#          pessoas_brancas = V002 + V004 + V006,
#          mulheres_negras = select(., 8:37) %>% rowSums(na.rm = TRUE)) %>%
#   select(Cod_setor, pop_res2, pessoas_negras, pessoas_brancas, mulheres_negras)

# Criar variáveis de raça e simplificar dataframe
pessoas <-
  pessoas %>%
  mutate(pop_res2        = V001,
         pessoas_negras  = V003 + V005,
         pessoas_brancas = V002 + V004,
         pessoas_indigen = V006) %>%
  select(Cod_setor, pop_res2, pessoas_negras, pessoas_brancas, pessoas_indigen)


# Limpar ambiente
rm(sel_cols, pessoas3, pessoas5)


# ------------------------------------------------------------------------------
# Pessoa13
# ------------------------------------------------------------------------------

# Listar arquivos baixados
pessoas13 <- data.frame(arqs = list.files(pasta_censo, pattern = '^[Pp]essoa13_(.)+.csv', recursive = TRUE, full.names = TRUE))

# Colunas de interesse
sel_cols <- c('Cod_setor', sprintf('V0%d', seq(40, 51)))

# Consolidar todos os arquivos em um único dataframe
pessoas13 <- abrir_e_selecionar(pessoas13$arqs, sel_cols)

# Atualizar dataframe, filtrando somente municípios de interesse
pessoas13 <- pessoas13 %>% filter(Cod_setor %in% basico$Cod_setor)

# Converter variáveis para número
pessoas13 <- pessoas13 %>% mutate_at(vars(matches('V([0-9]){3}')), as.numeric)

# Remover NAs
pessoas13 <- pessoas13 %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))

# Criar variáveis de faixa etária
pessoas13 <-
  pessoas13 %>%
  mutate(pessoas_06_08 = V040 + V041 + V042,
         pessoas_06_10 = V040 + V041 + V042 + V043 + V044,
         pessoas_11_14 = V045 + V046 + V047 + V048,
         pessoas_15_17 = V049 + V050 + V051) %>%
  select(Cod_setor, pessoas_06_08, pessoas_06_10, pessoas_11_14, pessoas_15_17)


# Juntar ao dataframe de pessoas
pessoas <- pessoas %>% left_join(pessoas13, by = 'Cod_setor')

# Limpar ambiente
rm(sel_cols, pessoas13)


# # ------------------------------------------------------------------------------
# # Renda da Pessoa
# # ------------------------------------------------------------------------------
# 
# # Listar arquivos baixados
# pess_renda <- data.frame(arqs = list.files(pasta_censo, pattern = '^PessoaRenda_(.)+.csv', recursive = TRUE, full.names = TRUE))
# 
# # Colunas de interesse
# sel_cols <- c('Cod_setor', 'V001', 'V002', 'V010', 'V045', 'V046', 'V047', 'V054')
# 
# # Consolidar todos os arquivos em um único dataframe
# pess_renda <- abrir_e_selecionar(pess_renda$arqs, sel_cols)
# 
# # Atualizar dataframe, filtrando somente municípios de interesse
# pess_renda <- pess_renda %>% filter(Cod_setor %in% basico$Cod_setor)
# 
# # Converter variáveis para número
# pess_renda <- pess_renda %>% mutate_at(vars(matches('V([0-9]){3}')), as.numeric)
# 
# # Remover NAs
# pess_renda <- pess_renda %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))
# 
# # Criar colunas com somas das rendas das pessoas até 1 SM
# pess_renda <-
#   pess_renda %>%
#   mutate(pessoas_ate1SM  = V010 + V001 + V002,
#          mulheres_ate1SM = V054 + V045 + V046,
#          mulheres_ate2SM = V054 + V045 + V046 + V047) %>%
#   select(Cod_setor, pessoas_ate1SM, mulheres_ate1SM, mulheres_ate2SM)
# 
# # Limpar ambiente
# rm(sel_cols)
# 
# 
# # ------------------------------------------------------------------------------
# # Responsável Renda
# # ------------------------------------------------------------------------------
# 
# # Listar arquivos baixados
# resp_renda <- data.frame(arqs = list.files(pasta_censo, pattern = '^ResponsavelRenda_(.)+.csv', recursive = TRUE, full.names = TRUE))
# 
# # Colunas de interesse
# sel_cols <- c('Cod_setor', 'V001', 'V002', 'V010', 'V045', 'V046', 'V047', 'V054')
# 
# # Consolidar todos os arquivos em um único dataframe
# resp_renda <- abrir_e_selecionar(resp_renda$arqs, sel_cols)
# 
# # Atualizar dataframe, filtrando somente municípios de interesse
# resp_renda <- resp_renda %>% filter(Cod_setor %in% basico$Cod_setor)
# 
# # Converter variáveis para número
# resp_renda <- resp_renda %>% mutate_at(vars(matches('V([0-9]){3}')), as.numeric)
# 
# # Remover NAs
# resp_renda <- resp_renda %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))
# 
# # Criar colunas com somas de mulheres responsáveis até 1 e 2 SM
# resp_renda <-
#   resp_renda %>%
#   mutate(pessoas_resp_ate1SM  = V010 + V001 + V002,
#          mulheres_resp_ate1SM = V054 + V045 + V046,
#          mulheres_resp_ate2SM = V054 + V045 + V046 + V047) %>%
#   select(Cod_setor, pessoas_resp_ate1SM, mulheres_resp_ate1SM, mulheres_resp_ate2SM)
# 
# # Limpar ambiente
# rm(sel_cols)
# 
# 
# # ------------------------------------------------------------------------------
# # Domicílio Renda
# # ------------------------------------------------------------------------------
# 
# # Listar arquivos baixados
# dom_renda <- data.frame(arqs = list.files(pasta_censo, pattern = '^DomicilioRenda_(.)+.csv', recursive = TRUE, full.names = TRUE))
# 
# # Colunas de interesse
# sel_cols <- c('Cod_setor', 'V005', 'V006', 'V007', 'V008', 'V009',
#               'V010', 'V011', 'V012', 'V013', 'V014')
# 
# # Consolidar todos os arquivos em um único dataframe
# dom_renda <- abrir_e_selecionar(dom_renda$arqs, sel_cols)
# 
# # Atualizar dataframe, filtrando somente municípios de interesse
# dom_renda <- dom_renda %>% filter(Cod_setor %in% basico$Cod_setor)
# 
# # Converter variáveis para número
# dom_renda <- dom_renda %>% mutate_at(vars(matches('V([0-9]){3}')), as.numeric)
# 
# # Remover NAs
# dom_renda <- dom_renda %>% mutate(across(where(is.numeric), ~replace_na(.x, 0)))
# 
# # Criar categorias de colunas de renda domiciliar per capita
# dom_renda <-
#   dom_renda %>%
#   mutate(DR_0_meio = V005 + V006 + V007 + V014,
#          DR_meio_1 = V008,
#          DR_1_3 = V009 + V010,
#          DR_3_mais = V011 + V012 + V013) %>%
#   select(Cod_setor, DR_0_meio, DR_meio_1, DR_1_3, DR_3_mais)
# 
# # Limpar ambiente
# rm(sel_cols)
# 
# 
# # ------------------------------------------------------------------------------
# # Juntar todos os dados
# # ------------------------------------------------------------------------------
# 
# # Criar dataframe de saída
# censo <-
#   basico %>%
#   left_join(pessoas, by = 'Cod_setor') %>%
#   left_join(pess_renda, by = 'Cod_setor') %>%
#   left_join(resp_renda, by = 'Cod_setor') %>%
#   left_join(dom_renda, by = 'Cod_setor')
# 
# # Gravar resultados
# out_file <- sprintf('%s/censo2010_todas_as_cidades.csv', pasta_saida)
# write_delim(censo, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Juntar todos os dados do Censo ao shapefile e fazer cálculos proporcionais
# ------------------------------------------------------------------------------

# Criar dataframe de saída
censo <- basico %>% left_join(pessoas, by = 'Cod_setor')

# Manter somente colunas de cod_setor e dados agregados
censo <- censo %>% select(cod_setor = Cod_setor, 7:17)


# Abrir shapefile de setores censitários
setores_hex <- sprintf('%s/hex_grid_sp_res09_areas_setores_censitarios.gpkg', pasta_aop_optimum)
setores_hex <- read_sf(setores_hex)

# Garantir que shapefile não apresente erros
# setores_hex <- setores_hex %>% st_make_valid()
# mapview(setores_hex)

# Juntar setores censitários com censo
setores_hex <- setores_hex %>% left_join(censo, by = 'cod_setor')
# mapview(setores_dados)

# Mover coluna de geometry para a última posição
# https://stackoverflow.com/questions/43897844/r-move-column-to-last-using-dplyr
setores_hex <- setores_hex %>% relocate(geom, .after = last_col())
head(setores_hex)

# Colunas para cálculos proporcionais
calc_cols <- c('pop_res2', 'domicilios',
               'pessoas_negras', 'pessoas_brancas', 'pessoas_indigen',
               'pessoas_06_08', 'pessoas_06_10', 'pessoas_11_14', 'pessoas_15_17')

# Fazer cálculos proporcionais por área
censo_hex <- 
  setores_hex %>% 
  st_drop_geometry() %>%
  # Calcular proporcionais para as variáveis de população
  mutate(across(.cols = all_of(calc_cols),
                # .fns = ~ .x * area_prop,
                # .fns = function(x) { round(x * area_prop) },
                .fns = function(x) { x * area_prop },
                .names = '{.col}_hex')) %>% 
  # Manter somente os resultados proporcionais e o id_hex (id do hexágono)
  select(matches('_hex$')) %>%
  # Somar dados por hexágono
  group_by(id_hex) %>%
  summarise_all(sum, na.rm = TRUE) %>% 
  # Arredondar valores
  mutate(across(where(is.numeric), round))

# Checarem - quantas pessoas ficam de fora ao fazer o cálculo proporcional?
# sum(censo$pop_res2) - sum(censo_hex$pop_res2_hex)
# sum(censo$pessoas_15_17) - sum(censo_hex$pessoas_15_17_hex)
# sum(censo$pessoas_06_08) - sum(censo_hex$pessoas_06_08_hex)

# Gravar resultados
out_file <- sprintf('%s/hex_grid_sp_res09_dados_censo_por_hexagono.csv', pasta_aop_optimum)
write_delim(censo_hex, out_file, delim = ';')
