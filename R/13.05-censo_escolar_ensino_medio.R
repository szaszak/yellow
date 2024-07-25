# Junta os dados do Censo Escolar 2019 (em especial, matrículas) aos dados de 
# georreferenciamento das escolas. Para as que não possuíam latlong, a atualização
# da localização é feita de forma manual

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('mapview')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais", pasta_dados)
pasta_inep        <- sprintf("%s/INEP", dados_originais)
pasta_ipea        <- sprintf("%s/IPEA", dados_originais)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)


# ------------------------------------------------------------------------------
# Dados de matrículas e localização - IPEA - 339.805 matrículas no ensino médio
# ------------------------------------------------------------------------------

# M004 - Number of school enrollments - high schools

# aop_landuse <- sprintf('%s/aop_landuse_2019_v2.csv', pasta_ipea)
# aop_landuse <- read_delim(aop_landuse, delim = ',', col_types = cols(.default = "c"))
# aop_landuse <- aop_landuse %>% filter(code_muni == '3550308')
# aop_landuse %>% mutate(M004 = as.numeric(M004)) %>% select(M004) %>% sum()


# ------------------------------------------------------------------------------
# Dados de geolocalização das escolas
# ------------------------------------------------------------------------------

# Catálogo de escolas do INEP, filtradas para o município de São Paulo
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/inep-data/catalogo-de-escolas

# Catálogo de escolas de SP
escolas <- sprintf('%s/Inepdata - Escolas SP.csv', pasta_inep)
escolas <- read_delim(escolas, delim = ';', col_types = cols(.default = "c"))

escolas <- 
  escolas %>% 
  # filter(`Restrição de Atendimento` == 'ESCOLA EM FUNCIONAMENTO E SEM RESTRIÇÃO DE ATENDIMENTO') %>%
  # filter(str_detect(`Etapas e Modalidade de Ensino Oferecidas`, 'Ensino Médio')) %>% 
  select(CO_ENTIDADE = `Código INEP`,
         # NOME = Escola,
         LOGRADOURO = Endereço, 
         CA_CATEGORIA   = 'Categoria Administrativa',
         CA_DEPENDENCIA = 'Dependência Administrativa',
         CA_ETAPAS      = 'Etapas e Modalidade de Ensino Oferecidas',
         CA_RESTRICAO   = 'Restrição de Atendimento',
         Latitude,
         Longitude)

# Melhorar redação do endereço
escolas <- 
  escolas %>% 
  mutate(LOGRADOURO_REV = str_replace(LOGRADOURO, '^([A-Z ]*) PROFESSOR,', 'PROFESSOR \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*) CORONEL,', 'CORONEL \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*) DEPUTADO,', 'DEPUTADO \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*) DOUTOR,', 'DOUTOR \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*) DOM,', 'DOM \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*) DONA,', 'DONA \\1,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*), ([A-Z0-9 ]*) ALAMEDA\\.', 'ALAMEDA \\1 \\2,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*), ([A-Z0-9 ]*) AVENIDA\\.', 'AVENIDA \\1 \\2,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*), ([A-Z0-9 ]*) ESTRADA\\.', 'ESTRADA \\1 \\2,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*), ([A-Z0-9 ]*) RUA\\.', 'RUA \\1 \\2,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^([A-Z ]*), ([A-Z0-9 ]*) TRAVESSA\\.', 'TRAVESSA \\1 \\2,'),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^ALAMEDA ALAMEDA ', 'ALAMEDA '),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^AVENIDA AVENIDA ', 'AVENIDA '),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^AVENIDA RUA ', 'AVENIDA '),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^ESTRADA ESTRADA ', 'ESTRADA '),
         LOGRADOURO_REV = str_replace(LOGRADOURO_REV, '^RUA RUA ', 'RUA '),
         .before = 'LOGRADOURO')




# ------------------------------------------------------------------------------
# Dados de matrículas (ver dicionário de dados para variáveis)
# ------------------------------------------------------------------------------

# Microdados do Censo Escolar da Educação Básica 2019 (Atualizado em 8/3/2023)
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar

# Dados de matrículoas do Censo Escolar 2019
matriculas <- sprintf('%s/microdados_ed_basica_2019/dados/microdados_ed_basica_2019.csv', pasta_inep)
matriculas <- read_delim(matriculas, delim = ';', col_types = cols(.default = "c"), locale = locale(encoding = 'iso_8859-1'))

# Considerar somente cidade de SP
matriculas <- matriculas %>% filter(CO_MUNICIPIO == '3550308')

# Ler vagas como números
matriculas <- matriculas %>% mutate_at(vars(matches('QT_MAT_')), as.numeric)

# # Manter somente variáveis de interesse (ver dicionário de dados)
# matriculas <- matriculas %>% select(QT_MAT_BAS,
#                                     QT_MAT_INF,
#                                     QT_MAT_INF_CRE,
#                                     QT_MAT_INF_PRE,
#                                     QT_MAT_FUND,
#                                     QT_MAT_FUND_AI,
#                                     QT_MAT_FUND_AF,
#                                     QT_MAT_MED,
#                                     QT_MAT_PROF,
#                                     QT_MAT_PROF_TEC,
#                                     QT_MAT_EJA,
#                                     QT_MAT_EJA_FUND,
#                                     QT_MAT_EJA_MED,
#                                     QT_MAT_ESP,
#                                     QT_MAT_ESP_CC,
#                                     QT_MAT_ESP_CE,
#                                     QT_MAT_BAS_0_3,
#                                     QT_MAT_BAS_4_5,
#                                     QT_MAT_BAS_6_10,
#                                     QT_MAT_BAS_11_14,
#                                     QT_MAT_BAS_15_17,
#                                     QT_MAT_BAS_18_MAIS,
#                                     QT_MAT_BAS_D,
#                                     QT_MAT_BAS_N,
#                                     QT_MAT_BAS_EAD,
#                                     QT_MAT_BAS_FEM,
#                                     QT_MAT_BAS_MASC
#                                     )
# 
# # 2.703.300
# sum(matriculas$QT_MAT_BAS)
# 
# sum(matriculas$QT_MAT_INF) == sum(matriculas$QT_MAT_INF_CRE) + sum(matriculas$QT_MAT_INF_PRE) # TRUE
# sum(matriculas$QT_MAT_FUND) == sum(matriculas$QT_MAT_FUND_AI) + sum(matriculas$QT_MAT_FUND_AF) # TRUE  
# sum(matriculas$QT_MAT_EJA) == sum(matriculas$QT_MAT_EJA_FUND) + sum(matriculas$QT_MAT_EJA_MED) # TRUE
# sum(matriculas$QT_MAT_ESP) == sum(matriculas$QT_MAT_ESP_CC) + sum(matriculas$QT_MAT_ESP_CE) # TRUE
# sum(matriculas$QT_MAT_PROF) == sum(matriculas$QT_MAT_PROF_TEC) # FALSE
# 
# 
# # 2.764.042
# matriculas %>% 
#   select(QT_MAT_INF,  # 689.420
#          QT_MAT_FUND, # 1.387.887
#          QT_MAT_MED,  # 388.593
#          QT_MAT_PROF, # 134.777
#          # QT_MAT_PROF_TEC, # 134.526
#          QT_MAT_EJA,  # 121.111
#          QT_MAT_ESP   # 42.254
#          ) %>% 
#   sum()
# 
# # 2.703.300
# matriculas %>% 
#   select(QT_MAT_BAS_0_3,
#          QT_MAT_BAS_4_5,
#          QT_MAT_BAS_6_10,
#          QT_MAT_BAS_11_14,
#          QT_MAT_BAS_15_17,
#          QT_MAT_BAS_18_MAIS) %>% 
#   sum()
# 
# # 2.703.300
# matriculas %>% 
#   select(QT_MAT_BAS_D,
#          QT_MAT_BAS_N,
#          QT_MAT_BAS_EAD) %>% 
#   sum()
# 
# # 2.703.300
# matriculas %>% 
#   select(QT_MAT_BAS_FEM,
#          QT_MAT_BAS_MASC) %>% 
#   sum()


# Filtrar conforme IPEA e Tainá Bittencourt
matriculas <-
  matriculas %>%
  # Considerar somente cidade de SP
  filter(CO_MUNICIPIO == '3550308') %>%
  # Situação de funcionamento - 1. Em atividade
  filter(TP_SITUACAO_FUNCIONAMENTO == '1') %>%
  # Etapa de Ensino - Ensino Médio, Educação Profissional, Educação Profissional
  # Técnica, EJA - Ensino Médio (Possui uma ou mais matrículas)
  filter(IN_MED == '1' | IN_PROF == '1' | IN_PROF_TEC == '1' | IN_EJA_MED == '1') %>%
  # Modo, maneira ou metodologia de ensino correspondente às turmas com etapas de escolarização consecutivas, Creche ao Ensino Médio
  filter(IN_REGULAR == '1') %>%
  # Número de Matrículas na Educação Básica - Entre 15 e 17 anos de idade, Número
  # de Matrículas na Educação Básica - Com 18 ou mais anos de idade
  # filter(QT_MAT_BAS_15_17 != '0' | QT_MAT_BAS_18_MAIS != '0') %>%
  # Remover escolas prisionais
  filter(IN_LOCAL_FUNC_UNID_PRISIONAL == '0' & IN_LOCAL_FUNC_PRISIONAL_SOCIO == '0')

# Checar - vagas para idades de 15 a 17 anos devem ser todos maior que zero
matriculas <- matriculas %>% filter(QT_MAT_BAS_15_17 != '0' | QT_MAT_BAS_18_MAIS != '0')

# Manter somente variáveis de interesse (ver dicionário de dados)
matriculas <- matriculas %>% select(CO_ENTIDADE,
                                    NO_ENTIDADE,
                                    TP_DEPENDENCIA,
                                    TP_CATEGORIA_ESCOLA_PRIVADA,
                                    TP_LOCALIZACAO,
                                    DS_ENDERECO,
                                    NU_ENDERECO,
                                    DS_COMPLEMENTO,
                                    NO_BAIRRO,
                                    CO_CEP,
                                    # TP_SITUACAO_FUNCIONAMENTO,
                                    # IN_LOCAL_FUNC_PREDIO_ESCOLAR,
                                    # IN_LOCAL_FUNC_SALAS_EMPRESA,
                                    # IN_LOCAL_FUNC_SOCIOEDUCATIVO,
                                    # IN_LOCAL_FUNC_UNID_PRISIONAL,
                                    # IN_LOCAL_FUNC_PRISIONAL_SOCIO,
                                    # IN_LOCAL_FUNC_TEMPLO_IGREJA,
                                    # IN_LOCAL_FUNC_CASA_PROFESSOR,
                                    # IN_LOCAL_FUNC_GALPAO,
                                    # IN_REGULAR,
                                    # IN_DIURNO,
                                    # IN_NOTURNO,
                                    # IN_EAD,
                                    # IN_BAS,
                                    # IN_INF,
                                    # IN_INF_CRE,
                                    # IN_INF_PRE,
                                    # IN_FUND,
                                    # IN_FUND_AI,
                                    # IN_FUND_AF,
                                    # IN_MED,
                                    # IN_PROF,
                                    # IN_PROF_TEC,
                                    # IN_EJA,
                                    # # IN_EJA_FUND,
                                    # IN_EJA_MED,
                                    # IN_ESP,
                                    # IN_ESP_CC,
                                    # IN_ESP_CE,
                                    # QT_MAT_BAS,
                                    # QT_MAT_INF,
                                    # QT_MAT_INF_CRE,
                                    # QT_MAT_INF_PRE,
                                    # QT_MAT_FUND,
                                    # QT_MAT_FUND_AI,
                                    # QT_MAT_FUND_AF,
                                    QT_MAT_MED,
                                    # QT_MAT_PROF,
                                    # QT_MAT_PROF_TEC,
                                    # QT_MAT_EJA,
                                    # QT_MAT_EJA_FUND,
                                    # QT_MAT_EJA_MED,
                                    # QT_MAT_ESP,
                                    # QT_MAT_ESP_CC,
                                    # QT_MAT_ESP_CE,
                                    # QT_MAT_BAS_0_3,
                                    # QT_MAT_BAS_4_5,
                                    # QT_MAT_BAS_6_10,
                                    # QT_MAT_BAS_11_14,
                                    QT_MAT_BAS_15_17,
                                    # QT_MAT_BAS_18_MAIS,
                                    # QT_MAT_BAS_D,
                                    # QT_MAT_BAS_N,
                                    # QT_MAT_BAS_FEM,
                                    # QT_MAT_BAS_MASC,
                                    # QT_MAT_BAS_EAD,
                                    # QT_MAT_INF_INT,
                                    # QT_MAT_INF_CRE_INT,
                                    # QT_MAT_INF_PRE_INT,
                                    # QT_MAT_FUND_INT,
                                    # QT_MAT_FUND_AI_INT,
                                    # QT_MAT_FUND_AF_INT,
                                    # QT_MAT_MED_INT,
                                    # QT_DOC_BAS,
                                    # QT_DOC_INF,
                                    # QT_DOC_INF_CRE,
                                    # QT_DOC_INF_PRE,
                                    # QT_DOC_FUND,
                                    # QT_DOC_FUND_AI,
                                    # QT_DOC_FUND_AF,
                                    # QT_DOC_MED,
                                    # QT_DOC_PROF,
                                    # QT_DOC_PROF_TEC,
                                    # QT_DOC_EJA,
                                    # QT_DOC_EJA_FUND,
                                    # QT_DOC_EJA_MED,
                                    # QT_DOC_ESP,
                                    # QT_DOC_ESP_CC,
                                    # QT_DOC_ESP_CE,
                                    # QT_TUR_BAS,
                                    # QT_TUR_INF,
                                    # QT_TUR_INF_CRE,
                                    # QT_TUR_INF_PRE,
                                    # QT_TUR_FUND,
                                    # QT_TUR_FUND_AI,
                                    # QT_TUR_FUND_AF,
                                    # QT_TUR_MED,
                                    # QT_TUR_PROF,
                                    # QT_TUR_PROF_TEC,
                                    # QT_TUR_EJA,
                                    # QT_TUR_EJA_FUND,
                                    # QT_TUR_EJA_MED,
                                    # QT_TUR_ESP,
                                    # QT_TUR_ESP_CC,
                                    # QT_TUR_ESP_CE
)
head(matriculas)


# ------------------------------------------------------------------------------
# Juntar dados de matrículas à geolocalização das escolas
# ------------------------------------------------------------------------------

# Matrículas de ensino médio em tempo integral (QT_MAT_MED_INT) sempre são menores
# ou iguais às matrículas de ensino médio (QT_MAT_MED), o que indica que QT_MAT_MED
# contempla QT_MAT_MED_INT
# 
# Matrículas de ensino básico de 15 a 17 anos (QT_MAT_BAS_15_17) são menores do 
# que as matrículas gerais de ensino médio (QT_MAT_MED), o que indica que há 
# estudantes no ensino médio que estão fora dessa faixa etária

# Juntar latlon de escolas
matriculas <- matriculas %>% left_join(escolas, by = 'CO_ENTIDADE')

# Fazer geocode das escolas que estão sem essas infos
escolas_com_geocode <- matriculas %>% filter(!is.na(Latitude))
escolas_sem_geocode <- matriculas %>% filter(is.na(Latitude))

# Dados de latlon pesquisados no Google
# escolas_sem_geocode %>% slice(30) %>% t()
novos <- list(c('35000280', '-23.43549830732259', '-46.717311856877146'),
              c('35004964', '-23.50239921492758', '-46.640273428018496'),
              c('35005594', '-23.60500366377796', '-46.700306245263770'),
              c('35005969', '-23.56972383024869', '-46.725713324914830'),
              c('35100092', '-23.50883722480011', '-46.655538880581304'),
              c('35101761', '-23.59055303342240', '-46.502778346716820'),
              c('35102027', '-23.52339402675061', '-46.525759478069830'),
              c('35102258', '-23.54721583377207', '-46.408668665802050'),
              c('35102635', '-23.53166288875540', '-46.719832630278480'),
              c('35102982', '-23.53236999193194', '-46.668661565368310'),
              c('35103263', '-23.53217018826825', '-46.643521044421874'),
              c('35104887', '-23.54407115102775', '-46.697065417898706'),
              c('35105797', '-23.61704213051163', '-46.670948818093500'),
              c('35106124', '-23.61634441160014', '-46.656377592628810'),
              c('35120418', '-23.61308502322052', '-46.623284773075300'),
              c('35134120', '-23.55391493562721', '-46.553488348382040'),
              c('35136232', '-23.54317445524563', '-46.683901146886590'),
              c('35137085', '-23.69371199935261', '-46.660254651204156'),
              c('35138198', '-23.46723162909702', '-46.631574400015470'),
              c('35138253', '-23.47448114403186', '-46.613031467347670'),
              c('35140200', '-23.65531744692543', '-46.668458407159490'),
              c('35140363', '-23.60984792482514', '-46.704479095232806'),
              c('35142220', '-23.55724323175262', '-46.444820662891600'),
              c('35151737', '-23.58426024614579', '-46.419181279609490'),
              c('35157685', '-23.51480910920090', '-46.582934487769380'),
              c('35173150', '-23.48877743626911', '-46.687795806420450'),
              c('35392315', '-23.50177232668354', '-46.635089851253376'),
              c('35567985', '-23.47196032465768', '-46.636430514957740'),
              c('35580193', '-23.55479321607006', '-46.509930934988040'),
              c('35812912', '-23.50103180667017', '-46.746285465807650')
)

# Criar e preencher nova matriz com dados de latlong
novos_geocodes <- matrix(nrow = 30, ncol = 3)
n <- 1
for (i in novos) { novos_geocodes[n,] <- i; n <- n + 1 }
rm(n, i)

# Tranformar matriz em dataframe para join
novos_geocodes <- novos_geocodes %>% as.data.frame() %>% setNames(c('CO_ENTIDADE', 'Latitude', 'Longitude'))

# Agregar dados de latlong às escolas que estavam sem 
escolas_sem_geocode <- 
  escolas_sem_geocode %>% 
  select(-c(Latitude, Longitude)) %>% 
  left_join(novos_geocodes, by = 'CO_ENTIDADE')

# Recompor o dataframe de matrículas
matriculas <- rbind(escolas_com_geocode, escolas_sem_geocode) %>% arrange(CO_ENTIDADE)

# Tranformar em sf para exportar
matriculas <- 
  matriculas %>%
  mutate(lon = as.double(Longitude),
         lat = as.double(Latitude)) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# mapview(matriculas, cex = 1)


# Gravar resultados
out_file <- sprintf('%s/matriculas_censo_escolar_2019_georref.gpkg', pasta_aop_optimum)
st_write(matriculas, out_file, driver = 'GPKG', append = FALSE)
