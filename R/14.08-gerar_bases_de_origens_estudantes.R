# Gerar as base de estudantes, distribuída proporcionalmente de forma integralizada
# após a dasimetria dos domicílios CNEFE do Censo 2022. Os filtros são de estudantes
# do ensino médio, independentemente da faixa etária

library('tidyverse')
library('tidylog')
library('sf')
library('arrow')


# Estrutura de pastas
pasta_dados       <- "../../yellow_dados"
dados_originais   <- sprintf("%s/00_dados_originais/Metro", pasta_dados)
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_dasimetria     <- sprintf("%s/03_dasimetria", pasta_aop_2024_2028)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)


# ------------------------------------------------------------------------------
# Transformar aquivos originais em .parquet em .gpkg
# ------------------------------------------------------------------------------

# library('foreign')
# library('readxl')
# library('openxlsx')

# # Transformar arquivo original .dbf em .parquet
# pasta_origin <- sprintf('%s/Arquivos_Metro/OD_2023/PESQUISA_ORIGEM_E_DESTINO_2023_anexos', pasta_base)
# dados <- sprintf('%s/Banco2023_divulgacao_120225.dbf', pasta_origin)
# dados <- read.dbf(dados) %>% as_tibble()
# head(dados)
#
# # Consertar primeiro ID_DOM, que está como NA
# dados <- dados %>%
#   # select(1:5) %>%
#   group_by(ZONA, MUNI_DOM) %>%
#   # Fill NAs with the first non-NA value in the group
#   mutate(ID_DOM = replace(ID_DOM, is.na(ID_DOM), first(ID_DOM[!is.na(ID_DOM)]))) %>%
#   # After filling the first NA, propagate values downward
#   # mutate(ID_DOM = zoo::na.locf(ID_DOM, fromLast = FALSE, na.rm = FALSE)) %>%
#   ungroup()
#
# write_parquet(dados, od_file)
#
# shape <- sprintf('%s/002_Site Metro Mapas/Shape/Zonas_2023.shp', pasta_origin)
# shape <- st_read(shape, options = "ENCODING=WINDOWS-1252")
# head(shape)
#
# shape <- shape %>% st_make_valid()
# shape <- shape %>% st_transform(31983)
#
# out_shape <- sprintf('%s/Zonas_OD_2023.gpkg', pasta_dados)
# st_write(shape, out_shape, driver = 'GPKG', append = FALSE)
#
# rm(dados, shape, pasta_origin, out_shape)

# A tibble: 143,038 × 5



# ------------------------------------------------------------------------------
# Colunas da OD 2023 exportadas para município de SP
# ------------------------------------------------------------------------------

od <- sprintf('%s/20250212_OD_2023.parquet', dados_originais)
od <- open_dataset(od)
# schema(od)

# Somente domicílios do município de SP
od_sp <- od %>%
  filter(MUNI_DOM == 36) %>%
  select(ZONA,
         ID_DOM, DOM, F_DOM, FE_DOM, TIPO_DOM, NO_MORAD, CD_ENTRE, CO_DOM_X, CO_DOM_Y,
         ID_FAM, FAMILIA, F_FAM, FE_FAM, TOT_FAM, NO_MORAF, CRITERIOBR, RENDA_FA, CD_RENFA,
         ID_PESS, PESSOA, F_PESS, FE_PESS, IDADE, SEXO, RA_A, ESTUDA, GRAU_INS,
         CD_ATIVI, CO_REN_I, VL_REN_I,
         ZONA_ESC, MUNIESC, CO_ESC_X, CO_ESC_Y, TIPO_ESC, CD_PRES_HI, CD_TIPO_HI, QT_HIBRIDO,
         # N_VIAG, FE_VIA, DIA_SEM, TOT_VIAG,
         # ZONA_O, MUNI_O, CO_O_X, CO_O_Y, ZONA_D, MUNI_D, CO_D_X, CO_D_Y,
         # ZONA_T1, MUNI_T1, CO_T1_X, CO_T1_Y, ZONA_T2, MUNI_T2, CO_T2_X, CO_T2_Y, ZONA_T3, MUNI_T3, CO_T3_X, CO_T3_Y,
         # MOTIVO_O, MOTIVO_D, MOTIVO,
         # SERVIR_O, SERVIR_D,
         # MODO1, MODO2, MODO3, MODO4,
         # H_SAIDA, MIN_SAIDA, ANDA_O, H_CHEG, MIN_CHEG, ANDA_D, DURACAO,
         # MODOPRIN, TIPVG, TP_ESAUTO, VL_EST, PE_BICI, TP_ESTBICI, PROP_BICI, 
         DISTANCIA) %>%
  collect()




# ------------------------------------------------------------------------------
# OD 2023 - Estudantes de ensino médio e escolas públicas
# ------------------------------------------------------------------------------

# Nos dados do INEP, temos 342.464 matrículas em escolas públicas para a faixa 
# etária de 15-17 anos. Os dados da OD (abaixo), se fizermos esses filtros, vão 
# ficar muito abaixo disso. Vamos então olhar pelo prisma de matrículas do ensino
# médio em escolas públicas, independentemente da faixa etária - no INEP, esse 
# número vai ser de 364.441. Neste caso, os filtros da OD chegam em um patamar
# bem mais próximo.

# 245641.1
od_sp %>%
  filter(ESTUDA == 4 & TIPO_ESC == 1 & CD_PRES_HI == 1 & between(IDADE, 15, 17)) %>%
  # filter(between(IDADE, 15, 17)) %>%
  select(ID_PESS, FE_PESS) %>%
  distinct(ID_PESS, .keep_all = TRUE) %>%
  select(FE_PESS) %>%
  sum()

# 235111.1
od_sp %>%
  filter(ESTUDA == 4 & TIPO_ESC == 1 & CD_PRES_HI == 1 & between(IDADE, 15, 17) & MUNIESC == 36) %>%
  # filter(between(IDADE, 15, 17)) %>%
  select(ID_PESS, FE_PESS) %>%
  distinct(ID_PESS, .keep_all = TRUE) %>%
  select(FE_PESS) %>%
  sum()



# # Estudantes de ensino médio, escola pública, de 15-17 anos, regime presencial em 
# # escola dentro do município de São Paulo: 235111.1
# estudantes <- od_sp %>%
#   filter(ESTUDA == 4 & TIPO_ESC == 1 & CD_PRES_HI == 1 & between(IDADE, 15, 17) & MUNIESC == 36) %>%
#   select(ZONA, ID_PESS, IDADE, FE_PESS, CRITERIOBR) %>%
#   mutate(CRITERIOBR = str_c('classe_', CRITERIOBR),
#          FX_ETARIA = case_when(between(IDADE,  0,  3) ~ '00-03',
#                                between(IDADE,  4,  6) ~ '04-06',
#                                between(IDADE,  7, 10) ~ '07-10',
#                                between(IDADE, 11, 14) ~ '11-14',
#                                between(IDADE, 15, 17) ~ '15-17',
#                                between(IDADE, 18, 22) ~ '18-22',
#                                between(IDADE, 23, 29) ~ '23-29',
#                                between(IDADE, 30, 39) ~ '30-39',
#                                between(IDADE, 40, 49) ~ '40-49',
#                                between(IDADE, 50, 59) ~ '50-59',
#                                between(IDADE, 60, 1000) ~ '60+',
#          )) %>%
#   group_by(ZONA, ID_PESS) %>%
#   mutate(MEAN_FE_PES = mean(FE_PESS)) %>%
#   distinct(ZONA, ID_PESS, .keep_all = TRUE) %>%
#   group_by(ZONA, CRITERIOBR, FX_ETARIA) %>%
#   summarise(fe = sum(FE_PESS)) %>%
#   pivot_wider(id_cols = c(ZONA, FX_ETARIA),
#               names_from = CRITERIOBR,
#               values_from = fe) %>%
#   ungroup() %>%
#   # Ordenar colunas
#   select(ZONA, FX_ETARIA, order(colnames(.))) %>%
#   # Substituir NAs por 0
#   mutate_all(., ~replace(., is.na(.), 0)) %>%
#   # Criar coluna de total
#   mutate(total = rowSums(across(starts_with("classe_"))))
# 
# sum(estudantes$total) # 235111.1

# Estudantes do 2° Grau/Ensino Médio de escolas públicas que estudam presencial
# ou híbrido em escolas situadas dentro do município de São Paulo
# ESTUDA = 4 - 2º Grau/Médio
# TIPO_ESC = 1 - Pública
# CD_PRES_HI = 1 - Presencial o tempo todo, 3 - Híbrido
# MUNIESC = 36 (São Paulo)
# GRAU_INS = 3 - Fundamental II Completo/Médio Incompleto
estudantes <- od_sp %>%
  # filter(ESTUDA %in% c(3, 4, 6) & TIPO_ESC == 1 & CD_PRES_HI %in% c(1, 3) & between(IDADE, 15, 17) & MUNIESC == 36) %>%
  filter(ESTUDA %in% c(4) & TIPO_ESC == 1 & CD_PRES_HI %in% c(1, 3) & MUNIESC == 36 & GRAU_INS == 3) %>%
  select(ZONA, ID_PESS, IDADE, FE_PESS, CRITERIOBR) %>%
  mutate(CRITERIOBR = str_c('classe_', CRITERIOBR),
         FX_ETARIA = case_when(between(IDADE,  0,  3) ~ '00-03',
                               between(IDADE,  4,  6) ~ '04-06',
                               between(IDADE,  7, 10) ~ '07-10',
                               between(IDADE, 11, 14) ~ '11-14',
                               between(IDADE, 15, 17) ~ '15-17',
                               between(IDADE, 18, 22) ~ '18-22',
                               between(IDADE, 23, 29) ~ '23-29',
                               between(IDADE, 30, 39) ~ '30-39',
                               between(IDADE, 40, 49) ~ '40-49',
                               between(IDADE, 50, 59) ~ '50-59',
                               between(IDADE, 60, 1000) ~ '60+',
         )) %>%
  group_by(ZONA, ID_PESS) %>%
  mutate(MEAN_FE_PES = mean(FE_PESS)) %>%
  distinct(ZONA, ID_PESS, .keep_all = TRUE) %>%
  group_by(ZONA, CRITERIOBR) %>%
  summarise(fe = sum(FE_PESS)) %>%
  pivot_wider(id_cols = c(ZONA),
              names_from = CRITERIOBR,
              values_from = fe) %>%
  ungroup() %>%
  # Ordenar colunas
  select(ZONA, order(colnames(.))) %>%
  # Substituir NAs por 0
  mutate_all(., ~replace(., is.na(.), 0)) %>%
  # Criar coluna de total
  mutate(total = rowSums(across(starts_with("classe_"))))

head(estudantes)

sum(estudantes$total) # 337974.8


# ------------------------------------------------------------------------------
# Distribuição proporcional integralizada à concentração de domicílios CNEFE
# ------------------------------------------------------------------------------

# Domicílios validados do CNEFE, distribuídos proporcionalmente por Zona OD
cnefe_dom <- sprintf('%s/03_domicilios_validados_cnefe_por_hexagono_e_zona_OD.csv', pasta_dasimetria)
cnefe_dom <- read_delim(cnefe_dom, delim = ';', col_types = 'icid')
head(cnefe_dom)


# Preparar para distribuição proporcional e integralização das variáveis

# Zonas OD de São Paulo
zonas_OD <- seq(1, 343)
# Variáveis a serem calculadas: classes_1 a _6
sel_vars <- c(sprintf('classe_%d', seq(1, 6)))

# Dataframe para guardar resultados
result_df <- data.frame()

# Fazer cálculo proporcional de estudantes por zona OD
# Adaptado do método 'Truncate, replicate, sample' (TRS) de integralização
# http://www.sciencedirect.com/science/article/pii/S0198971513000240
detach("package:tidylog")
for (zona in zonas_OD) {
  # zona <- zonas_OD[104]
  print(zona)
  
  # Dataframes de domicílios e estudantes por zona
  dom_zona <- cnefe_dom %>% filter(zona_OD == zona)
  est_zona <- estudantes %>% filter(ZONA == zona) %>% rename(zona_OD = ZONA)
  
  if (nrow(est_zona) > 0) {
    
    # Dataframe para armazenar resultados
    out_df <- dom_zona
    
    for (var in sel_vars) {
      # var <- sel_vars[2]
      # print(var)
      
      # Fazer cálculo por classe
      est_classe <- est_zona %>% select(zona_OD, !!var)
      
      # Fazer distribuição proporcional de estudantes, conforme distribuição
      # dos domicílios validados do CNEFE
      prop_ests <- 
        dom_zona %>% 
        left_join(est_classe, by = 'zona_OD') %>%
        # Valores proporcionais ficam na columa prop_vals
        mutate(across(.cols = !!var,
                      .fns = ~ .x * perc_zona,
                      .names = 'prop_vals')) %>%
        # Separar integrais de decimais (ex. 2.89 -> 2 e 0.89)
        mutate(int_vals = floor(prop_vals),
               dec_vals = prop_vals - int_vals)
      
      # Valor de incremento é a soma arredondada dos valores decimais
      inc_val <- round(sum(prop_ests$dec_vals))
      # inc_val <- floor(sum(prop_ests$dec_vals))
      
      # Se houver valores a serem incrementados
      if (inc_val > 0) {
        # Garantir reprodutibilidade
        set.seed(1234)
        
        # Sortear linhas (índices) do dataframe a serem atualizadas - a probabilidade
        # vem da coluna das casas decimais
        topup_indices <- sample(seq_along(prop_ests$zona_OD), size = inc_val, prob = prop_ests$dec_vals, replace = FALSE)
        
        # Atualizar coluna de valores inteiros de acordo com o índice sorteado
        prop_ests$int_vals[topup_indices] <- prop_ests$int_vals[topup_indices] + 1
        rm(topup_indices)
      }
      
      # Selecionar e renomear colunas para 
      prop_ests <- 
        prop_ests %>% 
        select(!!var, prop_vals, dec_vals, int_vals) %>% 
        rename_with(~ paste0(var, '_', .), c('prop_vals', 'dec_vals', 'int_vals'))
      
      # Juntar ao dataframe de resultados
      out_df <- cbind(out_df, prop_ests)
      
      rm(est_classe, prop_ests, inc_val)
    }
    result_df <- rbind(result_df, out_df)
    rm(out_df)
  }
  rm(est_zona, dom_zona)
}


# Os números originais batem com os da integralização?
estudantes %>% select(matches('classe_')) %>% sum() # 337974.8

# Integralização usando inc_val <- floor(sum(prop_ests$dec_vals))
result_df %>% select(matches('int_vals')) %>% sum() # 338048 (diferença de 312.4 a menos em teste anterior, sem GRAU_INS == 3)

# Integralização usando inc_val <- round(sum(prop_ests$dec_vals)) -> vai ser a utilizada
result_df %>% select(matches('int_vals')) %>% sum() # 337983 (diferença de 8.2 estudantes a mais)

# Criar coluna de total
result_df <- result_df %>% mutate(estudantes_totais = rowSums(across(matches("int_vals"))))
sum(result_df$estudantes_totais)


out_file <- sprintf('%s/01_divisao_prop_integral_estudantes_por_zona_OD_hexagonos_long.csv', pasta_ttmatrix_24_28)
write_delim(result_df, out_file, delim = ';')


result_df_short <- result_df %>% select(1:4, matches('int_vals'), estudantes_totais)
out_file <- sprintf('%s/01_divisao_prop_integral_estudantes_por_zona_OD_hexagonos_short.csv', pasta_ttmatrix_24_28)
write_delim(result_df_short, out_file, delim = ';')
