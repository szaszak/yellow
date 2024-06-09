# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
# dir.create(pasta_base_modelo, recursive = TRUE, showWarnings = FALSE)


# Abrir arquivos processados
# base_modelo <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
base_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')

head(base_modelo)


# Quantos NAs temos na base e onde?
colSums(is.na(base_modelo))


# ------------------------------------------------------------------------------
# Variáveis contínuas ou discretas
# ------------------------------------------------------------------------------

# Visão geral das variáveis contínuas ou discretas
cont_vars <- c('dens_lotes_100m', 'dist_total', 'dist_trecho_quadra', 'vel_med_m5',
               'elev_grad_rev', 'curv_h')

for (var in cont_vars) {
  this <- base_modelo %>% select(all_of(var))
  print(var)
  print(this %>% quantile(probs = seq(0, 1, 0.25), na.rm = TRUE) %>% round(2))
  print(round(mean(this[[var]]), 2))
  print(this %>% quantile(probs = c(0.85, 0.95, 0.99), na.rm = TRUE) %>% round(2))
  
}


# ------------------------------------------------------------------------------
# Variáveis categóricas (tabelas de frequência)
# ------------------------------------------------------------------------------

base_modelo <- 
  base_modelo %>%
  mutate(
    inicio_fim = factor(inicio_fim, levels = c('meio', 'início', 'final')),
    cat_dist_total = factor(cat_dist_total, levels = c('media', 'acima_media', 'longa', 'muito_longa', 'curta')),
    cat_dist_trecho = factor(cat_dist_trecho, levels = c('medio', 'acima_media', 'grande', 'muito_grande', 'curto')),
    cat_grad   = factor(cat_grad,   levels = c('plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver', 'subi_lev', 'subi_med', 'subi_for', 'subi_ver')),
    # cat_grad_prev   = factor(cat_grad_prev,   levels = c('plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver', 'subi_lev', 'subi_med', 'subi_for', 'subi_ver'))
    class_via    = factor(class_via,   levels = c('local', 'coletora', 'arterial', 'vtr', 'ciclo_expressa', 'ped_serv')),
    # class_viaosm = factor(class_via,   levels = c('local', 'coletora', 'arterial', 'vtr', 'ciclo_expressa', 'ped_serv')),
    infra_ciclo  = factor(infra_ciclo, levels = c('sem_infra_ciclo', 'ciclofaixa', 'ciclovia')),
    semaforos    = factor(semaforos, levels = c('sem_semaforos', 'inicio', 'fim', 'inicio_fim')),
    via_restr    = factor(via_restr,   levels = c('via_comum', 'via_restrita')),
    osm_oneway   = factor(osm_oneway, levels = c('yes', 'no')),
    contramao    = factor(contramao, levels = c('não', 'sim')),
    dia_util     = factor(dia_util,    levels = c('util', 'desc')),
    cat_fx_hora  = factor(cat_fx_hora, levels = c('vale', 'pico_manha', 'pico_tarde', 'madrugada')),
    cat_vg_loop  = factor(vg_loop, levels = c('não', 'sim')),
    cat_vg_exper = factor(vg_exper, levels = c('não', 'sim')),
    cat_vg_conm  = factor(vg_contramao, levels = c('não', 'sim')),
    cat_vg_parq  = factor(vg_parques, levels = c('não', 'sim')),
    cat_curv     = factor(cat_curv, levels = c('00 a 25 graus', '25 a 45 graus', '45 a 90 graus', 'acima de 90 graus')),
  )

estatisticas_descritivas <- function(var) {
  # Descrição das variáveis categóricas - uma por vez
  # https://uc-r.github.io/descriptives_categorical
  # Tabela de frequências
  table <- table(base_modelo %>% select({{var}}))
  # Tabela com proporcional do todo
  table2 <- prop.table(table)
  # Junção das tabelas
  out <- rbind(table, table2) %>% t() %>% as.data.frame() %>% mutate(table2 = round(table2 * 100, 2))

  print(var)
  print(out)
}

desc_var <- c('inicio_fim', 'cat_grad', 'class_via', 'infra_ciclo', 'semaforos',
              'via_restr', 'osm_oneway', 'contramao', 'cat_dist_total', 
              'cat_dist_trecho','cat_curv', 'dia_util',
              'cat_fx_hora', 'cat_vg_loop', 'cat_vg_exper', 'cat_vg_conm',
              'cat_vg_parq')
for (var in desc_var) { estatisticas_descritivas(var) }




# Por frequência de variável categórica
base_modelo %>%
  group_by(infra_ciclo, class_via) %>%
  tally() %>%
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(n / sum(n) * 100, 2),
         prop_total = round(n / sum(.$n) * 100, 2)) %>%
  as.data.frame()

# Por soma das distâncias
base_modelo %>% filter(infra_ciclo == 'ciclovia') %>% select(dist_trecho_quadra) %>% summary()



# Por soma das distâncias
base_modelo %>%
  group_by(dia_util, contramao) %>%
  summarise(dist = sum(dist_trecho_quadra)) %>%
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(dist / sum(dist) * 100, 2),
         prop_total = round(dist / sum(.$dist) * 100, 2))


























# Método Favero e Belfiore para remoção de outliers extremos
base_modelo %>% select(dist_total) %>% quantile(probs = seq(0.9, 1, 0.01), na.rm = TRUE)
# Retirar outliers extremos vel_med_m5
# Isolar coluna de interesse - velocidade
x <- base_modelo$dist_total
# Pegar o primeiro e quarto quantis
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x)
# Retirar outliers do dataframe
base_modelo <- base_modelo %>% filter(!(dist_total < (qnt[1] - H) | dist_total > (qnt[2] + H)))


