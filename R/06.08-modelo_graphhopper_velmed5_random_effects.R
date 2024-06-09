# Modelo com efeitos aleatórios para trecho e viagem, com categorias reordenadas
# para obter coeficientes finais que sejam somente somatórias do intercepto -
# os valores vão ser usados no custom model do GraphHopper


# carregar bibliotecas
source('fun/setup.R')
# library('leaps')
# library('broom')
# 
# # Para gvlma() // Testar as premissas de um modelo linear
# library('gvlma')
# # Para ols_test_breusch_pagan() // Testar heterocedasticidade
# library('olsrr')
# # Para lm_robust() // Calcular erros robustos, modo alternativo ao coeftest
# library('estimatr')
# # Para coeftest() e bptest() // Calcular erros robustos
# library('lmtest')
# library('sandwich')

# https://bookdown.org/steve_midway/DAR/random-effects.html ***
# https://rcompanion.org/handbook/G_03.html
# https://stackoverflow.com/questions/40711912/how-to-add-a-random-effect-to-a-linear-mixed-effects-model
library('lmerTest')
# library('lme4')
library('MuMIn') # require(remotes); install_version("MuMIn", version = "1.46.0", repos = "http://cran.us.r-project.org") - https://stackoverflow.com/questions/17082341/installing-older-version-of-r-package
# library('versions')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
# dir.create(pasta_base_modelo, recursive = TRUE, showWarnings = FALSE)


# Abrir arquivos processados
# base_modelo <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
base_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')

# head(base_modelo)

# Quantos NAs temos na base e onde?
# colSums(is.na(base_modelo))


# ------------------------------------------------------------------------------
# Seleção das variáveis e transformação em factor/dummies
# ------------------------------------------------------------------------------

# base_modelo <- base_modelo %>% mutate(dist_total_km = dist_total/1000)

# # Transformar colunas de interesse em factors
# base_modelo <- 
#   base_modelo %>%
#   mutate(
#     inicio_fim = factor(inicio_fim, levels = c('meio', 'início', 'final')),
#     cat_dist_total = factor(cat_dist_total, levels = c('media', 'acima_media', 'longa', 'muito_longa', 'curta')),
#     cat_dist_trecho = factor(cat_dist_trecho, levels = c('medio', 'acima_media', 'grande', 'muito_grande', 'curto')),
#     cat_grad   = factor(cat_grad,   levels = c('plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver', 'subi_lev', 'subi_med', 'subi_for', 'subi_ver')),
#     # cat_grad_prev   = factor(cat_grad_prev,   levels = c('plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver', 'subi_lev', 'subi_med', 'subi_for', 'subi_ver'))
#     class_via    = factor(class_via,   levels = c('local', 'coletora', 'arterial', 'vtr', 'ciclo_expressa', 'ped_serv')),
#     # class_viaosm = factor(class_via,   levels = c('local', 'coletora', 'arterial', 'vtr', 'ciclo_expressa', 'ped_serv')),
#     infra_ciclo  = factor(infra_ciclo, levels = c('sem_infra_ciclo', 'ciclofaixa', 'ciclovia')),
#     semaforos    = factor(semaforos, levels = c('sem_semaforos', 'inicio', 'fim', 'inicio_fim')),
#     via_restr    = factor(via_restr,   levels = c('via_comum', 'via_restrita')),
#     osm_oneway   = factor(osm_oneway, levels = c('yes', 'no')),
#     contramao    = factor(contramao, levels = c('não', 'sim')),
#     dia_util     = factor(dia_util,    levels = c('util', 'desc')),
#     cat_fx_hora  = factor(cat_fx_hora, levels = c('vale', 'pico_manha', 'pico_tarde', 'madrugada')),
#     cat_vg_loop  = factor(vg_loop, levels = c('não', 'sim')),
#     cat_vg_exper = factor(vg_exper, levels = c('não', 'sim')),
#     cat_vg_conm  = factor(vg_contramao, levels = c('não', 'sim')),
#     cat_vg_parq  = factor(vg_parques, levels = c('não', 'sim')),
#     cat_curv     = factor(cat_curv, levels = c('00 a 25 graus', '25 a 45 graus', '45 a 90 graus', 'acima de 90 graus')),
#   )

# Transformar colunas de interesse em factors - categorização na ordem, para graphhopper
base_modelo <- 
  base_modelo %>%
  mutate(
    inicio_fim = factor(inicio_fim, levels = c('meio', 'início', 'final')),
    cat_grad   = factor(cat_grad,   levels = c('subi_ver', 'subi_for', 'subi_med',  'subi_lev', 'plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver')),
    class_via    = factor(class_via,   levels = c('ped_serv', 'local', 'coletora', 'arterial', 'ciclo_expressa', 'vtr')),
    infra_ciclo  = factor(infra_ciclo, levels = c('sem_infra_ciclo', 'ciclovia', 'ciclofaixa')),
    semaforos    = factor(semaforos, levels = c('fim', 'inicio_fim', 'inicio', 'sem_semaforos')),
    via_restr    = factor(via_restr,   levels = c('via_comum', 'via_restrita')),
    osm_oneway   = factor(osm_oneway, levels = c('no', 'yes')),
    contramao    = factor(contramao, levels = c('não', 'sim')),
    cat_dist_total  = factor(cat_dist_total, levels = c('media', 'acima_media', 'longa', 'muito_longa', 'curta')),
    cat_dist_trecho = factor(cat_dist_trecho, levels = c('medio', 'acima_media', 'grande', 'muito_grande', 'curto')),
    dia_util     = factor(dia_util,    levels = c('util', 'desc')),
    cat_curv     = factor(cat_curv, levels = c('00 a 25 graus', '25 a 45 graus', '45 a 90 graus', 'acima de 90 graus')),
    cat_fx_hora  = factor(cat_fx_hora, levels = c('pico_manha', 'vale', 'pico_tarde', 'madrugada')),
    cat_vg_loop  = factor(vg_loop, levels = c('não', 'sim')),
    cat_vg_exper = factor(vg_exper, levels = c('não', 'sim')),
    cat_vg_conm  = factor(vg_contramao, levels = c('não', 'sim')),
    cat_vg_parq  = factor(vg_parques, levels = c('não', 'sim')),
    
  )

# Quantos NAs temos na base e onde?
colSums(is.na(base_modelo))


# ------------------------------------------------------------------------------
# Segundo modelo: OLS com regressão simples e random effects
# ------------------------------------------------------------------------------

base_modelo <- base_modelo %>% select(vel_med_m5,
                                      # log_speed ~
                                      # sqrt_speed ~
                                      inicio_fim, 
                                      # elev_grad_rev,
                                      # I(elev_grad_rev^2),
                                      # I(elev_grad_rev^3),
                                      cat_grad, 
                                      class_via,
                                      infra_ciclo,
                                      semaforos,
                                      via_restr,
                                      osm_oneway,
                                      contramao,
                                      # dist_total_km,
                                      cat_dist_total,
                                      # dist_trecho_quadra,
                                      cat_dist_trecho,
                                      # curv_h,
                                      cat_curv,
                                      dens_lotes_100m,
                                      # dens_lotes_100m_15m,
                                      # dens_lotes_100m_30m,
                                      dia_util,
                                      cat_fx_hora,
                                      cat_vg_loop,
                                      cat_vg_exper,
                                      cat_vg_conm,
                                      cat_vg_parq,
                                      qgis_id,
                                      trip_id)


head(base_modelo)

# Checar ordem dos resultados, se serão só somatórias
ols_resultados <- lm(vel_med_m5 ~
                       # log_speed ~
                       # sqrt_speed ~
                       inicio_fim + 
                       # elev_grad_rev +
                       # I(elev_grad_rev^2) +
                       # I(elev_grad_rev^3) +
                       cat_grad + 
                       class_via +
                       infra_ciclo +
                       semaforos +
                       via_restr +
                       osm_oneway +
                       contramao +
                       # dist_total_km +
                       cat_dist_total +
                       # dist_trecho_quadra +
                       cat_dist_trecho +
                       # curv_h +
                       cat_curv +
                       dens_lotes_100m +
                       # dens_lotes_100m_15m +
                       # dens_lotes_100m_30m +
                       dia_util +
                       cat_fx_hora +
                       cat_vg_loop +
                       cat_vg_exper +
                       cat_vg_conm +
                       cat_vg_parq
                     ,
                     data = base_modelo)

summary(ols_resultados)

gc(T)

ols_resultados <- lmer(vel_med_m5 ~
                         # log_speed ~
                         # sqrt_speed ~
                         inicio_fim + 
                         # elev_grad_rev +
                         # I(elev_grad_rev^2) +
                         # I(elev_grad_rev^3) +
                         cat_grad + 
                         class_via +
                         infra_ciclo +
                         semaforos +
                         via_restr +
                         osm_oneway +
                         contramao +
                         # dist_total_km +
                         cat_dist_total +
                         # dist_trecho_quadra +
                         cat_dist_trecho +
                         # curv_h +
                         cat_curv +
                         dens_lotes_100m +
                         # dens_lotes_100m_15m +
                         # dens_lotes_100m_30m +
                         dia_util +
                         cat_fx_hora +
                         cat_vg_loop +
                         cat_vg_exper +
                         cat_vg_conm +
                         cat_vg_parq +
                         (1 | qgis_id) +
                         (1 | trip_id)
                       ,
                       data = base_modelo)

summary(ols_resultados)
MuMIn::r.squaredGLMM(ols_resultados)
# # confint(ols_resultados)
# vcov(ols_resultados)
# fixef(ols_resultados)
# VarCorr(ols_resultados)
# # ols_eigen_cindex(ols_resultados)

# https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic
nobs(ols_resultados)
BIC(ols_resultados)
logLik(ols_resultados)
AIC(ols_resultados)


