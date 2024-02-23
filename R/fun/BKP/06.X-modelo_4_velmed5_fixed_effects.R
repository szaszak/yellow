# carregar bibliotecas
source('fun/setup.R')
library('leaps')
library('broom')

# Para gvlma() // Testar as premissas de um modelo linear
library('gvlma')
# Para ols_test_breusch_pagan() // Testar heterocedasticidade
library('olsrr')
# Para lm_robust() // Calcular erros robustos, modo alternativo ao coeftest
library('estimatr')
# Para coeftest() e bptest() // Calcular erros robustos
library('lmtest')
library('sandwich')


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

# Transformar colunas de interesse em factors
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


head(base_modelo)

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


library('fixest')
ols_resultados <- feols(vel_med_m5 ~
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
                          cat_vg_parq |
                          trip_id #+ qgis_id
                        ,
                        data = base_modelo,
                        # fixef = c('trip_id', 'qgis_id'),
                        nthreads = 0)

summary(ols_resultados)
ols_resultados$collin.var


# Removendo as variáveis que são derrubadas por colinearidade pelo feols()
ols_resultados <- feols(vel_med_m5 ~
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
                          # cat_dist_total +
                          # dist_trecho_quadra +
                          cat_dist_trecho +
                          # curv_h +
                          cat_curv +
                          dens_lotes_100m |
                          # dens_lotes_100m_15m +
                          # dens_lotes_100m_30m +
                          # dia_util +
                          # cat_fx_hora +
                          # cat_vg_loop +
                          # cat_vg_exper +
                          # cat_vg_conm +
                          # cat_vg_parq
                          trip_id
                        ,
                        data = base_modelo,
                        # fixef = c('trip_id', 'qgis_id'),
                        nthreads = 0)

summary(ols_resultados, vcov = ~trip_id)
etable(ols_resultados, vcov = ~trip_id, headers = c("OLS"))

# https://bookdown.org/steve_midway/DAR/random-effects.html ***
# https://rcompanion.org/handbook/G_03.html
# https://stackoverflow.com/questions/40711912/how-to-add-a-random-effect-to-a-linear-mixed-effects-model
library('lmerTest')
# library('lme4')
library('MuMIn') # require(remotes); install_version("MuMIn", version = "1.46.0", repos = "http://cran.us.r-project.org") - https://stackoverflow.com/questions/17082341/installing-older-version-of-r-package
# library('versions')

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
                         # (1 | qgis_id) +
                         (1 | trip_id)
                       ,
                       data = base_modelo)


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
                         # cat_dist_total +
                         # dist_trecho_quadra +
                         cat_dist_trecho +
                         # curv_h +
                         cat_curv +
                         dens_lotes_100m +
                         # dens_lotes_100m_15m +
                         # dens_lotes_100m_30m +
                         # dia_util +
                         # cat_fx_hora +
                         # cat_vg_loop +
                         # cat_vg_exper +
                         # cat_vg_conm +
                         # cat_vg_parq
                         # (1 | qgis_id) +
                         (1 | trip_id)
                       ,
                       data = base_modelo)

summary(ols_resultados)
MuMIn::r.squaredGLMM(ols_resultados)
# confint(ols_resultados)
vcov(ols_resultados)
fixef(ols_resultados)
VarCorr(ols_resultados)
# ols_eigen_cindex(ols_resultados)

# https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic
nobs(ols_resultados)
BIC(ols_resultados)
logLik(ols_resultados)
AIC(ols_resultados)


# ------------------------------------------------------------------------------
# Testar as premissas do modelo linear
# ------------------------------------------------------------------------------

# Chapter 11 Testing regression assumptions
# https://bookdown.org/jimr1603/Intermediate_R_-_R_for_Survey_Analysis/testing-regression-assumptions.html


# Testar as premissas de um modelo linear (Skewness, Kurtosis, Link Function, Heteroscedasticity)
# https://rforpoliticalscience.com/2020/10/29/check-linear-regression-assumptions-with-gvlma-package-in-r/
# Global Stat checks whether the relationship between the dependent and independent 
# relationship roughly linear. We can see that the assumption is met.
# Skewness and kurtosis assumptions show that the distribution of the residuals are normal.
# Link function checks to see if the dependent variable is continuous or categorical. Our variable is continuous. 
# Heteroskedasticity assumption means the error variance is equally random and we have homoskedasticity!
library(gvlma)
gvlma(ols_resultados)

# Calculate the standardized residuals
# https://www.statology.org/standardized-residuals-in-r/
standard_res <- rstandard(ols_resultados)
hist(standard_res)
# Identificar quais são as linhas com maior outliers (estão vindo de vel_med_m5 muito altas)
standard_res <- standard_res %>% as.data.frame() %>% setNames('x') %>% mutate(index = 1:nrow(.))
indexes <- standard_res %>% arrange(-x) %>% head(20)
base_modelo %>% mutate(index = 1:nrow(.), .before = 'vel_med_m5') %>% filter(index %in% indexes$index)



# ------------------------------------------------------------------------------
# Multicolinearidade
# ------------------------------------------------------------------------------

# Collinearity Diagnostics, Model Fit & Variable Contribution
# https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
# Collinearity Diagnostics - VIF
#  A VIF of 1 means that there is no correlation among the kth predictor and the 
#  remaining predictor variables, and hence the variance of βk is not inflated 
#  at all. The general rule of thumb is that VIFs exceeding 4 warrant further 
#  investigation, while VIFs exceeding 10 are signs of serious multicollinearity 
#  requiring correction.
# ols_vif_tol(ols_resultados)

# https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
# The rule of GVIF(1/(2×Df))<2 is applied in some publications, which would 
# equal to an ordinary VIF of 4 for one-coefficient variables.
library(car)
car::vif(ols_resultados)

# > car::vif(ols_resultados)
# GVIF Df GVIF^(1/(2*Df))
# inicio_fim      1.379579  2        1.083769
# cat_grad        1.035324  8        1.002172
# class_via       3.072292  4        1.150622
# infra_ciclo     1.359811  2        1.079865
# semaforos       1.116051  3        1.018468
# via_restr       1.602767  1        1.266004
# osm_oneway      1.317623  1        1.147877
# contramao       1.040101  1        1.019854
# cat_dist_total  1.146849  4        1.017275
# cat_dist_trecho 1.515166  4        1.053313
# cat_curv        1.141322  3        1.022276
# dens_lotes_100m 1.428990  1        1.195404
# dia_util        1.049236  1        1.024322
# cat_fx_hora     1.031449  3        1.005174
# cat_vg_loop     1.055212  1        1.027235
# cat_vg_exper    1.018233  1        1.009075
# cat_vg_conm     1.084340  1        1.041317
# cat_vg_parq     1.101853  1        1.049692


# car::vif não deve funcionar em modelos com fixed effects:
# https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/
# Usar vif.mer, de https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
vif.mer <- function(fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v) ^ 0.5
  v <- diag(solve(v / (d %o% d)))
  names(v) <- nam
  v
}
vif.mer(ols_resultados) %>% as.data.frame()

# inicio_fiminício            1.244602
# inicio_fimfinal             1.225797
# cat_graddesc_lev            6.168849
# cat_graddesc_med            4.467721
# cat_graddesc_for            4.464601
# cat_graddesc_ver            3.113914
# cat_gradsubi_lev            6.171116
# cat_gradsubi_med            4.469550
# cat_gradsubi_for            4.465761
# cat_gradsubi_ver            3.112556
# class_viacoletora           2.136074
# class_viaarterial           2.791398
# class_viaciclo_expressa     1.159550
# class_viaped_serv           1.512817
# infra_ciclociclofaixa       1.032593
# infra_ciclociclovia         1.328825
# semaforosinicio             1.082550
# semaforosfim                1.089611
# semaforosinicio_fim         1.063283
# via_restrvia_restrita       1.602767
# osm_onewayno                1.317623
# contramaosim                1.040101
# cat_dist_totalacima_media   1.126186
# cat_dist_totallonga         1.105893
# cat_dist_totalmuito_longa   1.065027
# cat_dist_totalcurta         1.204555
# cat_dist_trechoacima_media  1.229748
# cat_dist_trechogrande       1.174208
# cat_dist_trechomuito_grande 1.154352
# cat_dist_trechocurto        1.363204
# cat_curv25 a 45 graus       1.028178
# cat_curv45 a 90 graus       1.065647
# cat_curvacima de 90 graus   1.050175
# dens_lotes_100m             1.428990
# dia_utildesc                1.049236
# cat_fx_horapico_manha       1.292276
# cat_fx_horapico_tarde       1.340395
# cat_fx_horamadrugada        1.106117
# cat_vg_loopsim              1.055212
# cat_vg_expersim             1.018233
# cat_vg_conmsim              1.084340
# cat_vg_parqsim              1.101853


# Robust standard errors for mixed-effects models in lme4 package of R
# https://stackoverflow.com/questions/26412581/robust-standard-errors-for-mixed-effects-models-in-lme4-package-of-r
# merDeriv's results match type="CR0" (merDeriv provides robust Wald estimates 
# for all components, including the random effect parameters; it's up to you to 
# decide if Wald estimates for RE parameters are reliable enough)
library('clubSandwich')
sqrt(diag(vcovCR(ols_resultados, type = 'CR0')))

# ------------------------------------------------------------------------------
# Heterocedasticidade
# ------------------------------------------------------------------------------

# How to Fix Heteroscedasticity
# https://statisticsbyjim.com/regression/heteroscedasticity-regression/
# https://www.statology.org/breusch-pagan-test-r/
# https://stats.stackexchange.com/questions/193061/what-is-the-difference-between-these-two-breusch-pagan-tests
# If the p-value is less than 0.05, we reject the null hypothesis - this means 
# heteroscedasticity is present in the data
library(lmtest)
bptest(ols_resultados)
# bptest(ols_resultados, studentize = FALSE)

# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
library(olsrr)
ols_test_breusch_pagan(ols_resultados)
# ols_test_breusch_pagan(ols_resultados, rhs = TRUE, multiple = TRUE)



# Does heteroskedasticity matter if you have a large enough sample?
# https://stats.stackexchange.com/questions/92595/does-heteroskedasticity-matter-if-you-have-a-large-enough-sample
# https://data.library.virginia.edu/understanding-robust-standard-errors/
# https://www.statology.org/robust-standard-errors-in-r/
# https://stackoverflow.com/questions/60558143/how-to-interpret-linear-regression-assumptions-of-the-data
# Does heteroskedasticity matter if you have a large enough sample? Yes, it does. 
# Does a large enough sample mitigates the heteroskedasticity issue? No, it does not. 
# And, calculating robust-Standar Errors and other methods like WLS, etc... works 
# well to correct the heteroskedasticity. Yes.
# library(lmtest)
library(sandwich)
gc(T)
coeftest(ols_resultados, vcov = vcovHC(ols_resultados, type = 'HC0')) # Não funciona!!! Roda por 8h e dá erro
# Confidence intervals
coefci(ols_resultados, vcov. = vcovHC(ols_resultados, type = 'HC0'))
# vcov(ols_resultados)


# Alternativas ao HC0
# https://stackoverflow.com/questions/70485809/why-does-summary-show-different-standard-errors-than-coeftest
coeftest(ols_resultados, vcov. = vcov(ols_resultados))
# texreg::screenreg

# ------------------------------------------------------------------------------
# Gráficos
# ------------------------------------------------------------------------------

# lm.plot
# https://stackoverflow.com/questions/29044055/plot-which-parameter-where-in-r
# "which" selects which plot to be displayed:
# 1 - A plot of residuals against fitted values
# 2 - A normal Q-Q plot
# 3 - A Scale-Location plot of sqrt(| residuals |) against fitted values
# 4 - A plot of Cook's distances versus row labels
# 5 - A plot of residuals against leverages
# 6 - A plot of Cook's distances against leverage/(1-leverage)
# By default, the first three and 5 are provided.

# Change the panel layout to 2 x 2 (to look at all 4 plots at once)
par(mfrow = c(2, 2))
# Use plot() function to create diagnostic plots
plot(ols_resultados)

# STAT 462 - Applied Regression Analysis - 4.8 - Further Residual Plot Examples
# https://online.stat.psu.edu/stat462/node/124/

# For weighted regression, it is important to assess the standardized residuals 
# because only that type of residual will show us that weighted regression fixed 
# the heteroscedasticity.
# https://statisticsbyjim.com/regression/heteroscedasticity-regression/
# https://stackoverflow.com/questions/69331895/plotting-standardized-residual-vs-fitted-values-from-a-for-loop
# Fitted values for the model
SS <- rstandard(ols_resultados)
# extracting standardized residuals and fitted values
FV <- fitted(ols_resultados)
scatter.smooth(
  SS ~ FV, col = "grey",
  las = 1, ylab = "Standardized residuals", xlab = "Fitted values",
  main = bquote("Plot of residuals versus fitted"))


