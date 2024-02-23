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
pasta_graficos     <- sprintf('%s/graficos', pasta_base_modelo)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)


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

# # Histogram
# hist(base_modelo$vel_med_m5, pch = 16, cex = 1, breaks = 128)
# # Boxplot
# boxplot(base_modelo$vel_med_m5)
# 
# # Método Favero e Belfiore para remoção de outliers extremos
# base_modelo %>% select(dist_total) %>% quantile(probs = seq(0.9, 1, 0.01), na.rm = TRUE)
# # Retirar outliers extremos vel_med_m5
# # Isolar coluna de interesse - velocidade
# x <- base_modelo$vel_med_m5
# # Pegar o primeiro e quarto quantis
# qnt <- quantile(x, probs = c(0.25, 0.75))
# # Outliers extremos estão a 3 * IQR
# H <- 3 * IQR(x)
# # Retirar outliers do dataframe
# tmp <- base_modelo %>% filter(!(vel_med_m5 < (qnt[1] - H) | vel_med_m5 > (qnt[2] + H)))
# rm(x, qnt, H)


# ------------------------------------------------------------------------------
# Primeiro modelo: OLS com regressão simples
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
                                      cat_vg_parq)

base_modelo <- base_modelo %>% mutate(log_speed = log(vel_med_m5))

ols_resultados <- lm(#vel_med_m5 ~
                       log_speed ~
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
# confint(ols_resultados)

# https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic
nobs(ols_resultados)
BIC(ols_resultados)
logLik(ols_resultados)
AIC(ols_resultados)


# ------------------------------------------------------------------------------
# Gráfico - Predicted versus Actual values
# ------------------------------------------------------------------------------

# Predicted versus actual values - GERAR GRÁFICO E EXPORTAR, dev.off não está salvando
# out_png <- sprintf('%s/modelo_velmed5_predicted_vs_observed.png', pasta_graficos)
plot(predict(ols_resultados), base_modelo$log_speed, 
     xlab = "predicted", 
     ylab = "observed",
     main = 'Modelo 2 - Valores Previstos vs. Observados',
     cex = 0.2)
abline(a = 0, b = 1, col = "red", lwd = 2)
# dev.off()



# # ------------------------------------------------------------------------------
# # Segundo modelo: OLS com regressão simples
# # ------------------------------------------------------------------------------
# 
# ols_resultados <- lm(vel_med_m5 ~
#                        # log_speed ~
#                        # sqrt_speed ~
#                        inicio_fim + 
#                        # elev_grad_rev +
#                        # I(elev_grad_rev^2) +
#                        # I(elev_grad_rev^3) +
#                        cat_grad + 
#                        class_via +
#                        infra_ciclo +
#                        semaforos +
#                        via_restr +
#                        osm_oneway +
#                        contramao +
#                        # dist_total_km +
#                        # cat_dist_total +
#                        # dist_trecho_quadra +
#                        cat_dist_trecho +
#                        # curv_h +
#                        cat_curv +
#                        dens_lotes_100m 
#                        # dens_lotes_100m_15m +
#                        # dens_lotes_100m_30m +
#                        # dia_util +
#                        # cat_fx_hora +
#                        # cat_vg_loop +
#                        # cat_vg_exper +
#                        # cat_vg_conm +
#                        # cat_vg_parq
#                      ,
#                      data = base_modelo)
# 
# summary(ols_resultados)
# # confint(ols_resultados)
# 
# # https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic
# nobs(ols_resultados)
# BIC(ols_resultados)
# logLik(ols_resultados)
# AIC(ols_resultados)


# # ------------------------------------------------------------------------------
# # Terceiro modelo: OLS com regressão simples e variáveis contínuas
# # ------------------------------------------------------------------------------
# 
# base_modelo <- base_modelo %>% mutate(dist_total_km = dist_total / 1000)
# 
# ols_resultados <- lm(vel_med_m5 ~
#                        # log_speed ~
#                        # sqrt_speed ~
#                        inicio_fim + 
#                        # elev_grad_rev +
#                        # I(elev_grad_rev^2) +
#                        # I(elev_grad_rev^3) +
#                        poly(elev_grad_rev, 3, raw=TRUE) +
#                        # cat_grad + 
#                        class_via +
#                        infra_ciclo +
#                        semaforos +
#                        via_restr +
#                        osm_oneway +
#                        contramao +
#                        dist_total_km +
#                        # cat_dist_total +
#                        # dist_trecho_quadra +
#                        cat_dist_trecho +
#                        curv_h +
#                        # cat_curv +
#                        dens_lotes_100m +
#                        # dens_lotes_100m_15m +
#                        # dens_lotes_100m_30m +
#                        dia_util +
#                        cat_fx_hora +
#                        cat_vg_loop +
#                        cat_vg_exper +
#                        cat_vg_conm +
#                        cat_vg_parq
#                      ,
#                      data = base_modelo)
# 
# summary(ols_resultados)


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

# > vif(ols_resultados)
# GVIF Df GVIF^(1/(2*Df))
# inicio_fim      1.107442  2        1.025841
# cat_grad        1.060504  8        1.003678
# class_via       7.610947  4        1.288783
# infra_ciclo     2.417145  2        1.246883
# semaforos       1.175138  3        1.027263
# via_restr       3.632802  1        1.905991
# osm_oneway      2.157329  1        1.468785
# contramao       1.416649  1        1.190231
# cat_dist_total  1.358069  4        1.038999
# cat_dist_trecho 1.804322  4        1.076563
# cat_curv        1.458128  3        1.064877
# dens_lotes_100m 1.709951  1        1.307651
# dia_util        1.114413  1        1.055658
# cat_fx_hora     1.060790  3        1.009884
# cat_vg_loop     1.149473  1        1.072135
# cat_vg_exper    1.065746  1        1.032350
# cat_vg_conm     1.319598  1        1.148738
# cat_vg_parq     2.189877  1        1.479823


# car::vif não deve funcionar para modelos com fixed effects, usar vif.mer
# https://stats.stackexchange.com/questions/201205/variance-inflation-factors-vif-mer-verus-group-factor-level-vif
# https://github.com/aufrank/R-hacks/blob/master/mer-utils.R
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
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif.mer(ols_resultados)



# ------------------------------------------------------------------------------
# Heterocedasticidade
# ------------------------------------------------------------------------------

# How to Fix Heteroscedasticity
# https://statisticsbyjim.com/regression/heteroscedasticity-regression/
# https://www.statology.org/breusch-pagan-test-r/
# https://stats.stackexchange.com/questions/193061/what-is-the-difference-between-these-two-breusch-pagan-tests
# If the p-value is less than 0.05, we reject the null hypothesis - this means 
# heteroscedasticity is present in the data
bptest(ols_resultados)
# bptest(ols_resultados, studentize = FALSE)

# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
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
coeftest(ols_resultados, vcov = vcovHC(ols_resultados, type = 'HC0'))
# Confidence intervals
coefci(ols_resultados, vcov. = vcovHC(ols_resultados, type = 'HC0'))
# vcov(ols_resultados)


# Ordinary Least Squares with Robust Standard Errors
# https://declaredesign.org/r/estimatr/reference/lm_robust.html
ols_resultados <- lm_robust(#vel_med_m5 ~
                              log_speed ~
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
                            data = base_modelo,
                            se_type = "HC0")

summary(ols_resultados)

# ------------------------------------------------------------------------------
# Gráficos
# ------------------------------------------------------------------------------

# Histogram - dar nome, definir tamanho, plotar e salvar resultado como PNG
out_png <- sprintf('%s/modelo_logspeed_histogram.png', pasta_graficos)
png(out_png, width = 600, height = 350)
hist(base_modelo$log_speed, pch = 16, cex = 1, breaks = 128)
dev.off()

# Boxplot - dar nome, definir tamanho, plotar e salvar resultado como PNG
out_png <- sprintf('%s/modelo_logspeed_boxplot.png', pasta_graficos)
png(out_png, width = 600, height = 350)
boxplot(base_modelo$log_speed)
dev.off()

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

# # Change the panel layout to 2 x 2 (to look at all 4 plots at once)
# par(mfrow = c(2, 2))
# # Use plot() function to create diagnostic plots
# plot(ols_resultados)

# Gerar gráficos
for (i in seq(1, 5)) {
  
  out_png <- sprintf('%s/modelo_logspeed_plot%i.png', pasta_graficos, i)
  
  png(out_png, width = 600, height = 350)
  plot(ols_resultados, which = i, cex = 1)
  
  dev.off()
}


# https://stackoverflow.com/questions/39562631/obtain-standardised-residuals-and-residual-v-s-fitted-plot-for-mlm-object-f
f <- fitted(ols_resultados)
r <- rstandard(ols_resultados)
plot(f, r, pch = 16, cex = 1)

methods(rstandard)






# STAT 462 - Applied Regression Analysis - 4.8 - Further Residual Plot Examples
# https://online.stat.psu.edu/stat462/node/124/

# https://stackoverflow.com/questions/69331895/plotting-standardized-residual-vs-fitted-values-from-a-for-loop
# Fitted values for the model
FV <- fitted(ols_resultados)
# extracting standardized residuals and fitted values
SS <- rstandard(ols_resultados)
scatter.smooth(
  SS ~ FV, col = "grey",
  las = 1, ylab = "Standardized residuals", xlab = "Fitted values",
  main = bquote("Plot of residuals versus fitted"))


