# carregar bibliotecas
source('fun/setup.R')
library('leaps')
library('broom')

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
    cat_dist_trecho = factor(cat_dist_trecho, levels = c('medio', 'acima_media', 'grande', 'muito_grande', 'enorme', 'curto')),
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
# Summary das variáveis
# ------------------------------------------------------------------------------

base_modelo %>% 
  select(vel_med_m5, 
         inicio_fim, 
         elev_grad_rev,
         cat_grad, 
         class_via,
         infra_ciclo,
         semaforos,
         via_restr,
         osm_oneway,
         contramao,
         dist_total,
         dist_trecho_quadra,
         curv_h,
         cat_curv,
         dens_lotes_100m,
         dens_lotes_100m_15m,
         dens_lotes_100m_30m,
         dia_util,
         cat_fx_hora,
         cat_vg_loop,
         cat_vg_exper,
         cat_vg_conm,
         cat_vg_parq) %>% 
  summary()



# ------------------------------------------------------------------------------
# Primeiro modelo: OLS com regressão simples
# ------------------------------------------------------------------------------



# head(base_modelo)

# Melhor modelo (R² 0.2117) - modelo mais interessante é
# com cat_grad (em vez de elev_grad_rev) e cat_curv (R² 0.2047)
# ols_resultados <- lm(vel_med_m5 ~
#                        inicio_fim + 
#                        elev_grad_rev +
#                        I(elev_grad_rev^2) +
#                        I(elev_grad_rev^3) +
#                        # cat_grad + 
#                        class_via +
#                        infra_ciclo +
#                        semaforos +
#                        via_restr +
#                        osm_oneway +
#                        contramao +
#                        dist_total +
#                        dist_trecho_quadra +
#                        # curv_h +
#                        cat_curv +
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

# https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic
BIC(ols_resultados)
logLik(ols_resultados)
AIC(ols_resultados)

# QQplot - distribuição chi-square?
# https://www.statology.org/q-q-plot-r/
qqnorm(base_modelo$vel_med_m5)
qqline(base_modelo$vel_med_m5)

plot(ols_resultados$fitted.values, ols_resultados$residuals, cex = 0.025)
# Plot Standardized residuals x Theoretical quantiles
plot(ols_resultados, which = 2)

# https://www.statology.org/plot-lm-in-r/
plot(ols_resultados)
abline(ols_resultados)

# How to Fix Heteroscedasticity
# https://statisticsbyjim.com/regression/heteroscedasticity-regression/

# https://www.statology.org/breusch-pagan-test-r/
# https://stats.stackexchange.com/questions/193061/what-is-the-difference-between-these-two-breusch-pagan-tests
# If the p-value is less than 0.05, we reject the null hypothesis - this means 
# heteroscedasticity is present in the data
library(lmtest)
bptest(ols_resultados)
bptest(ols_resultados, studentize = FALSE)

# # https://www.statology.org/aic-in-r/
# library(AICcmodavg)
# # aictab(cand.set = models, modnames = mod.names)
# models <- list(ols_resultados, ols_resultados2, ols_resultados3)
# mod.names <- c('mod1', 'mod2', 'mod3')
# aictab(cand.set = ols_resultados, modnames = mod.names)

# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
library(olsrr)
ols_test_breusch_pagan(ols_resultados)
ols_test_breusch_pagan(ols_resultados, rhs = TRUE, multiple = TRUE)

# Teste de heterocedasticidade
# The parametric Goldfeld–Quandt test offers a simple and intuitive diagnostic 
# for heteroskedastic errors in a univariate or multivariate regression model. 
# However some disadvantages arise under certain specifications or in comparison 
# to other diagnostics, namely the Breusch–Pagan test, as the Goldfeld–Quandt 
# test is somewhat of an ad hoc test
# https://en.wikipedia.org/wiki/Goldfeld%E2%80%93Quandt_test
# https://www.statology.org/goldfeld-quandt-test-in-r/
# https://datasciencestunt.com/linear-regression-heteroskedasticity-and-transformations/
# https://www.geeksforgeeks.org/goldfeld-quandt-test/
library(lmtest)
gqtest(ols_resultados, 
       order.by = 
         ~inicio_fim + 
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
         dist_total_km +
         dist_trecho_quadra +
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
         cat_vg_parq, 
       data = base_modelo, 
       fraction = 1523111)


plot(ols_resultados$fitted.values, ols_resultados$residuals, cex = 0.25)

names(ols_resultados)
plot(ols_resultados, which = 5)


base_modelo <- base_modelo %>% mutate(log_speed = log(vel_med_m5))
hist(base_modelo$log_speed, breaks = 128)
summary(base_modelo$log_speed)

base_modelo <- base_modelo %>% mutate(sqrt_speed = sqrt(vel_med_m5))
hist(base_modelo$sqrt_speed, breaks = 128)



hist(base_modelo$vel_med_m5, breaks = 128)
hist(base_modelo$vel_med_m5_log, breaks = 128)
hist(base_modelo$vel_med_m5_log, breaks = 128, xlim = c(0,4))





base_modelo <- base_modelo %>% mutate(dist_trecho_quadra_log = log(dist_trecho_quadra))

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
                       (1 | qgis_id) +
                       (1 | trip_id)
                     ,
                     data = base_modelo)

summary(ols_resultados)
# vcov(ols_resultados)



# Marginal and Conditional R² - marginal represents the variance explained by the
# fixed effects; conditional is interpreted as a variance explained by the entire 
# model, including both fixed and random effects
# https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
# https://www.rdocumentation.org/packages/MuMIn/versions/1.47.5/topics/r.squaredGLMM
MuMIn::r.squaredGLMM(ols_resultados)
BIC(ols_resultados)
logLik(ols_resultados)
AIC(ols_resultados)

library(lmtest)
bptest(ols_resultados)

# https://stackoverflow.com/questions/69331895/plotting-standardized-residual-vs-fitted-values-from-a-for-loop
# Fitted values for the model
SS <- rstandard(ols_resultados)
# extracting standardized residuals and fitted values
FV <- fitted(ols_resultados)
scatter.smooth(
  SS ~ FV, col = "grey",
  las = 1, ylab = "Standardized residuals", xlab = "Fitted values")

ranova(ols_resultados)

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

plot(ols_resultados$fitted.values, ols_resultados$residuals, cex = 0.25)
plot(ols_resultados2$fitted.values, ols_resultados2$residuals, cex = 0.25)

# Change the panel layout to 2 x 2 (to look at all 4 plots at once)
par(mfrow = c(2, 2))
# Use plot() function to create diagnostic plots
plot(ols_resultados)


# https://stackoverflow.com/questions/74439607/how-to-save-a-model-summary-as-data-frame-in-r
ols_resultados_df <- tidy(ols_resultados)
glance(ols_resultados)

# Exportar resultados
out_file <- sprintf('%s/resultados_ols_modelo_1.csv', pasta_base_modelo)
write_delim(ols_resultados_df, out_file, delim = ';')




# Transformar colunas de interesse em factors - categorização na ordem, para graphhopper
base_modelo <- 
  base_modelo %>%
  mutate(
    inicio_fim = factor(inicio_fim, levels = c('meio', 'início', 'final')),
    cat_grad   = factor(cat_grad,   levels = c('subi_ver', 'subi_for', 'subi_med',  'subi_lev', 'plano', 'desc_ver', 'desc_lev', 'desc_med', 'desc_for')),
    # cat_grad_prev   = factor(cat_grad_prev,   levels = c('plano', 'desc_lev', 'desc_med', 'desc_for', 'desc_ver', 'subi_lev', 'subi_med', 'subi_for', 'subi_ver'))
    class_via    = factor(class_via,   levels = c('vtr', 'ped_serv', 'local', 'coletora', 'arterial', 'ciclo_expressa')),
    infra_ciclo  = factor(infra_ciclo, levels = c('sem_infra_ciclo', 'ciclofaixa', 'ciclovia')),
    semaforos    = factor(semaforos, levels = c('fim', 'inicio_fim', 'inicio', 'sem_semaforos')),
    via_restr    = factor(via_restr,   levels = c('via_comum', 'via_restrita')),
    osm_oneway   = factor(osm_oneway, levels = c('no', 'yes')),
    contramao    = factor(contramao, levels = c('não', 'sim')),
    dia_util     = factor(dia_util,    levels = c('util', 'desc')),
    cat_fx_hora  = factor(cat_fx_hora, levels = c('madrugada', 'vale', 'pico_manha', 'pico_tarde')),
    cat_vg_loop  = factor(vg_loop, levels = c('não', 'sim')),
    cat_vg_exper = factor(vg_exper, levels = c('não', 'sim')),
    cat_vg_conm  = factor(vg_contramao, levels = c('não', 'sim')),
    cat_vg_parq  = factor(vg_parques, levels = c('não', 'sim')),
    cat_curv     = factor(cat_curv, levels = c('00 a 25 graus', '25 a 45 graus', '45 a 90 graus', 'acima de 90 graus')),
  )




# stepwise <- step(ols_resultados, direction = "backward")
# summary(stepwise)
# formula(stepwise)


# Call:
#   lm(formula = vel_med_m5 ~ inicio_fim + cat_grad + class_via + 
#        infra_ciclo + via_restr + osm_oneway + contramao + dist_total + 
#        curv_h + dens_lotes_100m + dia_util + cat_fx_hora, data = base_modelo)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -18.26  -3.13  -0.25   2.73 432.81 
# 
# Coefficients:
#   Estimate   Std. Error  t value             Pr(>|t|)    
# (Intercept)             10.729530578  0.011090774  967.428 < 0.0000000000000002 ***
#   inicio_fiminício        -3.297762910  0.013245874 -248.965 < 0.0000000000000002 ***
#   inicio_fimfinal         -7.250147090  0.013524814 -536.063 < 0.0000000000000002 ***
#   cat_graddesc_lev         0.864013232  0.010365960   83.351 < 0.0000000000000002 ***
#   cat_graddesc_med         1.903535557  0.021193712   89.816 < 0.0000000000000002 ***
#   cat_graddesc_for         2.356103851  0.032292024   72.962 < 0.0000000000000002 ***
#   cat_graddesc_ver         0.889930369  0.045023510   19.766 < 0.0000000000000002 ***
#   cat_gradsubi_lev        -1.184885534  0.010205870 -116.098 < 0.0000000000000002 ***
#   cat_gradsubi_med        -1.818037220  0.020370856  -89.247 < 0.0000000000000002 ***
#   cat_gradsubi_for        -2.552318368  0.030630160  -83.327 < 0.0000000000000002 ***
#   cat_gradsubi_ver        -1.309264330  0.047388547  -27.628 < 0.0000000000000002 ***
#   class_viacoletora        0.362344670  0.009126742   39.701 < 0.0000000000000002 ***
#   class_viaarterial        0.333713451  0.009733152   34.286 < 0.0000000000000002 ***
#   class_viavtr             0.934451442  0.233966661    3.994        0.00006498214 ***
#   class_viaciclo_expressa  1.492825176  0.063180824   23.628 < 0.0000000000000002 ***
#   class_viaped_serv       -0.637500012  0.013120691  -48.587 < 0.0000000000000002 ***
#   infra_ciclociclofaixa    0.080425255  0.013570047    5.927        0.00000000309 ***
#   infra_ciclociclovia      0.336238453  0.006688614   50.270 < 0.0000000000000002 ***
#   via_restrvia_restrita   -0.011750075  0.010250041   -1.146                0.252    
# osm_onewayno            -0.427828053  0.007335241  -58.325 < 0.0000000000000002 ***
#   contramaosim            -0.786506839  0.007804458 -100.777 < 0.0000000000000002 ***
#   dist_total      0.000227557  0.000000884  257.413 < 0.0000000000000002 ***
#   curv_h                   0.495089163  0.055732242    8.883 < 0.0000000000000002 ***
#   dens_lotes_100m         -0.004120639  0.000347348  -11.863 < 0.0000000000000002 ***
#   dia_utildesc            -0.762626889  0.005205069 -146.516 < 0.0000000000000002 ***
#   cat_fx_horapico_manha    0.591558132  0.006455201   91.641 < 0.0000000000000002 ***
#   cat_fx_horapico_tarde   -0.081192443  0.005411018  -15.005 < 0.0000000000000002 ***
#   cat_fx_horamadrugada     0.621274356  0.011182085   55.560 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.098 on 5085778 degrees of freedom
# Multiple R-squared:  0.1114,	Adjusted R-squared:  0.1114 
# F-statistic: 2.361e+04 on 27 and 5085778 DF,  p-value: < 0.00000000000000022


ols_resultados <- lm(vel_med_m5 ~
                       inicio_fim + 
                       elev_grad_rev +
                       I(elev_grad_rev^2) +
                       I(elev_grad_rev^3) +
                       class_via +
                       infra_ciclo +
                       semaforos +
                       via_restr + 
                       osm_oneway +
                       contramao +
                       dist_total +
                       # curv_h +
                       # cat_curv +
                       # dens_lotes_100m +
                       # dens_lotes_100m_30m +
                       dia_util +
                       cat_fx_hora+
                       cat_vg_loop +
                       cat_vg_exper +
                       # cat_vg_conm +
                       cat_vg_parq,
                     data = base_modelo)

summary(ols_resultados)

# base_modelo <- base_modelo %>% mutate(log_vel_med_m5 = log(vel_med_m5))
# ols_resultados <- lm(log_vel_med_m5 ~
#                        inicio_fim + 
#                        elev_grad_rev +
#                        # I(elev_grad_rev^2) +
#                        class_via +
#                        infra_ciclo +
#                        via_restr + 
#                        osm_oneway +
#                        contramao +
#                        dist_total +
#                        # curv_h +
#                        cat_curv +
#                        dens_lotes_100m +
#                        # dens_lotes_100m_30m +
#                        dia_util +
#                        cat_fx_hora+
#                        cat_vg_loop +
#                        # cat_vg_conm +
#                        cat_vg_parq,
#                      data = base_modelo)
# 
# summary(ols_resultados)

cor(base_modelo$vel_med_m5, base_modelo$elev_grad_rev) # -0.1188
plot(base_modelo$elev_grad_rev, base_modelo$vel_med_m5, cex = 1, pch = 16)
abline()

ols_resultados1 <- lm(vel_med_m5 ~ elev_grad_rev,
                     data = base_modelo)
ols_resultados2 <- lm(vel_med_m5 ~ elev_grad_rev + I(elev_grad_rev^2),
                      data = base_modelo)
ols_resultados3 <- lm(vel_med_m5 ~ elev_grad_rev + I(elev_grad_rev^2) + I(elev_grad_rev^3),
                      data = base_modelo)
summary(ols_resultados1)
summary(ols_resultados2)
summary(ols_resultados3)

plot(base_modelo$elev_grad_rev~base_modelo$vel_med_m5, cex = 1, pch = 16)
lines(base_modelo$elev_grad_rev, predict(ols_resultados1), col = "blue", lwd = 2)
lines(base_modelo$elev_grad_rev, predict(ols_resultados2), col = "green", lwd = 2)
lines(base_modelo$elev_grad_rev, predict(ols_resultados3), col = "red", lwd = 2)


# Plot x vs y - relação linear entre as variáveis?
plot(base_modelo$curv_h, base_modelo$vel_med_m5, cex = 1, pch = 16)

# Call:
#   lm(formula = vel_med_m5 ~ inicio_fim + elev_grad_rev + class_via + 
#        infra_ciclo + via_restr + osm_oneway + contramao + dist_total + 
#        curv_h + dens_lotes_100m + dia_util + cat_fx_hora, data = base_modelo)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -28.50  -3.12  -0.25   2.73 433.00 
# 
# Coefficients:
#   Estimate    Std. Error  t value             Pr(>|t|)    
# (Intercept)             10.7213570686  0.0110486919  970.373 < 0.0000000000000002 ***
#   inicio_fiminício        -3.2973533005  0.0132419345 -249.008 < 0.0000000000000002 ***
#   inicio_fimfinal         -7.2434226720  0.0135215959 -535.693 < 0.0000000000000002 ***
#   elev_grad_rev           -0.2546981853  0.0010947049 -232.664 < 0.0000000000000002 ***
#   class_viacoletora        0.3671747238  0.0091233294   40.246 < 0.0000000000000002 ***
#   class_viaarterial        0.3318600366  0.0097240512   34.128 < 0.0000000000000002 ***
#   class_viavtr             0.7402330650  0.2338116630    3.166              0.00155 ** 
#   class_viaciclo_expressa  1.5103246371  0.0631632517   23.911 < 0.0000000000000002 ***
#   class_viaped_serv       -0.6695893332  0.0130782327  -51.199 < 0.0000000000000002 ***
#   infra_ciclociclofaixa    0.0810748759  0.0135637852    5.977        0.00000000227 ***
#   infra_ciclociclovia      0.3424439139  0.0066312173   51.641 < 0.0000000000000002 ***
#   via_restrvia_restrita   -0.0122191563  0.0102238354   -1.195              0.23202    
# osm_onewayno            -0.4436776320  0.0073234895  -60.583 < 0.0000000000000002 ***
#   contramaosim            -0.8062916597  0.0078020880 -103.343 < 0.0000000000000002 ***
#   dist_total      0.0002267273  0.0000008835  256.623 < 0.0000000000000002 ***
#   curv_h                   0.5164960490  0.0556131437    9.287 < 0.0000000000000002 ***
#   dens_lotes_100m         -0.0043448108  0.0003469956  -12.521 < 0.0000000000000002 ***
#   dia_utildesc            -0.7666580830  0.0052033012 -147.341 < 0.0000000000000002 ***
#   cat_fx_horapico_manha    0.5858668718  0.0064539276   90.777 < 0.0000000000000002 ***
#   cat_fx_horapico_tarde   -0.0790744364  0.0054096703  -14.617 < 0.0000000000000002 ***
#   cat_fx_horamadrugada     0.6189709477  0.0111783120   55.372 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.097 on 5085785 degrees of freedom
# Multiple R-squared:  0.1118,	Adjusted R-squared:  0.1118 
# F-statistic: 3.201e+04 on 20 and 5085785 DF,  p-value: < 0.00000000000000022



# ------------------------------------------------------------------------------
# Relações entre as variáveis
# ------------------------------------------------------------------------------

base_modelo %>% select(vel_med_m3, vel_med_m5) %>% summary()

base_modelo %>% filter(vel_med_m5 > 40) %>% nrow()




# Plot x vs y - relação linear entre as variáveis?
plot(base_modelo$curv_h, base_modelo$vel_med_m5, cex = 1, pch = 16)

plot(base_modelo$elev_grad_rev, base_modelo$vel_med_m5, 
     pch = 16, cex = 0.25, 
#     xlim = c(0, 40), 
     ylim = c(0, 20)
)

plot(base_modelo$elev_grad_rev, base_modelo$vel_med_m3, 
     pch = 16, cex = 0.25, 
     #     xlim = c(0, 40), 
     ylim = c(0, 20)
)

abline()


# ------------------------------------------------------------------------------
# Estatísticas resumitivas
# ------------------------------------------------------------------------------

# Summary 1 - Estão estranhas as camadas de curv_h (3º quartil maior que mediana),
# elev_grad poderia ser até +-20 e lotes precisa checar onde está dando números
# altos
base_modelo_agreg %>% 
  select(vel_med_gps, vel_med_m3, vel_med_m5,
         length_m, lotes, curv_h, elev_grad) %>% 
  summary()


# base_modelo %>% 
#   select(qgis_id, lotes) %>% 
#   arrange(-lotes) %>% 
#   distinct() %>% 
#   head(10)
# 
# 
# base_modelo %>% 
#   filter(n_pontos > 2) %>% 
#   filter(vel_med_m5 > 20 & elev_sent == 'subida' & elev_grad > 5) %>% 
#   select(trip_id, qgis_id, n_pontos, elev_sent, elev_grad, length_m, vel_med_gps, vel_med_m3, vel_med_m5) %>% 
#   head(20)
# 
# base_modelo %>% 
#   # filter(trip_id == '013918_12') %>% 
#   filter(vel_med_m5 > 40) %>% 
#   select(trip_id, qgis_id, n_pontos, elev_sent, elev_grad, speed_kph, vel_med_gps, vel_med_m3, vel_med_m5) %>% 
#   distinct() %>% 
#   head(20)
# 
# 
# base_modelo %>% 
#   filter(trip_id == '013918_12') %>% 
#   filter(vel_med_m3 > 40) %>% 
#   select(qgis_id) %>% 
#   distinct() %>% 
#   as.list()


# Histogram
hist(base_modelo$vel_med_gps, breaks = 200)
hist(base_modelo$vel_med_m3, breaks = 200)
hist(base_modelo$vel_med_m5, breaks = 200)

boxplot(base_modelo$vel_med_m5)

# QQplot - distribuição chi-square?
# https://www.statology.org/q-q-plot-r/
qqnorm(base_modelo$vel_med_m5)
qqline(base_modelo$vel_med_m5)


qqnorm(base_modelo$vel_med_m3)
qqnorm(base_modelo$vel_med_gps)

# # https://www.programmingr.com/animation-graphics-r/qq-plot/
# qqnorm(rnorm(50, 50, 20))
# boxplot(rnorm(50, 50, 20))
# hist(rnorm(50, 50, 20), breaks = 10)
# # normal QQ plot in R - normal quantile plot
# x = rnorm(50, 50, 20)
# y = base_modelo$vel_med_m5
# # normal QQ plot in R
# qqplot(x, y, xlab = "Normal Distribution", ylab = "Urban Population", main = "Q-Q Plot")



ols_resultados <- lm(vel_med_m5 ~ length_m, data = base_modelo_agreg)
summary(ols_resultados)

ols_resultados <- lm(vel_med_m5 ~ elev_grad, data = base_modelo_agreg)
summary(ols_resultados)

ols_resultados <- lm(vel_med_m5 ~ infra_ciclo, data = base_modelo_agreg)
summary(ols_resultados)



ols_resultados <- lm(vel_med_m5 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg)
summary(ols_resultados)


ols_resultados <- lm(vel_med_m5 ~ length_m + elev_grad + curv_h + dia_util + cat_fx_hora + infra_ciclo + via_restr,
                     data = base_modelo_agreg)
summary(ols_resultados)

ols_resultados <- lm(vel_med_m5 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo,
                     data = base_modelo_agreg)
summary(ols_resultados)


base_modelo_agreg_desc <- base_modelo_agreg %>% filter(elev_grad < 0)
ols_resultados <- lm(vel_med_m5 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg_desc)
summary(ols_resultados)

base_modelo_agreg_sub <- base_modelo_agreg %>% filter(elev_grad > 0)
ols_resultados <- lm(vel_med_m5 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg_sub)
summary(ols_resultados)

base_modelo_agreg %>% filter(infra_ciclo == 'expressa')


# base_modelo %>% filter(speed_kph > 60) %>% select(qgis_id, elev_grad) %>% head(20)

base_modelo <- 
  base_modelo %>% 
  mutate(log_speed_kph  = log(speed_kph),
         log_vel_med_m3 = log10(vel_med_m3),
         log_vel_med_m5 = log(vel_med_m5))

# Summary 1
base_modelo %>% 
  select('tempo_trecho', 'length_m', 
         'speed_kph', 'vel_med_gps', 'vel_med_m3', 'vel_med_m5',
         'log_speed_kph', 'log_vel_med_m3', 'log_vel_med_m5',
         'curv_h', 'lotes', 'elev_grad') %>% 
  summary()

hist(base_modelo_agreg$log_vel_med_m3, breaks = 250)

# # Summary 2
# base_modelo %>% 
#   select('class_via', 'infra_ciclo', 'via_restr', 'dia_semana', 'fx_hora') %>% 
#   summary()

# Histogram
hist(base_modelo$vel_med_m3, breaks = 128, xlim = c(0, 30))
h_breaks <- ceiling(nrow(base_modelo)/1000)
hist(base_modelo$vel_med_m3, breaks = h_breaks, xlim = c(0, 30))

hist(base_modelo$vel_med_m5, breaks = h_breaks, xlim = c(0, 30))

hist(base_modelo$elev_grad, breaks = 250, xlim = c(-10, 10))

hist(base_modelo$curv_h, breaks = 250)

hist(base_modelo$lotes, breaks = 100)




ols_resultados <- lm(speed_kph ~ length_m + lotes + curv_h + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg)
summary(ols_resultados)

ols_resultados <- lm(vel_med_m3 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg)
summary(ols_resultados)

ols_resultados <- lm(log_vel_med_m3 ~ length_m + elev_grad + dia_util + cat_fx_hora + class_via + infra_ciclo + via_restr,
                     data = base_modelo_agreg)
summary(ols_resultados)



# https://www.statology.org/ols-regression-in-r/
# https://www.statology.org/transform-data-in-r/

#define residuals
res <- resid(ols_resultados)

#produce residual vs. fitted plot
plot(fitted(ols_resultados), res, cex = 0.25)

#add a horizontal line at 0 
abline(0,0)


#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 
