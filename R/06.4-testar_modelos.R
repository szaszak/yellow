# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
# dir.create(pasta_base_modelo, recursive = TRUE, showWarnings = FALSE)
 

# Abrir arquivos processados
base_modelo <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'cccicidcidddddddddiiidddccc')

head(base_modelo)


# ------------------------------------------------------------------------------
# Preparações para modelos
# ------------------------------------------------------------------------------

base_modelo


# Feriados vão ser considerados dias de descanso, junto com finais de semana
dias_descanso <- c('sáb', 'dom', 'fer')

base_modelo_agreg <- 
  base_modelo %>%
  mutate(
    # Atualizar coluna de dia_semana para marcar feriados
    dia_util    = ifelse(dia_semana %nin% dias_descanso, 'util', 'desc'),
    # Categorizar picos da manhã, tarde e vale
    cat_fx_hora = case_when(between(fx_hora, 5, 10) ~ 'pico_manha',
                            between(fx_hora, 16, 21) ~ 'pico_tarde',
                            TRUE ~ 'vale'),
    # Juntar categorias de vias de serviço e vias de pedestres
    class_via   = case_when(is.na(class_via) | class_via == 'vias de pedestres' ~ 'ped_serv',
                            # class_via == 'vtr' | class_via == 'rodovia' ~ 'vtr_rodo',
                            TRUE ~ class_via),
    class_via = ifelse(infra_ciclo == 'expressa', 'ciclo_expressa', class_via),
    # Substituir NAs onde não há infraestrutura cicloviária
    infra_ciclo = ifelse(is.na(infra_ciclo), 'sem_infra_ciclo', infra_ciclo),
    # O que não for via em área de acesso restrito vai ser via comum
    via_restr   = ifelse(is.na(via_restr), 'via_comum', 'via_restr'),
    via_restr   = ifelse(infra_ciclo == 'expressa', 'ciclo_expressa', via_restr)
  ) %>% 
  # Transformar colunas de interesse em factors
  mutate(
    dia_util    = factor(dia_util, levels = c('util', 'desc')),
    cat_fx_hora = factor(cat_fx_hora, levels = c('vale', 'pico_manha', 'pico_tarde')),
    class_via   = factor(class_via, levels = c('arterial', 'coletora', 'local', 'ped_serv', 'vtr', 'rodovia', 'ciclo_expressa')), # vtr_rodo
    infra_ciclo = factor(infra_ciclo, levels = c('sem_infra_ciclo', 'ciclofaixa', 'ciclovia', 'expressa')),
    via_restr   = factor(via_restr, levels = c('via_comum', 'via_restr', 'ciclo_expressa'))
  )

# Selecionar colunas de interesse
base_modelo_agreg <- 
  base_modelo_agreg %>% 
  select(speed_kph, vel_med_gps, vel_med_m3, vel_med_m5, 
         length_m, lotes, curv_h, elev_grad,
         dia_util, cat_fx_hora, 
         class_via, infra_ciclo, via_restr)

base_modelo_agreg %>% filter(via_restr == 'via_comum') %>% distinct()

head(base_modelo_agreg)





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
