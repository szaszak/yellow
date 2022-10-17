# carregar bibliotecas
source('fun/setup.R')
library('nortest')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"

pasta_base1         <- sprintf("%s/05_testes_viagens_20181111", pasta_dados)
open_file1 <- sprintf('%s/viagens_processadas_todas.csv', pasta_base1)
resultados1 <- read_delim(open_file1, delim = ';', col_types = 'cciidddddiddiiiiiiicc')

pasta_base2         <- sprintf("%s/05_testes_viagens_20181112-20181117", pasta_dados)
open_file2 <- sprintf('%s/viagens_processadas_todas.csv', pasta_base2)
resultados2 <- read_delim(open_file2, delim = ';', col_types = 'cciidddddiddiiiiiiicc')

resultados <- rbind(resultados1, resultados2)
rm(resultados1, resultados2, open_file1, open_file2)

head(resultados)

resultados %>% 
  select('tempo', 'dist', 'veloc', 'semaforos', 'elev_pos', 'elev_neg') %>% 
  summary()

# Minimum and maximum values of a variable
range(resultados$dist)

# Interquartile range (IQR) = third quartile (Q3) - first quartile (Q1)
IQR(resultados$dist)

# Variance of a variable
var(resultados$dist)

# Standard deviation of a variable; the square root of the variance
sd(resultados$dist) 

# Correlation
cor(resultados$dist, resultados$semaforos, use = 'complete') # 0.457, 0.519
cor(resultados$dist, resultados$tempo, use = 'complete') # 0.64, 0.697
cor(resultados$dist, resultados$veloc, use = 'complete') # 0.647, 0.60
cor(resultados$tempo, resultados$veloc, use = 'complete') # 0.049, -0.003

# Scatterplots
plot(resultados$dist, resultados$veloc, cex = 0.25, xlim = c(0, 5000), ylim = c(0, 30))
plot(resultados$tempo, resultados$veloc, cex = 0.25, ylim = c(0, 40))

# Histogram
hist(resultados$veloc, breaks = 128, xlim = c(0, 30))
h_breaks <- ceiling(nrow(resultados)/10)
hist(resultados$veloc, breaks = h_breaks, xlim = c(0, 30))

# Boxplot
boxplot(resultados$veloc,
        ylab = 'Velocidade', # Título do eixo x
        cex.axis = 1.25, 
        cex.lab = 1.25, # Tamanho do título do eixo x
        pch = 20) # Ver as bolinhas em formato 20 (pequenas e preenchidas)

resultados %>% 
  select(veloc) %>% 
  filter(veloc < 20) %>% 
  boxplot(
          ylab = 'Velocidade', # Título do eixo x
          cex.axis = 1.25, 
          cex.lab = 1.25, # Tamanho do título do eixo x
          pch = 20) # Ver as bolinhas em formato 20 (pequenas e preenchidas)
  

# OLS object results (names(ols_results_object))
# 1. coefficients # intercept, beta1
# 2. residuals # resíduos ou termos de erro para cada estimação
# 3. effects # ? duas colunas: intercept e variável independente x utilizada
# 4. rank # ? 2
# 5. fitted.values # valores estimados
# 6. assign # ? 0 e 1
# 7. qr, attr(,"assign"), qraux, pivot, tol, rank, attr(,"class")
# -- qr parece ser os valores calculados para o intercept e a variável x utilizada
# -- assign = 0 ou 1 # 0 é a variável dependente e 1 a independente ? - ver $terms
# -- qraux = ? há dois valores
# -- pivot = ? 1 e 2
# -- tol = 0.0000001 é o valor-p?
# -- rank = 2
# -- attr(,"class") = qr
# 8. df.residual
# 9. xlevels = named list() # ?
# 10. call # formula utilizada, ex. lm(formula = veloc ~ dist, data = resultados)
# 11. term # descrição dos termos usados na fórmula
# 12. model # valores originais das variáveis y e x
ols_resultados <- lm(veloc ~ dist, data = resultados)
summary(ols_resultados)

# Check 1: The linear model is appropriate? Plot fitted values vs residuals - if 
# linearity holds, the residuals will show no pattern, looking like random 
# scatter. If there is any pattern in the residuals, this implies that the 
# linearity assumption does not hold
plot(ols_resultados$fitted.values, ols_resultados$residuals, cex = 0.25, xlim = c(7, 30))
plot(ols_resultados$fitted.values, ols_resultados$residuals, cex = 0.25)

# Teste de correlação entre as variáveis
cor.test(resultados$veloc, resultados$dist, method = "pearson")

# Shapiro-Wilk normality test
shapiro.test(ols_resultados$residuals)
# Shapiro-Francia normality test - Se p < 0.05, os termos de erro não apresentam
# uma distribuição normal
sf.test(ols_resultados$residuals)
plot(ols_resultados$residuals)

# Checar rota 112251 - parece ter sido feita de carro, como resolver?

resultados %>% 
  filter(veloc < 30) %>% 
  ggplot(aes(x = dist, y = veloc), size = 1) +
  geom_point()



resultados %>% 
  select(-c(qtd_quebras, qtd_it_outliers, prop_centr_150, vg_inicio, vg_termino)) %>% 
  # filter(veloc > 30) %>% 
  # mutate(dist_in = dist * prop_centr_100 / 100, .before = 'tempo') %>%
  mutate(prop_disttot_distedges = dist / dist_edges, .after = 'dist') %>% 
  arrange(-veloc) %>% 
  head(20)

resultados %>% 
  select(dist, prop_centr_100) %>% 
  # head(10) %>% 
  mutate(dist_in = dist * prop_centr_100 / 100) %>% 
  filter(dist_in <= 400) %>% 
  arrange(-dist_in)

