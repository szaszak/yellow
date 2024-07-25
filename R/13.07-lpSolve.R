# Aplica o lpSolve para carregar a rede - resulta na quantidade de viagens entre
# cada hexágono de origem e destino (relação população vs matrículas)

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('lpSolve')
options(scipen = 999)

# Definir ano de análise e limite máximo de tempo
# ano <- '2019'; tempo_max <- '15'
ano <- '2028'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ano   <- sprintf("%s/%s", pasta_aop_optimum, ano)


# ------------------------------------------------------------------------------
# Bases de dados - população (demanda) > matrículas (oferta)
# ------------------------------------------------------------------------------

# No nosso caso, teremos uma população maior do que a quantidade de matrículas,
# o que nos fará ter de transpor (inverter) as linhas com as colunas, deixando
# a população na origem como linhas e as matrículas no destino como colunas. A
# solução que a Tainá deu (https://github.com/tainabittencourt/) é inverter
# a demanda com a oferta - vamos usá-la aqui.

# Demanda: quantidade de pessoas por hexágono
pop <- sprintf('%s/hex_grid_sp_res09_dados_censo_por_hexagono.csv', pasta_aop_optimum)
pop <- read_delim(pop, delim = ';', col_types = cols(.default = "c"))
pop <- pop %>% mutate_at(2:ncol(.), as.numeric)
pop <- pop %>% select(orig = id_hex, pop = pessoas_15_17_hex)
# Checar se algum id ficou duplicado por qualquer motivo
# pop %>% group_by(orig) %>% tally() %>% filter(n > 1) %>% nrow()
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.csv', pasta_aop_optimum)
mat <- read_delim(mat, delim = ';', col_types = cols(.default = "c"))
mat <- mat %>% mutate_at(2:ncol(.), as.numeric)
mat <- mat %>% select(dest = id_hex, mat = matriculas_idades_15_17)
# Checar se algum id ficou duplicado por qualquer motivo
# mat %>% group_by(dest) %>% tally() %>% filter(n > 1) %>% nrow()
head(mat)

# Neste caso, temos uma demanda de população MAIOR do que a oferta de matrículas
print(sprintf('População: %s; Matrículas: %s (Diferença: %s)', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))

# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
custos <- sprintf('%s/01_ttmatrix_%s_res09_%smin.csv', pasta_opaop_ano, ano, tempo_max)
custos <- read_delim(custos, delim = ';', col_select = c('hex_id', 'time_adj'), col_types = cols(.default = "c"))
custos <- custos %>% rename(time = time_adj)
# Checar se algum id ficou duplicado por qualquer motivo
# custos %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% nrow()
custos <- custos %>% separate(hex_id, into = c('orig', 'dest'), sep = '-', remove = TRUE)
head(custos)


# ------------------------------------------------------------------------------
# Etapa 1 - Chegar se há origens ou destinos impossíveis de satisfazer
# ------------------------------------------------------------------------------

# Gerar a matriz de custos por combinações entre origens e destinos
cost.mat <-
  # Combinar todos os valores de origem para todos os de destino em um dataframe
  expand_grid(orig = pop$orig, dest = mat$dest) %>% 
  # Juntar com a matriz de custos
  left_join(custos, by = c('orig', 'dest')) %>% 
  # Se origem for igual ao destino, é um par OD possível e o custo é zero
  mutate(time = ifelse(orig == dest, '0', time))

# cost.mat %>% filter(orig == dest)
head(cost.mat)

# Se o tempo for NA, é porque a origem está no dataframe de origem (há 
# população), mas o destino não está no dataframe da matriz de custos, ou
# seja, não é possível chegar naquelas matrículas (está acima do limite de 
# tempo estabelecido previamente - 15, 30 , 40 minutos). O que vamos fazer é
# checar se não é possível chegar de nenhuma forma a esses destinos (não estão
# na matriz de custos) - se não for, vamos registrar e remover de cost.mat. Caso
# seja possível chegar neles por alguma combinação (ou seja, o destino está
# na matriz "custos", esses NA serão substituídos por valores proibitivos adiante
destinos_impossiveis <- 
  cost.mat %>% 
  filter(is.na(time)) %>% 
  select(dest) %>% 
  distinct() %>% 
  filter(!dest %in% custos$dest)

# destinos_impossiveis %>% summarise(this = toString(dest)) %>% pull()

# De forma similar, o mesmo acontece com as origens - detectar as isoladas
origens_impossiveis <- 
  cost.mat %>% 
  filter(is.na(time)) %>% 
  select(orig) %>% 
  distinct() %>% 
  filter(!orig %in% custos$orig)

# origens_impossiveis %>% summarise(this = toString(orig)) %>% pull()

hex_impossiveis <- 
  destinos_impossiveis %>% 
  rename(orig = dest) %>% 
  rbind(origens_impossiveis) %>% 
  rename(hex_impossiveis = orig) %>% 
  distinct()

# Gravar hexágonos impossíveis de chegar, para eventual revisão
out_file0 <- sprintf('%s/07_lpsolve_hexagonos_impossiveis_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(hex_impossiveis, out_file0, delim = ';')


# Remover destinos e origens impossíveis, caso existam
cost.mat <- cost.mat %>% filter(!dest %in% destinos_impossiveis$dest)
cost.mat <- cost.mat %>% filter(!orig %in% origens_impossiveis$orig)

# Atualizar dataframes de matrículas (destinos) e população (origens)
mat <- mat %>% filter(!dest %in% destinos_impossiveis$dest)
pop <- pop %>% filter(!orig %in% origens_impossiveis$orig)

# Como fica a relação entre demanda e oferta?
print(sprintf('População: %s; Matrículas: %s; Diferença: %s', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))

# Registrar composição de linhas e colunas
reg_linhas  <- cost.mat %>% select(orig) %>% arrange() %>% distinct()
reg_colunas <- cost.mat %>% select(dest) %>% arrange() %>% distinct()

# Continuar a gerar a matriz de custos
cost.mat <- 
  cost.mat %>% 
  # Aqui, destinos (oferta) ficam como COLUNAS; origens (demanda) ficam como LINHAS
  pivot_wider(id_cols     = orig,
              names_from  = dest,
              values_from = time) %>%
  # Precisamos ordenar as colunas para que a ordem seja a mesma dos DESTINOS/OFERTA
  select(orig, order(colnames(.))) %>%
  # Precisamos ordenar as linhas para que a ordem seja a mesma das ORIGENS/DEMANDA
  arrange(orig) %>%
  # Garantir que conteúdo da matriz esteja em número e que valores NA serão
  # considerados como valores proibitivos
  mutate(across(where(is.character), as.numeric),
         across(where(is.numeric), ~ replace_na(.x, 100000))) %>% 
  # across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  # Tendo garantido que linhas e colunas estão ordenadas, remover nomes das linhas
  select(-orig)

head(cost.mat)


# ------------------------------------------------------------------------------
# Etapa 2 - lpSolve
# ------------------------------------------------------------------------------

# Este é um problema de minimização de custos (tempo)
direction <- 'min'

# Aqui, as linhas são os hexágonos h3 resolução 09 com a POPULAÇÃO na faixa etária escolhida
row.signs <- rep('<=', nrow(pop))
row.rhs <- pop$pop

# As colunas são os hexágonos h3 resolução 09 com as quantidades de MATRÍCULAS
col.signs <- rep('>=', nrow(mat))
col.rhs <- mat$mat

# Solucionando o problema - Demora cerca de 1h40 (2019) para rodar tanto no 
# RStudio quanto no Jupyter (é um processador só)
(start = Sys.time())
solution <- lp.transport(cost.mat  = as.matrix(cost.mat),
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
Sys.time()
Sys.time() - start

solution


out_file1 <- sprintf('%s/03_lpsolve_cost_matrix_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(cost.mat, out_file1, delim = ';')

# solution$solution
out_file2 <- sprintf('%s/04_lpsolve_solution_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(as.data.frame(solution$solution), out_file2, delim = ';')

# solution$solution * cost.mat
out_file3 <- sprintf('%s/05_lpsolve_solution_x_costmatrix_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(as.data.frame(solution$solution * cost.mat), out_file3, delim = ';')


# > cost.mat
# # A tibble: 3 × 2
#     `2`   `3`
#   <dbl> <dbl>
# 1   5.4   2.7
# 2   0     7.1
# 3   7.1   0 
# > solution$solution
# [,1] [,2]
# [1,]   13   26
# [2,]   12    0
# [3,]    0   35
# > solution$solution * cost.mat
# 2    3
# 1 70.2 70.2
# 2  0.0  0.0
# 3  0.0  0.0

# Quantas pessoas foram atendidas?
sum(solution$solution) # 86
# Qauntas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 5


# Combinar origens e destinos para população (demanda) > matrículas (oferta)
resultados <- 
  solution$solution %>% 
  as_tibble(.name_repair = 'unique') %>%
  # Colunas são os destinos (matrículas / oferta)
  setNames(reg_colunas$dest) %>%
  # Inserir as origens (população / demanda) como linhas
  mutate(orig = reg_linhas$orig, .before = 1) %>%
  # Combinar origens e destinos
  pivot_longer(-orig,
               names_to = 'dest',
               values_to = 'viagens') %>% 
  # Manter somente onde houve viagens
  filter(viagens > 0)

head(resultados)


# Este número deve ser igual ao número de matrículas
sum(resultados$viagens)

out_file4 <- sprintf('%s/06_lpsolve_resultados_viagens_por_hexagono_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(resultados, out_file4, delim = ';')
