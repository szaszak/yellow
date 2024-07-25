library('tidyverse')
library('tidylog')
library('lpSolve')
options(scipen = 999)


# ------------------------------------------------------------------------------
# Exemplo de funcionamento do lpSolve
# ------------------------------------------------------------------------------

# install.packages('lpSolve')
# https://econweb.ucsd.edu/~jsobel/172aw02/notes8.pdf
# https://www.rdocumentation.org/packages/lpSolve/versions/5.6.20/topics/lp.transport
# https://www.supplychaindataanalytics.com/pt/resolvendo-problema-de-transporte-linear-com-lp-transport-em-r-usando-lpsolve/

# specifying cost matrix - the cost for supplying client i by suppyer j
# is defined for each possible combination and expressed in a cost matrix
cost.mat <- matrix(nrow = 3, ncol = 4)
cost.mat[1,] <- 1:4
cost.mat[2,] <- 4:1
cost.mat[3,] <- c(1, 4, 3, 2)
cost.mat

# this is a minimization problem
direction <- 'min'

# capacity may not be exceeded: 3 supplyers with capacities of 100, 300, 400
row.signs <- rep('<=', 3)
row.rhs <- c(100, 300, 700)

# demand must be satisfied: 4 buyers with demand of 100, 100, 200, 400
col.signs <- rep('>=', 4)
col.rhs <- c(100, 100, 200, 400)

# solving the problem
solution <- lp.transport(cost.mat  = cost.mat,
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
solution
solution$solution
solution$solution * cost.mat


# ------------------------------------------------------------------------------
# Exemplo de funcionamento do lpSolve com NAs
# ------------------------------------------------------------------------------

# No nosso exemplo, vamos ter vários NAs na matriz, que deverão ser substituídos
# por valores muito altos para não serem escolhidos
cost.mat[3,] <- c(NA, 1, NA, NA)
# Substituir NAs por valor alto, para não ser a opção escolhida - embora ainda
# assim, ela pode ser a escolhida devido às restrições de linha/coluna
cost.mat[is.na(cost.mat)] = 100000
cost.mat

# solving the problem
solution <- lp.transport(cost.mat  = cost.mat,
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
solution
solution$solution
solution$solution * cost.mat


# ------------------------------------------------------------------------------
# Exemplo como se fosse em acesso a oportunidades para escolas
# ------------------------------------------------------------------------------

# Para o lpSolve, precisamos dos seguintes dados:
# 1. Matriz de custos (tempos de viagem, já abaixo de um limite X de minutos);
# 2. Linhas da tabela: oferta no destino (matrículas de ensino médio)
# 3. Colunas da tabela: demanda na origem (população na faixa etária escolhida)

# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
custos <- data.frame(orig = c('1', '1', '2', '2', '3', '3'),
                     dest = c('B', 'C', 'A', 'C', 'A', 'B'),
                     time = c('5.4', '2.7', '5.4', '7.1', '2.7', '7.1'))

# A população é a demanda, seriam as colunas
pop <- data.frame(orig = c('1', '2', '3'),
                  pop  = c(34, 12, 23))

# As matrículas seriam a oferta, no caso, as linhas
mat <- data.frame(dest = c('A', 'B', 'C'),
                  mat  = c(25, 21, 23))

# Neste caso, temos uma demanda de população igual à oferta de matrículas
sum(pop$pop)
sum(mat$mat)

# Gerar a matriz de custos por combinações entre origens e destinos
cost.mat <-
  custos %>%
  # Destinos (oferta) ficam como linhas; origens (demanda) ficam como colunas
  pivot_wider(id_cols     = dest,
              names_from  = orig,
              values_from = time) %>%
  # Precisamos ordenar as colunas para que a ordem seja a mesma das origens/demanda
  select(dest, order(colnames(.))) %>%
  # Precisamos ordenar as linhas para que a ordem seja a mesma dos destinos/oferta
  arrange(dest) %>%
  # Garantir que conteúdo da matriz esteja em número e que valores NA serão
  # considerados como valores proibitivos
  mutate(across(where(is.character), as.numeric),
         across(where(is.numeric), ~ replace_na(.x, 100000))) %>% 
         # across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  # Tendo garantido que linhas e colunas estão ordenadas, remover nomes das linhas
  select(-dest)

cost.mat

# Este é um problema de minimização de custos (tempo)
direction <- 'min'

# As linhas são os hexágonos h3 resolução 09 com as quantidades de matrículas
row.signs <- rep('<=', nrow(mat))
row.rhs <- mat$mat

# As colunas são os hexágonos h3 resolução 09 com a população na faixa etária escolhida
col.signs <- rep('>=', nrow(pop))
col.rhs <- pop$pop

# Solucionando o problema
solution <- lp.transport(cost.mat  = as.matrix(cost.mat),
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
solution
solution$solution
solution$solution * cost.mat

# Vemos que toda a população (demanda) foi atendida com matrículas (ofertas)
# > solution$solution
# [,1] [,2] [,3]
# [1,]    0    2   23
# [2,]   21    0    0
# [3,]   13   10    0
# > solution$solution * cost.mat
# 1    2    3
# 1   0.0 10.8 62.1
# 2 113.4  0.0  0.0
# 3  35.1 71.0  0.0

# Quantas pessoas foram atendidas?
sum(solution$solution) # 69
# Quantas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 0


# Combinar origens e destinos para matrículas (oferta) >= população (demanda)
solution$solution %>% 
  as_tibble() %>%
  # Colunas são as origens (população / demanda)
  setNames(pop$orig) %>%
  # Inserir os destinos (matrículas / oferta) como linhas
  mutate(dest = mat$dest, .before = 1) %>%
  # Combinar origens e destinos
  pivot_longer(-dest,
               names_to = 'orig',
               values_to = 'pop_mat') %>%
  # Ordenar para facilitar a leitura
  select(2, 1, 3)


# ------------------------------------------------------------------------------
# Exemplo para casos em que a população (demanda) é maior do que as matrículas
# ------------------------------------------------------------------------------

# No nosso caso, teremos uma população maior do que a quantidade de matrículas,
# o que nos fará ter de transpor (inverter) as linhas com as colunas, deixando
# a população na origem como linhas e as matrículas no destino como colunas. A
# solução que a Tainá deu (https://github.com/tainabittencourt/) é inverter
# a demanda com a oferta - vamos usá-la aqui.

# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
custos <- data.frame(orig = c('1', '1', '2', '2', '3', '3'),
                     dest = c('B', 'C', 'A', 'C', 'A', 'B'),
                     time = c('5.4', '2.7', '5.4', '7.1', '2.7', '7.1'))

# A população é a demanda, seriam as colunas
pop <- data.frame(orig = c('1', '2', '3'),
                  pop  = c(54, 12, 30))

# As matrículas seriam a oferta, no caso, as linhas
mat <- data.frame(dest = c('A', 'B', 'C'),
                  mat  = c(25, 21, 23))

# Neste caso, temos uma demanda de população MAIOR do que a oferta de matrículas
sum(pop$pop)
sum(mat$mat)

# Gerar a matriz de custos por combinações entre origens e destinos
cost.mat <-
  custos %>%
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

cost.mat

# Este é um problema de minimização de custos (tempo)
direction <- 'min'

# Aqui, as linhas são os hexágonos h3 resolução 09 com a POPULAÇÃO na faixa etária escolhida
row.signs <- rep('<=', nrow(pop))
row.rhs <- pop$pop

# As colunas são os hexágonos h3 resolução 09 com as quantidades de MATRÍCULAS
col.signs <- rep('>=', nrow(mat))
col.rhs <- mat$mat

# Solucionando o problema
solution <- lp.transport(cost.mat  = as.matrix(cost.mat),
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
solution
solution$solution
solution$solution * cost.mat

# Vemos que o limite de vagas é respeitado, mas nem toda a população é atendida
# > solution$solution
# [,1] [,2] [,3]
# [1,]    0   21   23
# [2,]    0    0    0
# [3,]   25    0    0
# > solution$solution * cost.mat
# 1     2    3
# 1  0.0 113.4 62.1
# 2  0.0   0.0  0.0
# 3 67.5   0.0  0.0

# Quantas pessoas foram atendidas?
sum(solution$solution) # 69
# Quantas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 27


# Combinar origens e destinos para população (demanda) > matrículas (oferta)
solution$solution %>% 
  as_tibble() %>%
  # Colunas são os destinos (matrículas / oferta)
  setNames(mat$dest) %>%
  # Inserir as origens (população / demanda) como linhas
  mutate(orig = pop$orig, .before = 1) %>%
  # Combinar origens e destinos
  pivot_longer(-orig,
               names_to = 'dest',
               values_to = 'viagens')


# ------------------------------------------------------------------------------
# Exemplo complexo - casos em que população (demanda) é maior do que matrículas
# ------------------------------------------------------------------------------

# No nosso caso, teremos uma população maior do que a quantidade de matrículas,
# o que nos fará ter de transpor (inverter) as linhas com as colunas, deixando
# a população na origem como linhas e as matrículas no destino como colunas.

# A população é a demanda, seriam as colunas
pop <- data.frame(orig = c('1', '2', '3', '4'),
                  pop  = c(44, 12, 35, 7))

# As matrículas seriam a oferta, no caso, as linhas
mat <- data.frame(dest = c('2', '3', '5'),
                  mat  = c(25, 61, 4))

# Neste caso, temos uma demanda de população MAIOR do que a oferta de matrículas
print(sprintf('População: %s; Matrículas: %s; Diferença: %s', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))

# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
custos <- data.frame(orig = c('1', '1', '2', '2', '3', '3'),
                     dest = c('2', '3', '1', '3', '1', '2'),
                     time = c('5.4', '2.7', '5.4', '7.1', '2.7', '7.1'))

# Gerar a matriz de custos por combinações entre origens e destinos
cost.mat <-
  # Combinar todos os valores de origem para todos os de destino em um dataframe
  expand_grid(orig = pop$orig, dest = mat$dest) %>% 
  # Juntar com a matriz de custos
  left_join(custos, by = c('orig', 'dest')) %>% 
  # Se origem for igual ao destino, é um par OD possível e o custo é zero
  mutate(time = ifelse(orig == dest, '0', time))

cost.mat

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

# De forma similar, o mesmo acontece com as origens - detectar as isoladas
origens_impossiveis <- 
  cost.mat %>% 
  filter(is.na(time)) %>% 
  select(orig) %>% 
  distinct() %>% 
  filter(!orig %in% custos$orig)


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

cost.mat

# Este é um problema de minimização de custos (tempo)
direction <- 'min'

# Aqui, as linhas são os hexágonos h3 resolução 09 com a POPULAÇÃO na faixa etária escolhida
row.signs <- rep('<=', nrow(pop))
row.rhs <- pop$pop

# As colunas são os hexágonos h3 resolução 09 com as quantidades de MATRÍCULAS
col.signs <- rep('>=', nrow(mat))
col.rhs <- mat$mat

# Solucionando o problema
solution <- lp.transport(cost.mat  = as.matrix(cost.mat),
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
solution
solution$solution
solution$solution * cost.mat

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
# Quantas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 5


# Combinar origens e destinos para população (demanda) > matrículas (oferta)
resultados <- 
  solution$solution %>% 
  as_tibble() %>%
  # Colunas são os destinos (matrículas / oferta)
  setNames(reg_colunas$dest) %>%
  # Inserir as origens (população / demanda) como linhas
  mutate(orig = reg_linhas$orig, .before = 1) %>%
  # Combinar origens e destinos
  pivot_longer(-orig,
               names_to = 'dest',
               values_to = 'viagens')

resultados

# Este número deve ser igual ao número de matrículas
sum(resultados$viagens)