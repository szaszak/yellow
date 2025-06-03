# Aplica o lpSolve para carregar a rede - resulta na quantidade de viagens entre
# cada hexágono de origem e destino (relação população vs matrículas). Como temos
# um cenário em que a população (demanda) é menor do que a quantidade de vagas
# escolares (oferta), a aplicação do lpSolve é feita da forma original, sem
# inverter a matriz de demanda e oferta

# Faremos somente para 2024, pois nos scripts seguintes usaremos a mesma alocação 
# de 2024 só que com as rotas de 2028

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('lpSolve')
library('sf')
library('mapview')
options(scipen = 999)

# Definir ano de análise e limite máximo de tempo
ano <- '2024'; tempo_max <- '15'
# ano <- '2028'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados          <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)
pasta_lpsolve_24_28  <- sprintf("%s/05_lpsolve_2024_2028", pasta_aop_2024_2028)
pasta_opaop_ano      <- sprintf("%s/%s", pasta_lpsolve_24_28, ano)
dir.create(pasta_opaop_ano, recursive = TRUE, showWarnings = FALSE)

# pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
# pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
# pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
# pasta_opaop_ttmat <- sprintf("%s/01_ttmatrix", pasta_aop_optimum, ano)
# pasta_opaop_dados <- sprintf("%s/02_dados_pop_mat", pasta_aop_optimum)
# pasta_aop_lpsolve <- sprintf("%s/04_lpSolve2_pop_ajustada", pasta_aop_optimum)
# pasta_opaop_ano   <- sprintf("%s/%s", pasta_aop_lpsolve, ano)
# dir.create(pasta_opaop_ano, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Bases de dados - população (demanda) < matrículas (oferta)
# ------------------------------------------------------------------------------

# Demanda: quantidade de estudantes por hexágono
pop <- sprintf('%s/01_divisao_prop_integral_estudantes_por_zona_OD_hexagonos_short.csv', pasta_ttmatrix_24_28)
pop <- read_delim(pop, delim = ';', col_types = cols(.default = "c"))
pop <- pop %>% mutate_at(5:ncol(.), as.numeric)
pop <- pop %>% group_by(h3_address) %>% summarise(estudantes_totais = sum(estudantes_totais)) %>% ungroup()
# pop %>% filter(h3_address == '89a81000127ffff') # 34
pop <- pop %>% select(orig = h3_address, pop = estudantes_totais)
# Checar se algum id ficou duplicado por qualquer motivo
# pop %>% group_by(orig) %>% tally() %>% filter(n > 1) %>% nrow()
sum(pop$pop) # 337983
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/02_matriculas_censo_escolar_2023_associadas_a_hexagonos.gpkg', pasta_ttmatrix_24_28)
mat <- read_sf(mat) %>% st_drop_geometry()
# Aplicar filtros de escolas públicas sem restrição e com vagas de ensino médio
mat <- mat %>% filter(QT_MAT_MED > 0 & CA_CATEGORIA == 'Pública' & CA_RESTRICAO == 'ESCOLA EM FUNCIONAMENTO E SEM RESTRIÇÃO DE ATENDIMENTO')
mat <- mat %>% group_by(id_hex) %>% summarise(matriculas_totais = sum(QT_MAT_MED)) %>% ungroup()
# mat %>% filter(id_hex == '89a8107159bffff') # 520 + 705 = 1225
mat <- mat %>% select(dest = id_hex, mat = matriculas_totais)
# Checar se algum id ficou duplicado por qualquer motivo
# mat %>% group_by(dest) %>% tally() %>% filter(n > 1) %>% nrow()
sum(mat$mat) # 364441
head(mat)

# Neste caso, temos uma demanda de população MENOR do que a oferta de matrículas
print(sprintf('População: %s; Matrículas: %s (Diferença: %s)', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))


# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
if (ano == '2024') {
  custos <- sprintf('%s/10_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, tempo_max)
} else if (ano == '2028') {
  # custos <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano, tempo_max)
  custos <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano, tempo_max)
}
custos <- read_delim(custos, delim = ';', col_select = c('hex_id', 'time'), col_types = cols(.default = "c"))
# Checar se algum id ficou duplicado por qualquer motivo
# custos %>% group_by(hex_id) %>% tally() %>% filter(n > 1) %>% nrow()
custos <- custos %>% separate(hex_id, into = c('orig', 'dest'), sep = '-', remove = TRUE)
head(custos)


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
# orig            dest            time   
# <chr>           <chr>           <chr>  
# 1 89a81000003ffff 89a81000027ffff 644.656
# 2 89a81000003ffff 89a8100002bffff 693.182
# 3 89a81000003ffff 89a81000077ffff 601.127
# 4 89a81000003ffff 89a8100008fffff 258.346
# 5 89a81000003ffff 89a8100016fffff NA     
# 6 89a81000003ffff 89a8100032bffff NA     

# ------------------------------------------------------------------------------
# Etapa 1 - Chegar se há origens ou destinos impossíveis de satisfazer
# ------------------------------------------------------------------------------

# Se o tempo for NA, é porque a origem está no dataframe de origem (há 
# população), mas o destino não está no dataframe da matriz de custos, ou
# seja, não é possível chegar naquelas matrículas (está acima do limite de 
# tempo estabelecido previamente - 15, 30 , 40 minutos). O que vamos fazer é
# checar se não é possível chegar de nenhuma forma a esses destinos (não estão
# na matriz de custos) - se não for, vamos registrar e remover de cost.mat. Caso
# seja possível chegar neles por alguma combinação (ou seja, o destino está
# na matriz "custos", esses NA serão substituídos por valores proibitivos adiante
destinos_na     <- cost.mat %>% filter(is.na(time)) %>% select(dest) %>% distinct()
destinos_nao_na <- cost.mat %>% filter(!is.na(time)) %>% select(dest) %>% distinct()
destinos_impossiveis <- destinos_na %>% filter(!dest %in% destinos_nao_na$dest)

# destinos_impossiveis %>% summarise(this = toString(dest)) %>% pull()
# custos %>% filter(dest %in% destinos_impossiveis$dest)

# De forma similar, o mesmo acontece com as origens - detectar as isoladas
origens_na     <- cost.mat %>% filter(is.na(time)) %>% select(orig) %>% distinct()
origens_nao_na <- cost.mat %>% filter(!is.na(time)) %>% select(orig) %>% distinct()
# Origens impossíveis são as que não possuem destino possível (com tempo não NA) e
# cuja origem não esteja na ttmatrix como possível de ser feita no limite de tempo
# estabelecido pelo recorte (15 min)
origens_impossiveis <- origens_na %>% filter(!orig %in% origens_nao_na$orig) %>% filter(!orig %in% custos$orig)


# origens_impossiveis %>% summarise(this = toString(orig)) %>% pull()
# custos %>% filter(orig %in% origens_impossiveis$orig)
# cost.mat %>% filter(orig %in% origens_impossiveis$orig)
# custos %>% filter(orig == '89a81000637ffff')
# pop %>% filter(orig %in% origens_impossiveis$orig)


# Hexágonos que possuem oportunidades e/ou população, para checagem
hex_dados_popop <- sprintf('%s/01_shape_resumido_estudantes_por_hexagono.gpkg', pasta_ttmatrix_24_28)
hex_dados_popop <- read_sf(hex_dados_popop)
# Agregar oportunidades
hex_dados_popop <- 
  hex_dados_popop %>% 
  left_join(mat, by = c('id_hex' = 'dest')) %>% 
  select(id_hex, estudantes_totais, mat) %>% 
  mutate(mat = ifelse(is.na(mat), 0, mat)) %>% 
  rename(matriculas_totais = mat)

# Gravar hexágonos com origens/destinos impossíveis, para eventual revisão
hex_dados_popop <- 
  hex_dados_popop %>% 
  mutate(destino_impossivel = ifelse(id_hex %in% destinos_impossiveis$dest, 'sim', NA),
         origem_impossivel  = ifelse(id_hex %in% origens_impossiveis$orig, 'sim', NA)) %>% 
  relocate(geom, .after = last_col())


# Para 2024 - 15 minutos, são 290 hexágonos
# Para 2028 - 15 minutos, são  hexágonos
hex_impossiveis <- hex_dados_popop %>% filter(!is.na(destino_impossivel) | !is.na(origem_impossivel))
nrow(hex_impossiveis)
# mapview(hex_impossiveis)


# Gravar hexágonos impossíveis de chegar, para eventual revisão
out_file6 <- sprintf('%s/06_lpsolve_hexagonos_impossiveis_%s_%smin.gpkg', pasta_opaop_ano, ano, tempo_max)
st_write(hex_impossiveis, out_file6, driver = 'GPKG', append = FALSE, delete_layer = TRUE)


# ------------------------------------------------------------------------------
# Etapa 2 - Remover destinos e origens impossíveis
# ------------------------------------------------------------------------------

# Remover destinos e origens impossíveis, caso existam
cost.mat <- cost.mat %>% filter(!dest %in% destinos_impossiveis$dest)
cost.mat <- cost.mat %>% filter(!orig %in% origens_impossiveis$orig)

# Atualizar dataframes de matrículas (destinos) e população (origens)
mat <- mat %>% filter(!dest %in% destinos_impossiveis$dest)
pop <- pop %>% filter(!orig %in% origens_impossiveis$orig)

# Como fica a relação entre demanda e oferta?
# "População: 334318; Matrículas: 364441; Diferença: -30123"
print(sprintf('População: %s; Matrículas: %s; Diferença: %s', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))


# Registrar combinações de pares OD válidos (tempo até o limite estabelecido)
pares_OD_validos <- cost.mat %>% filter(!is.na(time))

# Gravar hexágonos impossíveis de chegar, para eventual revisão
out_file5 <- sprintf('%s/05_lpsolve_pares_OD_validos_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(pares_OD_validos, out_file5, delim = ';')



# Registrar composição de linhas e colunas
reg_linhas  <- cost.mat %>% select(dest) %>% arrange() %>% distinct()
reg_colunas <- cost.mat %>% select(orig) %>% arrange() %>% distinct()

# Continuar a gerar a matriz de custos
cost.mat <- 
  cost.mat %>%
  # Aqui, como a população é MENOR OU IGUAL `do que aà quantidade de matrículas, 
  # os destinos (oferta) ficam como LINHAS; origens (demanda) ficam como COLUNAS
  pivot_wider(id_cols     = dest,
              names_from  = orig,
              values_from = time) %>%
  # Precisamos ordenar as colunas para que a ordem seja a mesma dos DESTINOS/OFERTA
  select(dest, order(colnames(.))) %>%
  # Precisamos ordenar as linhas para que a ordem seja a mesma das ORIGENS/DEMANDA
  arrange(dest) %>%
  # Garantir que conteúdo da matriz esteja em número e que valores NA serão
  # considerados como valores proibitivos
  mutate(across(matches('^89a81'), as.numeric),
         across(where(is.numeric), ~ replace_na(.x, 100000))) %>% 
  # Tendo garantido que linhas e colunas estão ordenadas, remover nomes das linhas
  select(-dest)

head(cost.mat)



# ------------------------------------------------------------------------------
# Etapa 3 - lpSolve - Demorou 50 min para rodar 2024
# ------------------------------------------------------------------------------

# Este é um problema de minimização de custos (tempo)
direction <- 'min'

# Aqui, as linhas são os hexágonos h3 resolução 09 com as quantidades de MATRÍCULAS
# ATENÇÃO: row.signs sempre é <= a alguma coisa, que é a mesma utilizada em row.rhs
row.signs <- rep('<=', nrow(mat))
row.rhs <- mat$mat

# As colunas são os hexágonos h3 resolução 09 com a POPULAÇÃO na faixa etária escolhida
# ATENÇÃO: col.signs sempre é >= a alguma coisa, que é a mesma utilizada em col.rhs
col.signs <- rep('>=', nrow(pop))
col.rhs <- pop$pop

# Solucionando o problema - Demora cerca de 1h40 (2024) para rodar tanto no 
# RStudio quanto no Jupyter (é um processador só)
(start = Sys.time())
solution <- lp.transport(cost.mat  = as.matrix(cost.mat),
                         direction = direction,
                         row.signs = row.signs,
                         row.rhs   = row.rhs,
                         col.signs = col.signs,
                         col.rhs   = col.rhs)
Sys.time()
tempo_lpsolve <- Sys.time() - start

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

# Gravar resultados do lpSolve (solution$solution)
out_file2 <- sprintf('%s/02_lpsolve_solution_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(as.data.frame(solution$solution), out_file2, delim = ';')

# Registrar a cost_matrix utilizada
out_file3 <- sprintf('%s/03_lpsolve_cost_matrix_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(cost.mat, out_file3, delim = ';')

# Registrar o custo total da solução como matriz (solution$solution * cost.mat)
out_file4 <- sprintf('%s/04_lpsolve_solution_x_costmatrix_%s_%smin.csv', pasta_opaop_ano, ano, tempo_max)
write_delim(as.data.frame(solution$solution * cost.mat), out_file4, delim = ';')


# ------------------------------------------------------------------------------
# Etapa 4 - Gerar dataframe de resultados para exportação
# ------------------------------------------------------------------------------

# Combinar origens e destinos para população (demanda) <= matrículas (oferta)
resultados <- 
  solution$solution %>% 
  as_tibble(.name_repair = 'unique') %>%
  # Colunas são as origens (população / demanda)
  setNames(reg_colunas$orig) %>%
  # Inserir os destinos (matrículas / oferta) como linhas
  mutate(dest = reg_linhas$dest, .before = 1) %>%
  # Combinar origens e destinos
  pivot_longer(-dest,
               names_to = 'orig',
               values_to = 'viagens') %>% 
  # Manter somente onde houve viagens
  filter(viagens > 0) %>% 
  # Ordenar colunas
  select(orig, dest, viagens) %>% 
  arrange(orig, dest)

head(resultados)


# Juntar dados de tempo aos resultados - linhas que vão ficar com time = NA são
# as que entraram na solução, ainda que com custo alto (100000), mas cujo tempo 
# estava, na verdade, acima do limite permitido
resultados <- resultados %>% left_join(pares_OD_validos, by = c('orig', 'dest'))


# Registrar quantidade total de viagens por par OD
out_file1 <- sprintf('%s/01_lpsolve_resultados_viagens_por_par_OD_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
write_delim(resultados, out_file1, delim = ';')



# Relação final entre demanda e oferta
# População: 334318; Matrículas: 364441; Diferença: -30123"
print(sprintf('População: %s; Matrículas: %s; Diferença: %s', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))
print(start)
print(tempo_lpsolve)

# Quantas pessoas foram atendidas?
sum(solution$solution) # 334318
# Quantas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 0

# Este número deve ser igual ao número da população atendida
sum(resultados$viagens) # 334318
sum(pop$pop) # 334318
sum(mat$mat) # 364441

# Quantas viagens devem ser desconsideradas?
resultados %>% filter(is.na(time)) %>% select(viagens) %>% sum() # 10317
resultados %>% filter(!is.na(time)) %>% select(viagens) %>% sum() # 324001

solution # 1158430407
