# Aplica o lpSolve para carregar a rede - resulta na quantidade de viagens entre
# cada hexágono de origem e destino (relação população vs matrículas) - nesta 
# versão, a população é diminuída proporcionalmente conforme as matrículas, de
# forma que a aplicação do lpSolve considera população como demanda e matrículas
# como oferta

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('lpSolve')
library('sf')
library('mapview')
options(scipen = 999)

# Definir ano de análise e limite máximo de tempo
ano <- '2019'; tempo_max <- '15'
# ano <- '2028'; tempo_max <- '15'

# Estrutura de pastas e arquivos
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_aop_optimum <- sprintf("%s/13_aop_optimum", pasta_dados)
pasta_opaop_ttmat <- sprintf("%s/01_ttmatrix", pasta_aop_optimum, ano)
pasta_opaop_dados <- sprintf("%s/02_dados_pop_mat", pasta_aop_optimum)
pasta_aop_lpsolve <- sprintf("%s/04_lpSolve2_pop_ajustada", pasta_aop_optimum)
pasta_opaop_ano   <- sprintf("%s/%s", pasta_aop_lpsolve, ano)
dir.create(pasta_opaop_ano, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Bases de dados - população (demanda) > matrículas (oferta)
# ------------------------------------------------------------------------------

# Hexágonos que possuem oportunidades e/ou população, para checagem
hex_dados_popop <- sprintf('%s/tmp_sao_paulo_hexagonos_populacao_oportunidades.gpkg', pasta_aoprv_alter)
hex_dados_popop <- read_sf(hex_dados_popop)

# No nosso caso, teremos uma população maior do que a quantidade de matrículas,
# o que nos fará ter de transpor (inverter) as linhas com as colunas, deixando
# a população na origem como linhas e as matrículas no destino como colunas. A
# solução que a Tainá deu (https://github.com/tainabittencourt/) é inverter
# a demanda com a oferta - vamos usá-la aqui.

# Demanda: quantidade de pessoas por hexágono
pop <- sprintf('%s/hex_grid_sp_res09_dados_censo_por_hexagono.csv', pasta_opaop_dados)
pop <- read_delim(pop, delim = ';', col_types = cols(.default = "c"))
pop <- pop %>% mutate_at(2:ncol(.), as.numeric)
pop <- pop %>% select(orig = id_hex, pop = pessoas_15_17_hex)
# Checar se algum id ficou duplicado por qualquer motivo
# pop %>% group_by(orig) %>% tally() %>% filter(n > 1) %>% nrow()
head(pop)

# Oferta: quantidade de matrículas por hexágono
mat <- sprintf('%s/matriculas_censo_escolar_2019_por_hexagono.csv', pasta_opaop_dados)
mat <- read_delim(mat, delim = ';', col_types = cols(.default = "c"))
mat <- mat %>% mutate_at(2:ncol(.), as.numeric)
mat <- mat %>% select(dest = id_hex, mat = matriculas_idades_15_17)
# Checar se algum id ficou duplicado por qualquer motivo
# mat %>% group_by(dest) %>% tally() %>% filter(n > 1) %>% nrow()
head(mat)

# Neste caso, temos uma demanda de população MAIOR do que a oferta de matrículas
print(sprintf('População: %s; Matrículas: %s (Diferença: %s)', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))


# Ajustar população proporcional ao número de matrículas existentes
proporcao_pop_mat <- sum(pop$pop) / sum(mat$mat)
pop <- pop %>% mutate(pop_prop = round(pop / proporcao_pop_mat))
# A nova população de interesse será agora a população reduzida proporcionalmente
pop <- pop %>% select(orig, pop = pop_prop)

# Passamos a ter uma demanda de população MENOR OU IGUAL à oferta de matrículas
print(sprintf('População: %s; Matrículas: %s (Diferença: %s)', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))


# Matriz de tempo entre hexágonos, já removidos os tempos acima do limite
# estabelecido (15, 30, 40 minutos)
custos <- sprintf('%s/ttmatrix_%s_res09_%smin.csv', pasta_opaop_ttmat, ano, tempo_max)
custos <- read_delim(custos, delim = ';', col_select = c('hex_id', 'time_adj'), col_types = cols(.default = "c"))
custos <- custos %>% rename(time = time_adj)
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
origens_impossiveis <- origens_na %>% filter(!orig %in% origens_nao_na$orig) %>% filter(!orig %in% custos$orig)

# origens_impossiveis %>% summarise(this = toString(orig)) %>% pull()


# # Testar os hexágonos impossíveis - olhar no QGIS e ver se há origens que estão
# # perto dos destinos mas que por qualquer motivo não estão tendo resultados aqui
# 
# # Abrir base de todas as rotas com alternativas, resultado do routing via GrahHopper
# if (ano == '2019') {
#   rotas <- sprintf('%s/01_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
# } else if (ano == '2028') {
#   rotas <- sprintf('%s/04_base_alternatives_%s_res09_40min.csv', pasta_aoprv_alter, ano)
# }
# rotas <- rotas %>% read_delim(rotas, delim = ';', col_types = 'ciddddc')
# head(rotas)
# 
# hex_com_vizinhos <- sprintf("%s/00_base_para_routing_res09_26vizinhos.csv", pasta_aoprv_alter)
# hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
# head(hex_com_vizinhos)
# 
# 
# # Alguns casos de pares OD que eram impossíveis mas que, ao corrigir o filtro
# # de hexágonos no script 12.08 passaram a estar ok
# # Origem com 62 pessoas não chegava a um hexágono do lado com 386 matrículas
# test_orig <- '89a81039c77ffff'; test_dest <- '89a81039c63ffff'
# test_od <- str_c(str_sub(test_orig, 6, 11), str_sub(test_dest, 6, 11), sep = '-')
# rotas %>% filter(hex_id == test_od)
# 
# # 10 pessoas 'morando' no cemitério do Campo Grande e que não chegavam a 142 vagas
# # do outro lado da rua
# test_orig <- '89a810008b7ffff'; test_dest <- '89a81000d7bffff'
# test_od <- str_c(str_sub(test_orig, 6, 11), str_sub(test_dest, 6, 11), sep = '-')
# rotas %>% filter(hex_id == test_od)
# 
# # 28 pessoas na Av Moreira Guimarães não chegam a uma escola uma quadra acima - 
# # aqui, vemos que o touring dá uma mega volta e faz com que o tempo seja irreal
# test_orig <- '89a8100e007ffff'; test_dest <- '89a8100e033ffff'
# test_od <- str_c(str_sub(test_orig, 6, 11), str_sub(test_dest, 6, 11), sep = '-')
# rotas %>% filter(hex_id == test_od)
# 
# # 2 pessoas ao lado de uma ETEC não chegavam a ela - o estranho aqui era que a origem
# # está traçando rotas para vários destinos, mas para este não. E o destino aparece
# # também combinado a várias outras origens
# test_orig <- '89a810735afffff'; test_dest <- '89a810735a7ffff'
# test_od <- str_c(str_sub(test_orig, 6, 11), str_sub(test_dest, 6, 11), sep = '-')
# # O par OD não está nas possibilidades de rotas alternativas
# rotas %>% filter(hex_id == test_od)
# rotas %>% filter(str_starts(hex_id, str_sub(test_orig, 6, 11)))
# custos %>% filter(orig == test_orig)
# custos %>% filter(dest == test_dest)
# custos %>% filter(orig == test_orig & dest == test_dest)
# # Ele não estava no par de vizinhos a serem executados, mas entrou após a última
# # mudança - tudo ok
# hex_com_vizinhos %>% filter(id == str_c(test_orig, test_dest, sep = '-'))

# # Quais são os hexágonos impossíveis de acessar?
# hex_impossiveis <- 
#   destinos_impossiveis %>% 
#   rename(orig = dest) %>% 
#   rbind(origens_impossiveis) %>% 
#   rename(hex_impossiveis = orig) %>% 
#   distinct()

# # Gravar hexágonos impossíveis de chegar, para eventual revisão
# out_file0 <- sprintf('%s/07_lpsolve_hexagonos_impossiveis_%s_%smin.csv', pasta_opaop_ano, ano,tempo_max)
# write_delim(hex_impossiveis, out_file0, delim = ';')


# De quantos hexágonos estamos falando exatamente? Dos hexágonos com dados de
# população e oportunidades...
hex_impossiveis <- 
  hex_dados_popop %>% 
  # Juntar com os dados somente da população de interesse (15-17 anos)
  left_join(pop, by = c('id_hex' = 'orig')) %>% 
  select(id_hex, pop_interesse = pop.y) %>% 
  # Destes, quais de fato possuem população de interesse a ser considerada?
  filter(pop_interesse > 0) %>% 
  # E, finalmente, quantos desses estão marcados como origens impossíveis de 
  # chegar aos destinos de interesse (matrículas)?
  filter(id_hex %in% origens_impossiveis$orig)

# Para 2019 - 15 minutos, são 125 hexágonos
# Para 2028 - 15 minutos, são 117 hexágonos
nrow(hex_impossiveis)
# mapview(hex_impossiveis)


# Gravar hexágonos impossíveis de chegar, para eventual revisão
out_file6 <- sprintf('%s/06_lpsolve_hexagonos_origens_impossiveis_%s_%smin.gpkg', pasta_opaop_ano, ano, tempo_max)
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
# Etapa 3 - lpSolve - Demorou 14h para rodar os dois cenários juntos (2019, 2028)
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
# "População: 374159; Matrículas: 374710; Diferença: -551"
print(sprintf('População: %s; Matrículas: %s; Diferença: %s', sum(pop$pop), sum(mat$mat), sum(pop$pop) - sum(mat$mat)))

# Quantas pessoas foram atendidas?
sum(solution$solution) # 374710
# Quantas pessoas ficaram de fora?
sum(pop$pop) - sum(solution$solution) # 129814

# Este número deve ser igual ao número da população atendida
sum(resultados$viagens)
sum(pop$pop)
sum(mat$mat)

# Quantas viagens devem ser desconsideradas?
resultados %>% filter(is.na(time)) %>% select(viagens) %>% sum()
resultados %>% filter(!is.na(time)) %>% select(viagens) %>% sum()

solution
