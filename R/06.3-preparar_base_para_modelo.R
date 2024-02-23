# Faz todos os preparativos para que a base fique pronta para rodar os modelos - 
# demora cerca de 5 min para rodar tudo


# carregar bibliotecas
source('fun/setup.R')


# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_agrup   <- sprintf('%s/B_processados_agrupados', pasta_modelos)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
dir.create(pasta_base_modelo, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Atributos de viário
# ------------------------------------------------------------------------------

# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
# TODO: Atenção: algumas linhas da base atrib_viario estavam duplicadas (70) - isso
# só foi percebido depois, quando o modelo e os resultados já estavam gerados! Por
# isso a linha abaixo está comentada, para gerar resultados reproduzíveis, mas o
# correto é eliminar as duplicatas
# atrib_viario <- atrib_viario %>% distinct()
head(atrib_viario)


# Abrir arquivos com os marcações de vias expressas
vias_expressas <- sprintf('%s/B1_listagem_vias_expressas.csv', pasta_atrib_viario)
vias_expressas <- read_delim(vias_expressas, delim = ';', col_types = 'cc')

head(vias_expressas)


# Abrir arquivos com os marcações de corredores de ônibus
vias_corredores <- sprintf('%s/B2_listagem_vias_corredores_onibus.csv', pasta_atrib_viario)
vias_corredores <- read_delim(vias_corredores, delim = ';', col_types = 'cccc')

head(vias_corredores)


# ------------------------------------------------------------------------------
# Abrir e juntar bases de dados processadas
# ------------------------------------------------------------------------------

# Abrir arquivos processados
base_modelo <- list.files(path = pasta_base_agrup, 
                          pattern = '^\\d{6}_trechos_processados_todos.csv', 
                          recursive = FALSE, 
                          full.names = TRUE)

# arq_trechos_proc <- sprintf('%s/trechos_processados_todos.csv', pasta_base_agrup)
# base_modelo <- read_delim(arq_trechos_proc, delim = ';', col_types = 'cccicidddddddd')
base_modelo <- 
  lapply(X = base_modelo, FUN = read_delim, delim = ';', col_types = 'cccicciidddddddd') %>% 
  rbindlist(fill = TRUE)

# Retirar linhas que estão todas como NA (29 viagens cujo processamento falhou)
base_modelo <- base_modelo %>% filter(!is.na(trip_id))

head(base_modelo)

# # Checar quantidade de viagens restantes na base
# base_modelo %>% 
#   select(trip_id) %>% 
#   distinct()
# 
# # Checar quantidade de trechos restantes na base
# base_modelo %>% 
#   # head() %>% 
#   select(trip_id) %>% 
#   mutate(trip_id = str_sub(trip_id, 1, 6)) %>% 
#   distinct()



# ------------------------------------------------------------------------------
# Marcar inicio e fim da viagem
# ------------------------------------------------------------------------------

# Detectar inícios e finais de viagem, criar um id próprio para merge
inicio_fim <- 
  base_modelo %>% 
  group_by(trip_id) %>% 
  summarise(inicio = min(cluster), # sempre é igual a 1
            final  = max(cluster)) %>% 
  # Criar um id próprio para identificar linhas que são final de viagem
  mutate(if_id = str_c(trip_id, final, sep = '_')) %>% 
  select(if_id)

# Fazer marcações de início, fim e meio de viagem
base_modelo <- 
  base_modelo %>%
  # Criar id temporário para reconhecer as linhas que são finais de viagem
  mutate(if_id = str_c(trip_id, cluster, sep = '_')) %>% 
  # Marcar linhas de início, fim e meio de viagem
  mutate(inicio_fim = case_when(if_id %in% inicio_fim$if_id ~ 'final',
                                cluster == 1                ~ 'início',
                                TRUE                        ~ 'meio')) %>% 
  # Descartar id temporário
  select(-if_id)

head(base_modelo)

# Limpar ambiente
rm(inicio_fim)


# ------------------------------------------------------------------------------
# Categorizar aclividades/declividades
# ------------------------------------------------------------------------------

# Categorizar aclividades e declividades de acordo com gradiente - linhas nas
# quais elev_sent não pôde ser identificado vão ficar sem categorizar
base_modelo <- 
  base_modelo %>% 
  mutate(cat_grad = case_when(between(elev_grad_rev, -99, -9.0000000000001) ~ 'desc_ver', # vertiginosa
                              between(elev_grad_rev,  -9, -6.0000000000001) ~ 'desc_for',
                              between(elev_grad_rev,  -6, -4.0000000000001) ~ 'desc_med',
                              between(elev_grad_rev,  -4, -2.0000000000001) ~ 'desc_lev',
                              between(elev_grad_rev,  -2.0000000000000,  2) ~ 'plano',
                              between(elev_grad_rev,   2.0000000000001,  4) ~ 'subi_lev',
                              between(elev_grad_rev,   4.0000000000001,  6) ~ 'subi_med',
                              between(elev_grad_rev,   6.0000000000001,  9) ~ 'subi_for',
                              between(elev_grad_rev,   9.0000000000001, 99) ~ 'subi_ver')) # vertiginosa

# base_modelo %>% select(cat_grad) %>% distinct()
# base_modelo %>% filter(is.na(cat_grad)) %>% select(cluster, inicio_fim, elev_sent, elev_grad_rev) %>% distinct()
# base_modelo %>% filter(is.na(cat_grad)) %>% group_by(inicio_fim) %>% tally()

# Categorizar a aclividade do trecho anterior da viagem
# TODO: Atenção que como há um tanto de NAs vindos do passo acima, isso é replicado
# aqui fazendo com que haja mais NAs nesta categoria. Ou retirar para rodar o 
# modelo ou avaliar como fazer posteriormente
# base_modelo <- 
#   base_modelo %>% 
#   # Puxar a categoria do trecho anterior e, em seguida, atualizar as linhas que
#   # se referem ao início de viagem (cluster = 1), que não têm inclinação prévia
#   mutate(cat_grad_prev = shift(cat_grad, type = 'lag'),
#          cat_grad_prev = ifelse(cluster == 1, 'inicio_vg', cat_grad_prev))


head(base_modelo, 10)


# ------------------------------------------------------------------------------
# Extensões totais dos trechos
# ------------------------------------------------------------------------------

# Arquivos com distâncias totais por trecho
ano_mes <- c('201811', '201812', '201901')
arqs_dist_tot <- c()
for (a_m in ano_mes) {
  search_folder <- sprintf('%s/%s', pasta_map_matching, a_m)
  result_files  <- list.files(search_folder, 
                              pattern = '^\\d{6}_map_matching_rotas_completas.csv', 
                              recursive = FALSE, 
                              full.names = TRUE)
  
  arqs_dist_tot <- c(arqs_dist_tot, result_files)
}

# Abrir arquivos, já selecionando somente colunas de interesse
ext_totais <- 
  lapply(X = arqs_dist_tot, FUN = read_delim, delim = ';', col_types = 'cciiiddddTT') %>% 
  rbindlist(fill = TRUE) %>% 
  select(trip_id, cod_proc, n_points, prop_centr_100, dist_total, veloc_vg_kph) %>% 
  # Recriar coluna trip_id, considerando os dois primeiros caractares de 
  # cod_proc. A coluna vg_id vai se referir ao id original da viagem
  mutate(vg_id = trip_id,
         cod_proc   = substr(cod_proc, 1, 2),
         trip_id  = str_c(vg_id, '_', cod_proc, sep = ''),
         .before = 'trip_id')

# Registrar distâncias percorridas por ponto GPS, para filtragem posterior
# referente à degradação do sinal
ext_totais <- ext_totais %>% mutate(dist_por_ponto = dist_total / n_points)

# Descartar colunas que não serão utilizadas
ext_totais <- ext_totais %>% select(-c(cod_proc, n_points)) %>% distinct()

head(ext_totais)


# Juntar extensões totais à base principal - teremos, temporariamente, uma coluna
# chamada 'vg_id' que se refere ao id original da viagem
base_modelo <- base_modelo %>% left_join(subset(ext_totais, select = -c(dist_por_ponto)), by = 'trip_id')

head(base_modelo)

# Limpar ambiente
rm(ano_mes, a_m, arqs_dist_tot, search_folder, result_files)



# -----------------------------------------------------------------------------
# Filtros  por extensão e velocidade das viagens totais e prop. centroide
# -----------------------------------------------------------------------------

# Remover viagens em que a proporção de pontos dentro de um centróide de 100m
# é maior ou igual a 50% para a viagem como um todo
base_modelo <- base_modelo %>% filter(prop_centr_100 < 50)
base_modelo <- base_modelo %>% select(-prop_centr_100)

# Filtro por velocidades mínimas e máximas
base_modelo <- base_modelo %>% filter(veloc_vg_kph > 4 & veloc_vg_kph <= 30)
# base_modelo <- base_modelo %>% filter(veloc_vg_kph <= 30)


# ------------------------------------------------------------------------------
# Juntar atributos de viário que ainda não estão associados à base
# ------------------------------------------------------------------------------

# Marcar vias que estão dentro de parques, para análise de contramão (abaixo)
vias_rest_parques <- atrib_viario %>% filter(class == 'parques') %>% select(osm_id)

# Deixar somente colunas de interesse para esta etapa
atrib_viario <- atrib_viario %>% select(qgis_id, curv_h, lotes_tot, lotes_15m, lotes_30m, 
                                        dens_lotes_100m, dens_lotes_100m_15m, dens_lotes_100m_30m,
                                        class_via, infra_ciclo, semaforos, via_restr,
                                        osm_highway, osm_oneway, osm_lanes, osm_surface, osm_maxspeed)

# Associar os atributos de viário restantes à base
base_modelo <- base_modelo %>% left_join(atrib_viario, by = 'qgis_id')

head(base_modelo)



# ------------------------------------------------------------------------------
# Vale a pena manter as colunas de tags do OSM?
# ------------------------------------------------------------------------------

# # Quantos NAs temos na base e onde?
# colSums(is.na(base_modelo))
# 
# # Quantos trechos de via (qgis_id) estão sem info de sentido? # 0
# base_modelo %>% filter(is.na(osm_oneway)) %>% select(qgis_id) %>% distinct()
# 
# # Quantos qgis_id temos? # 42711
# base_modelo %>% select(qgis_id) %>% distinct() %>% nrow()
# 
# # Quantos trechos de via (qgis_id) estão sem info de quantidade de faixas? 20897 / 42711 * 100 = 48,9%
# base_modelo %>% filter(is.na(osm_lanes)) %>% select(qgis_id) %>% distinct() %>% nrow()
# 
# # Quantos trechos de via (qgis_id) estão sem info de superfície? 11148 / 42711 * 100 = 26,1%
# base_modelo %>% filter(is.na(osm_surface)) %>% select(qgis_id) %>% distinct() %>% nrow()
# 
# # Quantos trechos de via (qgis_id) estão sem info de velocidade máxima? 28360 / 42711 * 100 = 66,4%
# base_modelo %>% filter(is.na(osm_maxspeed)) %>% select(qgis_id) %>% distinct() %>% nrow()

# Retirar colunas que não serão usadas no modelo
base_modelo <- base_modelo %>% select(-c(lotes_tot, lotes_15m, lotes_30m,
                                         osm_lanes, osm_surface, osm_maxspeed))


# ------------------------------------------------------------------------------
# Finais de semana e feriados
# ------------------------------------------------------------------------------

# Feriados 2018 e 2019
feriados <- c(as.Date('2018-10-12'), # Nossa Senhora Aparecida
              as.Date('2018-11-02'), # Finados (sexta)
              as.Date('2018-11-15'), # Procl. República (quinta)
              as.Date('2018-12-24'), # Natal (terça)
              as.Date('2019-01-01'), # Ano novo (terça)
              as.Date('2019-02-13'), # Carnaval (terça)
              as.Date('2019-03-30'), # Sexta feira santa (sábado)
              as.Date('2019-04-21')  # Tiradentes (domingo)
)


# Agregar dados de data (conversão do timestamp), hora e dia da semana
base_modelo <- 
  base_modelo %>% 
  # Converter a coluna de timestamps para dia-mes-ano-hora
  add_column(dh_inicio = as.POSIXlt(.$ts_inicio, origin = '1970-01-01'), 
             .after = 'ts_inicio') %>% 
  # Inserir colunas de dia da semana e hora
  mutate(dia_semana = weekdays(dh_inicio, abbreviate = TRUE),
         fx_hora    = format(dh_inicio, "%H"),
         .after = 'dh_inicio') %>% 
  # Converter coluna de hora para numeric
  mutate(fx_hora = as.numeric(fx_hora))


# Atualizar coluna de dia da semana para marcar feriados
base_modelo <- 
  base_modelo %>% 
  mutate(dia_semana  = ifelse(as.Date(dh_inicio) %in% feriados, 'fer', dia_semana))

# Descartar coluna de dh_inicio
base_modelo <- base_modelo %>% select(-c(dh_inicio, ts_inicio))


# Feriados vão ser considerados dias de descanso, junto com finais de semana
dias_descanso <- c('sáb', 'dom', 'fer')

# Classificar dias úteis/descanso e faixas de horário
base_modelo <- 
  base_modelo %>%
  mutate(
    # Atualizar coluna de dia_semana para marcar feriados
    dia_util    = ifelse(dia_semana %nin% dias_descanso, 'util', 'desc'),
    # Categorizar picos da manhã, tarde e vale
    cat_fx_hora = case_when(between(fx_hora,  0,  4) ~ 'madrugada',
                            between(fx_hora,  5, 10) ~ 'pico_manha',
                            between(fx_hora, 16, 21) ~ 'pico_tarde',
                            between(fx_hora, 22, 24) ~ 'madrugada',
                            TRUE ~ 'vale')
  )


# base_modelo %>% select(dia_semana, dia_util) %>% distinct()
# base_modelo %>% select(cat_fx_hora, fx_hora) %>% arrange(fx_hora) %>% distinct() %>% head(20)

head(base_modelo)

# Limpar ambiente
rm(feriados, dias_descanso)



# ------------------------------------------------------------------------------
# Preparações para modelos - Criação de categorias (ajuda e lidar com outliers)
# ------------------------------------------------------------------------------

# Primeira mudança: onde é ciclovia expressa, vamos mudar class_via para 
# 'ciclo_expressa' e infra_ciclo para 'ciclovia', de forma que o controle
# dessas vias se dê pela classificação viária
base_modelo <- 
  base_modelo %>% 
  mutate(class_via   = ifelse(infra_ciclo == 'expressa', 'ciclo_expressa', class_via),
         infra_ciclo = ifelse(infra_ciclo == 'expressa', 'ciclovia', infra_ciclo))

# Atualizar categoria em class_via: vias de pedestres vai ser tornar ped_serv
base_modelo <- 
  base_modelo %>% 
  mutate(class_via = ifelse(class_via == 'vias de pedestres', 'ped_serv', class_via))


# Alguns poucos deslocamentos estão sendo classificados como em vtr e com 
# ciclovia - uma conferência desses qgis_id ('016281', '023092', '290933' e 
# '193075') mostram que são ids do Minhocão. Reclassificá-los como sem_infra_ciclo
# base_modelo %>% filter(class_via == 'vtr' & infra_ciclo == 'ciclovia') %>% select(qgis_id) %>% distinct()
base_modelo <- 
  base_modelo %>% 
  mutate(infra_ciclo = ifelse(class_via == 'vtr' & infra_ciclo == 'ciclovia', 'sem_infra_ciclo', infra_ciclo))

# base_modelo %>% select(class_via, infra_ciclo) %>% distinct()


# Marcar vias únicas onde o deslocamento foi na contramão
base_modelo <- 
  base_modelo %>%
  # Vias de mão única com deslocamento no sentido oposto da linha são inicialmente
  # marcadas como contramão
  mutate(contramao = ifelse(osm_oneway == 'yes' & linha_sent == 'opo_linha', 'sim', 'não')) %>% 
  # Contramão: vias ped_serv não podem ser consideradas contramão
  mutate(contramao = ifelse(class_via == 'ped_serv', 'não', contramao)) %>% 
  # Contramão: vias com ciclovia não podem ser consideradas contramão -assume-se
  # que a infraestrutura é bidirecional e que o deslocamento aconteceu nela
  mutate(contramao = ifelse(infra_ciclo == 'ciclovia', 'não', contramao)) %>% 
  # Contramão: vias com ciclofaixa até podem ser unidirecionais, mas a depender
  # de onde está a estrutura no viário, ela pode estar no sentido oposto ao 
  # desenho da linha. Para simplificar, vamos considerar que não pode haver 
  # contramão em vias onde há ciclofaixa
  mutate(contramao = ifelse(infra_ciclo == 'ciclofaixa', 'não', contramao)) %>% 
  # Contramão: vias dentro de parques não devem ser consideradas contramão
  mutate(contramao = ifelse(edges.way_id %in% vias_rest_parques$osm_id, 'não', contramao))

# base_modelo %>% select(linha_sent, osm_oneway, contramao) %>% distinct()


# Categorizar curvaturas do trecho - modelos com números parece estar ficando
# com sinal invertido pois a enorme maioria é com curvatura baixa
# base_modelo %>% select(curv_h) %>% summary()
base_modelo <- 
  base_modelo %>% 
  mutate(cat_curv = case_when(between(curv_h, 0.000, 0.063) ~ '00 a 25 graus', # vertiginosa
                              between(curv_h, 0.064, 0.125) ~ '25 a 45 graus',
                              between(curv_h, 0.126, 0.250) ~ '45 a 90 graus',
                              between(curv_h, 0.251, 1.000) ~ 'acima de 90 graus'))

# Categorizar por extensões de trechos de quadra - extensões que são zero ou NA 
# serão removidas mais adiante
# base_modelo %>% select(edges.length) %>% summary()
base_modelo <- 
  base_modelo %>% 
  mutate(cat_dist_trecho = case_when(between(edges.length,    0.01,    60) ~ 'curto',
                                     between(edges.length,   60.01,   120) ~ 'medio',
                                     between(edges.length,  120.01,   250) ~ 'acima_media',
                                     between(edges.length,  250.01,   500) ~ 'grande',
                                     between(edges.length,  500.01, 10000) ~ 'muito_grande'))

# Categorizar por extensões totais da viagem
# base_modelo %>% select(dist_total) %>% summary()
# base_modelo %>% group_by(cat_dist_trecho) %>% tally()
base_modelo <- 
  base_modelo %>% 
  mutate(cat_dist_total = case_when(between(dist_total,    0.001,  1300) ~ 'curta',
                                    between(dist_total, 1300.001,  3800) ~ 'media',
                                    between(dist_total, 3800.001,  5100) ~ 'acima_media',
                                    between(dist_total, 5100.001,  7700) ~ 'longa',
                                    between(dist_total, 7700.001, 40000) ~ 'muito_longa'))


# Limpar ambiente
# rm(vias_rest_parques)


# ------------------------------------------------------------------------------
# Filtros de remoção de linhas
# ------------------------------------------------------------------------------

# # Retirar alguns qgis_id que estão dando problema - valores precisam ser re-checados
# # caso o mapa base OSM do processamento tenha sido alterado
# qgis_id_problematicos <- c(
#   '035959' # Pista da Raia da USP
# )
# 
# trip_id_problematicos <- c(
#   # '013918_12' # Túnel da 9 de Julho
# )
# 
# # Executar filtro: retirar ids problemáticos e viagens problemáticas (se houver)
# base_modelo <- 
#   base_modelo %>% 
#   filter(qgis_id %nin% qgis_id_problematicos & trip_id %nin% trip_id_problematicos)


# Analisar média de distância de pontos para estabelecer uma linha de corte
ext_totais %>% select(dist_por_ponto) %>% summary()
# dist_por_ponto    
# Min.   :   2.434  
# 1st Qu.:  14.626  
# Median :  19.131  
# Mean   :  21.707  
# 3rd Qu.:  26.491  
# Max.   :1523.008  

# Analisar quantis de distância de pontos para estabelecer uma linha de corte
ext_totais %>% select(dist_por_ponto) %>% quantile(probs = seq(0.9, 1, 0.01), na.rm = TRUE)
# 90%        91%        92%        93%        94%        95%        96%        97%        98%        99%       100% 
# 33.77191   34.51802   35.32710   36.27057   37.32133   38.59043   40.28333   42.62087   46.49437   56.49106 1523.00777 

# nuvens_pontos <- ext_totais %>% filter(dist_por_ponto > 50) %>% select(trip_id)
# base_modelo %>% 
#   filter(trip_id %in% nuvens_pontos$trip_id) %>% 
#   select(trip_id, dist_total, veloc_vg_kph) %>% 
#   distinct() %>% 
#   sample_n(10)
# 
# rm(nuvens_pontos)

# Retirar viagens (todos os trechos, por vg_id) em que o sinal GPS parece
# degradado, apresentando nuvens de pontos. Para tanto, vamos estabelecer um
# limite máximo de distância percorrida por ponto em 50 metros e cortar tudo o 
# que estiver acima dele. 50 metros está em um percentil entre 98% e 99% e
# equivale a 3x a frequência esperada do sinal para um ciclista que se desloca
# a 12km/h: 3.33 m/s, ou 12 km/h = 1 sinal GPS a cada 16.65m
nuvens_pontos <- ext_totais %>% filter(dist_por_ponto >= 50) %>% select(vg_id) %>% distinct()

# Executar remoção dessas viagens
base_modelo <- base_modelo %>% filter(vg_id %nin% nuvens_pontos$vg_id)

# Limpar ambiente
rm(nuvens_pontos)


# Avaliar quantas viagens sobraram
base_modelo %>% select(trip_id) %>% mutate(trip_id = str_sub(trip_id, 1, 6)) %>% distinct() %>% nrow()

# Avaliar quantos trechos de viagens sobraram
base_modelo %>% select(trip_id) %>% distinct() %>% nrow()

# ------------------------------------------------------------------------------
# Viagens que possuem trechos de loop
# ------------------------------------------------------------------------------

# Descobrir quais viagens passam pelo mesmo qgis_id repetidas vezes - tipicamente,
# são viagens em que a pessoa está dando voltas dentro de um parque/área restrita

# Quais viagens registram que a pessoa passou pelo mesmo trecho (qgis_id) por
# várias vezes?
viagens_loop <- 
  base_modelo %>% 
  # Simplificar dataframe: queremos somente uma linha registrando cada
  # passagem por trecho
  select(trip_id, qgis_id, cluster, elev_sent) %>% 
  distinct() %>% 
  # Agrupar e registrar quantas passagens houve por trecho e sentido
  group_by(trip_id, qgis_id, elev_sent) %>% 
  tally()

# Isolar as viagens em que há trechos com mais de duas passagens
viagens_loop <- viagens_loop %>% ungroup() %>% filter(n > 2)
viagens_loop <- viagens_loop %>% select(trip_id) %>% distinct()

# Recriar id original da viagem, antes de separá-la por trechos
viagens_loop <- viagens_loop %>% mutate(vg_id = str_sub(trip_id, 1, 6))

# viagens_loop %>% ungroup() %>% select(trip_id) %>% distinct() %>% sample_n(5) %>% arrange(trip_id)
# viagens_loop %>% filter(trip_id == '123703_01')
# base_modelo %>% filter(trip_id == '023812_02') %>% select(trip_id, qgis_id, cluster, class_via, infra_ciclo, via_restr, elev_sent) %>% distinct()

# Executar filtro: marcar todas as viagens que possuem trechos de loop - notar
# que estamos marcando todos os seus trechos das viagens, não somente os
# trechos que apresentam loops. O id a ser usado neste reconhecimento é o vg_id
# base_modelo <- base_modelo %>% filter(vg_id %nin% viagens_loop$vg_id)
base_modelo <- base_modelo %>% mutate(vg_loop = ifelse(vg_id %in% viagens_loop$vg_id, 'sim', 'não'))


head(base_modelo)


# Limpar ambiente
rm(viagens_loop)


# ------------------------------------------------------------------------------
# Viagens que passam por vias agressivas, com corredores de ônibus ou VTR
# ------------------------------------------------------------------------------

# Isolar apenas osm_id de vias expressas e com corredores
vias_expressas  <- vias_expressas %>% select(osm_id)
vias_corredores <- vias_corredores %>% select(osm_id)

# Juntar esses osm_ids em um dataframe único
vias_agressivas <- vias_expressas %>% rbind(vias_corredores) %>% distinct()


# Adicionar VTRs às vias_agressivas
vtrs <- base_modelo %>% filter(class_via == 'vtr') %>% select(osm_id = edges.way_id) %>% distinct()
vias_agressivas <- vias_agressivas %>% rbind(vtrs) %>% distinct()


# Quais viagens possuem trechos que passam por vias expressas, VTR ou com
# corredores de ônibus e qual a extensão desses trechos em vias agressivas?
viagens_perfil_experiente <- 
  base_modelo %>% 
  # Puxar somente passagens por vias_agressivas
  filter(edges.way_id %in% vias_agressivas$osm_id) %>% 
  # Simplificar dataframe: queremos somente uma linha registrando cada
  # passagem por trecho e sentido - aqui, é importante saber se foi na contramao
  select(trip_id, edges.way_id, qgis_id, cluster, infra_ciclo, contramao, edges.length) %>% 
  distinct() %>% 
  # Agrupar e registrar a extensão total percorrida por trecho (osm_id) e sentido
  group_by(trip_id, edges.way_id, infra_ciclo, contramao) %>% 
  summarise(dist_aggres = sum(edges.length, na.rm = TRUE)) %>% 
  ungroup()


# Remover trechos que possuem infraestrutura cicloviária
viagens_perfil_experiente <- viagens_perfil_experiente %>% filter(infra_ciclo == 'sem_infra_ciclo')

# Remover percursos na contramão
viagens_perfil_experiente <- viagens_perfil_experiente %>% filter(contramao == 'não')


viagens_perfil_experiente %>% select(dist_aggres) %>% summary()
# dist_aggres    
# Min.   :   0.0  
# 1st Qu.:  39.0  
# Median :  82.0  
# Mean   : 110.3  
# 3rd Qu.: 125.0  
# Max.   :4714.0 

# Analisar quantis de distância de pontos para estabelecer uma linha de corte
viagens_perfil_experiente %>% select(dist_aggres) %>% quantile(probs = seq(0.9, 1, 0.01), na.rm = TRUE)
# 90%     91%     92%     93%     94%     95%     96%     97%     98%     99%    100% 
# 190.00  210.00  224.00  247.00  261.00  283.00  311.00  354.44  435.96  740.00 4714.00 


# Para marcar a viagem com perfil agressivo, vamos usar um valor acima de 350 m,
# que seria cerca do percentil 97%
viagens_perfil_experiente <- viagens_perfil_experiente %>% filter(dist_aggres > 350)


# Recriar id original da viagem, antes de separá-la por trechos
viagens_perfil_experiente <- viagens_perfil_experiente %>% mutate(vg_id = str_sub(trip_id, 1, 6))

# Executar filtro: marcar todas as viagens que possuem trechos expressivos que 
# passam por vias expressas, VTRs ou com corredores de ônibus. Notar que a
# marcação é para a viagem como um todo, não somente os trechos que estão 
# passando por estes tipo de via ou a eventual segmentação da viagem (trip_ids 
# com diferentes valores, ex. _00, _01, _02 etc)
base_modelo <- base_modelo %>% mutate(vg_exper = ifelse(vg_id %in% viagens_perfil_experiente$vg_id, 'sim', 'não'))


# Testar resultados
base_modelo %>% filter(vg_exper == 'sim') %>% select(trip_id) %>% distinct() %>% sample_n(5)


# Limpar ambiente
rm(vias_corredores, vias_expressas, vias_agressivas, vtrs, viagens_perfil_experiente)


# ------------------------------------------------------------------------------
# Viagens possivelmente feitas em automóvel
# ------------------------------------------------------------------------------

# Temos trechos com velocidades médias altíssimas
vgs_vel_altissima <- base_modelo %>% filter(vel_med_m5 >= 80) %>% select(vg_id, dist_total) %>% distinct()
base_modelo <- base_modelo %>% filter(vg_id %nin% vgs_vel_altissima$vg_id)

# Viagens com elevação alta e velocidades médias altas
vgs_vel_altissima2 <- base_modelo %>% filter(elev_grad_rev > 6 & vel_med_m5 >= 50) %>% select(vg_id, dist_total, veloc_vg_kph) %>% distinct()
base_modelo <- base_modelo %>% filter(vg_id %nin% vgs_vel_altissima2$vg_id)

# Viagens acima de 10 km e velocidade média "alta", acima de 15 km/h
vgs_vel_altissima3 <- base_modelo %>% filter(dist_total > 10000 & veloc_vg_kph > 15) %>% select(vg_id, dist_total, veloc_vg_kph) %>% distinct()
base_modelo <- base_modelo %>% filter(vg_id %nin% vgs_vel_altissima3$vg_id)

# base_modelo %>% filter(vel_med_m5 >= 80) %>% group_by(trip_id, dist_total) %>% summarise(vel_max = max(vel_med_m5))

# base_modelo %>%
#   filter(dist_total > 3000 & veloc_vg_kph > 20 & vg_contramao == 'não' & vg_parques == 'não') %>% 
#   select(trip_id) %>% 
#   distinct() %>% 
#   sample_n(5)


# Avaliar quantas viagens sobraram
base_modelo %>% select(trip_id) %>% mutate(trip_id = str_sub(trip_id, 1, 6)) %>% distinct() %>% nrow()

# Avaliar quantos trechos de viagens sobraram
base_modelo %>% select(trip_id) %>% distinct() %>% nrow()


# ------------------------------------------------------------------------------
# Viagens com trechos de contramão ou de parques
# ------------------------------------------------------------------------------

# Marcar rotas que passam por trechos (qgis_id) de contramão com extensão igual
# ou maior do que 30m - entrar na contramão é forte indicativo de que viagem foi
# feita em bicicleta
viagens_contramao <- 
  base_modelo %>% 
  filter(contramao == 'sim' & edges.length >= 30) %>% 
  select(trip_id) %>% 
  distinct()

base_modelo <- base_modelo %>% mutate(vg_contramao = ifelse(trip_id %in% viagens_contramao$trip_id, 'sim', 'não'))


# Marcar rotas que passam por áreas restritas em parques (não usp) - entrar em 
# área restrita é forte indicativo de que viagem foi feita em bicicleta
viagens_parques <- 
  base_modelo %>% 
  filter(edges.way_id %in% vias_rest_parques$osm_id) %>% 
  select(trip_id) %>% 
  distinct()

base_modelo <- base_modelo %>% mutate(vg_parques = ifelse(trip_id %in% viagens_parques$trip_id, 'sim', 'não'))

head(base_modelo)

# Limpar ambiente
rm(vgs_vel_altissima, vgs_vel_altissima2, vgs_vel_altissima3, 
   viagens_contramao, viagens_parques)




# 
# # Agrupar descidas médias, fortes e vertiginosas para calcular percentual da
# # extensão percorrida em grandes declives
# perfil_altimetria <- 
#   base_modelo %>% 
#   select(trip_id, cluster, cat_grad, edges.length) %>% 
#   distinct() %>% 
#   # Agrupar trechos de descidas/subidas médias, fortes e vertiginosas, bem como
#   # trechos de plano e não identificados para chegar a um percentual de cada
#   mutate(cat_grad_desc = case_when(str_starts(cat_grad, 'desc_[mfv]') ~ 'alt_desc_mfv',
#                                    str_starts(cat_grad, 'subi_[mfv]') ~ 'alt_subi_mfv',
#                                    is.na(cat_grad) ~ 'alt_indef',
#                                    TRUE ~ 'alt_plano')) %>% 
#   group_by(trip_id, cat_grad_desc) %>% 
#   # Somar extensão percorrida nos trechos e calcular percentual sobre a extensão total
#   summarise(dist = sum(edges.length, na.rm = TRUE)) %>% 
#   mutate(perc = round(dist / sum(dist, na.rm = TRUE) * 100, 1))
# 
# # Isso aqui leva uma eternidade e não termina nunca
# # perfil_altimetria <- 
# #   perfil_altimetria %>% 
# #   # Puxar linhas para virarem colunas com resultados
# #   pivot_wider(id_cols     = c(trip_id),
# #               names_from  = 'cat_grad_desc',
# #               values_from = 'perc') %>% 
# #   mutate(across(where(is.double), ~replace_na(.x, 0)))
# 
# head(perfil_altimetria)
# 
# 
# perfis_velocidades <- 
#   base_modelo %>% 
#   select(trip_id, dist_total, veloc_vg_kph, vel_med_m5) %>% 
#   group_by(trip_id, dist_total, veloc_vg_kph) %>% 
#   summarise(v20p = quantile(vel_med_m5, probs = 0.2, na.rm = TRUE),
#             v50p = quantile(vel_med_m5, probs = 0.5, na.rm = TRUE),
#             v60p = quantile(vel_med_m5, probs = 0.6, na.rm = TRUE),
#             v70p = quantile(vel_med_m5, probs = 0.7, na.rm = TRUE),
#             v80p = quantile(vel_med_m5, probs = 0.8, na.rm = TRUE))
# 
# head(perfis_velocidades)
# 
# 
# perfil_altimetria %>% filter(trip_id == '301368_01')
# 
# altas_veloc <- perfis_velocidades %>% ungroup() %>% filter(v20p > 10 & v80p > 30 & dist_total > 2000) %>% select(trip_id)
# altas_desc <- perfil_altimetria %>% filter(cat_grad_desc == 'alt_desc_mfv') %>% select(trip_id, alt_desc_mfv = perc)
# 
# altas_veloc %>% left_join(altas_desc, by = 'trip_id') %>% sample_n(20)
# perfis_velocidades %>% filter(trip_id == '301368_01')
# 
# perfis_velocidades %>% 
#   filter(trip_id %in% c('373504_02', '035270_02', '009651_00', '408078_05',
#                         '264789_00', '013918_12', '247325_00', '000135_00'))
# 
# perfil_altimetria %>% 
#   filter(trip_id %in% c('373504_02', '035270_02', '009651_00', '408078_05',
#                         '264789_00', '013918_12', '247325_00', '000135_00')) %>% 
#     pivot_wider(id_cols     = c(trip_id),
#                 names_from  = 'cat_grad_desc',
#                 values_from = 'perc') %>%
#     mutate(across(where(is.double), ~replace_na(.x, 0)))
# 
# # Detectar possíveis viagens de automóvel
# 
# If the 80%-percentile of trip speed ≤ 8 km/h
# 1. if v80trip ≤ 8 km/h, then M = 1, else 2.
# 
# 2. if B ∩ (C ∪ D) ∩ F = true, then M = 2, else 3.
# 
# If the 20% percentile of trip speed ≥ 10 km/h and...
# (the trip distance > 30 km OR the detour factor of the trip > 3) and...
# the 80%-percentile of trip speed < 50 km/h
# if v20trip ≥ 10 km/h & (Dtrip > 30 km | DFtrip > 3) & v80trip < 50 km/h then M = 2, else 3.
# 
# 3. if E = true, then M = 3, else M = 4
# if v90trip ≤ 40
# 
# A = v80trip ≤ αM1
# B = v20trip ≥ αM2:1
# C = Dtrip   > δM2
# D = DFtrip  > γM2
# E = v90trip ≤ αM3
# F = v80trip < αM2:2
# 
# αM1    8.0 km/h
# αM2.1 10.0 km/h
# αM2.2 50.0 km/h
# δM2   30.0 Km
# γM2    3.0 –
# αM3   40.0 km/h
# 
# 
# base_modelo %>% 
#   filter(vg_id == '247325') %>% 
#   select(trip_id) %>% 
#   distinct()
# 
# base_modelo %>% 
#   filter(trip_id == '423613_00') %>%
#   select(vel_med_m5) %>% 
#   quantile(probs = c(0.2, 0.6, 0.7, 0.8), na.rm = TRUE)
# 
# base_modelo %>% 
#   filter(trip_id == '247325_00') %>%
#   group_by(trip_id) %>% 
#   summarise(v20p = quantile(vel_med_m5, probs = 0.2, na.rm = TRUE))
# 
# 
# 
# perfis_velocidades %>% 
#   filter(v80p > 30)
# 
# 
# # Descida da Brigadeiro Luis Antonio - esta viagem é rápida pois é descida
# base_modelo %>% 
#   filter(trip_id == '003691_00') %>% 
#   select(cluster, cat_grad_prev, edges.length) %>% 
#   distinct() %>% 
#   group_by(cat_grad_prev) %>% 
#   summarise(dist = sum(edges.length))
#   
# 
# # base_modelo %>% 
# #   filter(trip_id == '408078_05' & contramao == 'sim') %>% 
# #   select(edges.way_id) %>% 
# #   distinct()
# 
# base_modelo %>% 
#   # filter(vel_med_m5 > 40 & elev_grad_rev < -4) %>% 
#   select(trip_id, dist_total, veloc_vg_kph) %>% 
#   filter(trip_id %in% c('373504_02', '035270_02', '009651_00', '408078_05',
#                         '264789_00', '013918_12', '247325_00', '000135_00')) %>% 
#   distinct() %>% 
#   arrange(trip_id)
#   sample_n(6)
# # trip_id elev_grad_rev dist_total veloc_vg_kph
# # 1: 373504_02     -6.207443            8951.266     19.63011 * provavelmente carro
# # 2: 035270_02     -6.925618           13159.708     18.11535 * provavelmente carro
# # 3: 009651_00     -8.589481            1261.899     20.36845 * factível ser de bike (Av. Paulista)
# # 4: 408078_05     -6.069691            3439.370     11.57141 * de bike, pega Av Pinheiros na contramão
# # 5: 264789_00     -5.091912            4038.210     29.00168 * nuvem de pontos
# # 6: 013918_12     -6.952808            10552.74     18.57863 * provavelmente carro
# # 7: 247325_00   -0.47878289            6587.528     9.368474 * bike pela faria lima
# # 8: 000135_00 - a pé?
# 
# 
# teste <- 
#   base_modelo %>% 
#   filter(dist_total > 1000 & veloc_vg_kph > 15) %>% 
#   select(vg_id) %>% 
#   distinct()
# 
# teste <- base_modelo %>% filter(vg_id %nin% teste$vg_id)
# 
# 
# 
# 
# base_modelo %>% 
#   filter(trip_id == '264789_00') %>% 
#   select(qgis_id) %>% 
#   distinct()










 
# trip_ids_possivel_carro <- 
#   ext_totais %>% 
#   filter(veloc_vg_kph >= 30) %>% 
#   select(vg_id) %>% 
#   distinct()
# ext_totais %>% filter(trip_id == '082000')
# 
# base_modelo %>%
#   mutate(trip_id_orig = str_sub(trip_id, 1, 6), .after = 'trip_id') %>%
#   filter(trip_id_orig %in% trip_ids_possivel_carro$trip_id_orig) %>%
#   select(trip_id_orig, trip_id) %>%
#   distinct()
# 
# base_modelo <- 
#   base_modelo %>%
#   mutate(trip_id_orig = str_sub(trip_id, 1, 6), .after = 'trip_id') %>% 
#   filter(trip_id_orig %nin% trip_ids_possivel_carro$trip_id_orig)
# 
# prop_center_teste <- 
#   ext_totais %>% 
#   filter(prop_centr_100 > 30) %>% 
#   select(trip_id_orig = trip_id) %>% 
#   distinct()
# 
# base_modelo <- 
#   base_modelo %>%
#   filter(trip_id_orig %nin% prop_center_teste$trip_id_orig)
# 
# 


# ------------------------------------------------------------------------------
# Filtros finais - Trechos (qgis_id) com um mínimo de viagens e de pontos GPS
# ------------------------------------------------------------------------------

# Retirar linhas que estão com NA na distância percorrida no trecho ou em que
# o trecho está com extensão zero
base_modelo <- base_modelo %>% filter(!is.na(edges.length)) %>% filter(edges.length != 0)

# Retirar da base os trechos em que o sentido do deslocamento não foi identificado
base_modelo <- base_modelo %>% filter(!is.na(elev_sent))

# Olhando o mapa, vias com gradiente absoluto maior do que 24.5 são escadas ou
# problemas com a elevação de viadutos (ex. vale da Av. Sumaré com o viaduto que
# passa pela Av. Dr. Arnaldo) - retirar esses segmentos
base_modelo <- base_modelo %>% filter(between(elev_grad_rev, -24.5, 24.5))

# Retirar da base trechosobservações em que a velocidade média calculada ficou em zero
base_modelo <- base_modelo %>% filter(vel_med_m5 != 0)


# Avaliar quantas viagens sobraram
base_modelo %>% select(trip_id) %>% mutate(trip_id = str_sub(trip_id, 1, 6)) %>% distinct() %>% nrow()

# Avaliar quantos trechos de viagens sobraram
base_modelo %>% select(trip_id) %>% distinct() %>% nrow()




# Algumas viagens ainda estão com velocidade média muito alta - se for aplicar
# a remoção de outliers extremos de Favero, retiraríamos as observações com
# velocidade média acima de 30 km/h...
base_modelo %>% select(vel_med_m5) %>% quantile(probs = seq(0.999, 1, 0.00001), na.rm = TRUE)

# # Retirar outliers extremos vel_med_m5
# # Isolar coluna de interesse - velocidade
# x <- base_modelo$vel_med_m5
# # Pegar o primeiro e quarto quantis 
# qnt <- quantile(x, probs = c(0.25, 0.75))
# # Outliers extremos estão a 3 * IQR
# H <- 3 * IQR(x) 
# # Retirar outliers do dataframe
# base_modelo <- base_modelo %>% filter(!(vel_med_m5 < (qnt[1] - H) | vel_med_m5 > (qnt[2] + H)))


# ... Vamos ser mais conservadores e retirar somente as acima de 45 km/h - dessa
# forma, a variável de descida vertiginosa não perde a significância estatística
# base_modelo <- base_modelo %>% filter(vel_med_m5 <= 45)



# bkp <- base_modelo

# ***** Atenção para ordem do filtro: aqui, pega todos os qgis_id com mais de
# três pontos para depois filtrar os que têm 50 viagens ou mais. Ex. o qgis_id
# 000889 tem 55 viagens, mas só 1 com mais de 2 pontos (com 14 pontos) - ele 
# passa se o filtro for aplicado depois do corte de >= 50 viagens
# Retirar arcos do viário (qgis_id) com quantidade de pontos menor do que 3
base_modelo <- base_modelo %>% filter(n_pontos > 2)

# base_modelo %>% filter(trip_id == '427026_00' & n_pontos > 2)

# Cada trecho aparece em quantas viagens diferentes?
viagens_por_trecho <- 
  base_modelo %>% 
  # Agrupar por trecho (qgis_id) e sentido (subida, descida)
  group_by(qgis_id) %>%
  # Somar ids únicos das viagens
  summarise(n_viagens = n_distinct(trip_id))

# Quantos trechos possuem pelo menos x viagens por sentido da via?
# viagens_por_trecho <- base_modelo %>% group_by(qgis_id, elev_sent) %>% tally()
# trechos_validos <- viagens_por_trecho %>% filter(n_viagens >= 20) *****
trechos_validos <- viagens_por_trecho %>% filter(n_viagens >= 50)
head(trechos_validos)

# Retirar da base do modelo os trechos com menos de x viagens por qgis_id
base_modelo <- base_modelo %>% filter(qgis_id %in% trechos_validos$qgis_id)


# Ainda há viagens que ficaram com menos de 1 passagem por sentido? - Sim, uma
# delas ficou com somente uma viagem no sentido oposto da linha (via bidirecional)
passagem_unica_no_sentido <- 
  base_modelo %>% 
  select(trip_id, qgis_id, linha_sent, osm_oneway) %>% 
  distinct() %>% 
  group_by(qgis_id, osm_oneway, linha_sent) %>% 
  tally() %>% 
  filter(n < 2)

# Remover trip_ids com passagem única
base_modelo <- base_modelo %>% filter(qgis_id != '024796' | (qgis_id == '024796' & linha_sent != 'opo_linha'))


# Avaliar quantas viagens sobraram
base_modelo %>% select(trip_id) %>% mutate(trip_id = str_sub(trip_id, 1, 6)) %>% distinct() %>% nrow()

# Avaliar quantos trechos de viagens sobraram
base_modelo %>% select(trip_id) %>% distinct() %>% nrow()




# ***** Atenção para ordem do filtro: aqui, pega somente dos qgis_id com 50 
# viagens ou mais, quais têm também mais de três pontos GPS
# # Retirar arcos do viário (qgis_id) com quantidade de pontos menor do que 3
# base_modelo <- base_modelo %>% filter(n_pontos > 2)


head(base_modelo)


# Checagem - Como ficou o resultado final?
viagens_por_trecho2 <- 
  base_modelo %>% 
  # Agrupar por trecho (qgis_id) e sentido (subida, descida)
  group_by(qgis_id) %>%
  # Somar ids únicos das viagens
  summarise(n_viagens = n_distinct(trip_id))

viagens_por_trecho2 %>% select(n_viagens) %>% summary()
# n_viagens     
# Min.   :   1.0  
# 1st Qu.:  25.0  
# Median :  67.5  
# Mean   : 173.4  
# 3rd Qu.: 157.0  
# Max.   :7505.0  

# n_viagens     
# Min.   :  50.0  
# 1st Qu.:  79.0  
# Median : 131.0  
# Mean   : 276.2  
# 3rd Qu.: 251.0  
# Max.   :7505.0 

# Limpar ambiente
rm(viagens_por_trecho, viagens_por_trecho2)


# ------------------------------------------------------------------------------
# Registros de quantidades de viagens e trechos
# ------------------------------------------------------------------------------

# TODO: revisar números após terminar filtros

# Quantos qgis_id temos? # 11.213 // 6,704
base_modelo %>% select(qgis_id) %>% distinct() %>% nrow()

# Quantas viagens com velocidades médias acima de 45 km/h?
base_modelo %>% filter(vel_med_m5 > 45) %>% nrow()

# Quantas viagens serão consideradas no modelo final? - 160,893 // 160,376
n_viagens <- base_modelo %>% select(vg_id) %>% distinct() %>% nrow()

# Quantos trechos dessas viagens serão consideradas no modelo final? - 178,426 // 177,796
n_trechos <- base_modelo %>% select(trip_id) %>% distinct() %>% nrow()

# Retirar coluna temporária de vg_id da base geral
base_modelo <- base_modelo %>% select(-vg_id)

# Limpar ambiente
rm(n_viagens, n_trechos)



# ------------------------------------------------------------------------------
# Extensões percorridas em infraestrutura cicloviária ou fora dela
# ------------------------------------------------------------------------------

base_modelo %>% 
  group_by(infra_ciclo) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

# infra_ciclo          dist  prop
# <chr>               <int> <dbl>
#   1 ciclofaixa        8197662   4.1
# 2 ciclovia         59979463  30.1
# 3 sem_infra_ciclo 131324269  65.8

base_modelo %>% 
  group_by(via_restr, infra_ciclo) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

# via_restr    infra_ciclo         dist  prop
# <chr>        <chr>              <int> <dbl>
#   1 via_comum    ciclofaixa       8197662   4.1
# 2 via_comum    ciclovia        59976641  30.1
# 3 via_comum    sem_infra_ciclo 90953644  45.6
# 4 via_restrita ciclovia            2822   0  
# 5 via_restrita sem_infra_ciclo 40370625  20.2

base_modelo %>% 
  group_by(infra_ciclo, class_via) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

# infra_ciclo     class_via          dist  prop
# <chr>           <chr>             <int> <dbl>
#   1 ciclofaixa      arterial         361441   0.2
# 2 ciclofaixa      coletora        6255771   3.1
# 3 ciclofaixa      local           1580450   0.8
# 4 ciclovia        arterial       51236132  25.7
# 5 ciclovia        ciclo_expressa  3677071   1.8
# 6 ciclovia        coletora        4887942   2.5
# 7 ciclovia        local            178318   0.1
# 8 sem_infra_ciclo arterial       35827183  18  
# 9 sem_infra_ciclo coletora       43748683  21.9
# 10 sem_infra_ciclo local          37736976  18.9
# 11 sem_infra_ciclo ped_serv       14011427   7 

# ------------------------------------------------------------------------------
# Deslocamentos em mão única que são na contramão
# ------------------------------------------------------------------------------

base_modelo %>% 
  group_by(osm_oneway, contramao) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

base_modelo %>% 
  group_by(infra_ciclo, osm_oneway, contramao) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

base_modelo %>% 
  group_by(class_via, osm_oneway, contramao) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

# Por frequência de variável categórica
base_modelo %>%
  group_by(dia_util, contramao) %>% 
  tally() %>%
  # Percentuais no grupo e no total
  mutate(prop_grupo = round(n / sum(n) * 100, 2),
         prop_total = round(n / sum(.$n) * 100, 2)) %>%
  as.data.frame()

# dia_util contramao       n prop_grupo prop_total
# 1     desc       não  508205      89.97      26.69
# 2     desc       sim   56684      10.03       2.98
# 3     util       não 1145680      85.56      60.18
# 4     util       sim  193319      14.44      10.15

# base_modelo %>% 
#   filter(osm_oneway == 'yes') %>% 
#   group_by(osm_oneway, contramao) %>% 
#   summarise(dist = sum(edges.length)) %>% 
#   mutate(prop = round(dist / sum(.$dist) * 100, 1))


# ------------------------------------------------------------------------------
# Deslocamentos em gradientes
# ------------------------------------------------------------------------------

base_modelo %>% 
  group_by(elev_sent, cat_grad) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

base_modelo %>% 
  group_by(cat_grad) %>% 
  summarise(dist = sum(edges.length)) %>% 
  mutate(prop = round(dist / sum(.$dist) * 100, 1))

base_modelo %>% 
  select(qgis_id, cat_grad) %>% 
  distinct() %>% 
  group_by(cat_grad) %>% 
  summarise(n_trechos = n()) %>% 
  mutate(prop = round(n_trechos / sum(.$n_trechos) * 100, 1))

# ------------------------------------------------------------------------------
# Exportar resultados
# ------------------------------------------------------------------------------

# Quantos NAs temos na base e onde?
colSums(is.na(base_modelo))

base_modelo %>% filter(cat_grad != 'plano') %>% select(qgis_id) %>% distinct() %>% nrow() # 1249
base_modelo %>% filter(cat_grad == 'plano') %>% select(qgis_id) %>% distinct() %>% nrow() # 5455

# head(base_modelo)

# Selecionar colunas de interesse
base_modelo_out <-
  base_modelo %>%
  select(trip_id, osm_id = edges.way_id, qgis_id, linha_sent,
         speed_kph, vel_med_gps, vel_med_m3, vel_med_m5,
         elev_grad_rev, elev_sent, cat_grad, # cat_grad_prev,
         dist_total, dist_trecho_quadra = edges.length, cat_dist_trecho, cat_dist_total,
         curv_h, cat_curv, inicio_fim,
         dens_lotes_100m, dens_lotes_100m_15m, dens_lotes_100m_30m,
         dia_util, cat_fx_hora, 
         class_via, infra_ciclo, via_restr, semaforos, osm_highway, osm_oneway, contramao,
         vg_loop, vg_exper, vg_contramao, vg_parques)

head(base_modelo_out)


# Exportar resultados
# out_file <- sprintf('%s/yellow_base_para_modelo.csv', pasta_base_modelo)
out_file <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
write_delim(base_modelo_out, out_file, delim = ';')

# Exportar resultados
out_file <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs_viagens_por_qgis_id.csv', pasta_base_modelo)
write_delim(trechos_validos, out_file, delim = ';')
