# Para 2028, temos um dilema. O algoritmo nos dará novas rotas a partir da nova
# infra cicloviária. No momento do lpSolve, vamos usar os mesmos pares origem-destino,
# mas essas novas rotas podem dar um desvio maior do que o roteado em 2024 para
# pegar mais trechos de infraestrutura cicloviária. Este desvio pode ultrapassar
# o limite de tempo de 900 segundos (ver 00f0a7-00f173 - imagem na pasta com
# título x_routing_2028_terceira_opcao_ignora_ciclofaixas_de_2024). 

# Temos duas possibilidades aqui:
# 1. Assumir o menor peso para as novas rotas, independentemente do tempo. Essa
# abordagem assume que sabemos o par OD que queremos e que para ter maior proteção
# de infra cicloviária, uma pequena ultrapassagem do limite de tempo dos 900 seg
# seria tolerável;

# 2. Assumir que as novas rotas, por ultrapassarem o limite de tempo de 900s são
# piores do que a rota utilizada em 2024. Esta abordagem é inflexível com relação
# ao limite de tempo, sugerindo que ele seja mais importante do que a busca por
# mais proteção

# É preciso comparar as rotas selecionadas para 2028 com as de 2024 para saber
# se são melhores mesmo. Mexer no limite de tempo implicaria em habilitar rotas
# às vezes com trajetos idênticos entre 2024-2028 e que haviam sido cortadas por
# tempo em 2024. Elas apareceriam em 2028, mesmo que o uso de infra cicloviária
# fosse o mesmo. Com isso, vamos manter o limite de tempo, mas:
# 
# 1. Manter as rotas de 2028, exceto se ela tem perda de uso de infra cicloviária
# maior do que 10 metros (para ser maior que poucos metros). Este valor é 
# arbitrário e equivale a 125 linhas (0.25%) da base;
# 2. Usar as rotas de 2024 para esses casos, pois eram rotas melhores, com mais
# uso de infra cicloviária. Se a perda não é tão grande, significa que está
# havendo uma economia de tempo razoável na nova rota, possibilitada pela nova
# rede cicloviária

# carregar bibliotecas
library('tidyverse')
library('tidylog')

# Definir ano e limites de tempo para ttmatrix final
ano_1 <- '2024'; ano_2 <- '2028'; min_thres <- 15;

# Estrutura de pastas
pasta_dados          <- "../../yellow_dados"
pasta_aop_2024_2028  <- sprintf("%s/14_aop_2024_2028", pasta_dados)
pasta_ttmatrix_24_28 <- sprintf("%s/04_ttmatrix_2024_2028", pasta_aop_2024_2028)


# ------------------------------------------------------------------------------
# Atualizar ttmatrix 2028 original - substituição de rotas 'piores'
# ------------------------------------------------------------------------------

# Abrir as duas ttmatrix finais para revisão da de 2028
ttmatrix_2024 <- sprintf('%s/10_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano_1, min_thres)
ttmatrix_2024 <- read_delim(ttmatrix_2024, delim = ';', col_types = 'cidddddddddc')
ttmatrix_2024 <- ttmatrix_2024 %>% mutate(ano = ano_1, .after = 'alt')
head(ttmatrix_2024)

ttmatrix_2028 <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano_2, min_thres)
ttmatrix_2028 <- read_delim(ttmatrix_2028, delim = ';', col_types = 'cidddddddddc')
ttmatrix_2028 <- ttmatrix_2028 %>% mutate(ano = ano_2, .after = 'alt')
head(ttmatrix_2028)

# Exemplo de rota pior: por conta do aumento de tempo com as rotas com nova
# infra cicloviária, a escolhida (única dentro do tempo limite) foi a com pouco
# uso de ciclovias e ciclofaixas
ttmatrix_2024 %>% filter(hex_id == '89a810050dbffff-89a81005617ffff')
ttmatrix_2028 %>% filter(hex_id == '89a810050dbffff-89a81005617ffff')


# Comparar o uso de infra cicloviária nos dois cenários
this <- ttmatrix_2024 %>% select(hex_id, dist = distance, infra_ciclo)
that <- ttmatrix_2028 %>% select(hex_id, dist = distance, infra_ciclo)

# Queremos comparar somente os pares origem-destino presentes nos dois cenários
those <- left_join(this, that, by = 'hex_id') %>% arrange(hex_id) %>% filter(!is.na(infra_ciclo.y))

# Qual a diferença de uso de infra cicloviária?
those <- those %>% mutate(dif_infra_ciclo = infra_ciclo.y - infra_ciclo.x,
                          dist_fora_ciclo_1 = dist.x  - infra_ciclo.x,
                          dist_fora_ciclo_2 = dist.y  - infra_ciclo.y,
                          dif_fora_ciclo = dist_fora_ciclo_2 - dist_fora_ciclo_1)

# Revendo nosso caso de exemplo
those %>% filter(hex_id == '89a810050dbffff-89a81005617ffff') %>% select(-hex_id)
# Confirmando se o filtro que faremos faz sentido: faz. São todos casos em que
# a rota aumentou e, com isso, também aumentou o trecho que se faz fora de infra
# cicloviária, ou mesmo casos em que a rota diminui, mas com ela há aumento dos
# trechos desprotegidos
those %>% filter(dif_fora_ciclo > 10) %>% select(-hex_id) %>% sample_n(20)

# Fazer o filtro. Temos 106 casos
nrow(those) # 49209 
those <- those %>% filter(dif_fora_ciclo > 25) # 106 / 49209 = 0.2154078%

# Selecionar as rotas melhores de cada cenário
ttmatrix_2024_rotas_melhores <- ttmatrix_2024 %>% filter(hex_id %in% those$hex_id)
ttmatrix_2028_rotas_melhores <- ttmatrix_2028 %>% filter(!hex_id %in% those$hex_id)

# E juntá-las para a ttmatrix revista
ttmatrix_2028_rev <- ttmatrix_2028_rotas_melhores %>% rbind(ttmatrix_2024_rotas_melhores) %>% arrange(hex_id)
# ttmatrix_2028_rev %>% select(ano) %>% distinct()

# Limpar ambiente
rm(this, that, those, ttmatrix_2024_rotas_melhores, ttmatrix_2028_rotas_melhores)


# ------------------------------------------------------------------------------
# Atualizar ttmatrix 2028 original - substituição rotas que ultrapassaram tempo
# ------------------------------------------------------------------------------

# Temos ainda 62 casos de pares OD que estão ficando de fora: existiam em 2024
# e passaram a não existir em 2028. O que está acontendo aqui é que os tempos
# das rotas em 2024 já eram muito próximos ao limite de 900s - em 2028 elas
# ultrapassam esse limite. Uma vez mais, vamos considerar as de 2024 para não
# perder esses pares OD:
ttmatrix_2024 %>% filter(!hex_id %in% ttmatrix_2028_rev$hex_id) %>% filter(time <= 900) %>% sample_n(20)
# (106+62)/49209*100 = 0.341401% da base

ttmatrix_2024_pares_a_mais <- ttmatrix_2024 %>% filter(!hex_id %in% ttmatrix_2028_rev$hex_id) %>% filter(time <= 900)
# ttmatrix_2028_rev %>% filter(hex_id %in% ttmatrix_2024_pares_a_mais$hex_id)

# Atualizar ttmatrix_2028_rev
ttmatrix_2028_rev <- ttmatrix_2028_rev %>% rbind(ttmatrix_2024_pares_a_mais) %>% arrange(hex_id)

# Conferência de um dos casos
# ttmatrix_2024 %>% filter(hex_id == '89a81009963ffff-89a81009b8bffff')
# ttmatrix_2028 %>% filter(hex_id == '89a81009963ffff-89a81009b8bffff')
# ttmatrix_2028_rev %>% filter(hex_id == '89a81009963ffff-89a81009b8bffff')

# Limpar ambiente
rm(ttmatrix_2024_pares_a_mais)

# Gravar resultados
out_file <- sprintf('%s/11_ttmatrix_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano_2, min_thres)
write_delim(ttmatrix_2028_rev, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Atualizar seleção de osm_ids com infraciclo da ttmatrix 2028 original
# ------------------------------------------------------------------------------

# Abrir seleção de osm_ids com infraciclo originais
ttmatrix_osmids_24 <- sprintf('%s/12_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano_1, min_thres)
ttmatrix_osmids_24 <- read_delim(ttmatrix_osmids_24, delim = ';', col_types = 'cccd')
head(ttmatrix_osmids_24)

ttmatrix_osmids_28 <- sprintf('%s/13_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso.csv', pasta_ttmatrix_24_28, ano_2, min_thres)
ttmatrix_osmids_28 <- read_delim(ttmatrix_osmids_28, delim = ';', col_types = 'cccd')
head(ttmatrix_osmids_28)

# Preparar bases para puxar osm_ids das rotas
ttmatrix_2028_rev <- 
  ttmatrix_2028_rev %>% 
  mutate(hex_id_alt = str_c(str_sub(hex_id, 6, 11), str_sub(hex_id, 22, 27), alt, sep = '-'), .before = 1)

relacao_osmids_2024 <- ttmatrix_2028_rev %>% filter(ano == '2024')
relacao_osmids_2028 <- ttmatrix_2028_rev %>% filter(ano == '2028')

# Separar osm ids das rotas que são melhores em 24. Atenção: 23 dessas rotas não
# possuem uso de infra cicloviária, então por isso os osm_ids não vão estar nessa
# seleção
osm_ids_rotas_melhores_24 <- ttmatrix_osmids_24 %>% filter(hex_id_alt %in% relacao_osmids_2024$hex_id_alt)
# osm_ids_rotas_melhores_24 %>% select(hex_id_alt) %>% distinct() # 145
# ttmatrix_2028_rev %>% filter(ano == '2024') %>% select(hex_id_alt) %>% distinct() # 168 (23 a mais)
# this <- ttmatrix_2028_rev %>% filter(ano == '2024') %>% filter(!hex_id_alt %in% osm_ids_rotas_melhores_24$hex_id_alt) %>% select(hex_id, hex_id_alt) %>% distinct()
# ttmatrix_2024 %>% filter(hex_id %in% this$hex_id) %>% filter(infra_ciclo > 0) # 23
# ttmatrix_osmids_24  %>% filter(hex_id_alt %in% this$hex_id_alt) # 0


# Separar osm ids das rotas que são melhores em 28. Atenção: 5.933 dessas rotas não
# possuem uso de infra cicloviária, então por isso os osm_ids não vão estar nessa
# seleção
osm_ids_rotas_melhores_28 <- ttmatrix_osmids_28 %>% filter(hex_id_alt %in% relacao_osmids_2028$hex_id_alt)
# osm_ids_rotas_melhores_28 %>% select(hex_id_alt) %>% distinct() # 44,782 
# ttmatrix_2028_rev %>% filter(ano == '2028') %>% select(hex_id_alt) %>% distinct() # 50,715 (5,933 a mais)
# this <- ttmatrix_2028_rev %>% filter(ano == '2028') %>% filter(!hex_id_alt %in% osm_ids_rotas_melhores_28$hex_id_alt) %>% select(hex_id, hex_id_alt) %>% distinct()
# ttmatrix_2028 %>% filter(hex_id %in% this$hex_id) %>% filter(infra_ciclo > 0 & time <= 900) # 5,933
# ttmatrix_osmids_28 %>% filter(hex_id_alt %in% this$hex_id_alt)%>% select(hex_id_alt) %>% distinct() # 0

# Juntar osm ids das melhores rotas
osm_ids_28_rev <- rbind(osm_ids_rotas_melhores_24, osm_ids_rotas_melhores_28) %>% arrange(hex_id_alt)


# Gravar resultados atualizados
out_file_2 <- sprintf('%s/13_ttmatrix_osmids_infraciclo_%s_res09_%smin_menor_peso_REV.csv', pasta_ttmatrix_24_28, ano_2, min_thres)
write_delim(osm_ids_28_rev, out_file_2, delim = ';')
