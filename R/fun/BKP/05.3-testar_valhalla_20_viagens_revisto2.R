# Faz o map matching e realiza os cálculos referentes a rotas completas

# carregar bibliotecas
source('fun/setup.R')
source('fun/valhalla_map_matching.R')
source('fun/funcoes_rotas.R')
library('geosphere')


# Estrutura de pastas
pasta_dados        <- "../yellow_dados"
pasta_viagens_sp   <- sprintf("%s/01_viagens_em_sp", pasta_dados)
pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao     <- sprintf("%s/03_curva_intermediaria_sp", pasta_dados)
pasta_semaforos    <- sprintf("%s/04_semaforos_sp", pasta_dados)
pasta_20_viagens   <- sprintf("%s/05_testes_20_viagens", pasta_dados)
pasta_viagens_gpkg <- sprintf("%s/viagens_processadas_gpkg", pasta_20_viagens)
pasta_viagens_img  <- sprintf("%s/viagens_processadas_img", pasta_20_viagens)
pasta_viagens_csv  <- sprintf("%s/viagens_processadas_csv", pasta_20_viagens)
dir.create(pasta_viagens_gpkg, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_viagens_img, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_viagens_csv, recursive = TRUE, showWarnings = FALSE)


# Criar arquivo de log com header simples
log_file <- sprintf('%s/viagens_processadas_log.csv', pasta_20_viagens)
log_head <- 'trip_id;proc_status'
write(log_head, file = log_file, append = FALSE)

# Criar arquivo de dicionário para o logfile
dic_file <- sprintf('%s/viagens_processadas_dic.csv', pasta_20_viagens)
siglas   <- 
'Sigla;Descrição
D-MQ;Viagem descartada por conter muitas quebras (interrupções de sinal)
D-TM;Viagem descartada por tempo inferior ao mínimo
D-VEE;Viagem descartada por erro na inserção dos dados de elevação do Geosampa
D-VET;Viagem descartada porque quantidades de pontos originais e resultantes do map matching diferiram
D-VMC;Viagem descartada porque trace_route() mostrou que é muito curta
D-VTA;Viagem descartada por falha no trace_attributes() do Valhalla
D-VTR;Viagem descartada por falha no trace_route() do Valhalla
OK-I;Viagem processada e finalizada na íntegra
OK-P;Viagem dividida entre interrupções de sinal, maior trecho processado e finalizado'
write(siglas, file = dic_file, append = FALSE)


# ----------------------------------------------------------
# Abrir dados de apoio para os cálculos nas rotas
# ----------------------------------------------------------
# Abrir listagem de vias com infraestrutura cicloviária
vias_ciclo <- sprintf('%s/listagem_vias_infra_cicloviaria.csv', pasta_osm_sp)
vias_ciclo <- read_delim(vias_ciclo, delim = ';', col_types = cols(.default = "c"))
vias_ciclo <- vias_ciclo %>% select(osm_id, osm_cycletype = tipo_2018)

# Abrir listagem de vias em áreas com alguma restrição - parques, principalmente
vias_restritas <- sprintf('%s/listagem_vias_em_areas_restritas.csv', pasta_osm_sp)
vias_restritas <- read_delim(vias_restritas, delim = ';', col_types = cols(.default = "c"))

# Abrir shape com linhas mestra e intermediária de elevação do Geosampa
pontos_elevacao <- sprintf('%s/geosampa_pontos_de_elevacao_no_viario.gpkg', pasta_elevacao)
pontos_elevacao <- read_sf(pontos_elevacao)

# Abrir esquinas com semáforos - buffers de 25m a partir do shape da CET
semaforos <- sprintf('%s/semaforos_buffer25_sp_dissolved.gpkg', pasta_semaforos)
semaforos <- read_sf(semaforos)



# ----------------------------------------------------------
# Abrir base de viagens
# ----------------------------------------------------------

# Abrir a base de viagens da Yellow em SP
open_file <- sprintf('%s/sp_20_viagens_teste.rds', pasta_20_viagens)
viagens <- read_rds(open_file) %>% select(-n_points)

# Abrir a base de viagens da Yellow em SP
open_file <- sprintf('%s/sp_viagens_yellow.rds', pasta_viagens_sp)
yellow_sp <- read_rds(open_file) %>% rename(lat = lats,
                                            lon = longs)


# Definir tempo mínimo de viagem, em segundos
tempo_min_viagem <- 30
tempo_max_quebra <- 90
dist_min_viagem  <- 300


# Função para processar a viagem como um todo - faz o map matching e retorna
# um shapefile da rota completa, com os dados principais
processar_viagens <- function(sel_trip, df_trips) {
  
  # Selecionar uma das viagens da base para testes
  # sel_trip <- '104746'; df_trips = yellow_sp
  
  message(sprintf('Começando viagem id: %s', sel_trip))
   
  # Status temporário do processamento da viagem
  ok_status <- 'OK-I'
  
  # Isolar viagem de acordo com o seu trip_id
  viagem <- df_trips %>% filter(trip_id == {{sel_trip}})
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Retirar pontos que possuem as mesmas coordenadas latlong - este passo precisa
  # ser feito antes do cálculo de tempo de tempo entre um ponto GPS e outro
  viagem <- retirar_pontos_sobrepostos(viagem)
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Criar coluna com o cálculo de tempo entre um ponto e outro
  viagem <- viagem %>% mutate(time_s = c(diff(timestamps), 0))
  # time_s = timestamps - lag(timestamps), # cálculo na linha posterior
  # time_s = c(diff(timestamps), 0),       # cálculo na mesma linha
  
  
  # ----------------------------------------------------------
  # Testes de validade da viagem - por quebras e tempo mínimo
  # ----------------------------------------------------------
  
  # Teste 1 - Se viagem tem quebras, maior trecho é maior do que o tempo mínimo?
  quebras_de_sinal <- detectar_quebras_em_viagens(viagem, max_break_time = tempo_max_quebra)
  qtd_quebras <- nrow(quebras_de_sinal)
  
  # Se viagem tiver até duas quebras, pegar o trecho maior; se tem mais, 
  # descartá-la; se tem uma só não precisa fazer nada
  if (qtd_quebras > 2) {
    # Registrar descarte no log, avisar e parar restante da execução para a viagem
    write(sprintf('%s;D-MQ', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Viagem %s descartada por conter muitas quebras', sel_trip), call. = FALSE)
    return(NULL)
    
  } else if (qtd_quebras == 1 | qtd_quebras == 2) {
    # Há pelo menos uma quebra, pegar trecho maior
    viagem <- retirar_quebras_em_viagens(viagem, 
                                         n_quebras = quebras_de_sinal)
    
    # Atualizar status de processamento da viagem
    ok_status <- 'OK-P'
  } 
  
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Teste 2 - Viagem é maior do que o tempo mínimo?
  # Pegar tempo total da viagem (último - primeiro ponto GPS), em segundos
  tempo_viagem <- get_elapsed_time(viagem)
  
  # Continuar somente se viagem for maior do que o tempo mínimo
  if (tempo_viagem <= tempo_min_viagem) {
    # Registrar descarte no log, avisar e parar restante da execução para a viagem
    write(sprintf('%s;D-TM', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Viagem %s menor do que o tempo mínimo', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Retirar outliers extremos de velocidade e aceleração
  # ----------------------------------------------------------
  
  # TODO - Fazer loop aqui - retirar até que não haja mais outliers extremos -llop com while?;
  # TODO - Atualizar cálculo de tempo, que deve ser feito após tirar os outliers:
  # 1. detecta quebras; 
  # 2. loop: faz todo o cálculo de tempos + retira outliers;
  # 3. faz checagem de tempo mínimo
  
  # Criar colunas com cálculos de velocidade, distância e aceleração entre pontos
  viagem <- calcular_vel_dist_acel(viagem)
  
  # Retirar outliers extremos de velocidade e aceleração
  viagem <- retirar_outliers_extremos(viagem)
  
  # Dar uma olhada em como está a viagem
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  
  library('stats')
  library('ggplot2')
  library('ggfortify')
  
  mydata <- iris %>% select(1:4)
  wssplot <- function(data, nc = 15, seed = 1234) {
    wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
    for (i in 2:nc) {
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers = i)$withinss)
    }
    
    plot(1:nc, wss, type = 'b', 
         xlab = 'Number of clusters', 
         ylab = 'Within groups sum of squares')
  }
  wssplot(mydata)
  
  KM <- kmeans(mydata, 2)
  autoplot(KM, mydata, frame = TRUE)
  
  mydata2 <- viagem %>% select(timestamps, lat, lon)
  wssplot(mydata2)
  autoplot(kmeans(mydata2, 2), viagem, frame = TRUE)
  
  
  # ----------------------------------------------------------
  # Map matching Valhalla - trace_route()
  # ----------------------------------------------------------
  
  # Traçar rota com trace_route, retorna o response do POST
  r_tr <- trace_route_valhalla(viagem, active_mode = 'pedestrian') # pedestrian, bicycle
  
  
  # Se roteamento não for bem sucedido, descartar viagem
  if (http_status(r_tr)$message != 'Success: (200) OK') {
    # Registrar viagem descartada no log e parar restante da execução para ela
    write(sprintf('%s;D-VTR', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Falha no trace_route() para viagem %s', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  
  # Pegar a resposta do POST e usar str_c() - string collapse para transformar
  # os vetores em uma única string, que por sua vez será transformada em um
  # dataframe - estes elementos estarão facilmente acessíveis a partir dai
  # https://www.qiushiyan.dev/post/json-column-r/
  response_text_tr <- 
    # Ignorar aviso 'argument is not an atomic vector; coercing'
    suppressWarnings(str_c(r_tr, collapse = ", ")) %>% 
    str_c("[", ., "]") %>% 
    fromJSON() %>% 
    as.data.frame()
  
  
  # Do trace_route(), pegar a seção de matchings, que contém
  # os dados de distância percorrida por trecho
  matchings <- route_matchings(response_text_tr, trip_id = sel_trip)
  
  # Pegar os polylines de todas as pernas (legs) da viagem
  matchings_polyline <- matchings$route_polyline
  
  # Criar um shapefile único para a rota toda
  shape_rota <- create_route_shape(matchings_polyline, sel_trip)
  # shape_rota %>% mapview()
  
  # Calcular a distância total da viagem - ver observações sobre o cálculo na
  # função create_route_shape()
  dist_rota <- shape_rota %>% st_length() %>% as.double()
  
  
  # Testar se viagem é curta demais - se for, descartá-la
  if (dist_rota < dist_min_viagem) {
    # Registrar viagem descartada no log e parar restante da execução para ela
    write(sprintf('%s;D-VMC', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Rota %s descartada por ser muito curta', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # Map matching Valhalla - trace_attributes()
  # ----------------------------------------------------------
  # Interessam os dados dos edges (id, nome, extensão) e matched_points
  
  # Traçar rota com trace_attributes, pegar resposta e transformar em lista
  r_at <- trace_attributes_valhalla(viagem, active_mode = 'pedestrian') # bicycle, pedestrian
  
  
  # Se roteamento não for bem sucedido, descartar viagem
  if (http_status(r_at)$message != 'Success: (200) OK') {
    # Registrar viagem descartada no log e parar o restante da execução para ela
    write(sprintf('%s;D-VTA', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Falha no trace_attributes() para viagem %s', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  
  # Pegar a resposta do POST e usar str_c() - string collapse para transformar
  # os vetores em uma única string, que por sua vez será transformada em um
  # dataframe - estes elementos estarão facilmente acessíveis a partir dai
  # https://www.qiushiyan.dev/post/json-column-r/
  response_text_at <- 
    # Ignorar aviso 'argument is not an atomic vector; coercing'
    suppressWarnings(str_c(r_at, collapse = ", ")) %>% 
    str_c("[", ., "]") %>% 
    fromJSON() %>% 
    as.data.frame()
  
  
  # Pegar dados OSM: edges.way_id é o id que pode ser encontrado no OSM e seria
  # o equivalende a um id do logradouro; edges.id é um id específico daquele
  # edge e que, aparentemente, depende do versão do mapa que se está usando
  trip_edges <- attributes_edges(response_text_at)
  
  # Pegar dados dos pontos resultantes do map matching
  trip_points <- attributes_matched_points(response_text_at)
  
  # Associar matched_points com os dados dos edges
  trip_points <- 
    trip_points %>% 
    left_join(trip_edges, by = c("matched_points.edge_index" = "edges.edge_index")) %>% 
    # Transformar way_id em character para left_joins futuros
    mutate(edges.way_id = as.character(edges.way_id))
  
  # A quantidade de pontos que resultante do map matching deve ser a mesma dos
  # pontos originais - juntar os dois dataframes
  if (nrow(viagem) == nrow(trip_points)) {
    viagem <- viagem %>% cbind(trip_points)
  } else {
    # Se quantidade de pontos for diferente, registrar erro no log e parar execução para viagem
    write(sprintf('%s;D-VET', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Quantidade de pontos do map matching difere do original para viagem %s', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  
  # Limpar pontos que não conseguiram ser associados a um edge no map matching
  viagem <- clean_matched_points(viagem, n_trip_edges = nrow(trip_edges))
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  
  # # Do resultado do trace_attributes(), selecionar seção de shape
  # attributes_shape <- response_text_at['shape']
  
  
  # ----------------------------------------------------------
  # Inserir dados de elevação
  # ----------------------------------------------------------
  
  # # Inserir dados de elevação vindos do OSM
  # if (usar_elev_osm == TRUE) {
  #   # Inserir dados de elevação - OSM
  #   viagem <- get_elevation_data(viagem)  
  # }
  
  # Inserir dados de elevação - Geosampa
  viagem <- get_geosampa_elevation(viagem, pontos_elevacao)
  
  # Checar se a inserção dos dados de elevação foi bem sucedida; se não, descartar
  if (!'ISOVALOR' %in% names(viagem)) {
    # Se dataframe 'viagem' não tem coluna 'ISOVALOR', registrar erro no log e
    # parar restante da execução para a viagem
    write(sprintf('%s;D-VEE', sel_trip), file = log_file, append = TRUE)
    warning(sprintf('Erro na inserção dos dados de elevação do Geosampa para viagem %s', sel_trip), call. = FALSE)
    return(NULL)
  }
  
  # Calcular variações positivas e negativas de elevação com base no geosampa
  elev_var <- viagem %>% select(isovalor_var)
  elev_geo_pos <- elev_var %>% filter(isovalor_var > 0) %>% sum()
  elev_geo_neg <- elev_var %>% filter(isovalor_var < 0) %>% sum()
  
  # ----------------------------------------------------------
  # Inserir dados de infracicloviária e áreas restritas
  # ----------------------------------------------------------
  
  # Juntar dados relativos a vias com infraestrutura cicloviária
  viagem <- viagem %>% left_join(vias_ciclo, by = c('edges.way_id' = 'osm_id'))
  
  # Juntar dados se trecho está dentro de áreas restritas, tais como parques
  viagem <- 
    viagem %>% 
    mutate(area_restrita = case_when(!edges.way_id %in% vias_restritas$osm_id ~ FALSE,
                                     TRUE ~ TRUE))
  
  
  # ----------------------------------------------------------
  # Quantidade de semátofos
  # ----------------------------------------------------------
  
  # Obter a quantidade de cruzamentos com semáforos ao longo da rota
  qtd_semaforos <- semaforos %>% filter(st_intersects(shape_rota, semaforos, sparse = FALSE))
  qtd_semaforos <- nrow(qtd_semaforos)
  
  
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview() 
  # gerar_resumo_viagem(viagem)
  
  
  # ----------------------------------------------------------
  # Cálculos de distâncias via edges - infra cicloviária, parques
  # ----------------------------------------------------------
  
  # Obter distâncias com base nos edges (infra cicloviária, áreas restritas)
  dados_edges <- obter_dados_edges(viagem)
  
  
  # ----------------------------------------------------------
  # Gerar shape resumitivo da viagem toda
  # ----------------------------------------------------------
  
  # Inserir dados da distribuição dos trechos dos edges no shape de rota
  shape_rota <- shape_rota %>% cbind(dados_edges)
  
  # Inserir dados
  shape_rota <- shape_rota %>% mutate(tempo = tempo_viagem,
                                      dist  = dist_rota,
                                      veloc = dist_rota / tempo_viagem * 3.6,
                                      semaforos = qtd_semaforos,
                                      elev_pos = elev_geo_pos, 
                                      elev_neg = elev_geo_neg,
                                      .before = 'dist_edges')
  
  # shape_rota %>% mapview()
  
  # Salvar arquivos resultantes
  # Em .gpkg
  st_write(shape_rota, sprintf('%s/%s.gpkg', pasta_viagens_gpkg, sel_trip), 
           driver = 'GPKG', append = FALSE, quiet = TRUE)
  
  # Salvar imagem da rota, para conferência
  png(sprintf('%s/%s.png', pasta_viagens_img, sel_trip))
  plot(shape_rota$geometry)
  dev.off()
  
  # Em .csv
  shape_rota <- shape_rota %>% st_drop_geometry()
  write_delim(shape_rota, sprintf('%s/%s.csv', pasta_viagens_csv, sel_trip), delim = ';')
  
  # Registrar sucesso da viagem no log
  write(sprintf('%s;%s', sel_trip, ok_status), file = log_file, append = TRUE)
  
}

# rotas <- c('209982', '297794', '303925', '039193')
# rotas <- viagens %>% select(trip_id) %>% distinct()
rotas <- yellow_sp %>% sample_n(60) %>% select(trip_id) %>% distinct()

(start = Sys.time())
walk(rotas$trip_id, processar_viagens, yellow_sp)
Sys.time() - start

# system.time( replicate(3, walk(rotas$trip_id, processar_viagens) ) )
# system.time( replicate(3, lapply(rotas$trip_id, processar_viagens) ) )




# ----------------------------------------------------------
# Aplicar filtros - outliers, número mínimo de pontos etc
# ----------------------------------------------------------

# '172378' - [margin] - rota longa pela Marginal Pinheiros e Parque do Povo
# '104746' - [rotaL]  - rota longa que passa pelo Parque Ibirapuera, mas com problemas de perda de edges


# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '297794' - [ativ]  - rota longa Fradique + Pedroso + Cunha Gago + Pinheiros, com paradas longas

# '098269' - [rotaL] - rota longa Av. Berrini + Rua Flórida
# '187252' - [rotaL] - rota longa e complexa  passando pela Ponte Laguna
# '237075' - [rotaL] - rota longa saindo do Shop. JK e entrando no Ibirapuera
# '247325' - [rotaL] - rota longa Av. Berrini + Faria Lima até o metrô
# '394614' - [rotaL] - rota longa saindo do Itaim e entrando no Ibirapuera
# '425829' - [rotaL] - rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers
# 
# '180975' - [rotaC] - rota curta Av. Pedroso de Morais rumo ao metrô F. Lima
# '212701' - [rotaC] - rota curta Rua Elvira Ferraz + Rua Prof Atilio Innocenti
# '389934' - [rotaC] - rota curta Av. Hélio Pellegrino
# 
# '303925' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim
# '353180' - [villa] - percurso pelo Parque Villa Lobos, geocode ruim

# '000643' - [estac] - pontos estacionários na Av. Pedroso de Morais
# '315373' - [estac] - pontos estacionários na Rua Pirajussara, perto da Vital Brasil

# '035917' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '039193' - [ibira] - percurso dentro do Ibirapuera, bom geocode
# '209982' - [ibira] - rota que passa pela Re. Líbano e entra no Ibirapuera, com poucos outliers
# '234740' - [ibira] - percurso dentro do Ibirapuera, aparente bom geocode
# '243795' - [ibira] - percurso curto dentro do Ibirapuera, bom geocode