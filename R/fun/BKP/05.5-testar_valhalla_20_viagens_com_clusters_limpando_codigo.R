# Faz o map matching e realiza os cálculos referentes a rotas completas

# carregar bibliotecas
source('fun/setup.R')
source('fun/st_dbscan.R')
source('fun/funcoes_rotas.R')
source('fun/valhalla_map_matching.R')
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
log_head <- 'trip_id;id_trecho;proc_status'
write(log_head, file = log_file, append = FALSE)



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


# Definir tempo mínimo de viagem, em segundos.Considerando uma média de 1 ponto 
# a cada 5 segundos, temos: 18 pontos - 90 segundos - cerca de 300 metros a 12km/h.
# Com isso, rotas curtas são:
# 1. Com tempo menor do que 30s;
# 2. Com distância menor do que 300m;
# 3. Com quantidade mínima de 18 pontos
tempo_min_viagem <- 90
dist_min_viagem  <- 300
qtd_min_pontos   <- 18

# van de Coevering et al. (2014) consideraram como paradas quando ciclistas
# ficam mais de 180 seg em um raio de 300 m. Filtro de viagens curtas: < 500m.

# Lissner e Huber (2021):
# 1. Viagens com menos de 30 seg são eliminadas;
# 2. Excluem pontos com velocidades maiores do que 90 km/h - dizem que valor pode 
# variar de acordo com a fonte dos dados (tratamento em outliers extremos)
# 3. Distância entre pontos é Harversine, calculada a partir do latlong
# 4. Se há pontos com acurácia abaixo de 50m, ou seja, se há pontos com erro 
# potencial maior do que o diâmetro de 50m em volta da posição original, os pontos
# GPS são excluídos (para lidar com refração ou multicaminho)

# Em Newson e Krumm (2009), o valor de amostragem (sampling) de 1 ponto a cada
# 90 segundos é quando há um salto de degradação no map matching da rota
tempo_max_quebra <- 90

# Rotas com frequência de sinal de 1 ponto a cada 30 seg possuíam erro de 0,11% 
# (Newson e Krumm, 2009), mas inspeções manuais nos dados mostraram que a 
# amostragem média deveria estar abaixo de 12 segundos (~200m)
interv_med_ptos  <- 12

# Threshold intervalo médio máximo entre pontos. rotas com muita dispersão
# têm média muito alta, acima de 20; rotas boas costumam ter entre 3.5 e 5. Um
# intervalo ok seria abaixo de 12, pois há rotas que funcionam com até 12


# Função para processar a viagem como um todo - faz o map matching e retorna
# um shapefile da rota completa, com os dados principais
processar_viagens <- function(sel_trip, df_trips) {
  
  # Selecionar uma das viagens da base para testes
  # sel_trip <- '104746'; df_trips = yellow_sp # rota com dispersão de pontos perto do ibirapuera
  # sel_trip <- '209982'; df_trips = yellow_sp # rota ok, passando pelo ibirapuera rumo à rep do líbano
  # sel_trip <- '098269'; df_trips = yellow_sp # tirar outliers está criando um buraco no meio da berrini
  # sel_trip <- '247325'; df_trips = yellow_sp # faria lima / berrini até o metrô - fica perfeita
  # sel_trip <- '187252'; df_trips = yellow_sp # pela ponte laguna
  # sel_trip <- '172378'; df_trips = yellow_sp # pela Marginal Pinheiros e Parque do Povo
  # sel_trip <- '297794'; df_trips = yellow_sp # rota longa Fradique + Pedroso + Cunha Gago + Pinheiros, com paradas longas
  # sel_trip <- '237075'; df_trips = yellow_sp # rota longa saindo do Shop. JK e entrando no Ibirapuera
  # sel_trip <- '394614'; df_trips = yellow_sp # rota longa saindo do Itaim e entrando no Ibirapuera
  # sel_trip <- '425829'; df_trips = yellow_sp # rota longa saindo de Moema / Av. Ibirapuera e entrando no Ibirapuera, com poucos outliers
  # sel_trip <- '180975'; df_trips = yellow_sp # rota curta Av. Pedroso de Morais rumo ao metrô F. Lima
  # sel_trip <- '212701'; df_trips = yellow_sp # rota curta Rua Elvira Ferraz + Rua Prof Atilio Innocenti
  # sel_trip <- '389934'; df_trips = yellow_sp # rota curta Av. Hélio Pellegrino
  # sel_trip <- '303925'; df_trips = yellow_sp # percurso pelo Parque Villa Lobos, geocode ruim
  # sel_trip <- '353180'; df_trips = yellow_sp # percurso pelo Parque Villa Lobos, geocode ruim
  # sel_trip <- '000643'; df_trips = yellow_sp # pontos estacionários na Av. Pedroso de Morais
  # sel_trip <- '315373'; df_trips = yellow_sp # pontos estacionários na Rua Pirajussara, perto da Vital Brasil
  # sel_trip <- '035917'; df_trips = yellow_sp # percurso dentro do Ibirapuera, bom geocode
  # sel_trip <- '039193'; df_trips = yellow_sp # percurso dentro do Ibirapuera, bom geocode
  # sel_trip <- '234740'; df_trips = yellow_sp # percurso dentro do Ibirapuera, aparente bom geocode [ mesma do último trecho da viagem acima?]
  # sel_trip <- '243795'; df_trips = yellow_sp # percurso dentro do Ibirapuera, aparente bom geocode
  
  message(sprintf('Começando viagem id: %s', sel_trip))
  
  # Status temporário do processamento da viagem
  cp_a <- 0; cp_b <- cp_c <- cp_d <- cp_e <- '0'
  # print(sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e))
  
  # Isolar viagem de acordo com o seu trip_id
  viagem <- df_trips %>% filter(trip_id == {{sel_trip}})
  tam_vg <- nrow(viagem)
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Retirar pontos que possuem as mesmas coordenadas latlong - este passo precisa
  # ser feito antes do cálculo de tempo de tempo entre um ponto GPS e outro
  viagem <- retirar_pontos_sobrepostos(viagem)
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  
  # Se houve alteração, atualizar código de processamento da viagem
  if (nrow(viagem) < tam_vg) { cp_b <- '1'; tam_vg <- nrow(viagem) }
  
  
  # ----------------------------------------------------------
  # Remover paradas (st_dbscan)
  # ----------------------------------------------------------
  
  # Reconhecer e retirar paradas com o st_dbscan() - valores de eps1, ep2 e minpts
  # seguem o artigo de Lissner e Huber (2021), que por sua vez se basearam em
  # van de Coevering et al (2014). Conversão da distância em graus de latitude/
  # longitude em metros estimados com base em:
  # https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
  clusters <- st_dbscan(x    = viagem$lon,
                        y    = viagem$lat, 
                        time = viagem$timestamps, 
                        # EPS1: 0.0001 = ~11 m; 0.0001352 = ~15 m; cálculo: eps1 = 0.0001*dist_m/11.1
                        eps1 = 0.0001352,
                        eps2 = 180,
                        # MINPTS: Em 1 ponto a cada 5 seg, 18 equivale a ~90 seg, para
                        # considerar eventuais paradas em semáforos
                        minpts = 18,
                        dry  = TRUE) %>% 
              as.data.frame()
  
  # # Incluir número de clusters ao dataframe da viagem
  # lala <- viagem %>% cbind(clusters)
  # lala %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% mapview(zcol = 'cluster', cex = 3)
  
  # Filtrar somente pontos que pertencem ao cluster zero (ou seja, não são paradas)
  viagem <- viagem %>% cbind(clusters) %>% filter(cluster == 0) %>% select(-cluster)
  viagem %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% mapview(cex = 3)
  rm(lala, clusters)
  
  # Se houve alteração, atualizar código de processamento da viagem
  if (nrow(viagem) < tam_vg) { cp_c <- '1'; tam_vg <- nrow(viagem) }
  
  # st_write(viagem, '../viagem_teste_apagar2.gpkg', driver = 'GPKG', append = FALSE)
  
  
  # ----------------------------------------------------------
  # Retirar outliers extremos de velocidade e aceleração
  # ----------------------------------------------------------
  
  # Remover outliers extremos de forma iterativa, até que não haja nenhum
  viagem <- retirar_outliers_extremos_iterativo(viagem)

  # Se houve alteração, atualizar código de processamento da viagem
  if (nrow(viagem) < tam_vg) { cp_d <- '1'; tam_vg <- nrow(viagem) }
  
  # Dar uma olhada em como está a viagem
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
  
  
  # ----------------------------------------------------------
  # Testes de validade da viagem - por quebras e tempo mínimo
  # ----------------------------------------------------------
  
  # Teste 1 - Se viagem tem quebras, maior trecho é maior do que o tempo mínimo?
  quebras_de_sinal <- detectar_quebras_em_viagens(viagem, max_break_time = tempo_max_quebra)
  qtd_quebras <- nrow(quebras_de_sinal)
  
  source('fun/funcoes_rotas_pos_mapmatching.R')
  # Se não houve quebras, faremos o map matching da viagem toda
  if (qtd_quebras == 0) {
    # Atualizar código de processamento da viagem e finalizar código
    cp_e <- '0'
    cp_viagem <- sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e)
    
    # Processar a viagem como um todo, fazendo map matching e registrando resultados
    processar_trecho(viagem, sel_trip, cp_a, cp_viagem, tempo_min_viagem, 
                     qtd_min_pontos, dist_min_viagem, active_mode = 'pedestrian',
                     pontos_elevacao, vias_ciclo, vias_restritas, semaforos) 
    
    # # Viagem possui duração, quantidade de pontos e frequência de sinal mínimas?
    # tempo_viagem <- get_elapsed_time(viagem)
    # cp_trecho <- validar_trecho(viagem, tempo_viagem, tempo_min_viagem, qtd_min_pontos, 'time_s')
    # 
    # # Se sim, cp_trecho == '000'
    # if (cp_trecho == '000') {
    #   # Fazer map matching e registrar resultados
    #   map_matching_results <- run_map_matching(viagem, sel_trip, dist_min_viagem, active_mode = 'pedestrian',
    #                                            pontos_elevacao, vias_ciclo, vias_restritas, semaforos)
    #   
    #   # Finalizar código de processamento, com resultado do map matching
    #   cp_mm <- map_matching_results[[1]]
    #   cod_proc <- sprintf('%s%s%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e, cp_trecho, cp_mm)
    #   
    #   # Se map matching foi bem sucedido
    #   if (cp_mm == '00') {
    #     # Registrar shape da rota
    #     viagem <- map_matching_results[[2]]
    #     shape_rota <- map_matching_results[[3]]
    #     # mapview(shape_rota)
    #     # 
    #     # Inserir código de processamento na viagem
    #     shape_rota <- shape_rota %>% add_column(cod_proc, .after = 'trip_id')
    #     
    #     # Salvar resultados nos formatos .gpkg, .csv e .png
    #     salvar_resultados_map_matching(sel_trip, cp_a, shape_rota, pasta_20_viagens) 
    #     
    #     # Registrar processamento da viagem no log
    #     write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
    #     
    #   } else {
    #     # Map matching foi mal sucedido - Registrar processamento da viagem no log
    #     write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
    #     
    #   }
    # 
    # } else {
    #   # Trecho não foi validado - Registrar processamento da viagem no log
    #   cp_mm <- 'XX'
    #   cod_proc <- sprintf('%s%s%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e, cp_trecho, cp_mm)
    #   write(sprintf('%s;%s;%s', sel_trip, cp_a, cod_proc), file = log_file, append = TRUE)
    #   
    # }
  

  } else {
    # Houve quebra da viagem em trechos - atualizar códigos de processamento
    cp_e <- '1'
    
    # Processar cada trecho de viagem separadamente - total de trechos é qtd_quebras + 1
    for (i in seq(1:qtd_quebras)) {
      # Atualizar códigos de processamento
      cp_a <- cp_a + 1
      cp_viagem <- sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e)
      # print(sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e))
      
      # i <- 1
      if (i == 1) {
        # Primeira viagem é do início do dataframe até linha antes da primeira quebra
        trecho <- viagem %>% slice(1:quebras_de_sinal$index[i] - 1) 
        # trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
        
      } else {
        # Viagens intermediárias são entre linha após a primeira quebra e linha antes da segunda
        trecho1 <- quebras_de_sinal$index[i - 1] + 1
        trecho2 <- quebras_de_sinal$index[i] - 1
        trecho <- viagem %>% slice(trecho1:trecho2)
        # trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
        
      }
      
      # Processar trecho da viagem, fazendo map matching e registrando resultados
      processar_trecho(trecho, sel_trip, cp_a, cp_viagem, tempo_min_viagem, 
                       qtd_min_pontos, dist_min_viagem, active_mode = 'pedestrian',
                       pontos_elevacao, vias_ciclo, vias_restritas, semaforos) 

    }
    
    # Viagem final é a da última quebra até o final do dataframe
    trecho1 <- pull(last(quebras_de_sinal)) + 1
    trecho2 <- nrow(viagem)
    trecho <- viagem %>% slice(trecho1:trecho2) 
    # trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
    
    # Atualizar códigos de processamento
    cp_a <- cp_a + 1
    cp_viagem <- sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e)
    # print(sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e))
    
    # Processar trecho da viagem, fazendo map matching e registrando resultados
    processar_trecho(trecho, sel_trip, cp_a, cp_viagem, tempo_min_viagem, 
                     qtd_min_pontos, dist_min_viagem, active_mode = 'pedestrian',
                     pontos_elevacao, vias_ciclo, vias_restritas, semaforos) 
    
    }
  }
  
  
  # # Processar cada trecho de viagem separadamente - total de trechos é qtd_quebras + 1
  # 
  # # Validar cada trecho e, se passar na validação, realizar map matching
  # for (i in seq(1:qtd_quebras)) {
  #   i <- 2
  #   # Primeira viagem é do início do dataframe até linha antes da primeira quebra
  #   if (i == 1) {
  #     viagem <- viagem %>% slice(1:quebras_de_sinal$index[i] - 1) 
  #     
  #     # Trecho possui duração, quantidade de pontos e frequência de sinal mínimas?
  #     cp_trecho <- validar_trecho(viagem, tempo_min_viagem, qtd_min_pontos, 'time_s')
  #     # Se sim, cp_trecho == '000' -> Fazer map matching e registrar resultados
  #     if (cp_trecho == '000') {
  #       # TODO - Juntar todas as funções do valhalla em uma?
  #       map_matching_1 <- trace_route_valhalla_main(viagem)
  #       cp_i <- map_matching_1[[1]]
  #       dist_rota <- map_matching_1[[2]]
  #       
  #       # Se primeiro map matching deu certo, podemos seguir para o integral
  #       if (cp_i == '0') {
  #         
  #       } else {
  #         # Se map primeiro matching falhou, ou se trecho era muito curto, pular trecho
  #         return(NULL)
  #       }
  #       
  #     } else {
  #       # Se não, descartar e registrar processamento do trecho no log
  #       # TODO
  #       print('Descartado')
  #     }
  #     
  #     
  #     
  #     # Só processar viagens com quantidade mínima de pontos
  #     
  #     
  #     
  #   } else {
  #     # Viagens intermediárias são entre linha após a primeira quebra e linha antes da segunda
  #     trecho1 <- quebras_de_sinal$index[i - 1] + 1
  #     trecho2 <- quebras_de_sinal$index[i] - 1
  #     viagem <- viagem %>% slice(trecho1:trecho2)
  #     viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
  #     
  #     trace_route_valhalla_main(viagem)
  #   }
  # }
  # # Viagem final é a da última quebra até o final do dataframe
  # trecho1 <- pull(last(quebras_de_sinal)) + 1
  # trecho2 <- nrow(viagem)
  # viagem <- viagem %>% slice(trecho1:trecho2) 
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
  # 
  
  

  # ----------------------------------------------------------
  # Map matching Valhalla - trace_route()
  # ----------------------------------------------------------
  
  # # Traçar rota com trace_route, retorna o response do POST
  # r_tr <- trace_route_valhalla(viagem, active_mode = 'pedestrian') # pedestrian, bicycle
  # 
  # 
  # # Se roteamento não for bem sucedido, descartar viagem
  # if (http_status(r_tr)$message != 'Success: (200) OK') {
  #   # Registrar viagem descartada no log e parar restante da execução para ela
  #   write(sprintf('%s;D-VTR', sel_trip), file = log_file, append = TRUE)
  #   warning(sprintf('Falha no trace_route() para viagem %s', sel_trip), call. = FALSE)
  #   return(NULL)
  # }
  # 
  # 
  # # Pegar a resposta do POST e usar str_c() - string collapse para transformar
  # # os vetores em uma única string, que por sua vez será transformada em um
  # # dataframe - estes elementos estarão facilmente acessíveis a partir dai
  # # https://www.qiushiyan.dev/post/json-column-r/
  # response_text_tr <- 
  #   # Ignorar aviso 'argument is not an atomic vector; coercing'
  #   suppressWarnings(str_c(r_tr, collapse = ", ")) %>% 
  #   str_c("[", ., "]") %>% 
  #   fromJSON() %>% 
  #   as.data.frame()
  # 
  # 
  # # Do trace_route(), pegar a seção de matchings, que contém
  # # os dados de distância percorrida por trecho
  # matchings <- route_matchings(response_text_tr, trip_id = sel_trip)
  # 
  # # Pegar os polylines de todas as pernas (legs) da viagem
  # matchings_polyline <- matchings$route_polyline
  # 
  # # Criar um shapefile único para a rota toda
  # shape_rota <- create_route_shape(matchings_polyline, sel_trip)
  # shape_rota %>% mapview()
  # 
  # # Calcular a distância total da viagem - ver observações sobre o cálculo na
  # # função create_route_shape()
  # dist_rota <- shape_rota %>% st_length() %>% as.double()
  # 
  # 
  # # Testar se viagem é curta demais - se for, descartá-la
  # if (dist_rota < dist_min_viagem) {
  #   # Registrar viagem descartada no log e parar restante da execução para ela
  #   write(sprintf('%s;D-VMC', sel_trip), file = log_file, append = TRUE)
  #   warning(sprintf('Rota %s descartada por ser muito curta', sel_trip), call. = FALSE)
  #   return(NULL)
  # }
  
  # # ----------------------------------------------------------
  # # Map matching Valhalla - trace_attributes()
  # # ----------------------------------------------------------
  # # Interessam os dados dos edges (id, nome, extensão) e matched_points
  # 
  # # Traçar rota com trace_attributes, pegar resposta e transformar em lista
  # r_at <- trace_attributes_valhalla(viagem, active_mode = 'pedestrian') # bicycle, pedestrian
  # 
  # 
  # # Se roteamento não for bem sucedido, descartar viagem
  # if (http_status(r_at)$message != 'Success: (200) OK') {
  #   # Registrar viagem descartada no log e parar o restante da execução para ela
  #   write(sprintf('%s;D-VTA', sel_trip), file = log_file, append = TRUE)
  #   warning(sprintf('Falha no trace_attributes() para viagem %s', sel_trip), call. = FALSE)
  #   return(NULL)
  # }
  # 
  # 
  # # Pegar a resposta do POST e usar str_c() - string collapse para transformar
  # # os vetores em uma única string, que por sua vez será transformada em um
  # # dataframe - estes elementos estarão facilmente acessíveis a partir dai
  # # https://www.qiushiyan.dev/post/json-column-r/
  # response_text_at <- 
  #   # Ignorar aviso 'argument is not an atomic vector; coercing'
  #   suppressWarnings(str_c(r_at, collapse = ", ")) %>% 
  #   str_c("[", ., "]") %>% 
  #   fromJSON() %>% 
  #   as.data.frame()
  # 
  # 
  # # Pegar dados OSM: edges.way_id é o id que pode ser encontrado no OSM e seria
  # # o equivalende a um id do logradouro; edges.id é um id específico daquele
  # # edge e que, aparentemente, depende do versão do mapa que se está usando
  # trip_edges <- attributes_edges(response_text_at)
  # 
  # # Pegar dados dos pontos resultantes do map matching
  # trip_points <- attributes_matched_points(response_text_at)
  # 
  # # Associar matched_points com os dados dos edges
  # trip_points <- 
  #   trip_points %>% 
  #   left_join(trip_edges, by = c("matched_points.edge_index" = "edges.edge_index")) %>% 
  #   # Transformar way_id em character para left_joins futuros
  #   mutate(edges.way_id = as.character(edges.way_id))
  # 
  # # A quantidade de pontos que resultante do map matching deve ser a mesma dos
  # # pontos originais - juntar os dois dataframes
  # if (nrow(viagem) == nrow(trip_points)) {
  #   viagem <- viagem %>% cbind(trip_points)
  # } else {
  #   # Se quantidade de pontos for diferente, registrar erro no log e parar execução para viagem
  #   write(sprintf('%s;D-VET', sel_trip), file = log_file, append = TRUE)
  #   warning(sprintf('Quantidade de pontos do map matching difere do original para viagem %s', sel_trip), call. = FALSE)
  #   return(NULL)
  # }
  # 
  # 
  # # Limpar pontos que não conseguiram ser associados a um edge no map matching
  # viagem <- clean_matched_points(viagem, n_trip_edges = nrow(trip_edges))
  # # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview()
  # 
  # 
  # # # Do resultado do trace_attributes(), selecionar seção de shape
  # # attributes_shape <- response_text_at['shape']
  
  
  # # ----------------------------------------------------------
  # # Inserir dados de elevação
  # # ----------------------------------------------------------
  # 
  # # # Inserir dados de elevação vindos do OSM
  # # if (usar_elev_osm == TRUE) {
  # #   # Inserir dados de elevação - OSM
  # #   viagem <- get_elevation_data(viagem)  
  # # }
  # 
  # # Inserir dados de elevação - Geosampa
  # viagem <- get_geosampa_elevation(viagem, pontos_elevacao)
  
  
  
  
  # ----------------------------------------------------------
  # Inserir dados de infracicloviária e áreas restritas
  # ----------------------------------------------------------
  
  # # Juntar dados relativos a vias com infraestrutura cicloviária
  # viagem <- viagem %>% left_join(vias_ciclo, by = c('edges.way_id' = 'osm_id'))
  # 
  # # Juntar dados se trecho está dentro de áreas restritas, tais como parques
  # viagem <- 
  #   viagem %>% 
  #   mutate(area_restrita = case_when(!edges.way_id %in% vias_restritas$osm_id ~ FALSE,
  #                                    TRUE ~ TRUE))
  
  
  # # ----------------------------------------------------------
  # # Quantidade de semátofos
  # # ----------------------------------------------------------
  # 
  # # Obter a quantidade de cruzamentos com semáforos ao longo da rota
  # qtd_semaforos <- semaforos %>% filter(st_intersects(shape_rota, semaforos, sparse = FALSE))
  # qtd_semaforos <- nrow(qtd_semaforos)
  # 
  # 
  # # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview() 
  # # gerar_resumo_viagem(viagem)
  # 
  
  # # ----------------------------------------------------------
  # # Cálculos de distâncias via edges - infra cicloviária, parques
  # # ----------------------------------------------------------
  # 
  # # Obter distâncias com base nos edges (infra cicloviária, áreas restritas)
  # dados_edges <- obter_dados_edges(viagem)
  
  
  # # ----------------------------------------------------------
  # # Gerar shape resumitivo da viagem toda
  # # ----------------------------------------------------------
  # 
  # # Inserir dados da distribuição dos trechos dos edges no shape de rota
  # shape_rota <- shape_rota %>% cbind(dados_edges)
  # 
  # # Calcular variações positivas e negativas de elevação com base no geosampa
  # elev_var <- viagem %>% select(isovalor_var)
  # elev_geo_pos <- elev_var %>% filter(isovalor_var > 0) %>% sum()
  # elev_geo_neg <- elev_var %>% filter(isovalor_var < 0) %>% sum()
  # 
  # # Inserir dados
  # shape_rota <- shape_rota %>% mutate(tempo = tempo_viagem,
  #                                     dist  = dist_rota,
  #                                     veloc = dist_rota / tempo_viagem * 3.6,
  #                                     semaforos = qtd_semaforos,
  #                                     elev_pos = elev_geo_pos, 
  #                                     elev_neg = elev_geo_neg,
  #                                     .before = 'dist_edges')
  # 
  # shape_rota %>% mapview()
  
  # # ----------------------------------------------------------
  # # Salvar arquivos resultantes
  # # ----------------------------------------------------------
  # # Em .gpkg
  # st_write(shape_rota, sprintf('%s/%s.gpkg', pasta_viagens_gpkg, sel_trip),
  #          driver = 'GPKG', append = FALSE, quiet = TRUE)
  # 
  # # Salvar imagem da rota, para conferência
  # png(sprintf('%s/%s.png', pasta_viagens_img, sel_trip))
  # plot(shape_rota$geometry)
  # dev.off()
  # 
  # # Em .csv
  # shape_rota <- shape_rota %>% st_drop_geometry()
  # write_delim(shape_rota, sprintf('%s/%s.csv', pasta_viagens_csv, sel_trip), delim = ';')
  # 
  # # Registrar sucesso da viagem no log
  # write(sprintf('%s;%s', sel_trip, ok_status), file = log_file, append = TRUE)
  
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