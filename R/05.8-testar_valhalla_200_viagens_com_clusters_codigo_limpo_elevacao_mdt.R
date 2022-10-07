# Faz o map matching e realiza os cálculos referentes a rotas completas

# carregar bibliotecas
source('fun/setup.R')
source('fun/st_dbscan.R')
source('fun/funcoes_rotas.R')
source('fun/valhalla_map_matching.R')



# Estrutura de pastas
pasta_dados        <- "../yellow_dados"
pasta_viagens_sp   <- sprintf("%s/01_viagens_em_sp", pasta_dados)
pasta_osm_sp       <- sprintf("%s/02_osm_simplificado_sp", pasta_dados)
pasta_elevacao     <- sprintf("%s/03_curva_elevacao_sp", pasta_dados)
pasta_semaforos    <- sprintf("%s/04_semaforos_sp", pasta_dados)
# pasta_base   <- sprintf("%s/05_testes_20_viagens_clusters", pasta_dados)
# pasta_base         <- sprintf("%s/05_testes_200_viagens_clusters", pasta_dados)
pasta_base         <- sprintf("%s/05_testes_viagens_20181111", pasta_dados)
pasta_viagens_gpkg <- sprintf("%s/viagens_processadas_gpkg", pasta_base)
pasta_viagens_pngs <- sprintf("%s/viagens_processadas_pngs", pasta_base)
pasta_viagens_csv1 <- sprintf("%s/viagens_processadas_csv1", pasta_base)
pasta_viagens_csv2 <- sprintf("%s/viagens_processadas_csv2", pasta_base)
dir.create(pasta_viagens_gpkg, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_viagens_pngs, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_viagens_csv1, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_viagens_csv2, recursive = TRUE, showWarnings = FALSE)


# Criar arquivo de log com header simples
log_file <- sprintf('%s/viagens_processadas_log.csv', pasta_base)
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

# # Abrir shape com linhas mestra e intermediária de elevação do Geosampa
# pontos_elevacao <- sprintf('%s/geosampa_pontos_de_elevacao_no_viario.gpkg', pasta_elevacao)
# pontos_elevacao <- read_sf(pontos_elevacao)

# Abrir shape com viário do OpenStreetMap com valoz Z incluído, vindo do MDT de SP
pontos_elevacao <- sprintf('%s/viario_osm_com_elevacao_mdt_sem_z.gpkg', pasta_elevacao)
pontos_elevacao <- read_sf(pontos_elevacao)
# Renomear coluna de elevação para 'ISOVALOR'
pontos_elevacao <- pontos_elevacao %>% rename(ISOVALOR = elev_mdt)

# Abrir esquinas com semáforos - buffers de 25m a partir do shape da CET
semaforos <- sprintf('%s/semaforos_buffer25_sp_dissolved.gpkg', pasta_semaforos)
semaforos <- read_sf(semaforos)



# ----------------------------------------------------------
# Abrir base de viagens
# ----------------------------------------------------------

# # Abrir a base de viagens da Yellow em SP
# open_file <- sprintf('%s/sp_20_viagens_teste.rds', pasta_base)
# viagens <- read_rds(open_file) %>% select(-n_points)

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
  # sel_trip <- '337281'; df_trips = yellow_sp # rota extremamente longa, que passa por santo amaro, vila olímpia, miguel yunes
  # sel_trip <- '035272'; df_trips = yellow_sp # rota extremamente longa, que passa pela marginal pinheiros e carlos caldeira filho (campo limpo)
  # sel_trip <- '276831'; df_trips = yellow_sp # rota pela faria lima em que alguns pontos estão jogando para dentro dos quarteiroes
  # sel_trip <- '106722'; df_trips = yellow_sp # dando pau na associação com os dados de elevação
  # sel_trip <- '082540'; df_trips = yellow_sp # parque villa lobos - talvez a prova de que rotas que têm que tirar tipo 20 outliers devem ser descartadas
  # 123333 - idem acima
  # sel_trip <-'082697' # parque das bicicletas - pontos dispersos não sendo identificados nos filtros - veloc médias finais super altas
  # sel_trip <- '282280' # viagens estranhas pelo entorno do Ibirapuera
  # sel_trip <- '290375' # estudar começo - santo amaro com roque petroni jr - grandes zigue-zagues
  # sel_trip <- '167857'
  # sel_trip <- '103770' # Viagem quebrada em 5 trechos... iguais!?
  
  # rm(cp_a, cp_b, cp_c, cp_d, cp_e, tam_vg, result_outliers, qtd_iteracoes, quebras_de_sinal, qtd_quebras, cp_viagem, cod_proc, trecho, trecho1, trecho2, cp_a2, cp_mm, cp_trecho, sel_trip, viagem)
  # rm(map_matching_results, shape_rota)
  
  message(sprintf('Começando viagem id: %s', sel_trip))
  
  # Status temporário do processamento da viagem
  cp_a <- 0; cp_b <- cp_c <- cp_d <- cp_e <- '0'
  # print(sprintf('%s%s%s%s%s', cp_a, cp_b, cp_c, cp_d, cp_e))
  
  
  # Isolar viagem de acordo com o seu trip_id
  viagem <- df_trips %>% filter(trip_id == {{sel_trip}})
  tam_vg <- nrow(viagem)
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3, zcol = 'timestamps', at = seq(min(.$timestamps), max(.$timestamps), 200))
  
  
  
  
  # desenhar_centroide(viagem)
  
  
  # ----------------------------------------------------------
  # Remover pontos de latlong sobrepostos
  # ----------------------------------------------------------
  
  # Retirar pontos que possuem as mesmas coordenadas latlong - este passo precisa
  # ser feito antes do cálculo de tempo de tempo entre um ponto GPS e outro
  viagem <- retirar_pontos_sobrepostos(viagem)
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
  
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
  # rm(lala)
  
  # Filtrar somente pontos que pertencem ao cluster zero (ou seja, não são paradas)
  viagem <- viagem %>% cbind(clusters) %>% filter(cluster == 0) %>% select(-cluster)
  # viagem %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% mapview(cex = 3)
  rm(clusters)
  
  # Se houve alteração e ainda há pontos, atualizar código de processamento da 
  # viagem e continuar
  if (nrow(viagem) < tam_vg & nrow(viagem) > 0) { 
    cp_c <- '1'; tam_vg <- nrow(viagem) 
    
    # Se viagem era trecho estacionário e todos os pontos foram retirados,
    # parar processamento e registrar código no log
  } else if (nrow(viagem) == 0) {
    # Atualizar código de processamento
    cp_c <- '2'; cp_a2 <- sprintf('%02d', cp_a)
    cod_proc <- sprintf('%s%s%sXXXXXXX', cp_a2, cp_b, cp_c)
    
    # Registrar processamento da viagem no log
    write(sprintf('%s;%s;%s', sel_trip, cp_a2, cod_proc), file = log_file, append = TRUE)
    
    # Abortar processamento desta viagem
    return(NULL)
    }
  
  # boo <- viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
  # st_write(boo, '../viagem_teste_apagar3.gpkg', driver = 'GPKG', append = FALSE)
  
  
  # ----------------------------------------------------------
  # Retirar outliers extremos de velocidade e aceleração
  # ----------------------------------------------------------
  
  # Remover outliers extremos de forma iterativa, até que não haja nenhum
  result_outliers <- retirar_outliers_extremos_iterativo(viagem, max_iteracoes = 10)
  
  # Registrar quantidade de iterações e atualizar dataframe da viagem
  qtd_iteracoes <- result_outliers[[1]]
  viagem <- result_outliers[[2]]

  # Se houve alteração, atualizar código de processamento da viagem
  if (nrow(viagem) < tam_vg) { cp_d <- '1'; tam_vg <- nrow(viagem) }
  
  # Dar uma olhada em como está a viagem
  # viagem %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
  
  
  # ----------------------------------------------------------
  # Processar viagem completa ou por trechos
  # ----------------------------------------------------------
  
  # Registrar quantidade de quebras detectadas na viagem
  quebras_de_sinal <- detectar_quebras_em_viagens(viagem, max_break_time = tempo_max_quebra)
  
  # Se a primeira linha do dataframe de viagem for uma quebra, retirá-la tanto
  # do dataframe de viagem quanto do dataframe de quebras_de_sinal
  if (1 %in% quebras_de_sinal$index) { 
    viagem <- viagem %>% slice(-1) 
    quebras_de_sinal <- quebras_de_sinal %>% slice(-1) 
  }
  
  # Registrar quantidade de quebras de sinal
  qtd_quebras <- nrow(quebras_de_sinal)
  
  
  # Se não houve quebras, faremos o map matching e o processamento da viagem toda
  if (qtd_quebras == 0) {
    # Atualizar código de processamento da viagem e finalizar código
    cp_e <- '0'; cp_a2 <- sprintf('%02d', cp_a)
    cp_viagem <- sprintf('%s%s%s%s%s', cp_a2, cp_b, cp_c, cp_d, cp_e)
    
    # Processar a viagem como um todo, fazendo map matching e registrando resultados
    processar_trecho(viagem, sel_trip, cp_a2, cp_viagem, qtd_quebras, qtd_iteracoes,
                     tempo_min_viagem, qtd_min_pontos, dist_min_viagem, 
                     active_mode = 'pedestrian',
                     pontos_elevacao, vias_ciclo, vias_restritas, semaforos,
                     pasta_viagens_gpkg, pasta_viagens_pngs, 
                     pasta_viagens_csv1, pasta_viagens_csv2) 
   

  } else if (qtd_quebras >= 5 & qtd_iteracoes >= 5) {
      # Viagens com muitas iterações de retirada de outliers e muitas quebras
      # de sinal devem ser ignoradas
      # TODO -  Qual seria o limite para ambos?
      # TODO - Retirar viagens com quebra acima de 10?

      # Atualizar código de processamento
      cp_e <- '2'; cp_a2 <- sprintf('%02d', cp_a)
      cod_proc <- sprintf('%s%s%s%s%sXXXXX', cp_a2, cp_b, cp_c, cp_d, cp_e)
      
      # Registrar processamento da viagem no log
      write(sprintf('%s;%s;%s', sel_trip, cp_a2, cod_proc), file = log_file, append = TRUE)
      
      # Abortar processamento desta viagem
      return(NULL)

  } else {
    # Houve quebra da viagem em trechos - atualizar códigos de processamento
    cp_e <- '1'
    
    # Processar cada trecho de viagem separadamente - total de trechos é qtd_quebras + 1
    for (i in seq(1:qtd_quebras)) {
      
      # Atualizar códigos de processamento
      cp_a <- cp_a + 1; cp_a2 <- sprintf('%02d', cp_a)
      cp_viagem <- sprintf('%s%s%s%s%s', cp_a2, cp_b, cp_c, cp_d, cp_e)
      
      # i <- 2
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
      processar_trecho(trecho, sel_trip, cp_a2, cp_viagem, qtd_quebras, qtd_iteracoes,
                       tempo_min_viagem, qtd_min_pontos, dist_min_viagem, 
                       active_mode = 'pedestrian',
                       pontos_elevacao, vias_ciclo, vias_restritas, semaforos,
                       pasta_viagens_gpkg, pasta_viagens_pngs, 
                       pasta_viagens_csv1, pasta_viagens_csv2) 

    }
    
    # Viagem final é a da última quebra até o final do dataframe
    trecho1 <- pull(last(quebras_de_sinal)) + 1
    trecho2 <- nrow(viagem)
    trecho <- viagem %>% slice(trecho1:trecho2) 
    # trecho %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% mapview(cex = 3)
    
    # Atualizar códigos de processamento
    cp_a <- cp_a + 1; cp_a2 <- sprintf('%02d', cp_a)
    cp_viagem <- sprintf('%s%s%s%s%s', cp_a2, cp_b, cp_c, cp_d, cp_e)
    
    # Processar trecho da viagem, fazendo map matching e registrando resultados
    processar_trecho(trecho, sel_trip, cp_a2, cp_viagem, qtd_quebras, qtd_iteracoes,
                     tempo_min_viagem, qtd_min_pontos, dist_min_viagem, 
                     active_mode = 'pedestrian',
                     pontos_elevacao, vias_ciclo, vias_restritas, semaforos,
                     pasta_viagens_gpkg, pasta_viagens_pngs, 
                     pasta_viagens_csv1, pasta_viagens_csv2)
    
    }
}


# ----------------------------------------------------------
# Testar com uma única viagem
# ----------------------------------------------------------
# rotas_de_teste <- '103770'
# walk(rotas_de_teste, processar_viagens, yellow_sp)


# ----------------------------------------------------------
# Testar com um conjunto de viagens
# ----------------------------------------------------------
# rotas <- c('209982', '297794', '303925', '039193')
# rotas_de_teste <- viagens %>% select(trip_id) %>% distinct()
# rotas_de_teste <- yellow_sp %>% sample_n(15) %>% select(trip_id) %>% distinct() %>% arrange(trip_id)

# (start = Sys.time())
# walk(rotas_de_teste$trip_id, processar_viagens, yellow_sp)
# Sys.time() - start


# ----------------------------------------------------------
# Testar com um dia inteiro
# Conversor online de timestamps: https://timestamp.online/
# ----------------------------------------------------------
yellow_sp <- yellow_sp %>% mutate(n_points = as.numeric(n_points))

# Filtrar rotas com 18+ pontos para rodar e em um determinado intervalo de tempo
rotas_de_teste <- 
  yellow_sp %>% 
  # Viagens no dia 2018-11-11
  filter(timestamps >= 1541901600 & timestamps <= 1541987999 & n_points >= 18) %>% 
  select(trip_id) %>% 
  distinct() %>% 
  arrange(trip_id)

(start = Sys.time())
walk(rotas_de_teste$trip_id, processar_viagens, yellow_sp)
Sys.time() - start

# system.time( replicate(3, walk(rotas$trip_id, processar_viagens) ) )
# system.time( replicate(3, lapply(rotas$trip_id, processar_viagens) ) )




# ----------------------------------------------------------
# Juntar e salvar resultados em arquivo único
# ----------------------------------------------------------

# Pegar nomes de todos os .csvs de resultado
result_files <- list.files(pasta_viagens_csv1, pattern = '*.csv', full.names = TRUE)

# Definir arquivo de saída
out_file <- sprintf('%s/viagens_processadas_todas.csv', pasta_base)

# Abrir cada arquivo de resultado e gravar no arquivo de saída, um por um
for (rf in seq(1, length(result_files))) {
  # Abrir arquivo de resultado
  # tmp_file <- read_delim(result_files[rf], delim = ';', col_types = 'ccdddiddiiiiiiiccdd')
  tmp_file <- read_delim(result_files[rf], delim = ';', col_types = cols(.default = "c"))
  
  # Se for o primeiro, gravar com header; se não for, anexar sem header
  if (rf != 1) {
    write_delim(tmp_file, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(tmp_file, out_file, delim = ';', append = FALSE)
  }
  
}


# # Avaliar resultados
# result_files <- list.files(pasta_viagens_csv1, pattern = '*.csv', full.names = TRUE)
# resultados <- result_files %>% map(read_delim, delim = ';', col_types = 'ccdddiddiiiiiiiccdd') %>% bind_rows()
# # Salvar arquivo .csv
# out_file <- sprintf('%s/resultados_200_viagens_clusters_trechos.csv', pasta_base)
# write_delim(resultados, out_file, delim = ';')