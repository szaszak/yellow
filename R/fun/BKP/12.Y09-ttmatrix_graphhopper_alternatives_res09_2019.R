# Comando para rodar o GraphHopper no terminal - atenção para o PBF a ser carregado:
# PBF da Rede 2019; custom model ajustado (LTS)
# clear && cd /home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_src/graphhopper/ && rm -rf graph-cache/ && java -Ddw.graphhopper.datareader.file=/home/livre/Desktop/Base_GtsRegionais/GitLab/yellow_dados/07_graphhopper/03_PBFs_SP_rede_2019/20220216_sao_paulo_edited_20230521_A_infraciclo_atual.osm.pbf -jar graphhopper/web/target/graphhopper-web-*.jar server graphhopper/config-example_LTS.yml

# carregar bibliotecas
source('fun/setup.R')
library('httr')
library('jsonlite')


# Estrutura de pastas
pasta_ssd         <- "/media/livre/SSD120GB/yellow"
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_aop_alternatives_2019_2028", pasta_aop_rev)
# pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
# pasta_rts_aopt_19 <- sprintf("%s/B_2019_rotas_modeladas_alternatives", pasta_aoprv_alter)
# dir.create(pasta_ids_aopt_19, recursive = TRUE, showWarnings = FALSE)
# dir.create(pasta_rts_aopt_19, recursive = TRUE, showWarnings = FALSE)

ano <- '2019'

# ------------------------------------------------------------------------------
# Routing a partir de dois pontos com rotas alternativas (até 3 por par OD)
# ------------------------------------------------------------------------------

# Faz query de routing no GraphHopper e retorna resultados principais em dataframe,
# com rotas até 3 alternativas por par OD - aceita um dataframe de uma linha como
# entrada
gh_route_alt_full <- function(hex_id) {
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=false&calc_points=true&details=average_speed'
  # url <- 'http://localhost:8989/route/?point=-23.5314933121698%2C-46.634354542765&point=-23.5390199310058%2C-46.6376369484305&profile=bike&instructions=true&calc_points=true&algorithm=alternative_route&details=osm_way_id&details=time&details=road_class&details=road_environment&details=surface&details=bike_network&details=smoothness&details=average_slope&details=distance'
  # url <- ods_vgs %>% slice(121) %>% select(url) %>% pull()
  # Estação Vila Madalena: -23.546258,-46.690898
  # IME: -23.559007,-46.73208
  # CCSP: -23.571498,-46.639806
  
  # hex_id <- '89a81044d93ffff-89a810733b3ffff'
  # hex_id <- hex_com_vizinhos %>% slice(122) %>% select(id) %>% pull()
  df_line <- hex_com_vizinhos %>% filter(id == hex_id)
  # Encurtar hex_id - todos aqui são '89a81' + 6 caracteres de dígito ou letra = 'ffff'
  hex_id_short <- str_replace(hex_id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2')
  hex_id_base  <- str_sub(hex_id_short, 1, 6)
  
  # Fazer a GET de roteamento no Grahphopper
  # print(df_line$url)
  gh_response <- GET(df_line$url)
  
  # Mensagem tem que ser "Success: (200) OK"
  if (http_status(gh_response)$message == 'Success: (200) OK') {
    
    # Resposta da query, já colapsada e transformada em dataframe
    # Remover aviso de 'No encoding supplied: defaulting to UTF-8' na linha fromJSON()
    suppressMessages(
      response_text <- 
        # Ignorar aviso 'argument is not an atomic vector; coercing'
        suppressWarnings(str_c(content(gh_response, 'text'), collapse = ", ")) %>% 
        # Concatenar toda a string de resultados
        str_c("[", ., "]") %>% 
        # Transformar em dataframe
        fromJSON() %>% 
        as.data.frame()
    )
    
    # Nos interessa a coluna de 'paths', como um novo dataframe
    paths <- response_text$paths %>% as.data.frame()
    
    # Puxar osm_way_ids dos resultados de cada alternativa e gravar em pasta 
    # separada; puxar extensões percorridas em infra cicloviária
    dados_infra_ciclo <- data.frame()
    for (i in seq(1, length(paths$details$osm_way_id))) {
      # i <- 1
      
      # Se há osm_way_ids na rota, fazer processamento
      if (length(paths$details$osm_way_id[i][[1]]) > 0) {
        
        # ----------------------------------------------------------------------
        # Dados dos osm_way_ids
        # ----------------------------------------------------------------------
        
        # osm_ways são os ids pelos quais a rota passou, na ordem de passagem
        # A estrutura desse tipo de info retornada pelo router está aqui, mas 
        # basicamente o X1 é onde começa e o X2 onde terminam os trechos
        # https://github.com/graphhopper/graphhopper/blob/master/docs/core/technical.md
        osm_way_ids <- paths$details$osm_way_id[i] %>% as.data.frame()
        #   X1 X2        X3
        # 1  0  1 260166853
        # 2  1  3 909962485
        # 3  3  4 260166848
        # 4  4  5 172943103
        # 5  5  7 260166842
        # 6  7 10 172943409
        
        # osm_ways_dists vão ser as distâncias por trecho
        osm_ways_dists <- paths$details$distance[i] %>% as.data.frame()
        #   X1 X2       X3
        # 1  0  1 47.46184
        # 2  1  2 53.60800
        # 3  2  3 56.60200
        # 4  3  4 61.91400
        # 5  4  5 60.27100
        # 6  5  7 65.84700
        # 7  7 10 78.85340
        
        # osm_rclass vão ser as infos do tipo de viário - estamos particularmente
        # interessados quando for 'cycleway', que vão ser as ciclovias comuns e
        # expressas (não vamos puxar a diferenciação aqui, pois o valor só vai ser
        # usado para correção dos tempos em cycleway, mas caso preciso é possível
        # fazer a diferenciação via osm_way_id, acima)
        osm_rclass <- paths$details$road_class[[i]] %>% as.data.frame()
        #   V1 V2          V3
        # 1  0 10 residential
        
        # ciclofaixas vão estar demarcadas como lcn em details.bike_network
        osm_lcn <- paths$details$bike_network[[i]] %>% as.data.frame()
        #   V1 V2      V3
        # 1  0 10 missing
        
        # ciclovias expressas são demarcadas como smoothness == EXCELLENT - só
        # elas interessam
        osm_smoothies <- paths$details$smoothness[[i]] %>% as.data.frame() %>% select(V1, smoothie = V3)
        
        
        # Finalmente, a soma das distâncias dos trechos deve ser igual à distância
        # total calculada, que está em paths$distance
        # osm_ways_totdist <- sum(paths$details$distance[[i]][,3])
        
        
        # Criar dataframe para juntar todas as infos relevantes - o tamanho dele
        # em linhas é igual ao último valor da coluna X2 - 1
        osm_ways <- data.frame(X1 = seq(0, max(osm_way_ids$X2) - 1))
        
        # Juntar osm_way_ids, distâncias por osm_id, class. viária e ciclofaixas
        osm_ways <- 
          osm_ways %>% 
          # Juntar osm_way_ids
          left_join(osm_way_ids, by = 'X1') %>% 
          fill(X3) %>% 
          # Juntar distâncias por osm_way_ids
          left_join(osm_ways_dists, by = 'X1') %>% 
          # Juntar classificação viária
          mutate(X1 = as.character(X1)) %>% 
          left_join(osm_rclass, by = c('X1' = 'V1')) %>% 
          fill(V3) %>% 
          # Juntar ciclofaixas (lcn)
          left_join(osm_lcn, by = c('X1' = 'V1')) %>% 
          fill(V3.y) %>% 
          select(X1, osm_way_id = X3.x, dist = X3.y, road_class = V3.x, lcn = V3.y) %>% 
          # Como X1 é único e não deixa repetir distâncias, tudo o que for NA 
          # em dis pode ser descartado:
          filter(!is.na(dist)) %>% 
          # Juntar smoothness, para detectar ciclovias expressas
          left_join(osm_smoothies, by = c('X1' = 'V1')) %>% 
          fill(smoothie) %>% 
          mutate(hex_id = hex_id_short,
                 alt    = i,
                 .before = 'osm_way_id') %>% 
          # Demarcar infra cicloviária em coluna única
          mutate(infra_ciclo = ifelse(road_class == 'cycleway', 'ciclo_comum', 'via_comum'),
                 infra_ciclo = ifelse(lcn == 'local', 'ciclofaixa', infra_ciclo),
                 infra_ciclo = ifelse(smoothie == 'excellent', 'ciclo_expressa', infra_ciclo)) %>% 
          select(-c(X1, smoothie))
        
        
        # ----------------------------------------------------------------------
        # Extensões percorridas em vias comuns e vias com infra cicloviária
        # ----------------------------------------------------------------------
        
        # Agrupar extensões por tipo de viário percorrido
        calc_infra_ciclo <- 
          osm_ways %>% 
          group_by(hex_id, alt, infra_ciclo) %>% 
          summarise(ext = sum(dist)) %>% 
          ungroup() %>% 
          pivot_wider(id_cols = c(hex_id, alt),
                      names_from = 'infra_ciclo',
                      values_from = ext)
        
        # Checar se todas as colunas de tipo de viário estão como colunas - se não, inserir
        for (c_type in c('ciclo_expressa', 'ciclo_comum', 'ciclofaixa', 'via_comum')) {
          if (!c_type %in% names(calc_infra_ciclo)) {
            # Inserir nova coluna como NA (NA_real_, NA_character_)
            calc_infra_ciclo <- calc_infra_ciclo %>% mutate(!!c_type := 0)
          }
          
        }
        
        # Somar extensões percorridas em infra cicloviária
        calc_infra_ciclo <- 
          calc_infra_ciclo %>% 
          select(hex_id, alt, via_comum, ciclo_expressa, ciclo_comum, ciclofaixa) %>% 
          mutate(infra_ciclo = ciclo_expressa + ciclo_comum + ciclofaixa,
                 .after = 'via_comum')
        
        # Juntar resultados de infra cicloviária em dataframe de agrupamento
        dados_infra_ciclo <- rbind(dados_infra_ciclo, calc_infra_ciclo)
        
        
        # ----------------------------------------------------------------------
        # Gravar bases de osm_way_ids com extensões percorridas
        # ----------------------------------------------------------------------
        
        # Remover colunas de road_class e lcn para exportar
        osm_ways <- osm_ways %>% select(-c(road_class, lcn))
        
        # Gravar resultados agrupados por hex_id_short de origem
        osm_way_out <- sprintf('%s/%s_%s.csv', pasta_ids_aopt_19, hex_id_short, ano)
        if (file.exists(osm_way_out)) {
          write_delim(osm_ways, osm_way_out, delim = ';', append = TRUE)
        } else {
          write_delim(osm_ways, osm_way_out, delim = ';', append = FALSE)
        }
        
        
      } else {
        # Se não há osm_way_ids, é porque os pontos estão muito próximos uns dos
        # outros, mesmo que seja um osm_way_id diferente entre a origem e o 
        # destino. A distância e a velocidade calculadas vão ser zero também - 
        # pular este registro, que vai ser vazio
        return(sprintf('Pulando: %s não tem osm_way_ids (provavelmente tem distância = 0)', hex_id))
        
      }
      
    }
    
    
    # Puxar dados resumitivos de interesse das rotas
    paths <- 
      paths %>% 
      # Calcular tempo em segundos e velocidade média
      mutate(time = time / 1000,
             speed = distance / time * 3.6) %>% 
      # Descartar colunas extras - a coluna poly é o shape da rota traçada
      select(distance, weight, time, speed, poly = points)
    
    # Juntar com dados da uso da infra cicloviária
    paths <- 
      cbind(dados_infra_ciclo, paths) %>% 
      relocate(c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa), 
               .before = 'poly')
    
    # Testar polyline:
    # https://valhalla.github.io/demos/polyline/?unescape=true&polyline6=false#%0A
    
    
  } else {
    
    # Se a query no GraphHopper não deu resultados, guardar como dataframe vazio
    paths <- data.frame(hex_id    = hex_id_short,
                        alt       = NA,
                        # id_hex.x  = df_line$id_hex_x,
                        # id_hex.y  = df_line$id_hex_y,
                        distance  = NA,
                        weight    = NA,
                        time      = NA,
                        speed     = NA,
                        poly      = NA,
                        via_comum = NA,
                        infra_ciclo    = NA,
                        ciclo_expressa = NA,
                        ciclo_comum    = NA,
                        ciclofaixa     = NA
                        # lon.x     = df_line$lon.x,
                        # lat.x     = df_line$lat.x,
                        # lon.y     = df_line$lon.y,
                        # lat.y     = df_line$lat.y
    )
    
  }
  
  # Guardar resultados temporários agrupados por hex_id_short de origem
  tmp_file <- sprintf('%s/%s_modalt_%s.csv', pasta_rts_aopt_19, hex_id_short, ano)
  if (file.exists(tmp_file)) {
    write_delim(paths, tmp_file, delim = ';', append = TRUE)
  } else {
    write_delim(paths, tmp_file, delim = ';', append = FALSE)
  }
  
  # Guardar ids já processados em arquivo próprio
  df_line <- df_line %>% select(id)
  ids_processados <- sprintf('%s/tmp_00_ids_processados_%s.csv', pasta_aoprv_alter, ano)
  
  if (file.exists(ids_processados)) {
    write_delim(df_line, ids_processados, delim = ';', append = TRUE)
  } else {
    write_delim(df_line, ids_processados, delim = ';', append = FALSE)
  }
  
}


# ------------------------------------------------------------------------------
# Calcular matriz de distâncias - RStudio
# ------------------------------------------------------------------------------

# # Abrir base para routing com 26 vizinhos, manter só id e url
# hex_com_vizinhos <- sprintf('%s/00_base_para_routing_res09_26vizinhos.csv', pasta_aoprv_alter)
# hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_types = cols(.default = "c"))
# hex_com_vizinhos <- hex_com_vizinhos %>% select(id, url)
# head(hex_com_vizinhos)
# 
# 
# # # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
# # # do dataframe hex_com_vizinhos se houver
# # # library('tidylog')
# # arqs_resultados <- data.frame(arq = list.files(pasta_rotas_aopt, recursive = FALSE, full.names = FALSE))
# # arqs_resultados <- arqs_resultados %>% mutate(hex_id = str_replace(arq, '_modalt.csv', ''))
# # hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$hex_id)
# # rm(arqs_resultados)
# 
# # Arquivos já processados
# ids_processados <- sprintf('%s/tmp_00_ids_processados_%s.csv', pasta_aoprv_teste, ano)
# 
# # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
# # do dataframe hex_com_vizinhos se houver
# # library('tidylog')
# if (file.exists(ids_processados)) {
#   arqs_resultados  <- read_delim(ids_processados, delim = ';', col_types = "c")
#   hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$id)
#   rm(arqs_resultados)
#   print(nrow(hex_com_vizinhos))
# }
# 
# nrow(hex_com_vizinhos)
# head(hex_com_vizinhos, 2)
# 
# # Para cada linha de origem e destino, gerar rotas modeladas com alternativas
# detach("package:tidylog")
# 
# # Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
# # se forem só as rotas originais, é ok rodar no RStudio
# for (id in hex_com_vizinhos$id) { gh_route_alt_full(id) }


# ------------------------------------------------------------------------------
# Para rodar no Jupyter com future
# ------------------------------------------------------------------------------

detach("package:tidylog")

# Arquivos a processar
arqs <- list.files(pasta_ssd, pattern = '^tmp_base_para_routing_res09_26vizinhos_[0-9]{3}.csv', full.names = TRUE, recursive = FALSE)
# Arquivos já processados
ids_processados <- sprintf('%s/tmp_00_ids_processados_2019.csv', pasta_ssd)

for (arq in arqs) {
  # arq <- arqs[5]
  print(arq)
  
  # Abrir base para routing com 26 vizinhos, manter só id e url  
  hex_com_vizinhos <- read_delim(arq, delim = ';', col_types = cols(.default = "c"))
  
  # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
  # do dataframe hex_com_vizinhos se houver
  # library('tidylog')
  if (file.exists(ids_processados)) {
    arqs_resultados <- read_delim(ids_processados, delim = ';', col_types = "c")
    hex_com_vizinhos <- hex_com_vizinhos %>% filter(!id %in% arqs_resultados$id)
    rm(arqs_resultados)
    print(nrow(hex_com_vizinhos))
  }
  
  # Se arquivo já foi completamente processado, removê-lo e passar para o seguinte
  if (nrow(hex_com_vizinhos) == 0) { 
    file.remove(arq) 
    next
  }
  
  # Criar pastas temporárias
  folder_number <- str_replace(str_extract(arq, '[0-9]{3}.csv'), '.csv', '')
  pasta_ids_aopt_19 <- sprintf("%s/tmp_%s_osmids_%s", pasta_aoprv_alter, folder_number, ano)
  pasta_rts_aopt_19 <- sprintf("%s/tmp_%s_rotasm_%s", pasta_aoprv_alter, folder_number, ano)
  dir.create(pasta_ids_aopt_19, recursive = TRUE, showWarnings = FALSE)
  dir.create(pasta_rts_aopt_19, recursive = TRUE, showWarnings = FALSE)
  
  # Criar ttmatrix a partir do GrahHopper - melhor rodar no Jupyter se for AOP;
  # se forem só as rotas originais, é ok rodar no RStudio
  # for (id in hex_com_vizinhos$id) { gh_route_alt(id, route_options = route_options) }
  # Rodar função para todos os arquivos- multi thread (Jupyter)
  (start = Sys.time())
  future::plan(future::multicore)
  invisible(future.apply::future_lapply(X   = hex_com_vizinhos$id,
                                        FUN = gh_route_alt_full,
                                        future.seed = TRUE))
  Sys.time()
  Sys.time() - start
  
  # Remover arquivo inteiramente processado
  file.remove(arq) 
  
  # Limpar memória
  rm(hex_com_vizinhos)  
  # gc(T)
  
}


# ------------------------------------------------------------------------------
# Verificar se algum id ficou de fora do processamento
# ------------------------------------------------------------------------------

# Abrir hexágonos para SP combinados com vizinhos
hex_com_vizinhos <- sprintf("%s/00_base_para_teste_routing_res09_26vizinhos.csv", pasta_aoprv_teste)
hex_com_vizinhos <- read_delim(hex_com_vizinhos, delim = ';', col_select = 'id', col_types = 'c')
# Simplificar id do hexágono, para ficar de acordo com os resultados
hex_com_vizinhos <- hex_com_vizinhos %>% mutate(id = str_replace(id, '^89a81([a-z0-9]{6})ffff-89a81([a-z0-9]{6})ffff', '\\1-\\2'))
head(hex_com_vizinhos)

# Abrir arquivos de resultados
arqs_resultados <- data.frame(arq = list.files(pasta_rotas_aopt, recursive = FALSE, full.names = FALSE))
arqs_resultados <- arqs_resultados %>% mutate(arq = str_replace(arq, '_modalt_20[12][98].csv', ''))
arqs_resultados <- arqs_resultados %>% distinct()

# Todos os ids foram processados?
hex_com_vizinhos %>% filter(!id %in% arqs_resultados$arq)

# # Abrir arquivos de resultados
# arqs_resultados <- data.frame(arq = list.files(pasta_rotas_aopt, recursive = FALSE, full.names = TRUE))
# arqs_resultados <- map_df(arqs_resultados, read_delim, delim = ';', col_select = 'hex_id', col_types = 'c')
# arqs_resultados <- arqs_resultados %>% distinct()

# # Todos os ids foram processados?
# hex_com_vizinhos %>% filter(!id %in% arqs_resultados$hex_id)

