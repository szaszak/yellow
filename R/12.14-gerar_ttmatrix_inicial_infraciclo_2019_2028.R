# Processa todas as rotas que passaram por infra cicloviária, a partir dos arquivos
# separados e nomeados com base em seu id de hexágono de orgem. De forma similar,
# gera os resultados com nomes também baseados nos ids de origem. Este script
# demora mais de 8 dias para rodar para o cenário 2028, considerando rotas de até
# 40 minutos e 6 instâncias do RStudio rodando em paralelo. Isso é possível porque
# ocupa pouco processamento e RAM, então dá para abrir vários R ao mesmo tempo -
# noJupyter não consegui fazer um processo com future que funcionasse de forma
# satisfatória. O resultado é a ttmatrix inicial das rotas que passaram por
# infra cicloviária, cujos tempos devem ser ajustados nas cycleways nos scripts
# seguintes para a geração da ttmatrix final

# carregar bibliotecas
library('tidyverse')
library('tidylog')
library('sf')
library('googlePolylines')

# Estrutura de pastas
pasta_ssd         <- "/media/livre/SSD120GB/yellow"
pasta_dados       <- "../../yellow_dados"
pasta_aop_rev     <- sprintf("%s/12_aop_revisitado", pasta_dados)
pasta_aoprv_alter <- sprintf("%s/03_alternatives_2019_2028", pasta_aop_rev)
pasta_ids_aopt_19 <- sprintf("%s/A_2019_osm_way_ids_aop", pasta_aoprv_alter)
pasta_ids_aopt_28 <- sprintf("%s/C_2028_osm_way_ids_aop", pasta_aoprv_alter)

# ano <- '2019'; min_thres <- 40 #; sec_thres <- min_thres * 60
ano <- '2028'; min_thres <- 40 #; sec_thres <- min_thres * 60

if (ano == '2019') {
  pasta_tmp_osmids  <- sprintf("%s/E_%s_osm_way_ids_tmp_%s_min", pasta_ssd, ano, min_thres)
} else if (ano == '2028') {
  pasta_tmp_osmids  <- sprintf("%s/F_%s_osm_way_ids_tmp_%s_min", pasta_ssd, ano, min_thres)
}
dir.create(pasta_tmp_osmids, recursive = TRUE, showWarnings = FALSE)
rm(min_thres)

if (ano == '2019') {
  pasta_tmp_divididas <- sprintf("%s/X_%s_tmp_base_dividida", pasta_ssd, ano)
} else if (ano == '2028') {
  pasta_tmp_divididas <- sprintf("%s/Y_%s_tmp_base_dividida", pasta_ssd, ano)
}

# Pasta para colocar arquivos de ttmatrix temporários
pasta_tmp_ttmatrix <- sprintf("%s/Z_%s_tmp_ttmatrix", pasta_ssd, ano)
dir.create(pasta_tmp_ttmatrix, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Funções
# ------------------------------------------------------------------------------

# Insere as distâncias calculadas em vias comuns ou infra cicloviária no df original
dist_ciclo_map <- function(rotas_ciclo) {
  # trip_id_alt <- rotas_ciclo$alt_id[1]
  # 89a81044d93ffff-89a81046d53ffff-1
  # base_id <- '000003'
  # rotas_ciclo <- grupo_rotas
  
  # Garantir que não há linhas duplicadas
  rotas_ciclo <- rotas_ciclo %>% distinct()
  
  # Puxar osm_ids da rota modelada
  if (ano == '2019') {
    trip_osm_ids <- list.files(pasta_ids_aopt_19, pattern = base_id, recursive = FALSE, full.names = TRUE) 
  } else if (ano == '2028') {
    trip_osm_ids <- list.files(pasta_ids_aopt_28, pattern = base_id, recursive = FALSE, full.names = TRUE) 
  }
  
  trip_osm_ids <- read_delim(trip_osm_ids, delim = ';', col_types = 'ciic')
  
  # Criar um id que contemple o número da rota alternativa
  trip_osm_ids <- trip_osm_ids %>% mutate(alt_id = str_c(hex_id, alt, sep = '-'))
  # Selecionar somente os que estão em rotas_ciclo
  trip_osm_ids <- trip_osm_ids %>% filter(alt_id %in% rotas_ciclo$alt_id)
  
  # Remover osm_ids repetidos - no momento do buffer com o shapefile da rota,
  # o intersection vai pegar todos os trechos que os osm_ids aparecem e vai
  # somar essas distâncias. Osm_ids repetidos são quando a rota entra, sai e
  # entra de novo em um mesmo osm_id
  # trip_osm_ids <- trip_osm_ids %>% group_by(alt_id, osm_way_id) %>% tally() %>% ungroup() %>% select(-n)
  
  # ----------------------------------------------------------------------------
  # Calcular extensões percorridas em vias comuns e infra cicloviária
  # ----------------------------------------------------------------------------
  
  # Filtrar osm_ids da rota modelada do viário completo de SP
  viario_rota <- viario_sp %>% filter(osm_id %in% trip_osm_ids$osm_way_id)
  rm(trip_osm_ids)
  # mapview(viario_rota)
  
  # Transformar os polylines das rotas modeladas em shapefile (lista)
  shape_rotas <- decode(as.character(rotas_ciclo$poly))
  # Formatação dos pontos está fora do lugar: 
  # de -235.641 para -23.5641 - ajeitar
  shape_rotas <- shape_rotas %>% map(mutate, 
                                     lat = str_replace(lat, '\\.', ''),
                                     lon = str_replace(lon, '\\.', ''),
                                     lat = as.double(str_replace(lat, '-23', '-23.')),
                                     lon = as.double(str_replace(lon, '-46', '-46.')))
  
  # Aqui, teremos a antiga função df_latlong_to_sf, só que aplicada em uma lista
  # em vez de um dataframe único
  shape_rotas <- 
    shape_rotas %>% 
    # Transformar em sf
    map(st_as_sf, coords = c("lon", "lat"), crs = 4326) %>% 
    # Transformar pontos em linha - ver possíveis erros em
    # https://github.com/r-spatial/sf/issues/321
    # # Modo 1 - Com st_coordinates, retorna matriz
    # Retrieve coordinates in matrix form 
    # st_coordinates() %>%
    # st_linestring()
    # Modo 2 - Com summarize, retorna sf
    # Aqui, o summarize pode ser qualquer coisa, o 
    # importante é o 'do_union=FALSE'
    map(function(x) {
      x <- x %>%
        mutate(boo = 'lala') %>% 
        group_by(boo) %>% 
        summarize(m = n(), do_union = FALSE) %>% 
        select(-c(m, boo)) %>% 
        st_cast('LINESTRING') %>% 
        # Transformar em SIRGAS
        st_transform(31983) %>%
        # Calcular a extensão da rota
        mutate(dist = st_length(geometry))
      
      return(x)
    })
  # mapview(shape_rotas)
  
  # A partir de um pequeno buffer criado no polyline da rota modelada, fazer uma 
  # interseção nos osm_ids originais - isso porque os osm_ids podem ter várias 
  # quadras e o segmento percorrido ser só um trechinho dele
  buffer_rotas <- shape_rotas %>% map(st_buffer, 2)
  viario_rotas_cropped <- suppressWarnings(buffer_rotas %>% map(st_intersection, viario_rota))
  # mapview(viario_rotas_cropped) + mapview(buffer_rotas)
  rm(buffer_rotas, shape_rotas, viario_rota)
  
  # Recaulcular as extensões dos arcos (no caso, as extensões percorridas dentro
  # daquele osm_id), transformar em dataframe e isolar colunas de interesse
  viario_rotas_cropped <- viario_rotas_cropped %>% map(mutate, new_ext = as.double(st_length(geometry)), .after = 'length_m')
  viario_rotas_cropped <- viario_rotas_cropped %>% map(st_drop_geometry) %>% map(select, c(osm_id, new_ext))
  # Quantidade de linhas por dataframe
  # data.frame(n = map_int(viario_rotas_cropped, nrow))
  
  # Quantidade de ids em rotas_ciclo tem que ser a mesma do número de dataframes na lista
  if (nrow(rotas_ciclo) == length(viario_rotas_cropped)) {
    # Juntar alt_ids aos dataframes da lista, para join a seguir
    viario_rotas_cropped <- map2(viario_rotas_cropped, rotas_ciclo$alt_id, ~ mutate(.x, alt_id = .y))
    
    # Juntar infraestrutura cicloviária do ano de referência
    viario_rotas_cropped <- 
      viario_rotas_cropped %>% 
      map(function(x) {
        x %>% 
          left_join(infra_ciclo, by = 'osm_id') %>% 
          mutate(infra_ciclo = ifelse(is.na(infra_ciclo), 'via_comum', infra_ciclo))
      })
    
    # Fator de ajuste para as distâncias - vamos aplicar um proporcional geral a partir
    # da diferença entre a extensão total da rota modelada e a calculada agora
    # 1. Gerar fatores de correção para cada dataframe da lista
    fator_correcao <-
      # Puxar novas extensões de cada dataframe da lista, mantendo o id
      map_df(viario_rotas_cropped, 
             ~ summarise(.x, 
                         alt_id = first(alt_id), 
                         total_new_ext = sum(new_ext, na.rm = TRUE))) %>% 
      # Juntar com distâncias originais, vindas de rotas_ciclo
      left_join(subset(rotas_ciclo, select = c(alt_id, distance)), by = 'alt_id') %>% 
      # Calcular o fator de correção
      mutate(fator_correcao = distance / total_new_ext)
    
    # 2. Aplicar o fator de correção - cada linha corresponde será usada no 
    # respectivo dataframe, conforme a ordem da lista
    viario_rotas_cropped <- map2(viario_rotas_cropped, fator_correcao$fator_correcao, ~ mutate(.x, ext_rev = new_ext * .y))
    # sum(map_int(viario_rotas_cropped, nrow)) # número de linhas de todos os dataframes
    
    # Exportar osm_ids com infra cicloviária e extensões percorridas
    ids_out <- bind_rows(viario_rotas_cropped)
    ids_out <- 
      ids_out %>% 
      filter(infra_ciclo != 'via_comum') %>% 
      select(hex_id_alt = alt_id, osm_way_id = osm_id, infra_ciclo, new_ext, ext_rev)
    
    out_ids_tmp_file <- sprintf('%s/%s_%s_tmp_osmids.csv', pasta_tmp_osmids, ano, base_id)
    if (file.exists(out_ids_tmp_file)) {
      write_delim(ids_out, out_ids_tmp_file, delim = ';', append = TRUE)
    } else {
      write_delim(ids_out, out_ids_tmp_file, delim = ';', append = FALSE)
    }
    
    rm(ids_out, fator_correcao)
    
    
    # Agrupar extensões por tipo de viário percorrido
    viario_rotas_cropped <- 
      viario_rotas_cropped %>% 
      map(function(x) {
        x <- x %>%
          group_by(alt_id, infra_ciclo) %>% 
          summarise(ext = sum(ext_rev), .groups = 'keep') %>% 
          ungroup() %>% 
          pivot_wider(id_cols    = alt_id,
                      names_from = 'infra_ciclo',
                      values_from = ext)
        
        # Checar se todas as colunas de tipo de viário estão como colunas - se não, inserir
        for (i in c('ciclo_expressa', 'ciclo_comum', 'ciclofaixa', 'via_comum')) {
          if (!i %in% names(x)) { x <- x %>% mutate(!!i := 0) }
        }
        
        # Somar extensões percorridas em infra cicloviária
        x <- x %>% mutate(infra_ciclo = ciclo_expressa + ciclo_comum + ciclofaixa)
        # Reordenar colunas
        x <- x %>% select(alt_id, via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa)
        
        return(x)
      })
    
    # Juntar lista de dataframes em um só
    viario_rotas_cropped <- bind_rows(viario_rotas_cropped)
    
    # Juntar todas as infos ao dataframe original e reordenar colunas
    rotas_ciclo <- 
      rotas_ciclo %>% 
      left_join(viario_rotas_cropped, by = 'alt_id') %>% 
      relocate(c(via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa), 
               .before = 'poly') %>% 
      # Reconstituir hex_ids originais e coluna com a numeração da rota alternativa
      mutate(alt_id = str_replace(alt_id, 
                                  '^([a-z0-9]{6})-([a-z0-9]{6})-([0-9])', 
                                  '89a81\\1ffff-89a81\\2ffff_\\3'),
             .before = 'alt_id') %>% 
      separate(alt_id, into = c('hex_id', 'alt'), sep = '_', remove = TRUE)
    
    # Gravar resultados
    if (file.exists(out_file)) {
      write_delim(rotas_ciclo, out_file, delim = ';', append = TRUE)
    } else {
      write_delim(rotas_ciclo, out_file, delim = ';', append = FALSE)
    }
    
    
    # shape_rotas <- 
    #   shape_rotas %>% 
    #   map(st_as_sf, coords = c("lon", "lat"), crs = 4326) %>% 
    #   reduce(function(x, y) {
    #     y <- y %>% 
    #       mutate(boo = 'lala') %>% 
    #       group_by(boo) %>% 
    #       summarize(m = n(), do_union = FALSE) %>% 
    #       select(-c(m, boo)) %>% 
    #       st_cast('LINESTRING') %>% 
    #       st_transform(31983) %>% 
    #       mutate(dist = st_length(geometry))
    #     bind_rows(x, y)
    #   })
    
    
  } else {
    # Se quantidade de ids em rotas_ciclo não for a mesma que o número de dataframes,
    # temos um problema sério
    warning(sprintf('CHECAR: PROBLEMAS AO EXECUTAR CONJUNTO: %s', base_id))
    
  }
  
  
}


# ------------------------------------------------------------------------------
# Tratamento - rotas que passaram por alguma infra cicloviária (vários dias)
# ------------------------------------------------------------------------------

# Abrir cópia do viário de SP com osm_ids
viario_sp <- read_sf(sprintf('%s/tmp_sao_paulo_osm_filtrado.gpkg', pasta_aoprv_alter))
head(viario_sp)

# Abrir infra cicloviária
infra_ciclo <- sprintf('%s/tmp_infra_ciclo_%s.csv', pasta_aoprv_alter, ano)
infra_ciclo <- read_delim(infra_ciclo, delim = ';', col_types = 'cc')
head(infra_ciclo)


detach("package:tidylog")
# Arquivos a processar
arqs <- data.frame(arqs = list.files(pasta_tmp_divididas, 
                                     pattern = '^[0-9a-z]{6}_dividida_20[0-9]{2}.csv', 
                                     full.names = TRUE))
# arqs <- arqs[1:1233]
# arqs <- sample_n(arqs, 35)
# arqs <- arqs %>% slice(1:135)

# Este loop vai ocupar pouca memória RAM e processamento, então é possível rodá-lo
# em várias instâncias paralelas do RStudio ao mesmo tempo. Ainda assim, vai 
# demorar vários dias (6-8 dias) para analisar todas as rotas para SP, ao limite
# de tempo de 40 minutos e à resolução 9 de hexágono h3. O bom é que ele vai
# gerar relativamente poucos arquivos de resultado, cerca de ~8.100, ficando
# mais fácil trabalhar com eles.
(start = Sys.time())
for (arq in arqs$arqs) {
  # arq <- arqs[1]
  # print(arq)
  if (!file.exists(arq)) { next }
  
  # Puxar id do hexágono de origem
  base_id <- str_extract(arq, '[0-9a-z]{6}_dividida_20[0-9]{2}.csv') %>% str_extract('[0-9a-z]{6}')
  
  # Abrir base de rotas a serem processadas
  grupo_rotas <- read_delim(arq, delim = ';', col_types = 'cddddc')
  
  # Definir arquivo de saída
  out_file <- sprintf('%s/tmp_ttmatrix_%s_%s_infraciclo.csv', pasta_tmp_ttmatrix, base_id, ano)
  
  # Checar quais resultados já foram rodados - abrir lista, puxar ids e remover
  # do dataframe grupo_rotas se houver
  if (file.exists(out_file)) {
    arq_resultados <- read_delim(out_file, delim = ';', col_select = c('hex_id', 'alt'), col_types = "cc")
    # Reconstituir id para comparar
    arq_resultados <- arq_resultados %>% mutate(hex_id = str_replace(hex_id, '^89a81', ''),
                                                hex_id = str_replace(hex_id, 'ffff-89a81', '-'),
                                                hex_id = str_replace(hex_id, 'ffff$', ''),
                                                id = str_c(hex_id, alt, sep = '-'))
    grupo_rotas <- grupo_rotas %>% filter(!alt_id %in% arq_resultados$id)
    rm(arqs_resultados)
    
  }

  # Se arquivo já foi completamente processado, removê-lo e passar para o seguinte
  if (nrow(grupo_rotas) == 0) {
    file.remove(arq)
    next
  }
  
  # Processar rotas com infraciclo - melhor rodar no Jupyter;
  # se forem só as rotas originais, é ok rodar no RStudio
  dist_ciclo_map(grupo_rotas)
  
  
  # Remover arquivo inteiramente processado
  file.remove(arq) 
  
  # Limpar memória
  # rm(rotas_ciclo)  
  # gc(T)
  
}
Sys.time()
Sys.time() - start


# ------------------------------------------------------------------------------
# Checar se todas as rotas foram processadas
# ------------------------------------------------------------------------------

# ODs e número da rota alternativa das rotas a serem processadas é a listagem de
# ids contida no arquivo tmp_ids_rotas_vias_ciclo_20XX.csv
# Abrir rotas que só passaram por vias com infra cicloviária
rotas_vias_ciclo <- sprintf('%s/tmp_ids_rotas_vias_ciclo_%s.csv', pasta_aoprv_alter, ano)
rotas_vias_ciclo <- read_delim(rotas_vias_ciclo, delim = ';', col_types = 'c')

# Rotas processadas estão na pasta "pasta_tmp_ttmatrix" - precisamos só das duas
# primeiras colunas dos arquivos
rotas_proc <- list.files(pasta_tmp_ttmatrix, full.names = TRUE, recursive = FALSE)
rotas_proc <- map_df(rotas_proc, read_delim, delim = ';', col_select = c('hex_id', 'alt'), col_types = 'cc')
rotas_proc <- rotas_proc %>% mutate(hex_id = str_replace(hex_id, '^89a81', ''),
                                    hex_id = str_replace(hex_id, 'ffff-89a81', '-'),
                                    hex_id = str_replace(hex_id, 'ffff$', ''),
                                    hex_id_alt = str_c(hex_id, alt, sep = '-'))

# Checar: todas as rotas que tinham que ser processadas, foram?
rotas_proc %>% filter(!hex_id_alt %in% rotas_vias_ciclo$alt_id) 

# Limpar ambiente
rm(rotas_proc)
gc(T)


# ------------------------------------------------------------------------------
# Juntar arquivos ttmatrix iniciais infra_ciclo em dataframe único
# ------------------------------------------------------------------------------

# Definir arquivo de saída
if (ano == '2019') {
  out_file <- sprintf('%s/03_tmp_ttmatrix_%s_rotas_infra_ciclo.csv', pasta_aoprv_alter, ano)
} else if (ano == '2028') {
  out_file <- sprintf('%s/06_tmp_ttmatrix_%s_rotas_infra_ciclo.csv', pasta_aoprv_alter, ano)
}

# Arquivos ttmatrix divididos por hexágono
rotas_proc <- data.frame(arq = list.files(pasta_tmp_ttmatrix, full.names = TRUE, recursive = FALSE))

# Abrir arquivos um por um, remover linhas duplicadas e gravar em arquivo único
detach("package:tidylog")
for (arq in rotas_proc$arq) {
  # arq <- rotas_proc$arq[1]
  
  # Abrir arquivo e remover linhas duplicadas
  tmp_df <- read_delim(arq, delim = ';', col_types = cols(.default = "c")) %>% distinct()
  # tmp_df <- tmp_df %>% distinct(hex_id, alt)
  
  # Juntar resultados em dataframe único
  if (file.exists(out_file)) {
    write_delim(tmp_df, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(tmp_df, out_file, delim = ';', append = FALSE)
  }
  
}
