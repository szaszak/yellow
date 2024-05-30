library('tidyverse')
library('tidylog')
source('fun/st_dbscan.R')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_atrib_viario <- sprintf('%s/04_atributos_viario', pasta_dados)
pasta_map_matching <- sprintf("%s/05_map_matching", pasta_dados)
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)


# Puxar listagem de viagens originais (latlon virá do map matching)
pasta_mm_1 <- sprintf('%s/201811/viagens_processadas_csv', pasta_map_matching)
pasta_mm_2 <- sprintf('%s/201812/viagens_processadas_csv', pasta_map_matching)
pasta_mm_3 <- sprintf('%s/201901/viagens_processadas_csv', pasta_map_matching)
mm_files1 <- list.files(pasta_mm_1, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files2 <- list.files(pasta_mm_2, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files3 <- list.files(pasta_mm_3, pattern = '^([0-9]{6})_([0-9]{2}).csv', recursive = FALSE, full.names = TRUE) %>% as.data.frame()
mm_files <- rbind(mm_files1, mm_files2, mm_files3) %>% rename(arq = '.')
rm(mm_files1, mm_files2, mm_files3, pasta_mm_1, pasta_mm_2, pasta_mm_3)


# Abrir arquivo com os atributos de viário agregados
atrib_viario <- sprintf('%s/00_listagem_viario_com_todos_atributos.csv', pasta_atrib_viario)
atrib_viario <- read_delim(atrib_viario, delim = ';', col_types = 'ccddcdididdiddccccccici')
atrib_viario <- atrib_viario %>% distinct() %>% select(osm_id, infra_ciclo) %>% distinct()
# Checar se osm_ids possuem apenas uma marcação de infra_cicloviária - sim 
# atrib_viario %>% group_by(osm_id, infra_ciclo) %>% tally() %>% filter(n > 1)
# Padronizar nomes das estruturas com análises posteriores
atrib_viario <- atrib_viario %>% mutate(infra_ciclo = case_when(infra_ciclo == 'ciclovia' ~ 'ciclo_comum',
                                                                infra_ciclo == 'ciclofaixa' ~ 'ciclofaixa',
                                                                infra_ciclo == 'expressa' ~ 'ciclo_expressa',
                                                                infra_ciclo == 'sem_infra_ciclo' ~ 'via_comum'))
head(atrib_viario)



# Abrir origens e destinos das rotas iniciais da Yellow - são consideradas aqui
# (pelos scripts anteriores) as rotas que (a) tiveram algum trecho considerado
# no modelo; (b) que não foram divididas em trechos menores; e (c) em que o
# trecho único considerado é o inicial (possui trip_id com _00)
ods_vgs <- sprintf('%s/01_origens_e_destinos_viagens_consideradas.csv', pasta_orig_vs_mod)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = cols(.default = "c"))
# Nos interessam os trip_ids
ods_vgs <- ods_vgs %>% select(trip_id) %>% distinct()
head(ods_vgs)


# Arquivo de saída
out_file <- sprintf('%s/02_uso_de_estrutura_cicloviaria_rotas_originais.csv', pasta_orig_vs_mod)

# Para cada viagem, puxar os osm_ids percorridos e sua extensões, agregar dados
# da infra cicloviária e exportar os resultados
detach("package:tidylog")
for (id in ods_vgs$trip_id) { 
  # id <- ods_vgs$trip_id[231]
  
  # Abrir a rota original (map matching)
  rota <- mm_files %>% filter(str_detect(arq, pattern = sprintf('%s.csv', id))) %>% pull()
  rota <- read_delim(rota, delim = ';', col_types = 'cidddddddicddcccdii')
  rota <- rota %>% select(trip_id, osm_id = edges.way_id, edges.length)
  
  # Clusterizar distâncias percorridas em cada osm_way_id - para isso, manter
  # a ordem do trajeto (criar seq_order) e clusterizar a partir das distâncias
  rota <- rota %>% add_column(seq_order = 1:nrow(.), .after = 'trip_id')
  rota <- rota %>% mutate(cluster = unlist(st_dbscan(x    = rota$osm_id,
                                                     y    = rota$edges.length, 
                                                     time = rota$seq_order, 
                                                     eps1 = 0,
                                                     eps2 = 1, # distância em seq_order tem de ser 1
                                                     minpts = 1, # cluster pode ter 1 ponto, sem problemas
                                                     dry  = TRUE)))
  
  # Somar as distâncias por osm_way_id
  rota <- rota %>% select(-seq_order) %>% distinct()
  rota <- 
    rota %>% 
    group_by(trip_id, osm_id) %>% 
    summarise(dist = sum(edges.length), .groups = 'keep') %>% 
    ungroup()
  
  # sum(rota$dist)
  
  # Associar aos atributos de infra cicloviária
  rota <- rota %>% left_join(atrib_viario, by = 'osm_id')
  
  # Agrupar extensões por tipo de viário percorrido
  rota <- 
    rota %>% 
    group_by(infra_ciclo) %>% 
    summarise(ext = sum(dist)) %>% 
    ungroup() %>% 
    mutate(trip_id = id) %>% 
    pivot_wider(id_cols = trip_id,
                names_from = 'infra_ciclo',
                values_from = ext)
  
  # Checar se todas as colunas de tipo de viário estão como colunas - se não, inserir
  for (i in c('ciclo_expressa', 'ciclo_comum', 'ciclofaixa', 'via_comum')) {
    if (!i %in% names(rota)) {
      # Inserir nova coluna como NA (NA_real_, NA_character_)
      rota <- rota %>% mutate(!!i := 0)
    }
  }
  
  # Somar extensões percorridas em infra cicloviária
  rota <- rota %>% mutate(infra_ciclo = ciclo_expressa + ciclo_comum + ciclofaixa)
  rota <- rota %>% select(trip_id, via_comum, infra_ciclo, ciclo_expressa, ciclo_comum, ciclofaixa)
  
  # Guardar resultados
  if (file.exists(out_file)) {
    write_delim(rota, out_file, delim = ';', append = TRUE)
  } else {
    write_delim(rota, out_file, delim = ';', append = FALSE)
  }
  
}

