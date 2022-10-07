# Separa as viagens que foram realizadas, no todo ou em parte, fora da cidade
# de São Paulo, por mês. A divisão por mês se deve aos limites de memória para
# processamento. O resultado é uma sequência de arquivos que listam os ids
# dessas viagens.

# carregar bibliotecas
source('fun/setup.R')

# Para preparo da base yellow para sp
# "although coordinates are longitude/latitude, st_intersects assumes that they are planar"
# https://r-spatial.github.io/sf/articles/sf7.html
# https://github.com/r-spatial/sf/issues/493
# https://github.com/r-spatial/s2/issues/99
library('Rcpp') # Requerido pelo s2
library('s2')
sf_use_s2(TRUE)
sf_use_s2()


# Estrutura de pastas
pasta_dados      <- "../yellow_dados"
pasta_viagens_sp <- sprintf("%s/01_viagens_em_sp", pasta_dados)

# Abrir a base integral original de viagens da Yellow (arquivo temporário)
open_file <- sprintf('%s/tmp_viagens_yellow_formato_longo.rds', pasta_viagens_sp)
# Este arquivo vai ser aberto para cada mês dentro da função, para conseguir 
# ficar no limite da RAM
# yellow    <- read_rds(open_file)

# Abrir shape cidade de São Paulo
open_sp_file <- '../../../Arquivos_Geosampa/LIMITESADMINISTRATIVOS/WGS84_SHP_Cidade_de_Sao_Paulo_polygon.shp'
shape_sp <- read_sf(open_sp_file)

# Checar CRS da camada
st_crs(shape_sp)$input

# Checar se polígono é válido
s2_is_valid(shape_sp)
# shape_sp %>% select(sp_nome) %>% plot()



# ----------------------------------------------------------
# Isolar viagens ocorridas fora da cidade de SP, por mês
# ----------------------------------------------------------

meses <- c('07', '08', '09', '10', '11', '12', '01')


isolar_viagens_sp_mes <- function(mes_selecionado){
  # mes_selecionado <- '07'
  
  # Avisar que processo está sendo iniciado
  message('\nProcessando mês: ', mes_selecionado,"\n")
  start_time <- Sys.time()
  start_time
  
  # Definir arquivos a serem utilizados
  if (mes_selecionado != '01') {
    trip_file <- sprintf('%s/viagens_yellow_2018%s.csv', pasta_viagens_sp, mes_selecionado)
    out_file  <- sprintf('viagens_yellow_fora_de_sp_2018%s.csv', mes_selecionado)
  } else {
    trip_file <- sprintf('%s/viagens_yellow_2019%s.csv', pasta_viagens_sp, mes_selecionado)
    out_file  <- sprintf('viagens_yellow_fora_de_sp_2019%s.csv', mes_selecionado)
  }
  
  # Só rodar se arquivo ainda não existir
  if (out_file %nin% list.files(pasta_viagens_sp)) {
    # Abrir arquivo com a seleção de viagens daquele mês
    viagens_mes <- read_delim(trip_file, delim = ';', col_types = "cc")
    viagens_mes <- viagens_mes %>% select(trip_id)

    # Abrir dataframe com todas as viagens (vai demorar)
    yellow <- read_rds(open_file)

    # Descartar coluna de timestamps, que não vamos usar agora
    yellow <- yellow %>% select(-timestamps)
    
    # Liberar espaço de memória
    gc(verbose = TRUE)

    # Filtrar viagens relativas àquele mês
    yellow <- yellow %>% filter(trip_id %in% viagens_mes$trip_id)

    # Liberar espaço de memória
    gc(verbose = TRUE)

    # Quais são as viagens que não interseccionam com o shape? Atenção: como a
    # marcação do s2_intersects se refere aos pontos (e queremos as viagens), este
    # filtro vai pegar todas as viagens para as quais há pontos fora do polígono
    # de base. Isso significa que se uma viagem tem parte dos pontos dentro do
    # polígono, ela estará marcada tanto como fazendo quanto não fazendo
    # intersecção - por isso, ela aparece aqui como não fazendo intersecção
    viagens_fora_sp <- yellow %>% filter(!s2_intersects(yellow, shape_sp, s2_options(model = 'closed')))
    viagens_fora_sp <- viagens_fora_sp %>% st_drop_geometry() %>% select(trip_id) %>% distinct()

    # Salvar resultados
    out_file_fullpath <- sprintf('%s/%s', pasta_viagens_sp, out_file)
    write_delim(viagens_fora_sp, out_file_fullpath, delim = ';')
    
    end_time <- Sys.time()
    end_time - start_time
  
  } else {
    message('Arquivo para mês ', mes_selecionado, " já existe, pulando...\n")
  }
  
}

# Rodar para todos os meses
lapply(X = meses, FUN = isolar_viagens_sp_mes)


# Apagar arquivo temporário
# file.remove(open_file)
