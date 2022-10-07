# Como a base é muito grande, para isolar as viagens realizadas em SP, vamos ter
# que dividi-la mês a mês, cobrindo julho de 2018 a janeiro de 2017. Este script
# cria as relações de viagens para cada mês, para serem usadas no script 
# seguinte, que efetivamente faz o filtro de viagens em SP.

# carregar bibliotecas
source('fun/setup.R')

# Estrutura de pastas
pasta_dados      <- "../yellow_dados"
pasta_viagens_sp <- sprintf("%s/01_viagens_em_sp", pasta_dados)


# Abrir a base integral original de viagens da Yellow
open_file <- sprintf('%s/tmp_viagens_yellow_formato_longo.rds', pasta_viagens_sp)
yellow    <- read_rds(open_file)


# ----------------------------------------------------------
# Criar coluna de mês para filtro (demora bastante para rodar)
# ----------------------------------------------------------

# Descartar dados de georreferenciamento - ficaremos só com trip_id e timestamps
yellow <- yellow %>% st_drop_geometry()

# Liberar espaço de memória
gc(verbose = TRUE, full = TRUE)

# Criar coluna de mês para ver distribuição de viagens
yellow <- yellow %>% mutate(time = as.POSIXlt(.$timestamps, origin = '1970-01-01'))
yellow <- yellow %>% select(-timestamps) 
# Liberar espaço de memória
gc(verbose = TRUE, full = TRUE)

yellow <- yellow %>% mutate(mes = str_sub(.$time, start = 6, end = 7))
yellow <- yellow %>% select(-time)
# Liberar espaço de memória
gc(verbose = TRUE, full = TRUE)




# ----------------------------------------------------------
# Separar viagens por mês
# ----------------------------------------------------------

meses <- c('07', '08', '09', '10', '11', '12', '01')


processar_viagens_mes <- function(mes_selecionado, df) {
  # mes_selecionado <- '07'
  
  # Avisar que processo está sendo iniciado
  message('\nProcessando mês: ', mes_selecionado, "\n")
  
  # Filtrar viagens relativas àquele mês
  viagens_mes <- df %>% filter(mes == mes_selecionado) %>% distinct()
  message('\nTotal de viagens no mês: ', nrow(viagens_mes),"\n")
  
  # Salvar resultados em um arquivo
  if (mes_selecionado != '01') {
    out_file <- sprintf('%s/viagens_yellow_2018%s.csv', pasta_viagens_sp, mes_selecionado)
  } else {
    out_file <- sprintf('%s/viagens_yellow_2019%s.csv', pasta_viagens_sp, mes_selecionado)
  }
  write_delim(viagens_mes, out_file, delim = ';')
  
}

# Rodar para todos os meses
lapply(X = meses, FUN = processar_viagens_mes, df = yellow)
