# Bibliotecas a serem carregadas
suppressPackageStartupMessages(library('tidyverse'))
suppressPackageStartupMessages(library('tidylog')) # detach("package:tidylog")
suppressPackageStartupMessages(library('data.table'))

library('mapview')
library('sf')

# Tidyverse coding style checker
# library('lintr')

# # Vão ser usadas no preparo da base, isolando viagens de SP
# library('Rcpp') # Requerido pelo s2
# library('s2')
# sf_use_s2(TRUE)
# sf_use_s2()

# Para uso no map matching com Valhalla
library('httr')
library('jsonlite')
library('geosphere')
library('googlePolylines')


# Opções gerais

# Mostrar valores sem notação científica
options(scipen = 999)
# Aumentar o número máximo de colunas exibido
options(repr.matrix.max.cols = 50)
# Facilitar o uso de negate %in%
`%nin%` = Negate(`%in%`)

# Reiniciar a sessão do RStudio
# .rs.restartR()