library('tidyverse')
library('tidylog')
library('geosphere')

# Estrutura de pastas
pasta_dados        <- "../../yellow_dados"
pasta_modelos      <- sprintf('%s/06_bases_para_modelo', pasta_dados)
pasta_base_modelo  <- sprintf('%s/C_base_para_modelo', pasta_modelos)
pasta_orig_vs_mod  <- sprintf('%s/10_rotas_originais_vs_modeladas', pasta_dados)


# ------------------------------------------------------------------------------
# Calcular distâncias em linha reta (beeline) entre origens e destinos
# ------------------------------------------------------------------------------

# Abrir viagens com latlon originais de origem e destino, já filtradas com
# somente as viagens a serem consideradas
ods_orig <- sprintf('%s/02_origens_e_destinos_com_latlon.csv', pasta_orig_vs_mod)
ods_orig <- read_delim(ods_orig, delim = ';', col_types = 'cccdddd')

# Calcular distância em linha reta entre os dois pontos
ods_orig <- ods_orig %>% mutate(dist_reta = distVincentyEllipsoid(cbind(lon.x, lat.x), 
                                                                  cbind(lon.y, lat.y)))

head(ods_orig)


# ------------------------------------------------------------------------------
# Puxar distâncias totais resultantes do map matching
# ------------------------------------------------------------------------------

# Abrir arquivos de viagens que foram consideradas nos modelos - aqui, vamos
# usar esta base só porque o que precisamos é o trip_id e a distância total
# da viagem, que constam nela e podem ser acessadas com distinct()
base_modelo <- sprintf('%s/yellow_base_para_modelo_3ptos_50vgs.csv', pasta_base_modelo)
base_modelo <- read_delim(base_modelo, delim = ';', col_types = 'ccccdddddccdiccdccdddccccccccccccc')
base_modelo <- base_modelo %>% select(trip_id, dist_mapmatch = dist_total) %>% distinct()
head(base_modelo)

# Juntar aos dados de distància originais
ods_orig <- ods_orig %>% left_join(base_modelo, by = 'trip_id')
head(ods_orig)


# ------------------------------------------------------------------------------
# Puxar distâncias das rotas calculadas pelo GraphHopper
# ------------------------------------------------------------------------------

# Abrir resultados com distâncias totais das viagens
ods_vgs <- sprintf('%s/03_ttmatrix_viagens_modeladas_a_partir_das_originais.csv', pasta_orig_vs_mod)
ods_vgs <- read_delim(ods_vgs, delim = ';', col_types = 'cccddddcc')
ods_vgs <- ods_vgs %>% select(trip_id, dist_graphhopper = distance)
head(ods_vgs)

# Juntar aos dados de distància originais
ods_orig <- ods_orig %>% left_join(ods_vgs, by = 'trip_id')
head(ods_orig)


# ------------------------------------------------------------------------------
# Calcular fatores de detour
# ------------------------------------------------------------------------------

# Manter somente colunas de interesse
ods_orig <- ods_orig %>% select(trip_id, dist_reta, dist_mapmatch, dist_graphhopper)

# Remover distâncias iguais a zero
ods_orig <- ods_orig %>% filter(dist_reta > 0 & dist_mapmatch > 0 & dist_graphhopper > 0)

# Considerar somente viagens cuja distância em linha reta seja maior do que 200m
# para manter a ideia de um filtro de viagem mínima de 300 metros (ver script 
# 05.1-map_matching.R) a um fator de detour de 1.5
ods_orig <- ods_orig %>% filter(dist_reta >= 200)

# Calcular fatores de detours de viagens reais e viagens modeladas
ods_orig <- 
  ods_orig %>% 
  mutate(detour_mm = dist_mapmatch / dist_reta,
         detour_gh = dist_graphhopper / dist_reta)

head(ods_orig)


# Retirar outliers extremos, segundo proposto por Favero e Belfiore (2021)
# Alterado da implementação em:
# https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102

# Isolar coluna de interesse - detour_mapmatching
x <- ods_orig$detour_mm
# Pegar o primeiro e quarto quantis 
qnt <- quantile(x, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(x) 
# Retirar outliers do dataframe
ods_orig <- ods_orig %>% filter(!(detour_mm < (qnt[1] - H) | detour_mm > (qnt[2] + H)))

# Isolar coluna de interesse - detour_graphhopper
y <- ods_orig$detour_gh
# Pegar o primeiro e quarto quantis 
qnt <- quantile(y, probs = c(0.25, 0.75))
# Outliers extremos estão a 3 * IQR
H <- 3 * IQR(y) 
# Retirar outliers do dataframe
ods_orig <- ods_orig %>% filter(!(detour_gh < (qnt[1] - H) | detour_gh > (qnt[2] + H)))


# Para garantir, temos algum NA?
colSums(is.na(ods_orig))


# Retirados os fatores de detour extremos
summary(ods_orig)

# trip_id            dist_reta         dist_mapmatch     dist_graphhopper 
# Length:118367      Min.   :  201.0   Min.   :  300.3   Min.   :  205.3  
# Class :character   1st Qu.:  667.3   1st Qu.:  897.5   1st Qu.:  987.8  
# Mode  :character   Median : 1013.6   Median : 1347.2   Median : 1441.7  
# Mean   : 1314.6    Mean   : 1759.6   Mean   : 1767.4  
# 3rd Qu.: 1597.8    3rd Qu.: 2140.4   3rd Qu.: 2155.0  
# Max.   :12664.0    Max.   :19805.6   Max.   :15022.7  
# 
# detour_mm        detour_gh     
# Min.   :0.5363   Min.   :0.8496  
# 1st Qu.:1.1550   1st Qu.:1.1882  
# Median :1.3053   Median :1.3451  
# Mean   :1.3647   Mean   :1.4259  
# 3rd Qu.:1.4898   3rd Qu.:1.5644  
# Max.   :2.6774   Max.   :2.7922 


# Calcular desvios padrões
# sd(ods_orig$dist_mapmatch)
sapply(ods_orig, sd)

# trip_id        dist_reta    dist_mapmatch dist_graphhopper        detour_mm 
# NA          1000.4022754     1351.6606996     1198.6585513        0.2973786 
# 
# detour_gh 
# 0.3396214 

# Gravar resultados
out_file <- sprintf('%s/04_viagens_modeladas_com_distancias_e_detours.csv', pasta_orig_vs_mod)
write_delim(ods_orig, out_file, delim = ';')


# ------------------------------------------------------------------------------
# Gráficos - distribuição das variáveis
# ------------------------------------------------------------------------------

# Detour - map matching
ods_orig %>% 
  ggplot(aes(x = detour_mm)) + 
  geom_histogram(binwidth = 0.025, colour = 'black', fill = 'orange') +
  ggtitle("Distribuição fator de detour (map matching)")

# Detour - rotas modeladas
ods_orig %>% 
  ggplot(aes(x = detour_gh)) + 
  geom_histogram(binwidth = 0.025, colour = 'black', fill = 'orange') +
  ggtitle("Distribuição fator de detour (rotas modeladas)")


