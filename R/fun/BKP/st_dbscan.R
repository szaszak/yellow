########################################################################
# ST-DBSCAN : An algorithm for clustering spatial-temporal data        #
# (Birant and Kut, 2006)                                 #   
# Application on a trajectory                           #
# https://github.com/CKerouanton/ST-DBSCAN/blob/master/stdbscan.R
########################################################################


########################################################################
# INPUTS :                                                             #
# traj = traj gps (x, y and time)                                      #
# eps1 = distance minimum for longitude and latitude                    #
# eps2 =  distance minimum for date                                    #
# minpts = number of points to consider a cluster                      #
########################################################################


# The algorithm starts with the ﬁrst point p in database D, and retrieves all neighbors of point p within Eps
# distance. If the total number of these neighbors is greater than MinPts—if p is a core object—a new cluster is
# created. The point p and its neighbors are assigned into this new cluster. Then, it iteratively collects the neigh-
#   bors within Eps distance from the core points. The process is repeated until all of the points have been
# processed.
# 
# In order to support temporal aspects, spatio-temporal data is ﬁrst ﬁltered by retaining only the temporal
# neighbors and their corresponding spatial values. Two objects are temporal neighbors if the values of these
# objects are observed in consecutive time units such as consecutive days in the same year or in the same day
# in consecutive years.


test_trip <- st_read('../viagem_teste_dbscan.gpkg')
mapview(test_trip)

test_trip <- 
  test_trip %>% 
  as.data.frame() %>% 
  select(timestamps, geom) %>% 
  mutate(geom = as.character(geom)) %>% 
  separate(geom, into = c('lon', 'lat'), sep = ', ' ) %>% 
  mutate(lon = str_replace(lon, 'c\\(', ''),
         lat = str_replace(lat, '\\)', ''),)



# pasta_dados        <- "../yellow_dados"
# pasta_20_viagens   <- sprintf("%s/05_testes_20_viagens", pasta_dados)
# open_file <- sprintf('%s/sp_20_viagens_teste.rds', pasta_20_viagens)
# viagens <- read_rds(open_file) %>% select(-n_points)
# viagens %>% group_by(trip_id) %>% tally()
# 
# test_trip <- viagens %>% filter(trip_id == '311194')
# 
# test_trip %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% st_write('../viagem_teste_apagar.gpkg', driver = 'GPKG', append = FALSE)


# x = data longitude 
x <- test_trip$lon
# y = data latitude 
y <- test_trip$lat
# time = data timestamps
time <- test_trip$timestamps


# Cada grau de latitude corresponde a cerca de:
# https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
#      1 grau  ~ 111.139  metros
#    0,1 grau  ~ 11.113,9 metros
#   0,01 grau  ~ 1.111,39 metros
#  0,001 grau  ~  111,139 metros
# 0,0001 grau  ~  11,1139 metros
# 0,00009 grau ~  10 metros

# Eps1 é o parâmetro de distância para atributos espaciais (latitude e longitude),
# usado para medir a proximidade de dois pontos geograficamente. É a distância 
# máxima do raio a partir do ponto para que pontos adjacentes sejam reconhecidos 
# ou não como vizinhos. No caso, estamos usando a distância máxima em graus
eps1 <- 0.0001

# Eps2 é o parâmetro de distância para atributos não espaciais, usado para medir
# a similaridade desses valores. No caso, estamos usando atributos temporais,
# medidos em uma escala de segundos
eps2 <- 90
eps2 <- 180 # Artigo Facing the needs for clean bicycle data
eps2 <- 10

# MinPts é o número mínimo de pontos (integer) dentro dos intervalos máximos 
# definidos em Eps1 e Eps2 para que o grupo seja classificado como um cluster
# minpts <- 15
minpts <- 18 # GPS Yellow em 1 pont oa cada 5 seg -> 18 equivale a ~90 seg

# cldensity (boolean) habilita a exibição de pontos alcançáveis para cada ponto
# dentro de um cluster. TRUE por padrão

this <- stdbscan(x, y, time, eps1, eps2, minpts)
boo <- data.frame(cluster_number  = this$cluster,
                  cluster_density = this$density,
                  cluster_isseed  = this$isseed)

lala <- test_trip %>% cbind(boo) %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)
# mapview(lala, zcol = 'cluster_number', cex = 3)

lala %>% filter(cluster_number == 2) %>% mapview()

stdbscan = function(x, 
                    y, 
                    time, 
                    eps1, 
                    eps2, 
                    minpts, 
                    cldensity = TRUE) { 

  # Contagem de linhas da base de dados
  countmode = 1:length(x)
  # Tudo está sendo marcado como seeds = TRUE?
  seeds = TRUE

  # Marizes de distância (euclidiana) entre os latlong dos pontos e entre os tempos
  data_spatial  <- as.matrix(dist(cbind(y, x)))
  data_temporal <- as.matrix(dist(time))
  # Contagem de linhas da base de dados de distância
  n <- nrow(data_spatial)

  # classn = densidade do objeto; cv = valor (número) do cluster
  classn <- cv <- integer(n)
  # Ponto faz parte de um cluster 'core object'?
  isseed <- logical(n)
  # Número do cluster
  cn <- integer(1)

  # Para cada linha da combinação entre base de dados de latlong e da matriz de distância...
  for (i in 1:n) {
    if (i %in% countmode)
      
      # cat("Processing point ", i, " of ", n, ".\n")
      unclass <- (1:n)[cv < 1]

    # Se o valor de cv ainda não tiver sido alterado, ou seja, de ponto ainda
    # não faz parte de um cluster...
    if (cv[i] == 0) {
      # Quais os pontos que estão ao alcance deste, considerando uma distância de 
      # latlong e de tempo menores ou iguais às estabelecidas?
      reachables <- intersect(unclass[data_spatial[i, unclass] <= eps1],  unclass[data_temporal[i, unclass] <= eps2])
      # Se número de pontos alcançáveis for menor do que o mínimo...
      if (length(reachables) + classn[i] < minpts)
        # ... marcação do ponto em cv vai sair de 0 para -1, ou seja, ele não
        # faz parte de um cluster
        cv[i] <- -1
      # Já se o número for maior...
      else {
        # ... número do cluster (cn) será atualizado e marcação do ponto em cv 
        # vai sair de 0 para +1
        cn <- cn + 1
        cv[i] <- cn
        # ... marcação do ponto em iseed vai sair de FALSE para TRUE, demarcando
        # que ponto pertece ao um cluster 'core object'
        isseed[i] <- TRUE
        # Retirar próprio ponto da lista de alcançáveis e de unclass
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)
        # Todos os pontos alcançáveis vão ser marcados como 1 em vez de 0 em classn,
        # significando que todos fazem parte do cluster identificado
        classn[reachables] <- classn[reachables] + 1
        
        # Para cada ponto alcançável já identificado...
        while (length(reachables)) {
          # ... atualizar o valor do cluster value (cv) para igual ao número do 
          # cluster atual (cn)
          cv[reachables] <- cn
          ap <- reachables
          reachables <- integer()

          # ... buscar os respectivos pontos alcançáveis
          for (i2 in seq(along = ap)) {
            j <- ap[i2]

            jreachables <- intersect(unclass[data_spatial[j, unclass] <= eps1], unclass[data_temporal[j, unclass] <= eps2])
            
            # Da mesma forma que antes, se quantidade de alcançáveis for maior
            # ou igual ao tamanho mínimo de cluster (minpts)...
            if (length(jreachables) + classn[j] >= minpts) {
              # ... marcar ponto como 'seed', ou 'core object'
              isseed[j] <- TRUE
              # ... e atualizar o cluster value (cv) apenas para os pontos
              # que ainda não haviam sido atualizados
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
    
  }
  
  # Atualizar pontos marcados como não fazendo parte de clusters (cv = -1) como 0
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  # Criar lista de saída com todos os outputs do algoritmo
  out <- list(cluster = cv, eps1 = eps1, minpts = minpts, density = classn)
  rm(classn)
  # Demarcar pontos que são 'seed', ou 'core object'
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "stdbscan"
  return(out)
}
