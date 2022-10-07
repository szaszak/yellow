# ------------------------------------------------------------------------------
# ST-DBSCAN : An algorithm for clustering spatial-temporal data
# Baseado no artigo de Birant and Kut (2006)
# 
# Fonte: https://github.com/CKerouanton/ST-DBSCAN/blob/master/st_dbscan.R
# No GitHub, citam que código vem de outro repositório, que não existe mais

# Revisto em 20220805. A implementação difere um pouco do algoritmo do
# artigo pois inclui mais coisas como saída. Uma delas é a quantidade de pontos
# alcançáveis para cada ponto, que parece não estar funcionando tão bem, mas é
# algo que não usaremos de qualquer forma. Os nomes das variáveis foram revistas
# e criada uma opção "dry" que retorna somente a marcação dos clusters


# ------------------------------------------------------------------------------
# Breve descrição de como o algoritmo funciona, destacada diretamente do artigo:

# The algorithm starts with the ﬁrst point p in database D, and retrieves all 
# neighbors of point p within Eps distance. If the total number of these 
# neighbors is greater than MinPts—if p is a core object—a new cluster is 
# created. The point p and its neighbors are assigned into this new cluster. 
# Then, it iteratively collects the neighbors within Eps distance from the core 
# points. The process is repeated until all of the points have been processed.
# 
# In order to support temporal aspects, spatio-temporal data is ﬁrst ﬁltered by 
# retaining only the temporal neighbors and their corresponding spatial values. 
# Two objects are temporal neighbors if the values of these objects are observed 
# in consecutive time units such as consecutive days in the same year or in the 
# same day in consecutive years.


# ------------------------------------------------------------------------------
# Inputs da função:
# x      - Longitude, in degrees
# y      - Latitude, in degrees
# time   - Timestamps
# eps1   - Maximum geographical coordinate (spatial) distance value
# eps2   - Maximum non-spatial distance value
# minpts - Minimum number of points within Eps1 and Eps2 distance



# Criar clusters com base nas distâncias geográficas e temporais entre os objetos
st_dbscan = function(x, y, time, eps1, eps2, minpts, dry = FALSE) {
  
  # Contagem de linhas da base de dados
  n_points <- length(x)
  
  # Cria matrizes de distância (euclidiana) entre os latlong dos pontos e entre os tempos
  data_spatial  <- as.matrix(dist(cbind(y, x)))
  data_temporal <- as.matrix(dist(time))
  
  # n_reachables = número de pontos alcancáveis por ponto; cluster_value = valor do cluster
  n_reachables <- cluster_value <- integer(n_points)
  # Ponto faz parte de um cluster 'core object'?
  isseed <- logical(n_points)
  # Número do cluster
  cluster_number <- 0
  
  # Para cada linha da base de dados...
  for (point in 1:n_points) {
    # point <- 73
    # print(sprintf("Processing point %s of %s", point, n_points))
    
    # Quais pontos ainda não possuem um valor de cluster atribuído a eles?
    unclassified_points <- (1:n_points)[cluster_value < 1]
    
    # Se o valor de cluster_value ainda não tiver sido alterado, ou seja, se o
    # ponto ainda não faz parte de um cluster...
    if (cluster_value[point] == 0) {
      # Selecionar pontos que estão ao alcance deste, considerando uma distância de 
      # latlong e de tempo menores ou iguais às estabelecidas
      reachables <- intersect(unclassified_points[data_spatial[point, unclassified_points] <= eps1],  unclassified_points[data_temporal[point, unclassified_points] <= eps2])
      
      # Se número de pontos alcançáveis for menor do que o mínimo...
      if (length(reachables) + n_reachables[point] < minpts)
        # ... marcação do ponto em cluster_value vai sair de 0 para -1, ou seja, ele não
        # faz parte de um cluster e é entendido como ruído
        cluster_value[point] <- -1
      
      # Já se o n_pointsúmero for maior...
      else {
        # ... número do cluster (cluster_number) será atualizado e marcação do 
        # ponto em cluster_value vai sair de 0 para +1
        cluster_number <- cluster_number + 1
        cluster_value[point] <- cluster_number
        
        # ... marcação do ponto em iseed vai sair de FALSE para TRUE, demarcando
        # que ponto pertece ao um cluster 'core object'
        isseed[point] <- TRUE
        
        # Retirar próprio ponto da lista de alcançáveis
        reachables <- setdiff(reachables, point)
        # Retirar ponto da lista de pontos ainda não classificados
        unclassified_points <- setdiff(unclassified_points, point)
        
        # Todos os pontos alcançáveis vão ter adição de 1 em n_reachables, para
        # demarcar que conseguem alcançar este ponto
        n_reachables[reachables] <- n_reachables[reachables] + 1
        
        
        # Fazer o mesmo para cada ponto alcançável reachable_ptá identificado
        while (length(reachables)) {
          # Atualizar o valor do cluster value para todos os pontos alcançáveis
          cluster_value[reachables] <- cluster_number
          
          # Guardar valores de reachables em nova variável para poder usá-lo e,
          # ao mesmo tempo, criar uma saída para o loop de while()
          new_reachables <- reachables
          
          # Zerar valor do tamanho de reachables (length = 0) - no loop a seguir,
          # o valor de reachables será atualizado a partir da interseção com os
          # pontos alcançáveis que ainda tiverem o valor de cluster igual a zero.
          # Quando o valor de cluster de todos tiver sido atualizado, é esta
          # linha que vai fazer sair do loop do while
          reachables <- integer()
          
          # ... buscar os respectivos pontos alcançáveis
          for (reachable in seq(along = new_reachables)) {
            reachable_pt <- new_reachables[reachable]
            
            reachables2 <- intersect(unclassified_points[data_spatial[reachable_pt, unclassified_points] <= eps1], unclassified_points[data_temporal[reachable_pt, unclassified_points] <= eps2])
            
            # Da mesma forma que antes, se quantidade de alcançáveis for maior
            # ou igual ao tamanho mínimo de cluster (minpts)...
            if (length(reachables2) + n_reachables[reachable_pt] >= minpts) {
              # ... marcar ponto como 'seed', ou 'core object'
              isseed[reachable_pt] <- TRUE
              # ... e atualizar o cluster value (cluster_value) apenas para os pontos
              # que ainda n_pointsão haviam sido atualizados
              cluster_value[reachables2[cluster_value[reachables2] < 0]] <- cluster_number
              reachables <- union(reachables, reachables2[cluster_value[reachables2] == 0])
              # print(reachables)
            }
            
            # Retirar ponto da lista de pontos ainda não classificados
            unclassified_points <- setdiff(unclassified_points, reachable_pt)
            
            # Todos os pontos alcançáveis vão ter adição de 1 em n_reachables, para
            # demarcar que conseguem alcançar este ponto
            n_reachables[reachables2] <- n_reachables[reachables2] + 1
            
          }
        }
      }
    }
  }
  
  # Atualizar pontos marcados como não fazendo parte de clusters (cluster_value = -1) como 0
  if (any(cluster_value == -1)) {
    cluster_value[cluster_value == -1] <- 0
  }
  
  
  # Preparar saída de dados
  if (dry == FALSE) {
    # Criar lista de saída com todos os outputs do algoritmo
    out <- list(cluster = cluster_value, eps1 = eps1, minpts = minpts, density = n_reachables)
    rm(n_reachables)
    
    # Demarcar pontos que são 'seed', ou 'core object'
    if (cluster_number > 0) {
      out$isseed <- isseed
    }
    
    class(out) <- "st_dbscan"
    
  } else {
    # Saída conterá somente dados sobre clusters
    out <- list(cluster = cluster_value)
    rm(n_reachables)
  }
  
  return(out)
}


