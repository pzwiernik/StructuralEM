get.corr <- function(T) {
  # takes a phylogenetic tree (class "igraph")
  # returns the correlation matrix by taking exp(-dij)
  igraph::E(T)$weight[((igraph::E(T)$weight>-1e-8)*(igraph::E(T)$weight<0))==1] <- 0
  return(exp(-igraph::shortest.paths(T, v=igraph::V(T), to=igraph::V(T))))
}
