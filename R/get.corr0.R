get.corr0 <- function(T0) {
  # takes a phylogenetic tree (class "phylo")
  # returns the correlation matrix by taking exp(-dij)
  return(exp(-igraph::dist.nodes(T0)))
}
