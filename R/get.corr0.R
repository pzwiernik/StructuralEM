get.corr0 <- function(T0) {
  # takes a phylogenetic tree (class "phylo")
  # returns the correlation matrix by taking exp(-dij)
  return(exp(-ape::dist.nodes(T0)))
}
