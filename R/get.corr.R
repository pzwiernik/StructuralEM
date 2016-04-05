get.corr <- function(T) {
  # takes a phylogenetic tree (class "igraph")
  # returns the correlation matrix by taking exp(-dij)
  E(T)$weight[((E(T)$weight>-1e-8)*(E(T)$weight<0))==1] <- 0 
  return(exp(-shortest.paths(T, v=V(T), to=V(T))))  
}