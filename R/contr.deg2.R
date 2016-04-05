#'  CONTRACT DEGREE TWO NODES
#'
#' This is an auxiliary function that...
#' @param T a tree
#' @keywords Gaussian phylogenetic trees, Chow-Liu algortihm
#' @export
#' @examples
contr.deg2 <- function(T){
  # contracts all degree two red nodes
  igraph::E(T)$weight[((igraph::E(T)$weight>-1e-8)*(igraph::E(T)$weight<0))==1] <- 0
  while (is.binary(T)==0) {
    v <- as.vector(igraph::V(T)[((igraph::degree(T)==2)*(igraph::V(T)$color=="red"))==1])[1]
    u <- as.vector(T[[v,]][[1]][1])
    w <- as.vector(T[[v,]][[1]][2])
    T[u,w] <- T[u,v]+T[v,w]
    T <- igraph::delete.vertices(T,v)
  }
  return(T)
}

