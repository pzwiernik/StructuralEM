#'  CONTRACT DEGREE TWO NODES
#'
#' This is an auxiliary function that...
#' @param T a tree
#' @keywords Gaussian phylogenetic trees, Chow-Liu algortihm
#' @export
#' @examples
contr.deg2 <- function(T){
  # contracts all degree two red nodes
  E(T)$weight[((E(T)$weight>-1e-8)*(E(T)$weight<0))==1] <- 0
  while (is.binary(T)==0) {
    v <- c(V(T)[((degree(T)==2)*(V(T)$color=="red"))==1])[1]
    u <- T[[v,]][[1]][1]
    w <- T[[v,]][[1]][2]
    T[u,w] <- T[u,v]+T[v,w]
    T <- delete.vertices(T,v)
  }
  return(T)
}

