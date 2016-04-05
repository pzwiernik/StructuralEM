#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'

chow.liu <- function(S){
  # input correlation matrix
  # optput minimum spanning tree
  n <- dim(S)[1]

  # first define the complete graph on n vertices (CG)
  am <- matrix(1,n,n)-diag(n)
  CG <- igraph::graph.adjacency(am,mode="undirected")

  # this piece of code takes care of negative correlations
  if (prod(S>0)*prod(S<=1)==1) igraph::E(CG)$weight <- -log(S[lower.tri(S)])
  else {
    Lam <- S[lower.tri(S)]
    # negative correlations are replaced with small constant
    Lam[(Lam<0)==1] <- 1e-12
    Lam[(Lam>1)==1] <- 1
    igraph::E(CG)$weight <- -log(Lam)
  }
  igraph::V(CG)$names <- 1:n
  # if a tree is in the 'igraph' format, we color its nodes
  # green nodes are the observed nodes and red corresponds
  # to hidden nodes
  igraph::V(CG)$color <- "red"
  igraph::V(CG)[1:m]$color <- "green"

  # we have now defined a complete graph with edge lengths given by correlations
  # chow-liu calls for computing the minimum spanning tree
  return(igraph::minimum.spanning.tree(CG))
}

