#' Moves internal nodes that represent observed variables to a leaf node.
#'
#' This function moves all observed (green) inner nodes outside.
# This is done by replacing any such node with an unobserved (red) copy which is
# zero length apart.
#
#' @param T A tree
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
#'
expand.internal.leaves <- function(T) {
  eps <- 1e-12 # probably can be zero
  for (v in as.vector(igraph::V(T))[(igraph::degree(T)>1)*(igraph::V(T)$color=="green")==1]) {
    T <- T + igraph::vertex(color="red") # adds a red node
    w <- as.vector(igraph::V(T))[(igraph::degree(T)==0)==1]
    nv <- as.vector(T[[v,]][[1]]) # neighbors of a green inner node v
    T[w,nv] <- T[v,nv] # the new node w takes all neighbours of v
    T[v,nv] <- NULL # v becomes disconnected
    T[v,w] <- eps # connect w to v by a short (zero length) edge
  }
  return(T)
}
