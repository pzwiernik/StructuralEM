#' Expands high degree nodes into many ternary nodes.
#'
#' This function takes all inner nodes of degree more than 4
#' and replaces them with a small binary tree. In the current implementation every high degree node is replaced with a chain.
#' In this way this step of the procedure is quick and we can attack bigger trees.
#'
#' @param T A tree
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
#'
expand.bad.nodes <- function(T) {
  eps <- 1e-12
  for (v in as.vector(igraph::V(T))[(igraph::degree(T)>=4)==1]) {
    d <- igraph::degree(T)[v]
    for (i in 1:(d-3)) T <- T + igraph::vertex(color="red");
    # we treat differently the degree 4 and degree >4 case
    if (d>4){
      ws <- as.vector(igraph::V(T))[(igraph::degree(T)==0)==1]
      nv <- T[[v,]][[1]]
      T[ws[[1]],v] <- eps
      for (i in 1:(d-4)) {
        T[ws[[i]],nv[i+2]] <- T[v,nv[i+2]]
        T[ws[[i]],ws[[i+1]]] <- eps
      }
      T[ws[[d-3]],nv[d-1]] <- T[v,nv[d-1]]
      T[ws[[d-3]],nv[d]] <- T[v,nv[d]]
      T[v,nv[3:d]] <- NULL
    }
    if (d==4){
      ws <- as.vector(igraph::V(T)[(igraph::degree(T)==0)==1])
      nv <- as.vector(T[[v,]][[1]])
      T[ws,v] <- eps
      T[ws,nv[3]] <- T[v,nv[3]]
      T[ws,nv[4]] <- T[v,nv[4]]
      T[v,nv[3:4]] <- NULL
    }
  }
  return(T)
}

