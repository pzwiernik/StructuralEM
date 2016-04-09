#' Get a binary tree
#'
#' This function takes the tree (of class 'igraph') output by the Chow-Liu algorithm and outputs a binary tree with the
#' same observed likelihood.
#'
#' @param T A tree
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
#'
get.binary <- function(T){
  # First remove red leaves
  T<- igraph::delete.vertices(T,((igraph::V(T)$color=="red")*(igraph::degree(T)==1)==1))

  #   Second, move internal green nodes outside
  T <- expand.internal.leaves(T)

  # expand inner red nodes whose degree is more than 3
  T <- expand.bad.nodes(T)

  # contract degree two red nodes
  T <- contr.deg2(T)
  return(T)
}
