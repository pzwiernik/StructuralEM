#' Check if a tree is binary
#'
#' This function checks is T is a binary tree
#'
#' @param T a tree
#' @param opt either 0 or 1, opt=1 allows for degree two nodes which are not obsrved
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
is.binary <- function(T,opt=0) {
  if (opt==1) return(prod((igraph::degree(T) %in% c(1,2,3))*((igraph::degree(T)==1)==(igraph::V(T)$color=="green"))));
  if (opt==0) return(prod((igraph::degree(T) %in% c(1,3))*((igraph::degree(T)==1)==(igraph::V(T)$color=="green"))))
}
