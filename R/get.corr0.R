#' Get the correlation matrix
#'
#' This function takes a phylogenetic tree (class "phylo")
#' returns the correlation matrix by taking exp(-dij)
#'
#' @param T A tree of class phylo
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
#'
get.corr0 <- function(T0) {
  return(exp(-ape::dist.nodes(T0)))
}
