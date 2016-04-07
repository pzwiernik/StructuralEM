#' Get distances
#'
#' This function takes a correlation matrix with possibly negative entries, and a threshold eps>0
#' returns the distance matrix where eps is a replacement for negative values in R
#'
#' @param R a correlation matrix
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
get.dist <- function(R,eps=1e-10) {
  R <- R*(R>0)+eps*(R<0)
  return(-log(R))
}
