#' The M-step of teh EM algorithm
#'
#' This function performs teh M-step of the EM algorithm.
#' In the M-step we run chow-liu to get a new update of the tree and the big correlation matrix.
#' The problem is that the tree does not always make sense so we need to replace it with
#' a binary tree whose leaves correspond exactly to observed nodes. Luckily such a tree, giving
#' exactly the same observed likelihood, always exists.
#'
#' @param S covariance matrix over the whole vector (both observed and hidden variables)
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
M.step <- function(S){
  T <- chow.liu(S)
  T <- get.binary(T)
  return(list(Sig=get.corr(T),tree=T))
}
