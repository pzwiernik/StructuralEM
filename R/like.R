#' Compute the likelihood function
#'
#' This function computes the Gaussian likelihood function
#'
#' @param S covariance matrix
#' @param X teh data matrix
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
like <- function(S,X){
  N <- dim(X)[1]
  C <- (1/N)*t(X)%*%X
  llike <- -log(det(S))-sum(diag(C%*%solve(S)))
  # the likelihood is not multiplied by N/2.
  # The reason is that then the tolerance for the convergence
  # of teh EM algorithm can be set uniformly.
  return(llike)
}
