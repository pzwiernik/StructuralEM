#' The M-step of teh EM algorithm
#'
#' This function takes a data matrix and
#' returns normalized data with zero mean and unit variances.
#'
#' @param X The data matrix
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
normalize.data <- function(dat) {
  N <- dim(dat)[1]
  mdat <- (1/N)*apply(dat,2,sum)
  dat <- dat - t(kronecker(t(rep(1,N)),mdat))
  D <- (1/sqrt(N))*diag(sqrt(diag(t(dat)%*%dat)))
  dat <- dat%*%solve(D)
}
