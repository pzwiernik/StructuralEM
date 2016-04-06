#' E-step
#'
#' This is the E-step of the structural EM algorithm.
#' It outputs the minimum cost spanning tree on the set of correlations.
#' @param X the matrix with the observations
#' @param S correlation matrix over the full vector including the hidden variables (obtained in the E-step)
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
#'
E.step <- function(X,S){
  # this is the E-step of the algorithm
  # input: the observed data X and the "big" correlation matrix S
  # output: expected sufficient statistics for the model
  m <- dim(X)[2] # number of observed nodes
  N <- dim(X)[1] # sample size
  n <- dim(S)[1] # number of nodes in the tree

  # in the E-step we compute conditional expectations of sufficient statistics
  # the sufficient statistics are explicitly mentioned in the notes
  # these are the "sample" covariances between H and X (denoted C_XH)
  # and the "sample" variance of H (denoted by C_HH)
  C <- (1/N)*t(X)%*%X
  iSX <- solve(S[1:m,1:m])
  A <- S[(m+1):n,1:m]%*%solve(S[1:m,1:m])%*%C%*%iSX%*%S[1:m,(m+1):n]
  # this is the formula for conditional expectation E( C_HH | X )
  VH_X <- S[(m+1):n,(m+1):n]-S[(m+1):n,1:m]%*%iSX%*%S[1:m,(m+1):n] + A
  V <- matrix(0,nrow=n,ncol=n)
  # this is the formula for conditional expectation E( C_XH | X )
  CXH_X <- C%*%iSX%*%S[1:m,(m+1):n]

  # we now create the whole sufficient statistics (including the observed part)
  V[1:m,1:m] <- C
  V[(m+1):n,(m+1):n] <- VH_X
  V[1:m,(m+1):n] <- CXH_X
  V[(m+1):n,1:m] <- t(CXH_X)
  D <- solve(diag(sqrt(diag(V))))
  S <- D%*%V%*%D
  # make sure that the diagonal is **exactly** 1
  S <- S-diag(diag(S))+diag(dim(S)[1])
  return(S)
}
