like <- function(S,X){
  # computes the Gaussian likelihood function
  # S - covariance, X - data 
  N <- dim(X)[1]
  C <- (1/N)*t(X)%*%X
  llike <- -log(det(S))-sum(diag(C%*%solve(S)))
  # the likelihood is not multiplied by N/2. 
  # The reason is that then the tolerance for the convergence
  # of teh EM algorithm can be set uniformly.
  return(llike)
}