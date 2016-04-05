#' Structural EM function
#'
#' This is the main function that performs the structural EM algorithm.
#' @param dat The data matrix.
#' @param T0 the tree of class phylo (together with edge lengths) used as the starting point
#' @param tol absolute tolerance for convergence
#' @keywords Gaussian phylogenetic trees, Chow-Liu algortihm
#' @export
#' @examples
#' m <- 5 # number of leaves
#' N <- 30 # sample size
#' Ttr <- rtree(m,rooted=FALSE,br=rep(-log(.6),7))
#' Ttr$tip.label <- 1:m # label leaves by 1:m
#' Str <- get.corr0(Ttr)
#' Rtr <- Str[1:m,1:m]
#' dat <- mvrnorm(N, rep(0,m), Rtr)
#' D <- get.dist(cor(dat))
#' T0 <- fastme.bal(D)
#' T0$edge.length <- (T0$edge.length>0)*T0$edge.length
#' res <- strEM(dat,T0,tol=1e-6)


strEM <- function(dat,T0,tol=1e-7){
  S0 <- get.corr0(T0)
  m <- length(T0$tip.label)
  R0 <- S0[1:m,1:m]
  l_1 <- like(R0,dat)
  lval <- l_1
  l_0 <- 0
  while (abs(l_0-l_1)>tol) {
    l_0 <- l_1
    S <- E.step(dat,S0)
    res <- M.step(S)
    S0 <- res$Sig
    R <- S0[1:m,1:m]
    R <- diag(sqrt(diag(cov(dat))))%*%R%*%diag(sqrt(diag(cov(dat))))
    l_1 <- like(R,dat)
    lval <- cbind(lval,l_1)
  }
  # plot(1:length(lval),lval,main='Increase in the likelihood')
  return(res)
}

