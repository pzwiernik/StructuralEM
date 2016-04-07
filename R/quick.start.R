#' The M-step of teh EM algorithm
#'
#' This function computes quickly the starting point
#' in the current version we use fastme.bal()
#' from teh ape package. Since the algorithm may produce negative lengths
#' we replace all those instances by zeros
#'
#' @param X The data matrix
#' @keywords structural EM algorithm
#' @export
#' @examples
#'
quick.start <- function(dat){
  D <- get.dist(cor(dat))
  T0 <- ape::fastme.bal(D)
  T0$edge.length <- (T0$edge.length>0)*T0$edge.length
  return(T0)
}
