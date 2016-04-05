# quickly compute the starting point
# in the current version we use fastme.bal()
# from teh ape package
quick.start <- function(dat){
  D <- get.dist(cor(dat))
  T0 <- ape::fastme.bal(D)
  #since the algorithm may produce negative lengths
  # we replace all those instances by zeros
  T0$edge.length <- (T0$edge.length>0)*T0$edge.length
  return(T0)
}
