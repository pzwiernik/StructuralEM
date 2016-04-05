expand.internal.leaves <- function(T) {
  # moves all green inner nodes outside
  # this is done by replacing any such node with a red copy which is 
  # zero length apart
  
  eps <- 1e-12 # probably can be zero
  for (v in V(T)[(degree(T)>1)*(V(T)$color=="green")==1]) {
    T <- T + vertex(color="red") # adds a red node
    w <- V(T)[(degree(T)==0)==1]
    nv <- T[[v,]][[1]] # neighbors of a green inner node v
    T[w,nv] <- T[v,nv] # the new node w takes all neighbours of v
    T[v,nv] <- NULL # v becomes disconnected
    T[v,w] <- eps # connect w to v by a short (zero length) edge 
  }
  return(T)
}