# a set of functions for the structural EM

get.corr0 <- function(T0) {
  # takes a phylogenetic tree (class "phylo")
  # returns the correlation matrix by taking exp(-dij)
  return(exp(-dist.nodes(T0)))  
}

get.corr <- function(T) {
  # takes a phylogenetic tree (class "igraph")
  # returns the correlation matrix by taking exp(-dij)
  E(T)$weight[((E(T)$weight>-1e-8)*(E(T)$weight<0))==1] <- 0 
  return(exp(-shortest.paths(T, v=V(T), to=V(T))))  
}


get.dist <- function(R,eps=1e-10) {
  # takes a correlation matrix with possibly negative entries, and a threshold eps>0
  # returns the distance matrix where eps is a replacement for negative values in R
  R <- R*(R>0)+eps*(R<0)
  return(-log(R))  
}

normalize.data <- function(dat) {
  # takes a data matrix
  # returns normalized data with zero mean and unit variances
  N <- dim(dat)[1]
  mdat <- (1/N)*apply(dat,2,sum)
  dat <- dat - t(kronecker(t(rep(1,N)),mdat))
  D <- (1/sqrt(N))*diag(sqrt(diag(t(dat)%*%dat)))
  dat <- dat%*%solve(D)
}

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

chow.liu <- function(S){
  # input correlation matrix
  # optput minimum spanning tree
  n <- dim(S)[1]
  
  # first define the complete graph on n vertices (CG)
  am <- matrix(1,n,n)-diag(n)
  CG <- graph.adjacency(am,mode="undirected")
  
  # this piece of code takes care of negative correlations
  if (prod(S>0)*prod(S<=1)==1) E(CG)$weight <- -log(S[lower.tri(S)])
  else {
    Lam <- S[lower.tri(S)]
    # negative correlations are replaced with small constant
    Lam[(Lam<0)==1] <- 1e-12
    Lam[(Lam>1)==1] <- 1
    E(CG)$weight <- -log(Lam)
  }
  V(CG)$names <- 1:n
  # if a tree is in the 'igraph' format, we color its nodes
  # green nodes are the observed nodes and red corresponds
  # to hidden nodes
  V(CG)$color <- "red"
  V(CG)[1:m]$color <- "green"
  
  # we have now defined a complete graph with edge lengths given by correlations
  # chow-liu calls for computing the minimum spanning tree
  return(minimum.spanning.tree(CG))
}



M.step <- function(S){

  # input - a 'big' covariance matrix
  # output - new correlation matrix
  
  # in the M-step we run chow-liu to get a new update of the tree and the big correlation matrix
  T <- chow.liu(S)
  # the problem is that the tree does not always make sense so we need to replace it with 
  # a binary tree whose leaves correspond exactly to observed nodes. Luckily such a tree, giving 
  # exactly the same observed likelihood, always exists. For details see the notes.
  T <- get.binary(T)
  return(list(Sig=get.corr(T),tree=T))
} 



is.binary <- function(T,opt=0) {
  # checks is T is a binary tree
  # opt=1 allows for degree two nodes which are not obsrved
  if (opt==1) return(prod((degree(T) %in% c(1,2,3))*((degree(T)==1)==(V(T)$color=="green"))));
  if (opt==0) return(prod((degree(T) %in% c(1,3))*((degree(T)==1)==(V(T)$color=="green"))))
}

get.binary <- function(T){
  # this function takes the tree (of class 'igraph') outputed by the chow-liu alg
  # and corrects it. Rememeber that observed nodes are green and hidden nodes are red.
  # The task is to obtain a binary tree with green leaves and red inner nodes. 
  # By construction, this tree gives the same observed likelihood as the input tree.
  
  # First remove red leaves
  T<- delete.vertices(T,((V(T)$color=="red")*(degree(T)==1)==1))
#   while (is.binary(T,1)==0) {
#     # expand degree two observed nodes
#     T <- expand.bad.nodes(T)
#   }

#   Second, move internal green nodes outside
  T <- expand.internal.leaves(T)

# expand inner red nodes whose degree is more than 3
  T <- expand.bad.nodes(T)

# contract degree two red nodes
  T <- contr.deg2(T)
  return(T)
} 

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

expand.bad.nodes <- function(T) {
  # this function takes all inner nodes of degree more than 4
  # and replaces them with a small binary tree (for details see the notes)
  # NOTE: implement Caroline's comment about joining close neighbors first
  
  # NOTE 2: in the current implementation every high degree node is replaced with a chain. 
  # In this way this step of the procedure is quick and we can attack bigger trees. 
  # I think that the iterative procedure makes no sense because it's expensive - 
  # maybe there is something smarter to do?? The current procedure works reasonably well.
  # But maybe upon convergence we could run couple of more steps in the more expensive implementation?

  
  eps <- 1e-12
  for (v in V(T)[(degree(T)>=4)==1]) {
    d <- degree(T)[v]
    for (i in 1:(d-3)) T <- T + vertex(color="red");
    # we treat differently the degree 4 and degree >4 case
    if (d>4){
      ws <- V(T)[(degree(T)==0)==1]
      nv <- T[[v,]][[1]]
      T[ws[[1]],v] <- eps
      for (i in 1:(d-4)) {
        T[ws[[i]],nv[i+2]] <- T[v,nv[i+2]]
        T[ws[[i]],ws[[i+1]]] <- eps
      }
      T[ws[[d-3]],nv[d-1]] <- T[v,nv[d-1]]
      T[ws[[d-3]],nv[d]] <- T[v,nv[d]]
      T[v,nv[3:d]] <- NULL      
    }
    if (d==4){
      ws <- V(T)[(degree(T)==0)==1]
      nv <- T[[v,]][[1]]
      T[ws,v] <- eps
      T[ws,nv[3]] <- T[v,nv[3]]
      T[ws,nv[4]] <- T[v,nv[4]]
      T[v,nv[3:4]] <- NULL            
    } 
  }
  return(T)
}


contr.deg2 <- function(T){
  # contracts all degree two red nodes
  E(T)$weight[((E(T)$weight>-1e-8)*(E(T)$weight<0))==1] <- 0 
  while (is.binary(T)==0) {
    v <- c(V(T)[((degree(T)==2)*(V(T)$color=="red"))==1])[1]
    u <- T[[v,]][[1]][1]
    w <- T[[v,]][[1]][2]
    T[u,w] <- T[u,v]+T[v,w]
    T <- delete.vertices(T,v)
  }
  return(T)
}


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

strEM <- function(dat,T0,tol=1e-7){
  # this is the main function that performs the structural EM
  # the input is the data matrix (dat) 
  # and the tree (T0) together with the lengths of its edges
  # the parameter tol is the tolerance for convergence
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







  