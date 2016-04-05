is.binary <- function(T,opt=0) {
  # checks is T is a binary tree
  # opt=1 allows for degree two nodes which are not obsrved
  if (opt==1) return(prod((degree(T) %in% c(1,2,3))*((degree(T)==1)==(V(T)$color=="green"))));
  if (opt==0) return(prod((degree(T) %in% c(1,3))*((degree(T)==1)==(V(T)$color=="green"))))
}