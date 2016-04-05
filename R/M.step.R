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