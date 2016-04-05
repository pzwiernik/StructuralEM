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