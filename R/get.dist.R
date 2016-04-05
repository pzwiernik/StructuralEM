get.dist <- function(R,eps=1e-10) {
  # takes a correlation matrix with possibly negative entries, and a threshold eps>0
  # returns the distance matrix where eps is a replacement for negative values in R
  R <- R*(R>0)+eps*(R<0)
  return(-log(R))  
}