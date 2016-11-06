#' Sample Discrete
#' 
#' Similar to the idea of runif, except draw from a non-uniform discrete distribution.
#' 
#' 
sampleDiscrete <- function(prob, r = 1, c = 1)
{
  n <- length(prob)
  
  if (length(r) == 0 || !is.numeric(r)) {
    r <- 1
  }
  if (length(c) == 0 || !is.numeric(c)) {
    c <- r
  }
  
  rr <- runif(c)
  m <- matrix(1, r, c)
  cumprob <- cumsum(prob)
  
  if (n < r*c) {
    for (i in 1:(n-1)) {
      m <- m + (rr > cumprob[i])
    }
  } else {
    # loop over the smaller index - can be much faster if length(prob) >> r*c
    cumprob2 <- cumprob(1:(end-1))
    for (i in 1:r) {
      for (j in 1:c) {
        m[i, j] <- sum(rr[i, j] > cumprob2) + 1
      }
    }
  }
  
  
}