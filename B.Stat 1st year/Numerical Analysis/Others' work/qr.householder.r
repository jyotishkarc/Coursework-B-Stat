qr.householder <- function(A) {
  require(Matrix)
  library(Matrix)
  R <- as.matrix(A) # Set R to the input matrix A
  
  n <- ncol(A)
  m <- nrow(A)
  H <- list() # Initialize a list to store the computed H matrices to calculate Q later
  
  if (m > n) {
    c <- n
  } else {
    c <- m
  }
  
  for (k in 1:c) {
    x <- R[k:m,k] # Equivalent to a_1
    e <- as.matrix(c(1, rep(0, length(x)-1)))
    vk <- sign(x[1]) * sqrt(sum(x^2)) * e + x
    
    # Compute the H matrix
    hk <- diag(length(x)) - 2 * as.vector(vk %*% t(vk)) / (t(vk) %*% vk)
    if (k > 1) {
      hk <- bdiag(diag(k-1), hk)
    }
    
    # Store the H matrix to find Q at the end of iteration
    H[[k]] <- hk
    
    R <- hk %*% R
  }
  
  Q <- Reduce("%*%", H) # Calculate Q matrix by multiplying all H matrices
  res <- list('Q'=Q,'R'=R)
  return(res)
}