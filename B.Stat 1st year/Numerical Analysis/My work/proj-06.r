### Name: Jyotishka Ray Choudhury (BS - 1903)
### Project No.: 6

LS.qr <- function(A,b){

  if(nrow(A)<ncol(A)) {stop("ERROR: Number of rows should not be smaller than number of columns !")}
  b <- as.matrix(b)
  m <- nrow(A)
  n <- ncol(A)
  D <- matrix(nrow=n,ncol=1)    # Row vector containing the diagonal elements of R
  
  for (i in 1:n)
  {
    h <- A[i,i]
    A[i,i] <- A[i,i] - sqrt(sum(A[i:m,i]^2))    # Subtracting the norm of the i-th column of A
      
    if(sum(A[i:m,i]^2)!=0)
    {
      u <- as.matrix(A[i:m,i]/sqrt(sum(A[i:m,i]^2)))    # Constructing u
      A[i,i] <- h                                     
      
    ## Performimg Householder on each column vector:
      
      for (z in i:n)
      {
        # Efficient Householder:
        A[i:m,z] <- as.matrix(A[i:m,z]) - as.numeric((2 * t(u)) %*% as.matrix(A[i:m,z])) * u
      }
      # Same operation on the vector b:
      b[i:m,1] <- as.matrix(b[i:m,1]) - as.numeric((2*t(u)) %*% as.matrix(b[i:m,1])) * u
      
      D[i,1] <- A[i,i]
      A[i:m,i] <- u     # Storing u in place of i-th column of A
    }
    
    else
    {
      D[i,1] <- h
      cat("Warning: The least square solution isn't unique since the given matrix isn't of full column rank. \n\n")
      next
    }
      
  }
  
  ## Backward Substitution :    
    
    x <- matrix(0, nrow = n , ncol=1)
    c <- b[1:n,1]
  
    if(D[n,1]==0 & c[n]==0) {x[n,1] <- 1}
    
    else if(D[n,1]==0 & c[n]!=0) {stop("ERROR: Given matrix is not of full column rank!")}
    
    else {x[n,1] <- c[n]/D[n,1]}
  
    for (k in (n-1):1)
    {
      for (i in (k+1):n)
      {
        c[k] <- c[k] - A[k,i]*x[i,1]
      }
    
      if(D[k,1]==0 & c[k]==0) {x[k,1] <- 1}
      
      else if(D[k,1]==0 & c[k]!=0) {stop("ERROR: Given matrix is not of full column rank!")}
      
      else {x[k,1] <- c[k]/D[k,1]}
    }
  
  final <- list(A,D,x)
  names(final) <- c('Efficiently computed QR matrix :' , 'Diagonal elements of R :' , 'Least Square solution :')
  cat("\n")
  return(final)
  
}

