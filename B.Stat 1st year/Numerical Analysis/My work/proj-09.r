### Name: Jyotishka Ray Choudhury (BS - 1903)
### Project No.: 9

crout <- function(A,b){
  
  if(nrow(A)!=ncol(A)) {stop("The coefficient matrix must be square !")}
    
  n <- nrow(A)
  
## Crout's Decompostion :
  
  for (m in 2:n)
  {
      if(A[1,1]!=0)  {A[1,m] <- A[1,m]/A[1,1]}
    
      else  {stop("LU decomposition doesn't exist.")}
  }

  for(j in 2:(n-1))
  {
    #Computing the columns of l :  
      for (i in j:n)
      {
        for (k in 1:(j-1))
        {
          A[i,j] <- A[i,j] - A[i,k] * A[k,j]
        }
      }
  
    #Computing the columns of U :
      for (i in (j+1):n)
      {
        for (k in 1:(j-1))
        {
          A[j,i] <- A[j,i] - A[j,k]*A[k,i]
        }
        
        if(A[1,1]!=0)  {A[j,i] <- A[j,i]/A[j,j]}
        
        else  {stop("LU decomposition doesn't exist.")}
    
      }
  }
    
  for (m in 1:(n-1))
  {
      A[n,n] <- A[n,n] - A[n,m]*A[m,n]
  }
    
    
## Forward Substitution :
  
  if(length(b)!=n) {stop("The constant vector is of wrong length.")}
    
  x <- rep(0,n)
  
  if(A[1,1]==0 & b[1]==0)
  {
    x[1] <- 1
    cat("WARNING: The solution won't be unique.")
  }
  
  else if(A[1,1]==0 & b[1]!=0)  {stop("ERROR: Inconsistent system.")}
  
  else  {x[1] <- b[1]/A[1,1]}
  
    for (k in 2:n)
    {
        for (i in 1:(k-1))
        {
          b[k] <- b[k] - A[k,i]*x[i] 
        }
      
        if(A[k,k]==0 & b[k]==0)
        {
          x[k] <- 1
          cat("Warning: The solution won't be unique.")
        }
      
        else if(A[k,k]==0 & b[k]!=0)  {stop("ERROR: Inconsistent system.")}
      
        else  {x[k] <- b[k]/A[k,k]}
    }

## Backward Substitution :    

    for (k in (n-1):1)
    {
      for (i in (k+1):n)
      {
        x[k] <- x[k] - A[k,i]*x[i]
      }
    }
  
  x <- as.matrix(x[1:n])
  result <- list(A,x)
  names(result) <- c('Efficiently computed LU matrix :' , 'Solution to the system :')
  cat("\n")
  return(result)  
}
