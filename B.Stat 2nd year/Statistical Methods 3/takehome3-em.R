EM <- function(x){
  st <- mean(x)
  for (j in 1:100) {
    
    L <- 1
    p <- 0
    for (i in 1:10) {
      p1 <- 1/3 * dpois(x[i],st)
      p <- p + p1 / (p1 + 2/3 * dpois(x[i],st))
      L <- L * dpois(x[i],st)^p * dpois(x[i],2*st)^(1-p)
    }
  }
    
    
    G <- function(T) {
      sum(x)/T - 20 - p
    }
    
    DG <- function(T) {
      - sum(x)/T^2
    }
    
    
    for(k in 1:40)
    {
      newst <- st - G(st)/DG(st)
      
      if(abs(newst - st) < 0.001)
      {
        cat(rand,"\n")
        cat("Sample mean = ",mean(rand),"\n")
        cat("Sample S.D. = ",sd(rand),"\n")
        cat("MLE for Lambda = ",st,"\n")
        cat("Steps = ",k)
        break
      }
      
      st <- newst
    }
    
  }