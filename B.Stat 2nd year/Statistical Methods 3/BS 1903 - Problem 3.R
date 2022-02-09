
psum <- function(B=2,i){
  fsum <- 0
  for (j in 0:i) {
    fsum <- fsum + 1/3*exp(-B) * B^j / factorial(j) + 2/3*exp(-2*B) * (2*B)^j / factorial(j)
  }
  return(fsum)
}

z <- c()
rand <- c()

for (h in 1:10) {
  z[h] <- runif(1)
  k <- 0
  while (psum(2,k) < z[h]) { k <- k+1 }
  
  rand[h] <- k  
}

### Estimation of lambda :

G <- function(T) {
  sum(rand)/T - 20 + sum(1/(1+2*exp(-T)*2^rand))
}

DG <- function(T) {
  -sum(rand)/T^2 + sum((2 * exp(-T) * 2^rand)/(1+2 * exp(-T) * 2^rand)^2)
}

p <- 2

for(k in 1:40)
{
  newp <- p - G(p)/DG(p)
  
  if(abs(newp - p) < 0.01)
  {
    cat(rand,"\n")
    cat("Sample mean = ",mean(rand),"\n")
    cat("Sample S.D. = ",sd(rand),"\n")
    cat("2-sigma interval = (",mean(rand)-2*sd(rand)," , ",mean(rand)+2*sd(rand),")\n")
    cat("MLE for Lambda = ",p)
    break
  }
  
  p <- newp
}
