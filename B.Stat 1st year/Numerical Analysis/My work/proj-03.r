### Name: Jyotishka Ray Choudhury (BS - 1903)
### Project No.: 3

tol <- 1e-30
bs <- function(p) {2 * p^45 * (1-p^2)^76 * (23-100*p^2)}
a <- 0.2
b <- 0.6

for(i in 1:50)
{
  m <- (a+b)/2
  
  if(abs(bs(m))<tol)
  {
    cat("p = ",m,"\n")
    break
  }
  
  else if(sign(bs(m))==sign(bs(a))) {a <- m}
  
  else b <- m
  
}