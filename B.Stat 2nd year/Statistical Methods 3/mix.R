## Name : Jyotishka Ray Choudhury
## Roll No. : BS 1903
## Problem No. 1
a <- 7
b <- 1

X <- runif(20)
Y <- c()
for (i in 1:20) {
  Y[i] <- rpois(1 , exp(a + b * X[i]))
}

f <- function(a,b){
  S <- 0
  for (i in 1:20) {
     S <- S + Y[i] - exp(a + b*X[i])
  }
  return(S)
}

g <- function(a,b){
  S <- 0
  for (i in 1:20) {
    S <- S + Y[i] * X[i] - X[i] * exp(a + b*X[i])
  }
  return(S)
}

a <- 7.01036
b <- 0.99251
V<- c()
count <- 0

for (k in 1:100) {
  S <- matrix(0,2,2)
  for (i in 1:20) {
    S <- S + exp(a + b*X[i]) *  matrix(c(1,X[i],X[i],X[i]^2),2,2)
  }
    V <- as.matrix(c(a,b)) + solve(S) %*% as.matrix(c(f(a,b) , g(a,b))) 
    diff <- V - as.matrix(c(a,b))
    
  tol <- 10^(-10)
  
  if(sqrt(sum(diff^2)) < tol){
    cat("\n alpha = ",V[1], " \n")
    cat("\n beta = ",V[2], " \n")
    cat("\n Tolerance = ",tol,"\n")
    cat("No. of iterations = ",count+1,"\n\n")
    break
  }
  
  count <- count + 1
  a <- V[1]
  b <- V[2]
}

           