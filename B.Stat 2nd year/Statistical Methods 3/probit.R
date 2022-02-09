x <- 0:4
y <- 

f1 <- function(A,B) {
  sum(dnorm(A+B*x[i])*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))))
}

f2 <- function(A,B) {
  sum(x[i]*dnorm(A+B*x[i])*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))))
}

df1A <- function(A,B){
  sum(-dnorm(A+B*x[i])*(A+B*x[i])*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))) - dnorm(A+B*x[i])^2 * (y[i]/(pnorm(A+B*x[i]))^2 + (1-y[i])/(1-pnorm(A+B*x[i]))^2))
}

df2A <- function(A,B){
  sum(x[i]*(-dnorm(A+B*x[i])*(A+B*x[i])*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))) - dnorm(A+B*x[i])^2 * (y[i]/(pnorm(A+B*x[i]))^2 + (1-y[i])/(1-pnorm(A+B*x[i]))^2)))
}

df1B <- function(A,B){
  sum(x[i]*(-dnorm(A+B*x[i])*(A+B*x[i])*x[i]*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))) - dnorm(A+B*x[i])^2 * (y[i]/(pnorm(A+B*x[i]))^2 + (1-y[i])/(1-pnorm(A+B*x[i]))^2)))
}

df2B <- function(A,B){
  sum(x[i]*x[i]*(-dnorm(A+B*x[i])*(A+B*x[i])*x[i]*(y[i]/pnorm(A+B*x[i]) - (1-y[i])/(1-pnorm(A+B*x[i]))) - dnorm(A+B*x[i])^2 * (y[i]/(pnorm(A+B*x[i]))^2 + (1-y[i])/(1-pnorm(A+B*x[i]))^2)))
}

A <- 
B <- 
for(k in 1:40)
{
  Z <- solve(matrix(c(df1A(A,B),df1B(A,B),df2A(A,B),df2B(A,B)),2,2,byrow = TRUE)) %*% matrix(c(f1(A,B),f2(A,B)),2,1)
  newA <- A - Z[1,1]
  newB <- B - Z[2,1]
  
  if(abs(newA - A) < 0.01 & abs(newB - B) < 0.01)
  {
    cat("MLE for A = ", A,"\n")
    cat("MLE for B = ", B,"\n")
    break
  }
  
  A <- newA
  B <- newB
}
