# Stat 3

u1 <- runif(15)
u2 <- runif(15)

X <- sqrt(-2 * log(1-u1)) * cos(2*pi*u2)
Y <- sqrt(-2 * log(1-u1)) * sin(2*pi*u2)

vec <- c(X,Y)
ray <- c()
u <- c()

for (d in 1:10) {
  ray[d] <- sqrt(sum((vec[3*d-2 : 3*d])^2) / 2)
  u[d] <- runif(1)
  ray[d] <- ray[d] * sign((u[d]) - 0.5) + 3
}

n <- 10

G <- function(theta) {
  2 * sum(ray - theta) - 2 * sum(1/(ray - theta))
}

DG <- function(theta) {
  -2 * n - sum(1/(ray - theta)^2)
}

theta <- 3

for(k in 1:40)
{
  newth <- theta - G(theta)/DG(theta)
  
  if(abs(newth - theta) < 0.005)
  {
    cat(ray,"\n")
    cat("Sample mean = ",mean(ray),"\n")
    cat("Sample S.D. = ",sd(ray),"\n")
    cat("MLE for Lambda = ",theta)
    break
  }
  
  theta <- newth
}