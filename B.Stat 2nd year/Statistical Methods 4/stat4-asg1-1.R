require(MASS)

p <- 4
mu <- rep(0,p)
new.mat <- matrix(c(1,1,2,0,1,3,1,-2,0,-5,2,2,0,0,1,4),p,p)
cov.mat <- t(new.mat) %*% new.mat

m <- 10000
n <- 100  ## Update the value of n
H <- diag(n) - 1/n * matrix(1,n,n)

LRT.sigma <- c()

for (i in 1:m) {
   X <- mvrnorm(n,mu = mu, Sigma = cov.mat)
   X.bar <- as.matrix(colMeans(X))
   S <- 1/n * t(X) %*% H %*% X
   
   LRT.sigma[i] <- n * (trace.mat(solve(cov.mat) %*% S) - p - 
                           log(det(solve(cov.mat) %*% S)))
}

H <- hist(LRT.sigma , breaks = c(0:29,max(LRT.sigma)),
          main = paste("Histogram of LRT Statistic for n=",n))
G <- c(H$breaks[-length(H$breaks)],Inf)

exp.freq <- c()
obs.freq <- c()
for (i in 1:length(G)-1) {
   exp.freq[i] <- 10000 * (pchisq(G[i+1],10) - pchisq(G[i],10))
   obs.freq[i] <- H$counts[i]
}

chi.sq.value <- sum((exp.freq - obs.freq)^2 / exp.freq)
p.value <- pchisq(chi.sq.value,length(obs.freq)-1,lower.tail = FALSE)
print(chi.sq.value)
print(p.value)

trace.mat <- function(A){
   n <- ncol(A)
   Z <- 0
   
   for (i in 1:n) {
      Z <- Z + A[i,i]
   }
   
   return(Z)
}

