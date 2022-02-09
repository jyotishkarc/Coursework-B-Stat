p <- 4
mu <- rep(0,p)
new.mat <- matrix(c(1,1,2,0,1,3,1,-2,0,-5,2,2,0,0,1,4),p,p)
cov.mat <- t(new.mat) %*% new.mat

m <- 10000
n <- 50  ## Update the value of n
H <- diag(n) - 1/n * matrix(1,n,n)
LRT.mu <- c()

for (i in 1:m) {
   X <- mvrnorm(n,mu = mu, Sigma = cov.mat)
   X.bar <- as.matrix(colMeans(X))
   S <- 1/n * t(X) %*% H %*% X
   
   LRT.mu[i] <- n * t(X.bar) %*% solve(cov.mat) %*% X.bar
}

H <- hist(LRT.mu , breaks = c(0:18,max(LRT.mu)),
          main = paste("Histogram of LRT Statistic for n=",n))
G <- c(H$breaks[-length(H$breaks)],Inf)

exp.freq <- c()
obs.freq <- c()
for (i in 1:length(G)-1) {
   exp.freq[i] <- 10000 * (pchisq(G[i+1],p) - pchisq(G[i],p))
   obs.freq[i] <- H$counts[i]
}

chi.sq.value <- sum((exp.freq - obs.freq)^2 / exp.freq)
p.value <- pchisq(chi.sq.value,length(obs.freq)-1,lower.tail = FALSE)
print(chi.sq.value)
print(p.value)



