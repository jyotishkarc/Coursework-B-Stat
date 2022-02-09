
tic()
profvis({P <- seq(0.0001,0.9999,by = 0.0001)

check <- function(p0){
n <- 20
S <- 10000
D <- rbinom(S,n,p0)


Dtr <- (D-n*p0)/sqrt(n*p0*(1-p0))
c1 <- 0
for (i in 1:S) {
  if (abs(Dtr[i])<=1.96) {c1 = c1 + 1}
}

#return(c1*100/S)


s <- D/n
c2 <- 0
for (i in 1:S) {
  if (abs(p0 - s[i])<=1.96 * sqrt(s[i]*(1-s[i])/n)) {c2 = c2 + 1}
}
return(c2*100/S)

#out <- c(c1*100/S,c2*100/S)
}
y <- c()
#cat("c1 = ",c1*100/S,"%\n")
#cat("c2 = ",c2*100/S,"%")
for (k in 1:length(P)) {
 y[k] <- check(P[k])
}

plot(y ~ P ,type = "l")
})
#exectime <- toc()
#time.taken <- exectime$toc - exectime$tic