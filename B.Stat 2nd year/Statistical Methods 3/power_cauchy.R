
c1 <- qcauchy(0.975)
B <- function(theta){
  prob1 <- pcauchy(c1+theta) - pcauchy(theta-c1)
  return(1-prob1)
}

x <- (-100:100)
y1 <- B(x)

plot(y1 ~ x, type = "l")

n <- 50
c2 <- qnorm(0.975,0,pi/sqrt(4*n))
BM <- function(theta){
  prob2 <- pnorm(c2+theta,0,pi/sqrt(4*n)) - pnorm(theta-c2,0,pi/sqrt(4*n))
  return(1-prob2)
}

y2 <- BM(x)

plot(y2 ~ x , type = "l")


5