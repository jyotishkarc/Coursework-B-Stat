### Author : Jyotishka Ray Choudhury
### Roll no. : BS 1903

test.conf <- function(p0){
n <- 20
S <- 10000
D <- rbinom(S,n,p0)

Dtr <- (D-n*p0)/sqrt(n*p0*(1-p0))
c1 <- 0
for (i in 1:S) {
  if (abs(Dtr[i])<=1.96) {c1 = c1 + 1}
}

s <- D/n
c2 <- 0
for (i in 1:S) {
  if (abs(p0 - s[i])<=1.96 * sqrt(s[i]*(1-s[i])/n)) {c2 = c2 + 1}
}

cat("For p0 = ",p0," :\n")
cat("Rejection proportion = ",1-c1/S,"\n")
cat("Coverage percentage = ",c2*100/S,"%\n\n")
}

p0 <- c(0.10,0.25,0.50,0.75,0.90)

for(k in 1:5){
  test.conf(p0[k])
}