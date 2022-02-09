### Name: Jyotishka Ray Choudhury (BS - 1903)
### Project No.: 4

G <- read.delim("https://arnabc74.github.io/numana/data.txt" , sep="" , header=FALSE)
M <- prod(G)
S <- sum(G)
p <- 2
n <- 996
tol <- 1e-8
H <- function(p) {S*exp(digamma(p))/(n*M^(1/n)) - p}

for(k in 1:20)
{
  newp <- p - H(p)/((p+H(p))*trigamma(p)-1)
  
  if(abs(newp - p) < tol)
  {
    p <- newp
    cat("p =",p,"\n")
    a <- n*p/S
    cat("a =",a,"\n")
    break
  }
  
  p <- newp
}

##Histogram and comparison:
x <- as.numeric(as.matrix(G))
x <- sort(x)
f <- function(x) {a^p * x^{p-1} * exp(-a*x) / gamma(p)}

hist(x, breaks=100,probability = TRUE, las=1, col="#99FF33",border= "#CC3300",
      main="Histogram of data  &  the graph of f(x|p,a)",
      xlab = "Values",ylab = "Relative Frequency",
      font=2, font.lab=2)

par(new = TRUE)

plot(seq(0,3,0.005),f(seq(0,3,0.005)),axes=FALSE, 
     frame.plot=TRUE, type = "l", col="blue", lwd=3,
     xlab = " ",ylab = " ")

