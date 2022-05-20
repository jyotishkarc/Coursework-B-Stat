
u <- seq(0,1, by = 0.01)

probacc <- function(n,p,c){
   s <- 0
   
   for(k in 0:c){
      s <- s + choose(n,k) * p^k * (1-p)^(n-k)
   }
   return(s)
}


p1 <- sapply(u, function(val) return(probacc(240,val,2)))
p2 <- sapply(u, function(val) return(probacc(170,val,1)))
p3 <- sapply(u, function(val) return(probacc(100,val,0)))

plot(u,p1, pch = 1,
     main="Superimposed OC Curves",
     lwd = 2, ylim = c(0,1), xlab = "Fraction defective (p)", 
     ylab = "Acceptance Probability")
lines(u,p1, lwd = 2)

points(u,p2, pch = 20)
lines(u,p2, lwd = 2)

points(u,p3, pch = 4)
lines(u,p3, lwd = 2)

legend(x = "topright",
       legend = c("n = 240 , c = 2","n = 170 , c = 1","n = 100 , c = 0"),
       lwd=2, lty=c(1,1,1),
       pch=c(1,20,4), cex = 1.4)

