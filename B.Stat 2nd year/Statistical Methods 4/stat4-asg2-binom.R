## Author : Jyotishka Ray Choudhury
## Roll no : BS 1903

binom.ci <- function(n,p){
   len <- 10000
   z <- qnorm(0.975)
   
   ee <- rbinom(len , n , p) / n
      
   a <- (sin(asin(sqrt(ee)) - z * 1/sqrt(4 * n)))^2
   b <- (sin(asin(sqrt(ee)) + z * 1/sqrt(4 * n)))^2

   counter <- 0
   for (k in 1:len) {
      if (a[k] <= p && b[k] >= p ) {
         counter <- counter + 1
      }
   }
   
   return(counter / len)
}

tt <- seq(30,1000,25)
m <- c()

graph <- function(p){
   for (i in 1:length(tt)) {
      s <- c()
      for (j in 1:1){
      s[j] <- binom.ci(tt[i] , p)
      }
   
      m[i] <- mean(s)
   }        
   return(m)
}

res1 <- graph(0.05)
res2 <- graph(0.5)
   
d1 <- data.frame(x = tt , y = res1)
d2 <- data.frame(x = tt , y = res2)
dd <- rbind(cbind(d1, case = "p = 0.05"), cbind(d2, case = "p = 0.5     "))
ddsmooth <- ddply(dd, .(case), function(k) as.data.frame(spline(k)))

resplot <- ggplot(dd, aes(x, y)) +
   geom_line(aes(colour = case, size = case), data = ddsmooth) +
   ggtitle("Samples from Bernoulli (n,p)\n") +
   scale_y_continuous(limits=c(0.75, 1)) +
   scale_colour_manual(values = c("red", "blue"), name = "Value of p :\n") +
   scale_size_manual(values = c(1, 1), name = "Value of p :\n") +
   xlab("\n No. of Samples (n)") +
   ylab("Proportion of Inclusion in approximate C.I.\n") +
   theme(plot.title = element_text(color="black",size=15,
                                face="bold.italic",hjust = 0.5),
      axis.title.x = element_text(color="#993333", size=11, face="bold"),
      axis.title.y = element_text(color="#993333", size=11, face="bold"),
      axis.text.x = element_text(size=10, face="bold", colour = "black"),
      axis.text.y = element_text(size=10, face="bold", colour = "black"),
      panel.border = element_rect(colour = "black", fill=NA, size=0.3)) +
   theme(legend.title = element_text(colour="black", size=10, face="bold"),
         legend.text = element_text(colour="black", size=10, face="bold"))
   

