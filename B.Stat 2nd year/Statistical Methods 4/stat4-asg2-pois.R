## Author : Jyotishka Ray Choudhury
## Roll no : BS 1903

pois.ci <- function(n,L){
   len <- 10000
   z <- qnorm(0.975)
   
   ee <- rpois(len , n * L) / n
   
   c <- (sqrt(ee) - z * 1/sqrt(4 * n))^2
   d <- (sqrt(ee) + z * 1/sqrt(4 * n))^2
   
   counter <- 0
   for (k in 1:len) {
      if (c[k] <= L && d[k] >= L ) {
         counter <- counter + 1
      }
   }
   
   return(counter / len)
}

tt <- seq(30,1000,25)
m <- c()

graph.pois <- function(L){
   for (i in 1:length(tt)) {
      s <- c()
      for (j in 1:1){
         s[j] <- pois.ci(tt[i] , L)
      }
      
      m[i] <- mean(s)
   }        
   return(m)
}

res1.p <- graph.pois(1)
res2.p <- graph.pois(20)

d1.p <- data.frame(x = tt , y = res1.p)
d2.p <- data.frame(x = tt , y = res2.p)
dd.p <- rbind(cbind(d1.p, case = "lambda = 1"),
              cbind(d2.p, case = "lambda = 20"))
ddsmooth.p <- ddply(dd.p, .(case), function(k) as.data.frame(spline(k)))

resplot.p <- ggplot(dd.p, aes(x, y), ylim(c(0.7,1))) +
   geom_line(aes(colour = case, size = case), data = ddsmooth.p) +
   ggtitle("Samples from Poisson (lambda)\n") +
   scale_y_continuous(limits=c(0.9, 1)) +
   scale_colour_manual(values = c("red", "blue"), name="Value of lambda :\n") +
   scale_size_manual(values = c(1, 1), name = "Value of lambda :\n") +
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















