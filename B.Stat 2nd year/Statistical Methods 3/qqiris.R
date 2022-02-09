iris.data <- data.frame( 
  char <- sort((iris3[,4,2])),
  theo <- qnorm((1:50)/51)  
  )

## Q-Q Plot :

qq <- ggplot(iris.data,aes(theo, char)) +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm' , se=FALSE, col = "red") +
  geom_point(col = "blue") +
  theme(
    axis.title.x = element_text(color="#31A821", face="bold"),
    axis.title.y = element_text(color="#993333", face="bold")) +
    labs( y = "Sample Quantiles (Ordered data)", 
          x = "Theoretical Quantiles")

print(qq)

### Chi-square test :

breaks <- c(-Inf,1.12,1.22,1.32,1.42,Inf)

char.cut = cut(char, breaks, right=TRUE)
char.freq = table(char.cut)
final <- cbind(char.freq)
obs <- c()    
exf <- c()

for (i in 1:(length(breaks)-1)) {
  exf <- c(exf,(pnorm(breaks[i+1],mean = mean(char), sd=sqrt(var(char)))
                -pnorm(breaks[i],mean = mean(char), sd=sqrt(var(char))))*50)
  obs <- c(obs,final[i,1])
}

chi2 <- sum((obs-exf)^2 / exf)
pval <- pchisq(chi2 , df = length(breaks)-4 , lower.tail = FALSE)

cat(": Result for Petal Width of Versicolor  :\n\n")
print(cbind(final , exf))
cat("\n Chi-square = ",chi2,"\n p-value = ",pval,"\n\n")
