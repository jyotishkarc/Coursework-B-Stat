L <- c(112,143,41,158,266,132,218,93,166)
v <- rep(0,9)
A <- v
for (i in 1:9) {
  v[i] <- sum(rpois(60,L[i]))
  A[i] <- sum(v[1:i])/10000
}

library(ggplot2)
library(plyr)

B <- c(5434,13669,15944,29365,38383,44549,59986,65090,79642)/10000
d1 <- data.frame(x=1:9,y=A)
d2 <- data.frame(x=1:9,y=B)
dd <- rbind(cbind(d1, case = "Pseudo"), cbind(d2, case = "Original     "))
ddsmooth <- plyr::ddply(dd, .(case), function(k) as.data.frame(spline(k)))

finplot <- ggplot(dd, aes(x, y)) +
  geom_line(aes(colour = case, size = case), data = ddsmooth) +
  geom_point(colour = "black") +
  scale_colour_manual(values = c("red", "blue"), name = " ") +
  scale_size_manual(values = c(1, 1), name = " ") +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(breaks = 1:9) +
  theme(legend.position = "bottom") +
  xlab("Timespan (Hours)") +
  ylab("No. of views (In 10 thousand)")

print(finplot)