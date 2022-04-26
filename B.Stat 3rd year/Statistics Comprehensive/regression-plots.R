library(ggplot2)
library(ggpubr)

x <- seq(0,2, by = 1/50)
eps <- rnorm(length(x),0,0.3)
y <- 1 + 1.5*x + eps

df <- data.frame(x = x, y = y)

out.1 <- data.frame(x = seq(1.3,1.5,0.025), 
                    y = rnorm(length(seq(1.3,1.5,0.025)), 0.8, 0.1))
df.out.1 <- rbind(df, out.1)

out.2 <- data.frame(x = seq(2.8,3,0.025), 
                    y = rnorm(length(seq(2.8,3,0.025)), 3.5, 0.1))
df.out.2 <- rbind(df, out.2)


if(T){
   plt.1 <- ggplot(df.out.1, aes(x = x, y = y)) + geom_point() +
      geom_smooth(method = "lm", size = 1.5, color = "red", se = FALSE) +
      geom_smooth(data = df, method = "lm", size = 1.5, color = "blue", se = FALSE) +
      #geom_quantile(quantiles = 0.5, size = 1.5, color = "green") +
      theme_bw() +
      xlab("x") + ylab("y")
   
   plt.2 <- ggplot(df.out.1, aes(x = x, y = y)) + geom_point() +
      #geom_smooth(method = "lm", size = 1.5, color = "red", se = FALSE) +
      #geom_smooth(data = df, method = "lm", size = 1.5, color = "blue", se = F) +
      geom_quantile(quantiles = 0.5, size = 1.5, color = "red") +
      geom_quantile(data = df, quantiles = 0.5, size = 1.5, color = "blue") +
      theme_bw() +
      xlab("x") + ylab("y")
   
   
   plt.3 <- ggplot(df.out.2, aes(x = x, y = y)) + geom_point() +
      geom_smooth(method = "lm", size = 1.5, color = "red", se = FALSE) +
      geom_smooth(data = df, method = "lm", size = 1.5, color = "blue", se = FALSE) +
      #geom_quantile(quantiles = 0.5, size = 1.5, color = "green") +
      theme_bw() + ggtitle("OLS lines") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +
      xlab("x") + ylab("y")
   
   plt.4 <- ggplot(df.out.2, aes(x = x, y = y)) + geom_point() +
      #geom_smooth(method = "lm", size = 1.5, color = "red", se = FALSE) +
      #geom_smooth(data = df, method = "lm", size = 1.5, color = "blue", se = F) +
      geom_quantile(quantiles = 0.5, size = 1.5, color = "red") +
      geom_quantile(data = df, quantiles = 0.5, size = 1.5, color = "blue") +
      theme_bw() + ggtitle("LAD lines") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18)) +
      xlab("x") + ylab("y")
   
   
   plt <- ggarrange(plt.3, plt.4, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
}



if(FALSE){
   plt.1 <- ggplot(df, aes(x = x, y = y)) + geom_point() +
      geom_smooth(method = "lm", size = 2, se = FALSE)+ theme_bw()
   
   plt.2 <- ggplot(df.out, aes(x = x, y = y)) + geom_point() +
      geom_smooth(method = "lm", size = 2, color = "red", se = FALSE) +
      #geom_smooth(data = df, method = "lm", size = 2, color = "blue", se = F) +
      theme_bw() +
      xlab("Timespan (Hours)") +
      ylab("No. of views (In 10 thousand)")
   
   plt <- ggarrange(plt.1, plt.2, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
}