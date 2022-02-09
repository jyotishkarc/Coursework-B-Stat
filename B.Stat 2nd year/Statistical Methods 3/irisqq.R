iris.data1 <- data.frame(
    char1 <- sort((iris3[,1,3])),
    theo1 <- qnorm((1:50)/51)  
  )
  
  gg1 <- ggplot(iris.data1,aes(theo1, char1)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_smooth(method='lm' , se=FALSE, col = "red") +
    geom_point(col = "blue") +
    theme(
      axis.title.x = element_text(color="#31A821", face="bold"),
      axis.title.y = element_text(color="#993333", face="bold")) +
    labs( y = "Sample Quantiles", 
          x = "Theoretical Quantiles\n")

    
iris.data2 <- data.frame(
    char2 <- sort((iris3[,2,3])),
    theo2 <- qnorm((1:50)/51)
  )
  
  gg2 <- ggplot(iris.data2,aes(theo2, char2)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_smooth(method='lm' , se=FALSE, col = "red") +
    geom_point(col = "blue") +
    theme(
      axis.title.x = element_text(color="#31A821", face="bold"),
      axis.title.y = element_text(color="#993333", face="bold")) +
    labs( y = "\nSample Quantiles", 
          x = "Theoretical Quantiles\n")
  

iris.data3 <- data.frame(
    char3 <- sort((iris3[,3,3])),
    theo3<- qnorm((1:50)/51)  
  )
  
  gg3 <- ggplot(iris.data3,aes(theo3, char3)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_smooth(method='lm' , se=FALSE, col = "red") +
    geom_point(col = "blue") +
    theme(
      axis.title.x = element_text(color="#31A821", face="bold"),
      axis.title.y = element_text(color="#993333", face="bold")) +
    labs( y = "Sample Quantiles", 
          x = "Theoretical Quantiles")

  
iris.data4 <- data.frame(
    char4 <- sort((iris3[,4,3])),
    theo4 <- qnorm((1:50)/51)  
  )
  
  gg4 <- ggplot(iris.data4,aes(theo4, char4)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_smooth(method='lm' , se=FALSE, col = "red") +
    geom_point(col = "blue") +
    theme(
      axis.title.x = element_text(color="#31A821", face="bold"),
      axis.title.y = element_text(color="#993333", face="bold")) +
    labs( y = "\nSample Quantiles", 
          x = "Theoretical Quantiles")

figure <- ggarrange(gg1, gg2, gg3, gg4,
                    labels = c("Sepal length :", "   Sepal width :", 
                               " Petal length :" , "   Petal width :"),
                    ncol = 2, nrow = 2)

print(figure)