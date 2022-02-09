library(ggplot2)
library(patchwork)

age <- c(45,19,50,73,17,19,19,19,20,20,
            20,28,13,11,44,50,20,23,23,22)
dsp <- c(7.5,9,6.5,3.5,8,8,8,9,8,7,9,
            3.5,7,6,5,4.5,7.5,8,8.5,9)
gend <- c("FEMALE",rep("MALE",2),"FEMALE",
          rep("MALE",4),"FEMALE",rep("MALE",2),
          "FEMALE", rep("MALE",2),"FEMALE",
          "MALE",rep("FEMALE",2),rep("MALE",2))
educ <- c(15,14,15,15,11,rep(14,6),8,8,
            6,15,15,14,17,17,16)

age <- result$AGE
dsp <- result$DIGIT.SPAN
gend <- result$GENDER
educ <- result$EDUCATION



gend2 <- gend
z <- factor(gend2,levels = c("FEMALE","MALE"))
gend[gend=="FEMALE"] <- 1
gend[gend=="MALE"] <- -1
gend <- as.numeric(gend)

df <- data.frame(GENDER = gend, AGE = age, 
                 EDU = educ, DIGIT.SPAN = dsp)

df2 <- data.frame(GENDER = z, AGE = age, 
                 EDU = educ, DIGIT.SPAN = dsp)

age.dsp <- c(7.86 , 7.56 , 5.4)
age.avg <- c(14.5 , 29.5 , 60)

finplot1 <- qplot(age.avg , age.dsp , 
              geom = c("point","line"), main = "(1)") +
  xlab("Age") + ylab("Average Digit Span") +
  ggtitle("(1)") + ylim(c(3,9)) +
  ggeasy::easy_center_title()


edu.dsp <- data.frame(Levels = c("Level 1" , 
            "Level 2" , "Level 3") , edu.level.dsp = 
              c(6 , 8.17 , 6.56))

finplot2 <- ggplot(edu.dsp,aes(Levels,
              edu.level.dsp, fill = Levels)) +
              geom_bar(stat = "identity",width = 0.5) +
  xlab("Education levels") + ylab("Average Digit Span") +
  ggtitle("(2)") + 
  ggeasy::easy_center_title() +
  theme(legend.position = "none" )


finplot3 <- qplot(z, dsp, data = df2, 
            geom=c("boxplot" , "jitter"),
            xlab="Gender", ylab="Digit Span") +
  ggtitle("(3)") + ylim(c(3,9)) +
  ggeasy::easy_center_title() +
  theme(legend.position = "bottom")


final <- finplot1 + finplot2 + finplot3
