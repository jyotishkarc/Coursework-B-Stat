## Author: JYOTISHKA RAY CHOUDHURY
## Roll No.: BS1903

library(ggplot2)
library(magrittr)
library(latex2exp)

CL.func <- function(Y){
  n <- 5
  A2 <- 0.577
  D3 <- 0
  D4 <- 2.114
  
  X_grand <- Y$`X-bar` %>% mean()
  R_bar <- Y$`R-bar` %>% mean()
  
  UCL_X <- X_grand + A2 * R_bar
  CL_X <- X_grand
  LCL_X <- X_grand - A2 * R_bar
  
  UCL_R <- D4 * R_bar
  CL_R <- R_bar
  LCL_R <- D3 * R_bar
  
  CC_X <- Y %>%
    ggplot(aes(x=Sample, y=`X-bar`)) +
    geom_line(linetype = "dashed", col = "blue") +
    geom_point() +
    scale_x_continuous(breaks = seq(1,35, by = 2)) +
    scale_y_continuous(breaks = NULL, 
                       limits = c(min(Y$`X-bar`)-0.0006,
                                  max(Y$`X-bar`)+0.0003)) + 
    geom_hline(yintercept = UCL_X, 
               col = "red", 
               linetype = "dashed") + 
    geom_hline(yintercept = LCL_X, 
               col = "red", 
               linetype = "dashed") + 
    geom_hline(yintercept = CL_X, 
               col = "darkgreen") + 
    xlab("Subgroup") +
    labs(title = TeX("$\\bf{\\bar{X}}\\textbf{-chart}$")) +
    theme(plot.title = element_text(color = "black", 
                                    size = 12, 
                                    face = "bold",
                                    hjust = 0.5))
  
  CC_R <- Y %>%
    ggplot(aes(x=Sample, y=`R-bar`)) +
    geom_line(linetype = "dashed", col = "blue") +
    geom_point() +
    scale_x_continuous(breaks = seq(1,35, by = 2)) +
    scale_y_continuous(breaks = NULL, 
                       limits = c(min(Y$`R-bar`)-0.0006,
                                  max(Y$`R-bar`)+0.0003)) +
    geom_hline(yintercept = UCL_R, 
               col = "red", 
               linetype = "dashed") + 
    geom_hline(yintercept = LCL_R, 
               col = "red", 
               linetype = "dashed") + 
    geom_hline(yintercept = CL_R, 
               col = "darkgreen") +
    xlab("Subgroup") +
    ylab("R") +
    labs(title = TeX("$\\bf{R}\\textbf{-chart}$")) +
    theme(plot.title = element_text(color = "black", 
                                    size = 12, 
                                    face = "bold",
                                    hjust = 0.5))
  
  return(list("X-bar" = c(LCL_X, CL_X, UCL_X),
              "R" = c(LCL_R, CL_R, UCL_R),
              CC_X, CC_R))
}
