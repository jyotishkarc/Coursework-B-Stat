# Author : Samahriti

library("scatterplot3d")

gwell  <-  function(h,n) 
{
   x <- y <- u <- z <- f <- x1 <- y1 <- u1 <- u2 <- 
      f1 <- f2 <- f3 <- x2 <- y2 <- Rtil <- R <- numeric(n)
   
   t <- 10 
   x[1] <- t 
   y[1] <- x1[1] <- 0
   y1[1] <- 5
   
   for (i in 2:n)
   {
      u[i-1] <- sqrt(x[i-1]^2 + y[i-1]^2)
      f[i-1] <- sqrt(u[i-1] - 1)
      z[i-1] <- f[i-1]
      
      u1[i-1] <- (x[i-1] * x1[i-1] + y[i-1] * y1[i-1]) / u[i-1]
      u2[i-1] <- (x1[i-1]^2 + y1[i-1]^2 + x[i-1] * x2[i-1] +
                     y[i-1] * y2[i-1] - u1[i-1]^2) / u[i-1]
      
      f1[i-1] <- 1 / (2 * sqrt(u[i-1] - 1))
      f2[i-1] <- -1 / (4 * (u[i-1] - 1)^1.5)
      f3[i-1] <- (3/8) * (u[i-1] - 1)^(-5/2)
      
      x2[i-1] <- -x[i-1] * R[i-1] - x1[i-1] * Rtil[i-1]
      y2[i-1] <- -y[i-1] * R[i-1] - y1[i-1] * Rtil[i-1]
      
      Rtil[i-1] <- ((f1[i-1] * (x1[i-1]^2 + y1[i-1]^2 - u1[i-1]^2) / u[i-1]) + 
                  u1[i-1]^2 * f2[i-1] + 9.8)/(u[i-1] * (f1[i-1] + (1/f1[i-1])))
      
      R[i-1] <- ((2 * f1[i-1] * f2[i-1] * (x1[i-1]^2 + y1[i-1]^2 - u1[i-1]^2) +
                  f1[i-1]^2 * (2 * x1[i-1] * x2[i-1] + 2 * y1[i-1] * y2[i-1] - 
                  2 * u1[i-1] * u2[i-1]) + 2 * u[i-1] * u1[i-1] * u2[i-1] 
                  * f1[i-1] * f2[i-1] + u1[i-1]^2 * u[i-1] * f2[i-1]^2 + 
                  u1[i-1]^2 * u[i-1] * f1[i-1] * f3[i-1] + u1[i-1]^3 * 
                  f1[i-1] * f2[i-1] + 9.8 * u[i-1] * f2[i-1] + 9.8 * u1[i-1] *
                  f1[i-1]) - (Rtil[i-1] * ( 2 * u[i-1] * f1[i-1]^2 + 
                  u[i-1]^2 * 2 * f1[i-1] * f2[i-1]))) / (u[i-1]^2 * 
                  f1[i-1]^2 + 1)
      
      x[i] <- x[i-1] + h * x1[i-1] + (h^2 / 2) * (-x[i-1] * Rtil[i-1])
      y[i] <- y[i-1] + h * y1[i-1] + (h^2 / 2) * (-y[i-1] * Rtil[i-1])
      
      x1[i] <- x1[i-1] + h * (-x[i-1] * Rtil[i-1]) + 
               (h^2 * (-x[i-1] * R[i-1] - x1[i-1] * Rtil[i-1])) / 2
      y1[i] <- y1[i-1] + h * (-y[i-1] * Rtil[i-1]) + 
               (h^2 * (-y[i-1] * R[i-1] - y1[i-1] * Rtil[i-1])) / 2
   }
   
   u[n] <- sqrt(x[n]^2 + y[n]^2)
   f[n] <- sqrt(u[n] - 1)
   z[n] <- f[n]
   
   scatterplot3d(x , y , z , type = "l")
}

#gwell(0.01 , 20000)


