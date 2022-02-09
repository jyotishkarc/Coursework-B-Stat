elastic.pendulum <- function(x0,y0,dt,vx0,vy0,l,m,gamma,n)
{
   l <- x <- y <- vx <- vy <- ax <- ay <- numeric(n)
   
   l[1] <- sqrt((x0)^2 + (y0)^2)
   x[1] <- x0
   y[1] <- y0
   vx[1] <- vx0
   vy[1] <- vy0
   
   ax[1] <- (((-gamma*(l[1]-l))*(x[1]/l[1]))/m)
   ay[1] <- ((((-gamma*(l[1]-l))*(y[1]/l[1]))/m)-9.8)
   
   for(k in 2:n)
   {
      x[k] <- x[k-1]+(vx[k-1]*dt)
      y[k] <- y[k-1]+(vy[k-1]*dt)
      
      l[k] <- sqrt((x[k])^2 + (y[k])^2)
      
      vx[k] <- vx[k-1]+(ax[k-1]*dt)
      vy[k] <- vy[k-1]+(ay[k-1]*dt)
      
      ax[k] <- (((-gamma*(l[k]-l))*(x[k]/l[k]))/m)
      ay[k] <- ((((-gamma*(l[k]-l))*(y[k]/l[k]))/m)-9.8)
   }
   
   for(k in 2:n)
   {
      plot(x[2:k],y[2:k],xlim=c(min(x),max(x)), ylim=c(min(y),max(y)) ,
           ty="l", col="blue")
      
      segments(0,0,x[k],y[k],lty=8)
      Sys.sleep(0.1)
   }
#   Sys.sleep(0.1)
   
}