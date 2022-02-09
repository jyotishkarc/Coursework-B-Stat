library("rmutil")

fn <- function(y,x) {(x*y-y^2)/x^2}

z <- seq(1,3,by=1/10)
soln <- runge.kutta(fn,2,z)
exct <- z/(0.5+log(z))

plot(z,soln,type = "l",col = "red")
lines(z,exct,type = "l",col = "blue")


