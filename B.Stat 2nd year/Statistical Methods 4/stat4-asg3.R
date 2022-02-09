plot(AirPassengers , type = "o", lwd = 2, pch = 19 , col = "blue" ,
     main = "Time Series Plot of AirPassengers data")

Y <- log(AirPassengers)

plot(Y , type = "o" , pch = 19 , col = "blue" , lwd = 2,
     main = "Time Series Plot of AirPassengers data")
class(Y)

Y <- ts(Y, frequency = 12)
plot(decompose(Y , type = "additive"))

diff.season <- diff(Y , 12)
diff.full.1 <- diff(diff.season , 1)
plot(diff.full.1, type = "p", pch = 19, col = "orange")
lines(diff.full.1, lwd = 2, type = "l")

acf2(diff.full.1)

model_1 <- sarima(Y , 1 , 1 , 1 , 1 , 1 , 1 , 12)
model_2 <- sarima(Y , 1 , 1 , 3 , 1 , 1 , 1 , 12)

sarima.for(Y,24,1,1,1,1,1,1,12)
