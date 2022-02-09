## Name : Jyotishka Ray Choudhury
## Roll No. : BS 1903

data <- rbind(c(65 ,NA, 87),c(NA ,89 ,67),c(87  ,56 ,78),c(78  ,82 ,86),
              c(67  ,NA ,NA),c(94  ,89,  NA),c(78  ,87,  92),c(NA ,NA, 98),
              c(78  ,89,  94),c(72  ,NA,  93),c(69 ,NA, 84),c(72  ,89, 67),
              c(87  ,NA, 72),c(NA  ,86, 76),c(67  ,78, NA),c(NA  ,89,  NA),
              c(73  ,84,  92),c(NA ,81, 98),c(78  ,79,  95),c(NA  ,NA,  96),
              c(75 ,NA, 87),c(NA ,83, 69),c(87  ,59, NA),c(78  ,83,  76),
              c(77  ,NA, NA),c(NA  ,89,  NA),c(79  ,82,  93),c(NA ,NA, 88),
              c(78  ,84,  94),c(72  ,86,  97))

TL <- nrow(data)
red.data <- na.omit(data)
L <- nrow(red.data)

### Step 0 (Initial estimates) :
mu <- c()
for (i in 1:3) {
  mu[i] <- mean(red.data[,i])
}
mu0 <- mu

VC <- matrix(c(0),3,3)
for(i in 1:L){
  VC <- VC + as.matrix(red.data[i,] - mu) %*% (red.data[i,] - mu)
}
VC <- VC / L
VC0 <- VC

count <- 0

for (rot in 1:200) {
  
  d1 <- na.omit(data)
  na1 <- data[which(rowSums(is.na(data)) %in% c(1)),]
  na2 <- data[which(rowSums(is.na(data)) %in% c(2)),]
  
### E - Step :
  
  # 1 Unknown variable ~
  for (i in 1:nrow(na1)) {
    if(match(NA,na1[i,]) == 1){
      na1[i,1] <- mu[1] + VC[1,2:3] %*% solve(VC[2:3,2:3]) %*% 
        as.matrix(c(na1[i,2],na1[i,3]) - c(mu[2],mu[3]))
    }
    
    else if(match(NA,na1[i,]) == 2){
      na1[i,2] <- mu[2] + VC[2,c(1,3)] %*% solve(VC[c(1,3),c(1,3)]) %*% 
        as.matrix(c(na1[i,1],na1[i,3]) - c(mu[1],mu[3]))
    }
    
    else if(match(NA,na1[i,]) == 3){
      na1[i,3] <- mu[3] + VC[3,1:2] %*% solve(VC[1:2,1:2]) %*% 
        as.matrix(c(na1[i,1],na1[i,2]) - c(mu[1],mu[2]))
    }
  }
  
  # 2 Unknown variables ~
  for (i in 1:nrow(na2)) {
    if(is.na(na2[i,1]) == FALSE){
      na2[i,c(2,3)] <- c(mu[2],mu[3]) + c(VC[1,2],VC[1,3]) * (na2[i,1]-mu[1]) / VC[1,1]
    }
    
    else if(is.na(na2[i,2]) == FALSE){
      na2[i,c(1,3)] <- c(mu[1],mu[3]) + c(VC[1,2],VC[2,3]) * (na2[i,2]-mu[2]) / VC[2,2]
    }
    
    else if(is.na(na2[i,3]) == FALSE){
      na2[i,c(1,2)] <- c(mu[1],mu[2]) + c(VC[1,3],VC[2,3]) * (na2[i,3]-mu[3]) / VC[3,3]
    }
  }

### M - Step :
  
  estdata <- rbind(d1,na1,na2)
  estmu <- c(mean(estdata[,1]),mean(estdata[,2]),mean(estdata[,3]))
  estVC <- matrix(c(0),3,3)
  for(i in 1:TL){
      estVC <- estVC + as.matrix(estdata[i,] - estmu) %*% (estdata[i,] - estmu)
  }
  estVC <- estVC / TL
  
  munorm <- sqrt(sum((mu - estmu)^2))
  VCnorm <- sqrt(sum((VC - estVC)^2))
  
  tol <- 10^(-8)
  
  if(munorm <= tol && VCnorm <= tol){
    cat("\nMean = ",estmu, " \n\n")
    cat("V-C matrix = \n")
    print(estVC)
    cat("\n")
    cat("Tolerance = ",tol,"\n")
    cat("No. of iterations = ",count,"\n\n")
    break
  }
  
  count <- count + 1
  mu <- estmu
  VC <- estVC
}













