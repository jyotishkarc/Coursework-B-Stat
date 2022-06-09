library(dplyr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

HHV1 <- path %>% paste0(path.files[2]) %>% read.csv(header = FALSE)
HHRV <- path %>% paste0(path.files[1]) %>% read.csv(header = FALSE)

temp.hce.v1 <- HHV1 %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

temp.hce.rv <- HHRV %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

colnames(temp.hce.v1) <- colnames(temp.hce.rv) <- c(1:ncol(temp.hce.v1)) %>% 
   sapply(function(val) paste0("V",val))

temp.urban.hce.v1 <- temp.hce.v1 %>% as.data.frame() %>% filter(V12 == 2)
temp.urban.hce.rv <- temp.hce.rv %>% as.data.frame() %>% filter(V12 == 2)

temp.qivj <- hce <- id.qivj <- list()

for(i in 1:4) {
   hce[[i]] <- temp.qivj[[i]] <- id.qivj[[i]] <- list()
   
   for(j in 1:4){
      if(j == 1){doc <- temp.urban.hce.v1}
      if(j > 1){doc <- temp.urban.hce.rv}
      
      hce[[i]][[j]] <- id.qivj[[i]][[j]] <- 0
      
      df <- temp.qivj[[i]][[j]] <- doc %>% as.data.frame() %>% 
         filter(V9 == i & V11 == j)
      
      for(k in 1:nrow(df)){
         hce[[i]][[j]][k] <- df[k, 48:55] %>% paste0() %>% 
            stringr::str_c(collapse = "") %>% 
            as.numeric()
         
         id.qivj[[i]][[j]][k] <- df[k, 12:37] %>% paste0() %>% 
            stringr::str_c(collapse = "")
      }
      
      print(paste0(i," ",j))
   }
   
   beepr::beep(10)
}

Q.1.2 <- Q.2.3 <- Q.3.4 <- list()

for(i in 1:3){
   id.c1 <- c(id.qivj[[i]][[1]], id.qivj[[i]][[2]], id.qivj[[i]][[3]])
   id.c2 <- c(id.qivj[[i+1]][[2]], id.qivj[[i+1]][[3]], id.qivj[[i+1]][[4]])
   
   comm.pos <- data.frame(V1 = 1:length(id.c1),
                          V2 = match(id.c1,id.c2)) %>% na.omit()
   
   A <- c(hce[[i]][[1]], hce[[i]][[2]], hce[[i]][[3]])
   B <- c(hce[[i+1]][[2]], hce[[i+1]][[3]], hce[[i+1]][[4]])
   
   if(i == 1){
      Q.1.2$c1 <- A[comm.pos$V1]
      Q.1.2$c2 <- B[comm.pos$V2]
      Q.1.2$X <- c(hce[[i]][[4]], A[-comm.pos$V1])
      Q.1.2$Y <- c(hce[[i+1]][[1]], B[-comm.pos$V2])
   }
   
   if(i == 2){
      Q.2.3$c1 <- A[comm.pos$V1]
      Q.2.3$c2 <- B[comm.pos$V2]
      Q.2.3$X <- c(hce[[i]][[4]], A[-comm.pos$V1])
      Q.2.3$Y <- c(hce[[i+1]][[1]], B[-comm.pos$V2])
   }
   
   if(i == 3){
      Q.3.4$c1 <- A[comm.pos$V1]
      Q.3.4$c2 <- B[comm.pos$V2]
      Q.3.4$X <- c(hce[[i]][[4]], A[-comm.pos$V1])
      Q.3.4$Y <- c(hce[[i+1]][[1]], B[-comm.pos$V2])
   }
}

rm(doc, df, id.c1, id.c2, comm.pos, A, B)


hce.comp <- function(X,Y,c1,c2,f = NULL){
   
   if(is.null(f) == FALSE){
      X <- f(X)
      Y <- f(Y)
      c1 <- f(c1)
      c2 <- f(c2)
   }
   
   n <- c1 %>% length()
   
   c1.bar <- sum(c1) / n
   c2.bar <- sum(c2) / n
   X.bar <- sum(X) / length(X)
   Y.bar <- sum(Y) / length(Y)
   
   sigma.hat <- sqrt((sum((X - X.bar)^2) + sum((Y - Y.bar)^2) + sum((c1 - c1.bar)^2))/(length(X) + length(Y) + n - 3))
   
   rho.hat <- sum((c1 - c1.bar) * (c2 - c2.bar))/(n * sigma.hat^2)
   
   if(rho.hat > 0.9999) {
      rho.hat <- 0.9999
   }
   
   s1 <- (X.bar - Y.bar) / (sigma.hat * sqrt(1/length(X) + 1/length(Y)))
   s2 <- sqrt(n) * (c1.bar - c2.bar) / (sigma.hat * sqrt(2*(1 - rho.hat)))
   
   statistic <- (s1 + s2)/sqrt(2)
   
   if(statistic < 0){p.value <- 2 * pnorm(statistic)}
   if(statistic >= 0){p.value <- 2 *(1 - pnorm(statistic))}
   
   
   return(list("sigma.hat" = sigma.hat, "rho.hat" = rho.hat, 
               "s1" = s1, "s2" = s2, 
               "statistic" = statistic,
               "p-value" = p.value))
}


# hce.sig <- hce[hce > 5 & hce < 130500]


################

# ks.test(rnorm(1000,mean = 1, sd = 1)-1,"pnorm")

#### Box-Cox Transformation
if(FALSE){
   G <- boxcox(hce.sig ~ 1)
   lmb <- G$x[which.max(G$y)]
   tr.hce <- (hce.sig^lmb - 1)/lmb
}

if(FALSE){
   if(TRUE) {
      tr.hce <- G <- VGAM::yeo.johnson(hce.sig, 0.055)
   }
   
   MASS::fitdistr(tr.hce, "normal") -> param
   M <- param$estimate[1]
   S <- param$estimate[2]
   
   ks.test((tr.hce + rnorm(length(tr.hce), 
                           mean = 1, 
                           sd = sqrt(S^2/10))-M-1)/sqrt(S^2+S^2/10), 
           "pnorm")
}

# for(i in 1:1000){
#    J <- ks.test((log(hce.sig) + rnorm(length(hce.sig),1,sd = sqrt(V/10)) - M - 1)/sqrt(V+V/10),
#                 "pnorm")
#    if(J$p.value > 0.05){
#       a <- a+1
#    }
# }
