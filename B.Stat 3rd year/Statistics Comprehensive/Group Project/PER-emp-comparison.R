library(dplyr)
library(tictoc)
library(beepr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

PERV1 <- path %>% paste0(path.files[4]) %>% read.csv(header = FALSE)
PERRV <- path %>% paste0(path.files[3]) %>% read.csv(header = FALSE)

temp.v1 <- PERV1 %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

beep()

temp.rv <- PERRV %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

beep()

colnames(temp.v1) <- c(1:ncol(temp.v1)) %>% 
                        sapply(function(val) paste0("V",val))
colnames(temp.rv) <- c(1:ncol(temp.rv)) %>% 
                        sapply(function(val) paste0("V",val))

temp.urban.v1 <- temp.v1 %>% as.data.frame() %>% filter(V12 == "2")
temp.urban.rv <- temp.rv %>% as.data.frame() %>% filter(V12 == "2")

beep()

if(FALSE){
   temp.urban.v1 <- temp.v1 %>% as.data.frame() %>%
                        filter(V12 == "2" & V46 == "1") %>% ## Education
                        filter(V47 == "1" | V47 == "2" | V47 == "3")
   temp.urban.rv <- temp.rv %>% as.data.frame() %>%
                        filter(V12 == "2" & V46 == "1") %>% ## Education
                        filter(V47 == "1" | V47 == "2" | V47 == "3")
}


##########

emp.status <- unemp <- id.qivj <- list()

for(i in 1:4) {
   tic()
   
   emp.status[[i]] <- unemp[[i]] <- id.qivj[[i]] <- list()
   
   for(j in 1:4){ #################### Gender filter
      if(j == 1){doc <- temp.urban.v1# %>% filter(V41 == "2")
      }
      if(j > 1){doc <- temp.urban.rv#  %>% filter(V41 == "2")
      }
      
      emp.status[[i]][[j]] <- unemp[[i]][[j]] <- id.qivj[[i]][[j]] <- 0
      
      df <- doc %>% filter(V9 == as.character(i) & V11 == as.character(j))
      
      for(k in 1:nrow(df)){
         if(j == 1){
            emp.status[[i]][[j]][k] <- df[k, c(98:99, 124:125, 150:151, 176:177,
                                               202:203, 228:229, 254:255,
                                               109:110, 135:136, 161:162, 187,188,
                                               213:214, 239:240, 265:266)] %>% 
                                          paste0() %>% 
                                          stringr::str_c(collapse = "")
         }
         
         if(j > 1){
            emp.status[[i]][[j]][k] <- df[k, c(54:55, 80:81, 106:107, 132:133,
                                               158:159, 184:185, 210:211,
                                               65:66, 91:92, 117:118, 143:144,
                                               169:170, 195:196, 221:222)] %>% 
                                          paste0() %>% 
                                          stringr::str_c(collapse = "")
         }
         
         emp.days <- emp.status[[i]][[j]][k] %>% 
                        substring(first = seq(1,27,2), last = seq(2,28,2)) %>%
                        as.numeric() %>%
                        stats::na.exclude()
         
         if(min(emp.days) <= 80){
            unemp[[i]][[j]][k] <- "E"
         } else if(81 %in% emp.days || 82 %in% emp.days) {
                   # 92 %in% emp.days | 93 %in% emp.days |
                   # 94 %in% emp.days | 95 %in% emp.days)
            unemp[[i]][[j]][k] <- "U"
         } else unemp[[i]][[j]][k] <- "O"
         
         id.qivj[[i]][[j]][k] <- df[k, 12:39] %>% paste0() %>% 
                                    stringr::str_c(collapse = "")
      }
      
      print(paste0(i," ",j))
   }
   
   toc()
   if(i %% 2 == 0) beepr::beep(1)
}



###########

E.1.2 <- E.2.3 <- E.3.4 <- list()

for(i in 1:3){
   id.c1 <- c(id.qivj[[i]][[1]], id.qivj[[i]][[2]], id.qivj[[i]][[3]])
   id.c2 <- c(id.qivj[[i+1]][[2]], id.qivj[[i+1]][[3]], id.qivj[[i+1]][[4]])
   
   comm.pos <- data.frame(V1 = 1:length(id.c1),
                          V2 = match(id.c1,id.c2)) %>% na.omit()
   
   A <- c(unemp[[i]][[1]], unemp[[i]][[2]], unemp[[i]][[3]])
   B <- c(unemp[[i+1]][[2]], unemp[[i+1]][[3]], unemp[[i+1]][[4]])
   
   if(i == 1){
      E.1.2$c1 <- A[comm.pos$V1]
      E.1.2$c2 <- B[comm.pos$V2]
      E.1.2$X <- c(unemp[[i]][[4]], A[-comm.pos$V1])
      E.1.2$Y <- c(unemp[[i+1]][[1]], B[-comm.pos$V2])
   }
   
   if(i == 2){
      E.2.3$c1 <- A[comm.pos$V1]
      E.2.3$c2 <- B[comm.pos$V2]
      E.2.3$X <- c(unemp[[i]][[4]], A[-comm.pos$V1])
      E.2.3$Y <- c(unemp[[i+1]][[1]], B[-comm.pos$V2])
   }
   
   if(i == 3){
      E.3.4$c1 <- A[comm.pos$V1]
      E.3.4$c2 <- B[comm.pos$V2]
      E.3.4$X <- c(unemp[[i]][[4]], A[-comm.pos$V1])
      E.3.4$Y <- c(unemp[[i+1]][[1]], B[-comm.pos$V2])
   }
}

rm(doc, df, id.c1, id.c2, comm.pos, A, B)


unemp.comp <- function(X,Y,c1,c2,f = NULL){
   
   # if(is.null(f) == FALSE){
   #    X <- f(X)
   #    Y <- f(Y)
   #    c1 <- f(c1)
   #    c2 <- f(c2)
   # }
   
   n <- c1 %>% length()
   df <- data.frame(V1 = c1, V2 = c2)
   
   U <- df %>% filter(V1 == "U" & V2 == "U")
   
   n1 <- which(c1 == "U") %>% length()
   n2 <- which(c2 == "U") %>% length()
   n.X <- which(X == "U") %>% length()
   n.Y <- which(Y == "U") %>% length()
   
   c1.bar <- n1 / n
   c2.bar <- n2 / n
   X.bar <- n.X / length(X)
   Y.bar <- n.Y / length(Y)
   
   p.hat <- (n.X + n.Y + n1)/(length(X) + length(Y) + n)
   sigma.hat <- sqrt(p.hat * (1 - p.hat))
   
   rho.hat.num <- nrow(U)/n - c1.bar * c2.bar
   rho.hat.denom <- sqrt(c1.bar) * sqrt(1 - c1.bar) * sqrt(c2.bar) * sqrt(1 - c2.bar)
   
   rho.hat <- rho.hat.num / rho.hat.denom
   
   if(rho.hat.denom == 0){
      rho.hat <- 0
   }
   if(rho.hat > 0.9999) {
      rho.hat <- 0.9999
   }
   
   # temp.1 <- c(X,Y)
   # temp.U.1 <- which(temp.1 == "U") %>% length()
   # sigma.hat.1 <- sqrt(temp.U.1) * sqrt(length(temp.1) - temp.U.1) / length(temp.1)
   # 
   # temp.2 <- c(c1,c2)
   # temp.U.2 <- which(temp.2 == "U") %>% length()
   # sigma.hat.2 <- sqrt(temp.U.2) * sqrt(length(temp.2) - temp.U.2) / length(temp.2)
   
   # if(rho.hat == NaN) {rho.hat <- 0.99}
   
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


X <- E.1.2$X
Y <- E.1.2$Y
c1 <-E.1.2$c1
c2 <- E.1.2$c2

X <- E.2.3$X
Y <- E.2.3$Y
c1 <-E.2.3$c1
c2 <- E.2.3$c2

X <- E.3.4$X
Y <- E.3.4$Y
c1 <-E.3.4$c1
c2 <- E.3.4$c2




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
