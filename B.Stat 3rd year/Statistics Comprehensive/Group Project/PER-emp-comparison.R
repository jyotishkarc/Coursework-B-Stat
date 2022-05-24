library(dplyr)
library(tictoc)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

PERV1 <- path %>% paste0(path.files[4]) %>% read.csv(header = FALSE)
PERRV <- path %>% paste0(path.files[3]) %>% read.csv(header = FALSE)

temp.v1 <- PERV1 %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

temp.rv <- PERRV %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

colnames(temp.v1) <- c(1:ncol(temp.v1)) %>% 
                        sapply(function(val) paste0("V",val))
colnames(temp.rv) <- c(1:ncol(temp.rv)) %>% 
                        sapply(function(val) paste0("V",val))

temp.urban.v1 <- temp.v1 %>% as.data.frame() %>% filter(V12 == "2")
temp.urban.rv <- temp.rv %>% as.data.frame() %>% filter(V12 == "2")

if(TRUE){
   temp.urban.v1 <- temp.v1 %>% as.data.frame() %>%
                        filter(V12 == "2" & V46 == "1") %>%
                        filter(V47 == "1" | V47 == "2" | V47 == "3")
   temp.urban.rv <- temp.rv %>% as.data.frame() %>%
                        filter(V12 == "2" & V46 == "1") %>%
                        filter(V47 == "1" | V47 == "2" | V47 == "3")
}


##########

emp.status <- unemp <- id.qivj <- list()

for(i in 1:4) {
   tic()
   
   emp.status[[i]] <- unemp[[i]] <- id.qivj[[i]] <- list()
   
   for(j in 1:4){
      if(j == 1){doc <- temp.urban.v1}
      if(j > 1){doc <- temp.urban.rv}
      
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
   
   temp.1 <- c(X,Y)
   temp.U.1 <- which(temp.1 == "U") %>% length()
   sigma.hat.1 <- sqrt(temp.U.1) * sqrt(length(temp.1) - temp.U.1) / length(temp.1)
   
   temp.2 <- c(c1,c2)
   temp.U.2 <- which(temp.2 == "U") %>% length()
   sigma.hat.2 <- sqrt(temp.U.2) * sqrt(length(temp.2) - temp.U.2) / length(temp.2)
   
   
   rho.hat.num <- n * nrow(U) - n1 * n2
   rho.hat.denom <- sqrt(n1) * sqrt(n - n1) * sqrt(n2) * sqrt(n - n2)
   rho.hat <- rho.hat.num / rho.hat.denom
                  
   
   
   s1 <- (n.X/length(X) - n.Y/length(Y))/
      (sigma.hat.1 * sqrt(1/length(X) + 1/length(Y)))
   
   
   s2 <- (n1 - n2)/(sqrt(2*n) * sigma.hat.2 * sqrt(1-rho.hat))
   
   return(list("sigma.hat.1" = sigma.hat.1, "sigma.hat.2" = sigma.hat.2,
               "rho.hat" = rho.hat, "s1" = s1, "s2" = s2, 
               "statistic" = (s1 + s2)/sqrt(2)))
}


X <- E.1.2$X
Y <- E.1.2$Y
c1 <-E.1.2$c1
c2<- E.1.2$c2

X <- E.2.3$X
Y <- E.2.3$Y
c1 <-E.2.3$c1
c2<- E.2.3$c2

X <- E.3.4$X
Y <- E.3.4$Y
c1 <-E.3.4$c1
c2<- E.3.4$c2




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
