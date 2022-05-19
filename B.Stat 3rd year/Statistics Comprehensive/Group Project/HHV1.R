library(dplyr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

HHV1 <- path %>% paste0(path.files[2]) %>% read.csv(header = FALSE)
HHRV <- path %>% paste0(path.files[1]) %>% read.csv(header = FALSE)

temp.v1 <- HHV1 %>%
            apply(1, function(val){
               return(val %>% strsplit(split = "") %>% unlist())
            }) %>% t()

temp.rv <- HHRV %>%
            apply(1, function(val){
               return(val %>% strsplit(split = "") %>% unlist())
            }) %>% t()

colnames(temp.v1) <- colnames(temp.rv) <- c(1:ncol(temp.v1)) %>% 
                                             sapply(function(val) paste0("V",val))

temp.urban.v1 <- temp.v1 %>% as.data.frame() %>% filter(V12 == 2)
temp.urban.rv <- temp.rv %>% as.data.frame() %>% filter(V12 == 2)

temp.qivj <- hce <- id.qivj <- list()

for(i in 1:4) {
   hce[[i]] <- temp.qivj[[i]] <- id.qivj[[i]] <- list()
   
   for(j in 1:4){
      if(j == 1){doc <- temp.urban.v1}
      if(j > 1){doc <- temp.urban.rv}
      
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
      
      print(j)
   }
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
