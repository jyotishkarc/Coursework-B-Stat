library(dplyr)
library(tictoc)
library(readxl)
library(writexl)
library(beepr)

layout.path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/"
target.path <- "D:/My Documents/R/R Codes/Coursework (B.Stat)/B.Stat 3rd year/Statistics Comprehensive/Group Project/state-total.xlsx"

state.codes <- layout.path %>% paste0(list.files(layout.path)[6]) %>% 
   read_xlsx() %>% as.data.frame()
state.codes.char <- state.codes$Code
state.total <-list()
H <- data.frame()

for(num in setdiff(1:6, 31)){
   
   state <- state.codes.char[num] %>% strsplit(split = "") %>% unlist()
   cat("\n\n",state.codes.char[num],state.codes$State[num],"\n\n")
   
   temp.urban.hce.v1.state <- temp.urban.hce.v1 %>% filter(V13 == state[1] 
                                                           & V14 == state[2])
   temp.urban.hce.rv.state <- temp.urban.hce.rv %>% filter(V13 == state[1] 
                                                           & V14 == state[2])
   
   hce <- id.qivj <- list()
   
   for(i in 1:4) {
      tic()
      
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
      
      toc()
      # if(i %% 2 == 0) beepr::beep(1)
   }
   
   rm(temp.urban.hce.v1.state, temp.urban.hce.rv.state)
   
   ###########
   
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
   
   rm(doc, df, id.c1, id.c2, comm.pos, A, B, id.qivj)
   
   state.total[[k]] <- list()
   state.total[[k]][[1]] <- 0
   state.total[[k]][[2]] <- 0
   state.total[[k]][[3]] <- 0
   
   X <- Q.1.2$X
   Y <- Q.1.2$Y
   c1 <- Q.1.2$c1
   c2 <- Q.1.2$c2
   
   state.total[[num]][[1]] <- hce.comp(X,Y,c1,c2) %>% unlist()
   print("Q1 vs Q2")
   print(state.total[[num]][[1]])
   
   X <- Q.2.3$X
   Y <- Q.2.3$Y
   c1 <- Q.2.3$c1
   c2 <- Q.2.3$c2
   
   state.total[[num]][[2]] <- hce.comp(X,Y,c1,c2) %>% unlist()
   print("Q2 vs Q3")
   print(state.total[[num]][[2]])
   
   X <- Q.3.4$X
   Y <- Q.3.4$Y
   c1 <- Q.3.4$c1
   c2 <- Q.3.4$c2
   
   state.total[[num]][[3]] <- hce.comp(X,Y,c1,c2) %>% unlist()
   print("Q3 vs Q4")
   print(state.total[[num]][[3]])
   
   G <- rbind(rep(num,6),rep(NA,6),
              state.total[[num]][[1]], 
              state.total[[num]][[2]], 
              state.total[[num]][[3]],
              rep(NA,6),rep(NA,6))
   
   H <- rbind(H,G)
   
   beep()
   Sys.sleep(0.8)
}



