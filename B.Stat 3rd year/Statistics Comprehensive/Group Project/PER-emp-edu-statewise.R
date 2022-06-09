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
edu.all.v1 <- edu.all.rv <- c()

for(k in 1:nrow(temp.urban.v1)){
   edu.all.v1[k] <- temp.urban.v1[k,46:47] %>% paste0() %>% 
                     stringr::str_c(collapse = "") %>% as.numeric()
}

for(k in 1:nrow(temp.urban.rv)){
   edu.all.rv[k] <- temp.urban.rv[k,46:47] %>% paste0() %>% 
      stringr::str_c(collapse = "") %>% as.numeric()
}

beep()

edu.class.1.v1 <- which(edu.all.v1 <= 11)
edu.class.2.v1 <- which(edu.all.v1 >= 12)
edu.class.3.v1 <- which(edu.all.v1 >= 0)

edu.class.1.rv <- which(edu.all.rv <= 11)
edu.class.2.rv <- which(edu.all.rv >= 12)
edu.class.3.rv <- which(edu.all.rv >= 0)

for(num in setdiff(1:36, 31)){

   state <- state.codes.char[num] %>% strsplit(split = "") %>% unlist()
   cat("\n\n",state.codes.char[num],state.codes$State[num],"\n\n")
   
   temp.urban.v1.state <- temp.urban.v1[edu.class.3.v1, ] %>% 
                              filter(V13 == state[1] & V14 == state[2])
   temp.urban.rv.state <- temp.urban.rv[edu.class.3.rv, ] %>% 
                              filter(V13 == state[1] & V14 == state[2])
   
   emp.status <- unemp <- id.qivj <- list()
   
   for(i in 1:4) {
      tic()
      
      emp.status[[i]] <- unemp[[i]] <- id.qivj[[i]] <- list()
      
      for(j in 1:4){
         if(j == 1){doc <- temp.urban.v1.state}
         if(j > 1){doc <- temp.urban.rv.state}
         
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
      # if(i %% 2 == 0) beepr::beep(1)
   }
   
   rm(temp.urban.v1.state, temp.urban.rv.state)
   
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
   
   rm(doc, df, id.c1, id.c2, comm.pos, A, B, 
      id.qivj, unemp, emp.status, emp.days)
   
   state.total[[k]] <- list()
   state.total[[k]][[1]] <- 0
   state.total[[k]][[2]] <- 0
   state.total[[k]][[3]] <- 0
   
   X <- E.1.2$X
   Y <- E.1.2$Y
   c1 <- E.1.2$c1
   c2 <- E.1.2$c2
   
   state.total[[num]][[1]] <- unemp.comp(X,Y,c1,c2) %>% unlist()
   print("Q1 vs Q2")
   print(state.total[[num]][[1]])

   X <- E.2.3$X
   Y <- E.2.3$Y
   c1 <- E.2.3$c1
   c2 <- E.2.3$c2
   
   state.total[[num]][[2]] <- unemp.comp(X,Y,c1,c2) %>% unlist()
   print("Q2 vs Q3")
   print(state.total[[num]][[2]])
   
   X <- E.3.4$X
   Y <- E.3.4$Y
   c1 <- E.3.4$c1
   c2 <- E.3.4$c2
   
   state.total[[num]][[3]] <- unemp.comp(X,Y,c1,c2) %>% unlist()
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



