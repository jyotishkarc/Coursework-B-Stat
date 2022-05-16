library(dplyr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

HHV1 <- read.csv(paste0(path,path.files[2]), header = FALSE)
HHRV <- read.csv(paste0(path,path.files[1]), header = FALSE)

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

#### Urban
temp.urban.v1 <- temp.v1 %>% as.data.frame() %>% filter(V12 == 2)
temp.urban.rv <- temp.rv %>% as.data.frame() %>% filter(V12 == 2)

q1v2 <- q2v3 <- q3v4 <- c()

## Q1 V2 Urban
temp.q1v2 <- temp.rv %>% as.data.frame() %>% 
               filter(V9 == 1 & V11 == 2 & V12 == 2)

for (i in 1:nrow(temp.q2v3)) {
   q1v2[i] <- temp.q1v2[i, 12:37] %>% paste0() %>% 
                  stringr::str_c(collapse = "")
}


## Q2 V3 Urban

temp.q2v3 <- temp.rv %>% as.data.frame() %>% 
               filter(V9 == 2 & V11 == 3 & V12 == 2)

for (i in 1:nrow(temp.q2v3)) {
   q2v3[i] <- temp.q2v3[i, 12:37] %>% paste0() %>% 
                  stringr::str_c(collapse = "")
}


## Q3 V4 Urban

temp.q3v4 <- temp.rv %>% as.data.frame() %>% 
   filter(V9 == 3 & V11 == 4 & V12 == 2)

for (i in 1:nrow(temp.q3v4)) {
   q3v4[i] <- temp.q3v4[i, 12:37] %>% paste0() %>% 
      stringr::str_c(collapse = "")
}


####################################################

hce <- c()
for (i in 1:nrow(temp.urban)) {
   hce[i] <- paste0(temp.urban[i, 48:55]) %>% 
      stringr::str_c(collapse = "") %>% 
      as.numeric()
}










