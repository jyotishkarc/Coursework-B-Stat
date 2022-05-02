library(dplyr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

PERV1 <- read.csv(paste0(path, path.files[4]), header = FALSE)

temp <- PERV1 %>%
   apply(1, function(val){
      return(val %>% strsplit(split = "") %>% unlist())
   }) %>% t()

colnames(temp) <- col1 <- c(1:ncol(temp)) %>% 
   sapply(function(val) paste0("V",val))

temp.urban <- temp %>% as.data.frame() %>% filter(V12 == 2)

inc1 <- inc2 <- hrs <- c()
hrs.pos <- q <- list(c(102,103), c(113,114), c(128,129), c(139,140),
                     c(154,155), c(172,173), c(180,181), c(191,192),
                     c(206,207), c(217,218), c(232,233), c(243,244),
                     c(258,259), c(269,270))

for (i in 1:nrow(temp.urban)) {
   inc1[i] <- paste0(temp.urban[i, 287:294]) %>% 
                  stringr::str_c(collapse = "") %>% 
                  as.numeric()
   
   inc2[i] <- paste0(temp.urban[i, 295:302]) %>% 
                  stringr::str_c(collapse = "") %>% 
                  as.numeric()
   
   hrs[i] <- lapply(hrs.pos, function(vec){
               paste0(temp.urban[i, vec[1]:vec[2]]) %>% 
                  stringr::str_c(collapse = "") %>% 
                  as.numeric()
               }) %>% unlist() %>% sum()
}

inc2[inc2 < 0] <- 0
inc <- inc1 + inc2

hce.sig <- hce[hce > 5 & hce < 130500]


################

ks.test(rnorm(1000,mean = 1, sd = 1)-1,"pnorm")

ks.test((log(hce) + rnorm(length(hce),5,1) - 14.23876)/1.6228533,"pnorm")


for(i in 1:1000){
   J <- ks.test((log(hce.sig) + rnorm(length(hce.sig),1,sd = sqrt(V/10)) - M - 1)/sqrt(V+V/10),
                "pnorm")
   if(J$p.value > 0.05){
      a <- a+1
   }
}
