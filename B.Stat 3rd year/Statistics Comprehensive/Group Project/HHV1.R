library(dplyr)

path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

HHV1 <- read.csv(paste0(path,path.files[2]), header = FALSE)

temp <- HHV1 %>%
         apply(1, function(val){
                     return(val %>% strsplit(split = "") %>% unlist())
               }) %>% t()

colnames(temp) <- col1 <- c(1:ncol(temp)) %>% 
                              sapply(function(val) paste0("V",val))

temp.urban <- temp %>% as.data.frame() %>% filter(V12 == 2)

hce <- c()
for (i in 1:nrow(temp.urban)) {
   hce[i] <- paste0(temp.urban[i, 48:55]) %>% 
               stringr::str_c(collapse = "") %>% 
               as.numeric()
}

hce.sig <- hce[hce > 5 & hce < 130500]


################

# ks.test(rnorm(1000,mean = 1, sd = 1)-1,"pnorm")

#### Box-Cox Transformation
if(FALSE){
   G <- boxcox(hce.sig ~ 1)
   lmb <- G$x[which.max(G$y)]
   tr.hce <- (hce.sig^lmb - 1)/lmb
}

if(TRUE){
   if(TRUE) {
      tr.hce <- G <- VGAM::yeo.johnson(hce.sig, 0.01)
   }
   
   MASS::fitdistr(tr.hce, "normal") -> param
   M <- param$estimate[1]
   S <- param$estimate[2]
   
   ks.test((tr.hce + rnorm(length(tr.hce), 1, sd = sqrt(S^2/10))-M-1)/sqrt(S^2+S^2/10),
           "pnorm")
}

for(i in 1:1000){
   J <- ks.test((log(hce.sig) + rnorm(length(hce.sig),1,sd = sqrt(V/10)) - M - 1)/sqrt(V+V/10),
                "pnorm")
   if(J$p.value > 0.05){
      a <- a+1
   }
}
