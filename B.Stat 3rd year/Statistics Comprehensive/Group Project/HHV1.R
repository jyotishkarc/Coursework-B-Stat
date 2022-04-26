
path <- "D:/My Documents/PLFS-data/FINAL_DATA_2019-20/TEXT/"
path.files <- list.files(path)

HHV1 <- read.csv(paste0(path,path.files[2]), header = FALSE)

temp <- HHV1 %>%
         apply(1, function(val){
                     return(val %>% strsplit(split = "") %>% unlist())
               }) %>% t()

temp.urban <- temp %>% as.data.frame() %>% filter(V12 == 2)

colnames(temp.urban) <- col1 <- c(1:ncol(temp.urban)) %>% 
                                    sapply(function(val) paste0("V",val))

hce <- c()
for (i in 1:nrow(temp.urban)) {
   hce[i] <- paste0(temp.urban[i, 48:55]) %>% 
               stringr::str_c(collapse = "") %>% 
               as.numeric()
}


hce.sig <- hce[hce < 130500]




