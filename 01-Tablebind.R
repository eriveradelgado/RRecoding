install.packages("dtplyr")
require(dtplyr)
#Again, this directory is my own 
directory <- "./Downloaded Data/ri_table_list.rds"
ri_table_list <- readRDS(directory)

index10 <- ri_table_list %>% 
  lapply(names) %>% 
  lapply(length) == 10 %>% 
  as.vector()

index11 <-  ri_table_list %>% 
  lapply(names) %>% 
  lapply(length) == 11 %>% 
  as.vector()

ri_delcp <- rbindlist(ri_table_list[index11]) %>%
  lapply(as.character) %>%
  as.data.frame(stringsAsFactors = FALSE)

ri_delcp  <- ri_delcp[ , c(1:8, 10, 11, 9)]

ri_nodelcp  <- rbindlist(ri_table_list[index10]) %>% 
  lapply(as.character) %>% 
  as.data.frame(stringsAsFactors = F) 

ri_nodelcp[ , 11] <- NA

ri_boundDF <- rbindlist(list(ri_delcp, ri_nodelcp))
dir.create("~/SREP LAB/R Recoding/Bound Data")

save(ri_boundDF, file = "~/SREP LAB/R Recoding/Bound Data/ri_boundDF.RData")
saveRDS(ri_boundDF, file = "~/SREP LAB/R Recoding/Bound Data/ri_boundDF.RDS")
