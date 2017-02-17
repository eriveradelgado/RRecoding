#=============================================================================== 
#                   Downloading data from ACS                                  =
#===============================================================================

require(xml2)  # Import XML files
require(tidyverse) # Collection of tidy data analysis workflow libraries
require(rvest)
# Access Rekharsky and Inoue's paper in ACS Chemical Reviews 1998
# and save page as HTML to the project working directory.
# URL: http://pubs.acs.org/doi/full/10.1021/cr970015o. *Paper behind paywall

file <- "./Downloaded Data/Complexation Thermodynamics of Cyclodextrins - Chemical Reviews (ACS Publications).htm"
read_html(file) %>%
  html_table(header = T) -> ri_table_list 

dir.create("./Output Data")

save(ri_table_list, file = "./Output Data/00-ri_table_list.RData")
saveRDS(ri_table_list, file = "./Output Data/00-ri_table_list.rds")



