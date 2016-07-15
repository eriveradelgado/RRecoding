#=============================================================================== 
#                   Downloading data from ACS                                  =
#===============================================================================
install.packages(c('XML','RCurl'))
require(XML)  # Import XML files
require(RCurl) # Read webpages with special access requirements


# Solution by spacedman from stackoverflow
# http://stackoverflow.com/questions/3616406/
# object-moved-error-in-using-the-rcurl-geturl-function-in-order-to-access-an-as

url           <- "http://pubs.acs.org/doi/full/10.1021/cr970015o"
file          <- getURL(url,.opts = curlOptions(followlocation=TRUE, cookiefile="nosuchfile"))
ri_table_list <- readHTMLTable(url, header = T, as.data.frame = T, stringAsFactors = F)
dateaccessed  <- date()
#Adjusting the path to lead to my own folder
new_dir       <- "~/SREP LAB/R Recoding/Downloaded Data/"
dir.create(path = new_dir)
save(ri_table_list, file = paste0(new_dir, "ri_table_list.RData"))
saveRDS(ri_table_list,file = paste0(new_dir, "ri_table_list.RDS"))



