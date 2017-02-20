require(tidyverse)

directory <- "./Output Data/00-ri_table_list.rds"
ri_table_list <- readRDS(directory)

# Cleaning HTML import prior to binding due to:
# (1) Ref Column in list entry 26 is an integer when Ref columns in other 
#     list entries are integer

ri_table_list[[26]][11] <- ri_table_list[[26]][11] %>% map(as.character)

# (2) Some list entries [14,17,21] are actually table entries that were imported 
#     as empty table names. Some table list entries [15,18,22] had no title 
#     and its colnames were actual results from the paper.

ri_15 <- ri_table_list[[15]] %>% names()
names(ri_table_list[[15]]) <- c("host",
                                "guest",
                                "solvent",
                                "T/K",
                                "log K",
                                "ΔG°/ kJ mol-1",
                                "ΔH°/ kJ mol-1",
                                "TΔS°/ kJ mol-1",
                                "methoda" ,
                                "ref")

ri_18 <- ri_table_list[[18]] %>% names()

names(ri_table_list[[18]]) <- c("host",
                                "guest",
                                "solvent",
                                "T/K",
                                "log K",
                                "ΔG°/ kJ mol-1",
                                "ΔH°/ kJ mol-1",
                                "TΔS°/ kJ mol-1",
                                "methoda" ,
                                "ref")

ri_22 <- ri_table_list[[22]] %>% names()
names(ri_table_list[[22]]) <- c("host",
                                "guest",
                                "solvent",
                                "T/K",
                                "log K",
                                "ΔG°/ kJ mol-1",
                                "ΔH°/ kJ mol-1",
                                "TΔS°/ kJ mol-1",
                                "methoda" ,
                                "ref")

# (2.3) List table 21 is missing the log K value


ri_table_list[[21]] <- ri_table_list[[21]] %>%
  names() %>% 
  data_frame(entries =. ) %>%
  mutate(key = c("host",
                 "guest",
                 "solvent",
                 "T/K",
                 "ΔG°/ kJ mol-1",
                 "ΔH°/ kJ mol-1",
                 "TΔS°/ kJ mol-1",
                 "methoda" ,
                 "ref")) %>% 
  spread(key = key, value = entries) %>%
  mutate("T/K" = as.integer(`T/K`),
         "log K" = "NA")


# (2.2) List table 17 is missing its reference (Likely to be reference  
# 242 since all other 5-methoxyresorcinol are from reference 242)
ri_table_list[[17]] <- ri_table_list[[17]] %>% 
  names() %>% 
  data_frame(entries = . ) %>%
  mutate(key = c("host",
                 "guest",
                 "solvent",
                 "T/K",
                 "log K",
                 "ΔG°/ kJ mol-1",
                 "ΔH°/ kJ mol-1",
                 "TΔS°/ kJ mol-1",
                 "methoda")) %>%
  spread(key = key, value = entries) %>%
  mutate("T/K" = as.integer(`T/K`),
         ref = "NA")

# (2.1) List table 14 is missing the log K value
ri_table_list[[14]] <- ri_table_list[[14]] %>% 
  names() %>% 
  data_frame(entries = . ) %>%
  mutate(key = c("host",
                 "guest",
                 "solvent",
                 "T/K",
                 "ΔG°/ kJ mol-1",
                 "ΔH°/ kJ mol-1",
                 "TΔS°/ kJ mol-1",
                 "methoda" ,
                 "ref")) %>% 
  spread(key = key, value = entries) %>%
  mutate("T/K" = as.integer(`T/K`),
         "log K" = "NA")

# (3) Remove the summary output from the linear regression and save into its own
# data frame

delThermPararm_delAlk <- ri_table_list[[28]] 

summary_table <- ri_table_list[[29]]

ri_table_list[[29]] <- NULL
ri_table_list[[28]] <- NULL

ri_table_list %>%
  tibble() %>%
  unnest() -> ri_10and11col_allhost

# Saving efforts to file

save(ri_10and11col_allhost, file = "./Output Data/01-ri_boundDF.RData")
saveRDS(ri_10and11col_allhost, file = "./Output Data/01-ri_boundDF.rds")
save(delThermPararm_delAlk, file = "./Output Data/01.2-delThermodynamicParameter_delCcontent.RData")
saveRDS(delThermPararm_delAlk, file = "./Output Data/01.2-delThermodynamicParameter_delCcontent.rds")
save(summary_table, file = "./Output Data/01.3-DifferentCyclodextrins_SummaryTable.RData")
save(summary_table, file = "./Output Data/01.3-DifferentCyclodextrins_SummaryTable.rds")