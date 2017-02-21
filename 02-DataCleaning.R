#I did reformatting throughout the entire code
#Trouble shooting will be indicated by comments -AX

require(tidyverse)
require(stringr)
# require(tidyr)
# require(stringr)
ri_bound <- readRDS(file = "./Output Data/01-ri_boundDF.rds")


# Rename columns 

ri_bound <- ri_bound %>%
  rename(DelG = `ΔG°/ kJ mol-1`, 
         DelH =`ΔH°/ kJ mol-1`,
         TDelS = `TΔS°/ kJ mol-1`,
         DelCp = `ΔCp°/ J mol-1 K-1`,
         log.K = `log K`,
         method = `methoda`)


# Splitting columns containing a variable value and its uncertainty

ri_clean <- ri_bound %>% 
  separate(solvent, c("solvent","solvent.specs"),
           sep = "(?=\\s*\\()", extra = "merge", fill = "right") %>% 
  separate(log.K, c("log.K", "log.K.Uncertainty"), 
           sep = "\\s\\±\\s", extra = "merge", fill = "right") %>% 
  separate(DelG, c("DelG", "DelG.Uncertainty"),
           sep = "\\s\\±\\s",extra = "merge", fill = "right") %>%
  separate(DelH, c("DelH", "DelH.Uncertainty"),
           sep = "\\s\\±\\s", extra = "merge", fill = "right") %>%
  separate(TDelS, c("TDelS", "TDelS.Uncertainty"), 
           sep = "\\s\\±\\s",extra = "merge", fill = "right") %>%
  separate(DelCp, c("DelCp", "DelCp.Uncertainty"), 
           sep = "\\s\\±\\s",extra = "merge", fill = "right")

# Cleaning strings that contain inconsistent patterns such as more than one space
# separation and unconventional separation symbols. Converting alpha, beta and
# gamma symbols to words for easier subset of tables 

ri_clean <- ri_clean %>%
  lapply(str_replace_all, pattern = "\\−", replacement = "-") %>%
  lapply(str_replace_all, pattern = "\\ +", replacement = " ") %>%
  lapply(str_replace_all, pattern = "\\s+", replacement = " ") %>%
  lapply(str_replace_all, pattern = "·", replacement = " ") %>% 
  as_tibble() %>%
  mutate(host = str_replace(host, pattern =  "\\α",replacement = "alpha" )) %>%
  mutate(host = str_replace(host, pattern = "\\β|\\B\\s+H\\+", replacement = "beta")) %>%
  mutate(host = str_replace(host, pattern = "\\γ", replacement = "gamma"))
         
      
                       


#=============================================================================== 
#                             pH Imputation                                    =
#===============================================================================
# Remove pH string and parenthesis to convert the pH Column to numerical. Assume 
# that columns with no value have a pH of 7.0. pH values with a range of 
# phValue1-phValue2 will be assigned an average pH value ((phValue1+phValue2)/2)
# ph's with an inequality (<,>) will be set at the value given. Guests with 
# the molarity of the acid given in the solvent composition will be set
# at the theoretical value of the solution

pattern.molarity <- gsub("\n", replacement = "", x = "(([0-9]*\\.*[0-9]*\\s+[M]
\\s+[A-Za-z]*[A-Za-z0-9]*\\s*[a-z]*)(\\;*\\,*\\+*\\s+[0-9]+\\.*[0-9]*\\s+[M]
\\s+[A-Za-z]*[A-Za-z0-9]*[a-z]*)*)")

pH.numeric <-  "[0-9]+\\.*[0-9]*"

ri_clean <- ri_clean %>% 
  mutate(ref.notes= str_extract(ref, pattern = "[:alpha:]"),
         ref = str_extract(ref, pattern = "\\d+(\\,\\s\\d+)*"),
         pH = str_extract(solvent.specs, 
                          pattern = "(pH\\s(\\<\\s)*[0-9]+(\\.[0-9]+)*)"),
         pH.range = str_extract(solvent.specs,
                                pattern = "pH\\s[0-9]+\\.[0-9]+\\-[0-9]+(\\.[0-9]+)*"),
         solvent.ratio = str_extract(solvent.specs,
                                     pattern = "[0-9]+(\\.[0-9]+)*\\:[0-9]+(\\.[0-9]+)*"),
         solvent.molarity = str_extract(solvent.specs, 
                                        pattern = pattern.molarity)) %>%
  mutate(pH = ifelse(!is.na(pH.range), NA, pH)) %>%
  mutate(pH = str_extract(pH, pattern = pH.numeric)%>% as.numeric())%>%
  separate(., pH.range, c("pH1", "pH2"), sep = "-") %>%
  mutate(pH1 = str_extract(pH1, pattern = pH.numeric) %>% as.numeric(),
         pH2 = as.numeric(pH2)) %>%
  mutate(pH = ifelse(!is.na(pH1), (pH1+pH2)/2, pH)) %>%
  mutate(pH = ifelse(is.na(pH), 7.0, pH))%>%
  select(-pH1, -pH2) 



# Setting pH to acidic value when the solution contained an acid

sulfuric_acid <- grep(pattern = "[M]+\\s+\\bH2SO4\\b", 
                      x = ri_clean$solvent.specs)

hcl_acid <- grep(pattern = "M+\\s+\\bHCl\\b", 
                 x = ri_clean$solvent.specs)

ri_clean$pH[sulfuric_acid] <- 1
ri_clean$pH[hcl_acid]  <-  1.21

# ------------------------------------------------------------------------------
#                          Reconsider this Section
# ------------------------------------------------------------------------------
# It would be nice to explain what each of these cleanup step is actually doing
# to the guest column and why is needed. I did a quick search in google 
# and was able to find many   of the structures without modifying the original 
# name. Maybe a guest.special.cases column could be more useful - ERD
# 
# ri_engineered <- ri_clean %>%
#   mutate(clean.guest = str_replace_all(
#     string = guest,
#     pattern = "\\·+\\s*[A-Z]*[a-z]*",
#     replacement = ""
#   )) %>% 
#   mutate(
#     clean.guest = str_replace_all(
#       string = clean.guest,
#       pattern = "[0-9]HCl",
#       replacement = "hydrochloride"
#     )
#   ) %>%
#   mutate(clean.guest = str_replace_all(
#     string = clean.guest,
#     pattern = "\\β",
#     replacement = "beta"
#   )) %>%
#   mutate(clean.guest = str_replace(
#     string = clean.guest,
#     pattern = "\\α",
#     replacement = "alpha"
#   )) %>%
# 
#   mutate(clean.guest = str_replace(
#     string = clean.guest,
#     pattern = "HCl",
#     replacement = "hydrochloride"
#   )) %>%
#   mutate(clean.guest = str_replace(
#     string = clean.guest,
#     pattern = "H2SO4",
#     replacement = "sulfonate"
#   )) %>% select(guest, clean.guest) %>% unique() %>% View()
#   tbl_df() %>%
#   bind_cols(ri_clean) %>%
#   
# 
# #fixed file path -AX
# clean_dir       <- "./Output Data/"
# saveRDS(ri_engineered, file = paste0(clean_dir, "02-ri_engineered.RDS"))
# save(ri_engineered, file = paste0(clean_dir, "ri_engineered.RData"))
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#                             Formatting Table           
# Reorganizing Table  into the following colunn families vector types
# Host <chr> | Guest <chr> | Solvent Specs <chr>| pH + Thermodynamic Values (dbl)
# | Method+References <chr>
ri_clean <- ri_clean %>%
  select(1:4, 20:21, pH, 5:13, DelCp, DelCp.Uncertainty, method, ref, ref.notes) %>%
  mutate_at(vars(7:18), as.numeric) 
        

# -----------------------------------------------------------------------------
#                     Save output of script 

saveRDS(ri_clean, file = "./Output Data/02-ri_clean.rds")
save(ri_clean, file = "./Output Data/02-ri_clean.RData")