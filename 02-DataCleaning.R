#I did reformatting throughout the entire code
#Trouble shooting will be indicated by comments -AX

require(dplyr)
require(stringr)
require(tidyr)
require(stringr)
ri_bound <- readRDS(file = "./Output Data/01-ri_boundDF.rds")

ri_bound <- ri_bound %>%
  rename(DelG = `ΔG°/ kJ mol-1`, 
       DelH =`ΔH°/ kJ mol-1`,
       TDelS = `TΔS°/ kJ mol-1`,
       DelCp = `ΔCp°/ J mol-1 K-1`,
       log.K = `log K`)


# Splitting columns containing a variable value and its uncertainty
#Fixed the separator -AX
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
                 sep = "\\s\\±\\s",extra = "merge", fill = "right") %>%
         lapply(str_replace_all, pattern = "\\−", replacement = "-") %>%
         lapply(str_replace_all, pattern = "\\ +", replacement = " ") %>%
         lapply(str_replace_all, pattern = "\\s+", replacement = " ") %>%
         lapply(str_replace_all, pattern = "·", replacement = " ") %>%
         as_tibble()
         
      
                       


#=============================================================================== 
#                 "Variable Engineering or Imputation"                         =
#===============================================================================
# Remove pH string and \\) to convert the pH Column to numerical. Assume that 
# columns with no value have a pH of 7.0. pH values with a range of 
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
         pH = str_extract(solvent.specs, pattern = "(pH\\s(\\<\\s)*[0-9]+(\\.[0-9]+)*)"),
         pH.range = str_extract(solvent.specs, pattern = "pH\\s[0-9]+\\.[0-9]+\\-[0-9]+(\\.[0-9]+)*"),
         ratio = str_extract(solvent.specs, pattern = "[0-9]+(\\.[0-9]+)*\\:[0-9]+(\\.[0-9]+)*"),
         solvent.molarity = str_extract(solvent.specs, pattern = pattern.molarity)) %>%
  mutate(pH = ifelse(!is.na(pH.range), NA, pH)) %>%
  mutate(pH = str_extract(pH, pattern = pH.numeric)%>% as.numeric())%>%
  separate(., pH.range, c("pH1", "pH2"), sep = "-") %>%
  mutate(pH1 = str_extract(pH1, pattern = pH.numeric) %>% as.numeric(),
         pH2 = as.numeric(pH2)) %>%
  mutate(pH = ifelse(!is.na(pH1), (pH1+pH2)/2, pH)) %>%
  mutate(pH = ifelse(is.na(pH), 7.0, pH))%>%
  select(-pH1, -pH2) 



# pattern 2 and pattern 3 extract the numeric ph values from solvent composition 
#the \u2212 is the unicode for a "minus" sign, different from the keyboard default hyphen -AX
#putting a regular dash in the code cuts off some entries -AX
#Also summarized the regex a bit -AX



# Setting pH to acidic value when the solution contained an acid
sulfuric_acid <- grep(pattern = "[M]+\\s+\\bH2SO4\\b", 
                      x = ri_clean$solvent.specs)

hcl_acid <- grep(pattern = "M+\\s+\\bHCl\\b", 
                 x = ri_clean$solvent.specs)

ri_clean$pH[sulfuric_acid] <- 1
ri_clean$pH[hcl_acid]  <-  1.21


# It could be nice to explain what each of these cleanup step is actually doing
# to guest. I did a quick search in google and was able to find many of the
# structures without modifying the original name
ri_engineered <- ri_clean %>%
  mutate(clean.guest = str_replace_all(
    string = guest,
    pattern = "\\·+\\s*[A-Z]*[a-z]*",
    replacement = ""
  )) %>% 
  # transmute(
  #   clean.guest = str_replace_all(
  #     string = clean.guest,
  #     pattern = "\\([di]*[mono]*anion\\)",
  #     replacement = ""
  #   )
  # ) %>%
  transmute(
    clean.guest = str_replace_all(
      string = clean.guest,
      pattern = "\\s*derivative\\s*(\\(E*Z*\\))*(cis)?(trans)?\\-*[0-9]*",
      replacement = ""
    )
  ) %>%select(guest, clean.guest) %>% unique() %>% View()
  transmute(
    clean.guest = str_replace_all(
      string = clean.guest,
      pattern = "[0-9]HCl",
      replacement = "hydrochloride"
    )
  ) %>%
  # transmute(clean.guest = str_replace_all(
  #   string = clean.guest,
  #   pattern = gsub(
  #     pattern = "\\n",
  #     replacement = "",
  #     x = "\\(\\-*\\±*[0-9A-Z]*\\,*[0-9A-Z]*\\)\\-\\(*\\+*\\-*\\)*\\-*|nor(?!t)|\\([a-z]*\\,*\\s*[a-z]*(I[0-9]\\-)*\\)"
  #   ),
  #   replacement = ""
  # )) %>%
  transmute(clean.guest = str_replace_all(
    string = clean.guest,
    pattern = "\\β",
    replacement = "beta"
  )) %>%
  transmute(clean.guest = str_replace(
    string = clean.guest,
    pattern = "\\α",
    replacement = "alpha"
  )) %>%
  # transmute(
  #   clean.guest = str_replace(
  #     string = clean.guest,
  #     pattern = "cis\\-|trans\\-|\\((?=\\[)",
  #     replacement = ""
  #   )
  # ) %>%
  transmute(clean.guest = str_replace(
    string = clean.guest,
    pattern = "HCl",
    replacement = "hydrochloride"
  )) %>%
  transmute(clean.guest = str_replace(
    string = clean.guest,
    pattern = "H2SO4",
    replacement = "sulfonate"
  )) %>%
  # transmute(
  #   clean.guest = str_replace(
  #     string = clean.guest,
  #     pattern = "\\s[0-9]+[a-z]$|\\s[A-Z][0-9]",
  #     replacement = ""
  #   )
  # ) %>%
  tbl_df() %>%
  bind_cols(ri_clean) %>%
  select(2:3, 1, everything())

#fixed file path -AX
clean_dir       <- "~/SREP LAB/R Recoding/Cleaned Data/"
saveRDS(ri_engineered, file = paste0(clean_dir, "ri_engineered.RDS"))
save(ri_engineered, file = paste0(clean_dir, "ri_engineered.RData"))
                
                
        

ri_clean[ , 6:15] <- ri_clean%>%
        select(pH:TDelS.Uncertainty) %>%
        lapply(destring)

#SREP LAB is my own personal file

saveRDS(object = ri_clean, file = "./Output Data/02-ri_clean.rds")
save(ri_clean, file = "./Output Data/02-ri_clean.RData")