#I did reformatting throughout the entire code
#Trouble shooting will be indicated by comments -AX
require(data.table)
require(dplyr)
require(plyr)
require(taRifx)
require(stringr)
require(tidyr)
ri_bound <- readRDS(file = "./Output Data/01-ri_boundDF.rds")

ri_bound <- ri_bound %>%
  rename(DelG = `ΔG°/ kJ mol-1`, 
       DelH =`ΔH°/ kJ mol-1`,
       TDelS = `TΔS°/ kJ mol-1`,
       DelCp = `ΔCp°/ J mol-1 K-1`)


# Splitting columns containing a variable value and its uncertainty
#Fixed the separator -AX
ri_clean <- ri_bound %>%
        separate(solvent, c("solvent","solvent.specs"),
<<<<<<< HEAD
                 sep = "(?=\\s*\\()", extra = "merge") %>%
=======
                 sep = "(?=\\s*\\()", extra = "merge") %>%select(solvent.specs) %>% unique() %>% View()
>>>>>>> 8b0425472c60a8c5165d37359915ec9f4092279e
        separate(log.K, c("log.K", "log.K.Uncertainty"), 
                 sep = "\\s\\?\\s", extra = "merge") %>%
        separate(DelG, c("DelG", "DelG.Uncertainty"),
                 sep = "\\s\\?\\s",extra = "merge") %>%
        separate(DelH, c("DelH", "DelH.Uncertainty"),
                 sep = "\\s\\?\\s", extra = "merge") %>%
        separate(TDelS, c("TDelS", "TDelS.Uncertainty"), 
                 sep = "\\s\\?\\s",extra = "merge") %>% 
        lapply(str_replace_all, pattern = "\\−", replacement = "-") %>%
        lapply(str_replace_all, pattern = "\\ +", replacement = " ") %>%
        lapply(str_replace_all, pattern = "\\s+", replacement = " ") %>%
        lapply(str_replace_all, pattern = "·", replacement = " ") %>% 
                       as.data.frame()

#SREP LAB is my own personal file
dir.create(path = "~/SREP LAB/R Recoding/Cleaned Data")
saveRDS(object = ri_clean, file = "~/SREP LAB/R Recoding/Cleaned Data/ri_clean.RDS")
save(ri_clean, file = "~/SREP LAB/R Recoding/Cleaned Data/ri_clean.RData")
#=============================================================================== 
#                       "Variable Engineering"                                 =
#===============================================================================
# Remove pH string and \\) to convert the pH Column to numerical. Assume that 
# columns with no value have a pH of 7.0 pH values with a range of 
# phValue1-phValue2 will be assigned an average pH value ((phValue1+phValue2)/2)
# ph's with an inequality (<,>) will be set at the value given guests with 
# the molarity of the acid given in the solvent composition will be set
# at the theoretical value of the solution


# Solvent Composition Pattern extraction (Ratios and Molarity)
pattern1 <- gsub("\n", replacement = "", x = "([0-9]+\\.*[0-9]*\\:[0-9]+
\\.*[0-9]*)|(([0-9]*\\.*[0-9]*\\s+[M]\\s+[A-Za-z]*[A-Za-z0-9]*
\\s*[a-z]*)(\\;*\\,*\\+*\\s+[0-9]+\\.*[0-9]*\\s+[M]\\s+[A-Za-z]*
[A-Za-z0-9]*[a-z]*)*)")

regex.solvent <- str_extract(ri_clean$solvent.specs,pattern1)

# pattern 2 and pattern 3 extract the numeric ph values from solvent composition 
#the \u2212 is the unicode for a "minus" sign, different from the keyboard default hyphen -AX
#putting a regular dash in the code cuts off some entries -AX
#Also summarized the regex a bit -AX
pattern2 <-  "pH\\s\\<*\\>*\\s*[0-9\\.]+\u2212*[0-9\\.]*"
pattern3 <- "[0-9\\.]+\u2212*[0-9\\.]*"
 
regex.pH <- ri_clean$solvent.specs %>%
  str_extract(pattern2)%>%
  str_extract(pattern3)

ph.range <- grep(pattern = "\u2212", x = regex.pH)

ph.average <- strsplit(regex.pH[ph.range], split = "\u2212") %>%
        ldply() %>% 
        transform(V1 = as.numeric(V1), V2 = as.numeric(V2)) %>% 
        rowMeans()

regex.pH[ph.range]      <- ph.average
regex.pH                <- as.numeric(regex.pH)
naindex                 <- is.na(regex.pH)
regex.pH[naindex]       <- 7.0



ri_clean <- tbl_df(ri_clean)%>%
        bind_cols(data_frame(regex.pH, regex.solvent))%>%
        setnames(c(17, 18), c("pH", "solvent.composition"))%>%
        select(1:4, 18, 17, 5:16)

# Setting pH to acidic value when the solution contained an acid
sulfuric_acid <- grep(pattern = "[M]+\\s+\\bH2SO4\\b", 
                      x = ri_clean$solvent.specs)
hcl_acid <- grep(pattern = "M+\\s+\\bHCl\\b", 
                 x = ri_clean$solvent.specs)
ri_clean$pH[sulfuric_acid] <- 1
ri_clean$pH[hcl_acid]  <-  1.21

ri_engineered <- ri_clean %>%
  mutate(clean.guest = str_replace_all(
    string = guest,
    pattern = "\\·+\\s*[A-Z]*[a-z]*",
    replacement = ""
  )) %>%
  transmute(
    clean.guest = str_replace_all(
      string = clean.guest,
      pattern = "\\([di]*[mono]*anion\\)",
      replacement = ""
    )
  ) %>%
  transmute(
    clean.guest = str_replace_all(
      string = clean.guest,
      pattern = "\\s*derivative\\s*(\\(E*Z*\\))*(cis)?(trans)?\\-*[0-9]*",
      replacement = ""
    )
  ) %>%
  transmute(
    clean.guest = str_replace_all(
      string = clean.guest,
      pattern = "[0-9]HCl",
      replacement = "hydrochloride"
    )
  ) %>%
  transmute(clean.guest = str_replace_all(
    string = clean.guest,
    pattern = gsub(
      pattern = "\\n",
      replacement = "",
      x = "\\(\\-*\\±*[0-9A-Z]*\\,*[0-9A-Z]*\\)\\-\\(*\\+*\\-*\\)*\\-*|nor(?!t)|\\([a-z]*\\,*\\s*[a-z]*(I[0-9]\\-)*\\)"
    ),
    replacement = ""
  )) %>%
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
  transmute(
    clean.guest = str_replace(
      string = clean.guest,
      pattern = "cis\\-|trans\\-|\\((?=\\[)",
      replacement = ""
    )
  ) %>%
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
  transmute(
    clean.guest = str_replace(
      string = clean.guest,
      pattern = "\\s[0-9]+[a-z]$|\\s[A-Z][0-9]",
      replacement = ""
    )
  ) %>%
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