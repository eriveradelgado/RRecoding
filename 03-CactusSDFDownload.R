library(tidyverse)
library(RCurl)
library(stringr)

dataset <-readRDS(file = "../RRecoding/Output Data/02-ri_clean.rds") 
# Consider using webchem a library for chemical queries

guest.sdf <- dataset %>% 
  select(guest) %>%
  unique() %>% 
  rowwise() %>%
  mutate(encoded = URLencode(guest, reserved = T)) %>% 
  mutate(url = paste0("https://cactus.nci.nih.gov/chemical/structure/", encoded, "/sdf")) %>%
  mutate(sdf = try(getURL(url = url))) 


# Cactus was able to resolve 475 out of 594 molecules for an 80% success rate

guest.sdf %>% filter(str_detect(string = sdf, pattern = "Page")) %>% count()

problem.sdf <- guest.sdf %>%
  filter(str_detect(sdf, "Page")) %>%
  select(guest)

problem.sdf %>% 
  filter(!str_detect(guest, pattern = "anion")) %>%
  filter(!str_detect(guest, pattern = "carboxylate")) %>%
  filter(!str_detect(guest, pattern = "[0-9][Hh]ydrochloride")) %>%
  filter(!str_detect(guest, pattern = "\\‘")) %>%
  filter(!str_detect(guest, pattern = "ferrocen")) %>% write.csv("../RRecoding/Output Data/02.1-problemSDF.csv")

# Cleaning issues with download :

# 1. Presence of anionic species - 26 cases

  
problem.sdf <- problem.sdf %>%
  rowwise() %>%
  mutate(guest.charge = str_extract(guest, pattern = "anion|monoanion|dianion"),
         guest = gsub(x = guest,
                      pattern = "\\(anion\\)|\\(monoanion\\)|\\(dianion\\)",
                      replacement = "")) %>%


# 3. Carboxylate is carboxylic acid - 2


# 4. 2HCl should be dihydrichloride - 4
guest.sdf %>% filter(str_detect(guest, "[0-9]hydrochloride"))

# 5. Unconventional apostrophe \\‘  - 7
guest.sdf %>% filter(str_detect(guest, "\\‘")) 



# 6. non covalent structures(ferrocenes), using inchikey instead- 10
guest.sdf %>%  str_replace(guest, pattern = "(S)-1-ferrocenylethanol|(R)-1-ferrocenylethanol",
                                 replacement = "1-(Ferrocenyl)ethanol")

# 7. Name not compatible by any search engine (Cactus, Google, Chemspider). Assumed 
#    to be close to the one replaced - 1


# 8. viologen compounds 

filter(str_detect(guest, "viologen"))

# 8. Misspellings by hand? -  8

# Option 1 detect and replace
# Reassign name
data.frame(
  pattern = "biebricht scarlet", replacement = "biebrich scarlet",
  pattern = "4-([(4-hydroxyphenyl)azo]benzoate", replacement = "4-[(4-hydroxyphenyl)azo]benzoate",
  pattern ="3-(aminomethyl)proxyl", replacement = "3-(aminomethyl)-proxyl",
  pattern = "3-carbamoylproxyl", replacement = "3-carbamoyl-proxyl",
  pattern = "l-α-O-benzylglycerol", replacement = "1-o-benzyl-rac-glycerol",
  pattern = "4-nitrophenyl-β-d-glucoside", replacement ="4-Nitrophenyl-beta-D-glucopyranoside",
  pattern = "3-(aminomethyl)proxyl", replacement ="3-aminomethyl-proxyl",
  pattern = "bromodiphenhydramine hydrochloride", replacement = "Bromdiphenhydramine hydrochloride",
  pattern ="diammine(1,1-cyclobutane- dicarboxylato)platinum(II)" , replacement = "cis-diammine(1,1-cyclobutanedicarboxylato)platinum(II)",
  pattern = "(1R,2S)-(-)-ephedrine", replacement = "L-Ephedrine",
  pattern = "(1S,2R)-(+)-ephedrine", replacement = "D-Ephedrine",
  pattern = "sulfoisomidine", replacement = "sulfisomidine",
  pattern = "diphenyhydramine hydrochloride", replacement = "diphenhydramine hydrochloride",
  pattern = "trans,trans-2,4- hexadienedioic (muconic) acid", replacement = "muconic acid",
  pattern = "hexyl-β-d-glucopyranoside", replacement = "hexyl-β-d-glucopyranoside",
  pattern = "4-(hydroxyphenethyl)ammonium", replacement = "",
  pattern = "pentakis(ethyleneglycol) monohexyl ether)", replacement = "2-(Hexyloxy)ethanol",
  pattern ="2-thiophenobarbital", replacement = "5-Ethyl-5-phenyl-2-thioxodihydro-4,6(1H,5H)-pyrimidinedione",
  pattern ="4-(hydroxyphenethyl)ammonium" , replacement ="2-(4-hydroxyphenyl)ethylazanium") %>%
  gather("argument", "chemical.name") %>% rowwise() %>%
  mutate(argument = str_replace(argument, pattern = "\\.[0-9]+", replacement = "")) %>% View()
  spread(key = argument, value = "chemical.name")


  
    
    
    
    
  problem.sdf %>% 
  filter(!str_detect(guest, pattern = "anion")) %>%
  filter(!str_detect(guest, pattern = "carboxylate")) %>%
  filter(!str_detect(guest, pattern = "[0-9][Hh]ydrochloride")) %>%
  filter(!str_detect(guest, pattern = "\\‘")) %>%
  filter(!str_detect(guest, pattern = "ferrocen")) %>%  mutate(copythis = paste0("pattern = ", guest, ", ", "replacement = `` ")) %>% View()
