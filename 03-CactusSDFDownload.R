library(tidyverse)
library(stringr)
library(RCurl)
# Testing connection from MBPSG

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
  filter(!str_detect(guest, pattern = "ferrocen")) %>%
#   paste0
#   write.csv("../RRecoding/Output Data/02.1-problemSDF.csv")

# Cleaning issues with download :

# 1. Presence of anionic species - 26 cases

  
problem.sdf <- problem.sdf %>%
  rowwise() %>%
  mutate(guest.charge = str_extract(guest, pattern = "anion|monoanion|dianion"),
         guest = gsub(x = guest,
                      pattern = "\\(anion\\)|\\(monoanion\\)|\\(dianion\\)",
                      replacement = "")) %>%



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

# 8. Manual replacements
# Reassign name

fixed.sdf <- data.frame(
  pattern = "biebricht scarlet", replacement = "biebrich scarlet",
  pattern = "3-(aminomethyl)proxyl", replacement = "3-(aminomethyl)-proxyl",
  pattern = "3-carbamoylproxyl", replacement = "3-carbamoyl-proxyl",
  pattern = "l-α-O-benzylglycerol", replacement = "1-o-benzyl-rac-glycerol",
  pattern = "4-nitrophenyl-β-d-glucoside", replacement ="4-Nitrophenyl-beta-D-glucopyranoside",
  pattern = "bromodiphenhydramine hydrochloride", replacement = "Bromdiphenhydramine hydrochloride",
  pattern = "diammine(1,1-cyclobutane- dicarboxylato)platinum(II)", replacement = "cis-diammine(1,1-cyclobutanedicarboxylato)platinum(II)",
  pattern = "(1R,2S)-(-)-ephedrine", replacement = "L-Ephedrine",
  pattern = "(1S,2R)-(+)-ephedrine", replacement = "D-Ephedrine",
  pattern = "sulfoisomidine", replacement = "sulfisomidine",
  pattern = "diphenyhydramine hydrochloride", replacement = "diphenhydramine hydrochloride",
  pattern = "trans,trans-2,4- hexadienedioic (muconic) acid", replacement = "muconic acid",
  pattern = "hexyl-β-d-glucopyranoside", replacement = "hexyl-β-d-glucopyranoside",
  pattern = "pentakis(ethyleneglycol) monohexyl ether)", replacement = "2-(Hexyloxy)ethanol",
  pattern = "2-thiophenobarbital", replacement = "5-Ethyl-5-phenyl-2-thioxodihydro-4,6(1H,5H)-pyrimidinedione",
  pattern = "4-(hydroxyphenethyl)ammonium", replacement ="2-(4-hydroxyphenyl)ethylazanium",
  pattern = 'carbazole-viologen linked compound 28a', replacement = 'NA',
  pattern = 'carbazole-viologen linked compound 28b', replacement = 'NA',
  pattern = 'carbazole-viologen linked compound 28c', replacement = 'NA',
  pattern = '(1R,2S)-(-)-ephedrine', replacement = 'L-Ephedrine',
  pattern = '(1S,2R)-(+)-ephedrine', replacement = 'D-Ephedrine',
  pattern = '4-[(4-hydroxyphenyl)azo]- naphthalene-1-sulfonate', replacement = 'NA',
  pattern = 'd-mandelate', replacement = '(R)-Mandelate',
  pattern = 'l-mandelate', replacement = '(S)-Mandelate',
  pattern = 'methapyriline hydrochloride', replacement = 'Methoxylene',
  pattern = 'methyl red (cation, protonated)', replacement = 'methyl red',
  pattern = 'modant yellow 7', replacement = 'Disodium 3-methyl-5-((4-sulphonatophenyl)azo)salicylate',
  pattern = '4-nitrophenyl-β-d-galactoside', replacement = ' 4-Nitrophenylgalactoside',
  pattern = '4-nitrophenyl-β-d-glucosamide', replacement = 'N-[(2R,3R,4R,5S,6R)-4,5-dihydroxy-6-(hydroxymethyl)-2-[(4-nitrophenyl)methoxy]oxan-3-yl]acetamide',
  pattern = '4-nitrophenyl-β-d-xyloside', replacement = '(2S,3R,4S,5R)-2-(4-nitrophenoxy)oxane-3,4,5-triol',
  pattern = '(±)-norphenylephrine', replacement = '3-(2-amino-1-hydroxyethyl)phenol',
  pattern = '(±)-octopamine', replacement = '4-(2-amino-1-hydroxyethyl)phenol',
  pattern = 'pentakis(ethyleneglycol) monohexyl ether', replacement = '2-hexoxyethanol',
  pattern = '(R)-(-)-phenylephrine', replacement = '[(2R)-2-hydroxy-2-(3-hydroxyphenyl)ethyl]-methylazanium',
  pattern = 'phenyl-β-d-glucopyranoside', replacement = '(2R,3S,4S,5R,6S)-2-(hydroxymethyl)-6-phenoxyoxane-3,4,5-triol',
  pattern = 'd-phenyltrifluoroethanol', replacement = ' 2,2,2-trifluoro-1-phenylethanol',
  pattern = 'pyrilammonium maleate', replacement = 'pyrilamine maleate',
  pattern = '(E)-stilbene derivative (E)-30', replacement = 'NA',
  pattern = '(Z)-stilbene derivative (Z)-30', replacement = 'NA',
  pattern = 'terfenadine hydrochloride', replacement = '1-(4-tert-butylphenyl)-4-[4-[hydroxy(diphenyl)methyl]piperidin-1-yl]butan-1-ol hydrochloride',
  pattern = 'triiodide (I3-)', replacement = 'triiodide',
  pattern = 'l-tryprophan', replacement = 'l-tryptophan',
  pattern = 'Tyr-Gly-Gly-Phe-Leu', replacement = 'Leucine enkephalin',
  pattern = '1-adamantaneammonium', replacement = '1-Adamantanaminium',
  pattern = '(±)-anisodamine', replacement = 'anisodamine',
  pattern = '(-)-anisodamine', replacement = 'anisodamine',
  pattern = '(-)-anisodamine HBr', replacement = '[(3S,6S)-6-hydroxy-8-methyl-8-azabicyclo[3.2.1]octan-3-yl] (2R)-3-hydroxy-2-phenylpropanoate',
  pattern = '(-)-anisodine HBr', replacement = 'Anisodine hydrobromide',
  pattern = '(±)-atropine H2SO4', replacement = 'HOBWAPHTEJGALG-UHFFFAOYSA-N',
  pattern = '(4Z,15Z)-bilirubin-IXa', replacement = 'BPYKTIZUTYGOLE-KDUUSRDASA-N',
  pattern = '(4Z,15Z)-bilirubin-IXa + cyclooctanol', replacement = 'BPYKTIZUTYGOLE-KDUUSRDASA-N',
  pattern = 'dansyl-l-hydroxyplorine', replacement = 'dansyl-l-hydroxyproline',
  pattern = 'di-2-(1-adamantyl)ethyl hydrogen phosphate', replacement = 'NA',
  pattern = 'dicumarol', replacement = '4-hydroxy-3-[(4-hydroxy-2-oxochromen-3-yl)methyl]chromen-2-one',
  pattern = 'ethylbis(coumacetate)', replacement = 'Ethyl biscoumacetate',
  pattern = 'ethylthiobarbituic acid', replacement = '5-Ethyl-2-thioxodihydropyrimidine-4,6(1H,5H)-dione',
  pattern = '4-[(4-hydroxy-1-naphthyl)azo]- naphthalene-1-sulfonate', replacement = '4-[(4-hydroxy-1-naphthyl)azo]- naphthalene-1-sulfonic acid',
  pattern = 'maprotilin', replacement = 'maprotiline',
  pattern = '2-naphthalenesulfonate derivative 31', replacement = 'NA',
  pattern = 'naproxenate', replacement = 'NA',
  pattern = 'd-nitroxide 32a', replacement = 'NA',
  pattern = 'l-nitroxide 32b', replacement = 'NA',
  pattern = 'nortriptylin', replacement = ' Aventyl',
  pattern = 'protriptylin', replacement = 'protriptyline',
  pattern = '(-)-scopolamine HBr', replacement = '(-)-scopolamine hydrobromide',
  pattern = 'sulfasnilamide', replacement = '4-aminobenzenesulfonamide',
  pattern = 'sulfathidole', replacement = 'sulfaethidole',
  pattern = 'thiophenobarbital', replacement = '5-(1,1,2,2,2-pentadeuterioethyl)-5-phenyl-2-sulfanylidene-1,3-diazinane-4,6-dione',
  pattern = '6-(p-toluidinyl)-2-naphthalenesufonate', replacement = '6-(p-toluidinyl)-2-naphthalenesulfonic acid',
  pattern = 'α-(2,4,6-trimethoxyphenyl)benzyl tert-butyl nitroxide', replacement = 'NA',
  pattern = '4-([(4-hydroxyphenyl)azo]benzoate', replacement = '(E)-4-[(4-Hydroxyphenyl)azo]benzoic acid',
  pattern = 'cis-1,2,3,4-tetraphenylcyclobutane derivative cis-33', replacement = 'NA',
  pattern = 'trans-1,2,3,4-tetraphenylcyclobutane derivative trans-33', replacement = 'NA')


pattern.replacement <- fixed.sdf %>%
  gather("argument", "chemical.name") %>% 
  separate(argument, into = c("argument", "attribute"),sep = "\\.", fill ="right") %>% 
  spread(key = argument, value = "chemical.name") %>%
  select(-attribute) 
  
    
    
problem.sdf %>%
  filter(!str_detect(guest, pattern = "anion")) %>%
  filter(!str_detect(guest, pattern = "carboxylate")) %>%
  filter(!str_detect(guest, pattern = "[0-9][Hh]ydrochloride")) %>%
  filter(!str_detect(guest, pattern = "\\‘")) %>%
  filter(!str_detect(guest, pattern = "ferrocen")) %>%
  full_join(., fixed.sdf, by = c("guest" = "pattern")) %>% 
  mutate(copythis = paste0("pattern = '", guest, "', ", "replacement = '", replacement, "',")) %>% 
  select(copythis) %>%
  write.table("../RRecoding/Output Data/02.1-problemSDF.txt",quote = F, row.names = F)
