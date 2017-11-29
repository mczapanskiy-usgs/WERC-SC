## This script reformats Fefer (1983) and Pyle and Pyle (2017) to be compatable 
## also create chuncks for "MHIatlas_dataFormat" R markdown

setwd("~/WERC-SC/MHIatlas")

## load packages
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
library(mosaic)
library(markdown)
library(knitr)

## load data
read.csv('~/WERC-SC/MHIatlas/Pyle&Pyle2017_SEHI.csv',
         stringsAsFactors = FALSE) -> pyle
read.csv('~/WERC-SC/MHIatlas/Pyle&Pyle2017_coordinates.csv',
         stringsAsFactors = FALSE) -> pyle_coords
read.csv('~/WERC-SC/MHIatlas/Fefer1983.csv',
         stringsAsFactors = FALSE) -> fefer


#### format data
### Pyle, R.L., and P. Pyle. 2017. The Birds of the Hawaiian Islands: Occurrence, History, Distribution, and Status. B.P. Bishop Museum, Honolulu, HI, U.S.A. Version 2 (1 January 2017) http://hbs.bishopmuseum.org/birds/rlp-monograph
pyle_rev <- pyle %>% 
  select(-Total, -Comments) %>% 
  gather(Island, Breeding_pairs, Kaula:Kaohikaipu) %>% 
  mutate(
    comments = case_when(
      .$Breeding_pairs == "M" ~ "seen but doesn't breed",
      .$Breeding_pairs == "B" ~ "historically, since 1778",
      .$Breeding_pairs == "U" ~ "unknown"),
    Island = case_when(
      .$Island == "Kakepa" ~ "Kekepa", # spelled wrong
      .$Island == "Mokoleia" ~ "Oahu", # just a point off of Oahu, not an island
      TRUE ~ as.character(.$Island)),
    Breeding_pairs = as.numeric(gsub(",", "", .$Breeding_pairs)), # remove commas from numbers >999
    Breeding_pairs = ifelse(is.na(Breeding_pairs),"",Breeding_pairs), # remove NAs
    Species = Species.Name,
    Species = case_when(
      .$Species == "Bulwer Petrel" ~ "Bulwer's Petrel",
      .$Species == "Newell Shearwater" ~ "Newell's Shearwater",
      TRUE ~ as.character(.$Species)),
    Database = "Pyle and Pyle 2017") %>% 
  select(-Species.Name)

pyle_coords_rev <- pyle_coords %>% 
  mutate(Island = gsub("'", "", .$Island),
         Island = case_when(
           .$Island == "Kakepa" ~ "Kekepa", # spelled wrong
           .$Island == "Mokoleia" ~ "Oahu", # just a point off of Oahu, not an island
           .$Island == "Kaua'I" ~ "Kauai",
           .$Island == "Lana'I" ~ "Lanai",
           .$Island == "Hawai'I" ~ "Hawaii",
           .$Island == "Moloka'I" ~ "Molokai",
           # .$Island == "Moku Manu" ~ "MokuManu",
           TRUE ~ gsub("'", "", .$Island)))

# verify that Island labels match
unique(pyle_rev$Island)
unique(pyle_coords_rev$Island)
diff <- setdiff(pyle_rev$Island, pyle_coords_rev$Island)

pyle_full <- left_join(pyle_rev, pyle_coords_rev, by="Island") %>% 
  select(-ID, -NWRNAME, -FWS_UNIT) %>% 
  mutate(spp_island = paste(Species, Island, sep = '_'))


### Fefer, Stewart I., D. Hu, M. B. Naughton. 1983. Catolog of Hawaiian Seabird Colonies. U.S. Fish and Wildlife Service, Pacific Islands Office. Honolulu, Hawaii.
fefer_rev <- fefer %>% 
  filter(!Species == "No Information Available") %>%
  mutate(
    comments = case_when(
      .$Breeding_pairs == "+" ~ "confirmed, unknown number",
      .$Breeding_pairs == "?" ~ "suspected breeding"),
    Island = case_when(
      .$Colony == "Kahoolawe" ~ "Kahoolawe",
      .$Colony == "Kekepa Island" ~ "Kekepa",
      .$Colony == "Kaohikaipu Island" ~ "Kaohikaipu",
      .$Colony == "Kapapa Island" ~ "Kapapa",
      .$Colony == "Kuala Island" ~ "Kaula",
      .$Colony == "Lehua Island" ~ "Lehua",
      .$Colony == "Manana Island" ~ "Manana",
      .$Colony == "Mokolii Island" ~ "Mokolii",
      .$Colony == "Mokuauia Island" ~ "Mokuauia",
      .$Colony == "Mokulua Islands" ~ "Mokuluas",
      .$Colony == "Moku Manu" ~ "MokuManu",
      .$Colony == "Niihau" ~ "Niihau",
      .$Colony == "Molokini" ~ "Molokini",
      .$Colony == "Kahoolawe" ~ "Kahoolawe",
      .$Colony == "Popoia Island" ~ "Popoia",
      .$Colony == "Puu Koae" ~ "Mokolii",
      .$Island == "Kauaii" ~ "Kauai",
      TRUE ~ as.character(.$Island)),
    Species = case_when(
      .$Species == "Black-Footed Albatross" ~ "Black-footed Albatross",
      TRUE ~ as.character(.$Species)),
    Breeding_pairs = ifelse(is.na(Breeding_pairs), "", Breeding_pairs),
    Breeding_pairs = as.numeric(Breeding_pairs),
    Survey_date = as.Date(as.character(Survey_date), format = "%m/%d/%Y"))

fefer_islandSum <- fefer_rev %>% 
  group_by(Database, Island, Species) %>% 
  summarise(Breeding_pairs = sum(Breeding_pairs)) %>% 
  mutate(spp_island = paste(Species, Island, sep = '_'))

DB_merged <- full_join(pyle_full, fefer_islandSum, by = "spp_island") %>% 
  mutate(Island = Island.x,
         Breeding_pairs2017 = Breeding_pairs.x,
         Species = Species.x,
         Comments_2017 = comments,
         Database2017 = Database.x,
         Database1983 = Database.y,
         Breeding_pairs1983 = Breeding_pairs.y) %>% 
  select(Island, Species, Breeding_pairs1983, Breeding_pairs2017, ID1, Xcoor, Ycoor, 
         -Island.x, -Island.y, -Species.x, -Species.y, -comments, -Database.x, -Database.y)
      
write.csv(fefer_rev, file = '~/WERC-SC/MHIatlas/Fefer1983_rev.csv',
          row.names = FALSE)
write.csv(pyle_rev, file = '~/WERC-SC/MHIatlas/Pyle&Pyle2017_rev.csv',
          row.names = FALSE)
      

# Fefer (1983) Survey_type codes
# # cr = crude estimate
# # dc = direct count
# # es = estimate, method unspecified
# # li = Lincoln index estimate
# # no = no quantitative census method used
# # q = quadrats
# # sc = sample count
# # st = strip transect
# # unk = unknown method.
  
