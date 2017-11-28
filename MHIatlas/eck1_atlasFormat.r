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
         stringsAsFactors = FALSE) -> newDat
read.csv('~/WERC-SC/MHIatlas/Fefer1983.csv',
         stringsAsFactors = FALSE) -> oldDat


### format data
newDat2 <- newDat %>% 
  select(-Total, -Comments) %>% 
  gather(Island, Breeding_pairs, Kaula:Kaohikaipu) %>% 
  mutate(
    comments = case_when(
      .$Breeding_pairs == "M" ~ "seen but doesn't breed",
      .$Breeding_pairs == "B" ~ "historically, since 1778",
      .$Breeding_pairs == "U" ~ "unknown"),
    Breeding_pairs = as.numeric(gsub(",", "", .$Breeding_pairs)),
    Island = case_when(
      .$Island == "Kakepa" ~ "Kekepa",
      .$Island == "Mokoleia" ~ "Oahu",
      TRUE ~ as.character(.$Island)))

oldDat2 <- oldDat %>% 
  mutate(
    comments = case_when(
      .$Breeding_pairs == "+" ~ "confirmed, unknown number",
      .$Breeding_pairs == "?" ~ "suspected breeding"),
    Breeding_pairs = as.numeric(Breeding_pairs),
    Survey_date = as.Date(as.character(Survey_date), format = "%m/%d/%Y"),
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
      TRUE ~ as.character(.$Island)))
  
      
write.csv(oldDat2, file = '~/WERC-SC/MHIatlas/oldDat2.csv',
          row.names = FALSE)
write.csv(newDat2, file = '~/WERC-SC/MHIatlas/newDat2.csv',
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
  
