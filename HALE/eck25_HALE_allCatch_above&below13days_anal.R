#### This script formats all data  
#### through all the formatting found in 'eck6_HALE_CPUE_timeSeries', 'eck7_HALE_CPUE_baitType', and 'eck10_HALE_CPUE_eventType'

setwd("~/WERC-SC/HALE")

## load packages
# library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
# library(stringr)
library(ggplot2)
# library(mosaic)

#### FUNCTIONS
## function to label event as INVALID if rat was left in from previous check (from 'eck6_HALE_CPUE_timeSeries') 
is.RatInvalid <- function(predCaught, TrapStatus) {
  RatInvalid <- rep(FALSE, length(predCaught))
  flag <- FALSE
  for(i in seq(predCaught)) {
    if(flag && TrapStatus[i] == 'C')
      RatInvalid[i] <- TRUE
    if(predCaught[i] == 'RR')
      flag <- TRUE
    else if(TrapStatus[i] == 'O')
      flag <- FALSE
  }
  RatInvalid
} ##  writen by M. Czapanskiy

#### function to identify different predator event types (from script 'eck10_HALE_CPUE_eventType.R')
is.predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {
  case_when(
    predCaught == "FC" ~ "catCaught",
    predCaught == "HA" ~ "mongooseCaught",
    predCaught == "MM" ~ "mouseCaught",
    predCaught %in% c("RR", "RN", "RB") ~ "ratCaught",
    birdCaught != "" | otherCaught != "" ~ "birdOtherCaught",
    trapStatus == 'O' & baitStatus == "N" ~ "baitLost",
    predCaught == "" & birdCaught == "" & trapStatus == "C" ~ "trapTriggered", ## this includes events when predator escaped
    TRUE ~ "none")
} ##  writen by M. Czapanskiy


#### LOAD DATA
read.csv('~/WERC-SC/HALE/catch_5_duplicateID_withtraploc_20161209.csv',
         stringsAsFactors = FALSE) -> allCatch

#### run data through analysis from 'eck6_HALE_CPUE_timeSeries'
allCatch_eck6 <- allCatch %>%
  # remove entries for traps that are not present/inactive
  filter(TrapStatus != "M") %>% 
  # flag trap checks with closed rat
  mutate(RatInvalid = is.RatInvalid(predCaught, TrapStatus)) %>% 
  filter(!RatInvalid) %>%
  # remove "not rebaited" data (traps where BaitStatus = N and Comments said not rebaited)
  mutate(NotRebaited = grepl('not rebaited', Comments, ignore.case = TRUE) | 
                  grepl("didn't rebait", Comments, ignore.case = TRUE), 
         BaitPresent = !(BaitStatus %in% c('N', 'NR', 'NI') & NotRebaited)) %>% 
  filter(BaitPresent) %>% 
  # count check interval days, flag intervals >14 days
  mutate(date = as.POSIXct(date, format = '%Y-%m-%d')) %>%
  arrange(Trapline, TrapNum, date) %>%
  group_by(Trapline, TrapNum, StartDate) %>%
  mutate(prevCheckInterval = difftime(date, lag(date), units = 'days') %>% 
           as.numeric %>% floor) %>% 
  ungroup %>%
  # flag check intervals over 13 days, but do not remove
  mutate(TrapChecked = !is.na(prevCheckInterval) & prevCheckInterval < 14)

#### run data through analysis from 'eck7_HALE_CPUE_baitType'
allCatch_eck7 <- allCatch_eck6 %>%
  mutate(baitType = mosaic::derivedFactor(
    "Lure" = grepl("lure", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog" = grepl("^CD$", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog+DryDog+Oil" = grepl("CDDO$", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog+Other" = grepl("CD", BaitPrev, ignore.case = TRUE),
    "CannedCat"= grepl("^C$", BaitPrev, ignore.case = TRUE),
    "CannedDog" = grepl("^D$", BaitPrev, ignore.case = TRUE),
    "DryDog+Oil(+Other)" = grepl("DO+", BaitPrev, ignore.case = TRUE),
    "DryDog+Oil" = grepl("^DO$", BaitPrev, ignore.case = TRUE),
    "ProfessionalBait"= grepl("cave$|coll$", BaitPrev, ignore.case = TRUE),
    "CannedCat+Other"= grepl("^C+", BaitPrev, ignore.case = TRUE), 
    "CannedDog+Other" = grepl("^D+", BaitPrev, ignore.case = TRUE),
    "Not recorded" = grepl("NR" , BaitPrev, ignore.case = TRUE),
    "None" = grepl("none", BaitPrev, ignore.case = TRUE),
  .method = "first",
  .default = "Other"))

#### run data through analysis from ''eck10_HALE_CPUE_eventType
## create predEvent column based on is.predEvent function (and remove mouse caught and NA events)
allCatch_eck10 <- mutate(allCatch_eck7, predEvent = is.predEvent(predCaught, birdCaught, otherCaught, TrapStatus, BaitStatus)) %>% 
  filter(predEvent != 'mouseCaught',
         predEvent != 'NA') %>% 
  mutate(eventType = mosaic::derivedFactor(
    "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
    "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
    "noEvent" = predEvent =="none",
    .default = "noEvent"),
    loc = mosaic::derivedFactor(
      front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
      # back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
      .default = "back"))

write.csv(allCatch_eck10, file = '~/WERC-SC/HALE/allCatch_25_20171120.csv',
          row.names = FALSE)

