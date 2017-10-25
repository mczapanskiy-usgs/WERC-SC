## This script is to analyze the frequency and distribution of cat catches
## in response to Seth's questions

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

## RatInvalid function from "eck6" (writen by M. Czapanskiy)
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
}

## load data (change directory to the folder holding 'TrapsGrid20170905.csv')
read.csv('~/WERC-SC/HALE/TrapsGrid20170905.csv',
         stringsAsFactors = FALSE) -> spatialCatch # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)

read.csv('~/WERC-SC/HALE/catch_5_duplicateID_withtraploc_20161209.csv',
         stringsAsFactors = FALSE) -> allCatch

table(allCatch$TrapStatus)
missingTraps <- allCatch %>%
  filter(TrapStatus == "M") 

allCatch <- allCatch %>%
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
           as.numeric %>% floor) %>% # mutate(prevCheckInterval = difftime(lead(date), date, units = 'days') %>% as.numeric %>% floor) %>% 
  ungroup %>%
  mutate(TrapChecked = !is.na(prevCheckInterval) & prevCheckInterval < 14)

catch_over13days <- allCatch %>% 
  filter(TrapChecked == FALSE)

cats <- allCatch %>% 
  filter(predCaught == 'FC') %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(cats, file = '~/WERC-SC/HALE/outputs/cats20171025.csv',
          row.names = FALSE)

rats <- allCatch %>% 
  filter(predCaught %in% c('RR', 'RN', 'RE')) %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(rats, file = '~/WERC-SC/HALE/outputs/rats20171025.csv',
          row.names = FALSE)

mongoose <- allCatch %>% 
  filter(predCaught == 'HA') %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(mongoose, file = '~/WERC-SC/HALE/outputs/mongoose20171025.csv',
          row.names = FALSE)

write.csv(allCatch, file = '~/WERC-SC/HALE/outputs/allCatch20171025.csv',
          row.names = FALSE)

