#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited: Jan 22, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(suncalc)


### READ IN DATA
# banding catches data 
catches_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_20200128.csv') %>%
  mutate(capture_time = as.POSIXct(capture_time, format="%m/%d/%Y %H:%M"),
         release_time = as.POSIXct(release_time, format="%m/%d/%Y %H:%M")) %>% 
  # remove unnecessary rows
  select(-sunset, -hrs_postSS, -mass.cor , -X, -X.1, -X.2, -X.3) %>% 
  filter(TRUE)

# netting site data
sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20200122.csv') %>% 
                select(-Notes, -Site_Name)

# CPUE metadata from "ASSP_CPUE_1"
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_1_CPUE_metadata_Sun_noMoon.csv') %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(.)) %>% 
  mutate(date = as.POSIXct(date, format="%m/%d/%Y")) %>% 
  filter(TRUE)
  # mutate(min = as.integer(min), min_std = as.integer(min_std)) %>% 
  

## update catches/banding datasheet: add catchID, locations, catch date (is diff from 'date' after midnight)
catches <- catches_raw %>% 
  mutate(measurers = measurer.s.,
         recapture = recapture.,
         diet = diet.,
         uncorr_mass = mass,
         mass_tare = tare) %>%
  left_join(sites_tbl, by = c("Site", "Location", "Island")) %>% 
  # reorder
  select(catchID:Site, Lat, Long, capture_time:bandno, recapture, diet, BP, uncorr_mass, mass_tare, culmen:Notes)

# compare "Site" factors in both tables -> inconsistencies?
summary(as.factor(catches_raw$Site))
summary(as.factor(catches$Site))
summary(sites_tbl$Site)

## bind sunset, net open, net closed, std_ending times with catches dataset
metadata_effort <- metadata %>% 
  select(Site, date, sessionID, net_close_1:net_open_5, App_sunset, std_ending, Lat, Long)

catches_metadata <- catches %>%
  left_join(metadata_effort, by = c("Site", "sessionID", "Lat", "Long")) %>% 
  mutate(App_sunset = as.POSIXct(App_sunset, format = "%Y-%m-%d %H:%M:%S"),
         std_ending = as.POSIXct(std_ending, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(TRUE)

# standardize recapture and species
catches_std <- catches_metadata %>% 
  mutate(std = if_else(std_ending > capture_time, "1", "0"), # if bird was caught before std_ending = 1, after = 0
         # pre_close = if_else(net_close > capture_time, "1", "0"), # if bird was caught before net_close = 1, after = 0
         post_open = if_else(net_open_1 < capture_time, "1", "0"),  # if bird was caught after net_open = 1, before = 0
         SNR = mosaic::derivedFactor(
           "Y" = (recapture =="SNR" | recapture =="YSN"), # remove same night recaptures
           "N" = (recapture =="N" | recapture =="Y"| recapture =="y"), # recaps from other nights still count 
           .default = "UNK")) %>%
  filter(TRUE)

# # compare Sites listed in catches vs. metadata datasets
# summary(as.factor(catches_metadata$Site))
# summary(as.factor(catches$Site))
# 
# summary(catches_metadata$recapture.)
# summary(catches_std$SNR)

catches_save <- catches_std %>% 
  select(catchID, sessionID, Month, day, year:Notes)

### SAVE CATCHES FILE BEFORE IT IS FILTERED
write.csv(catches_save, file = '~/WERC-SC/ASSP_share/ASSP_4_catches_BANDING_20200306.csv',
          row.names = FALSE)

missingMETADATA <- anti_join(metadata, catches_std, by = "sessionID")
missingBANDING <- anti_join(catches_std, metadata, by = "sessionID")

## filter catches, sum by species, than sum by catch effort with metadata
# view what will be filtered
summary(as.factor(catches_std$std)) # catches before vs. after standard ending
summary(as.factor(catches_std$pre_close)) # catches before net opened, also ones duplicated for ea. open/close during one night when merged w/ metadata
summary(as.factor(catches_std$post_open)) # catches after net closed, also ones duplicated for ea. open/close during one night when merged w/ metadata
summary(catches_std$recapture == "N") # recaptures
summary(catches_std$species == "ASSP")

# # filter out other spp, and catches that don't count as part of effort
# catches_filtered <- catches_std %>% 
#   filter(post_open == "1",
#          recapture == "N",
#          pre_close == "1",
#          spp == "ASSP") 

# sum catches for each species and night
metadata_catches <- catches_std %>%
  filter(post_open == "1",
         species == "ASSP") %>% 
  group_by(sessionID, Site) %>% # , "net_open"
  # count(species) %>% 
  summarise(ASSP = n(),
            ASSPstd = sum(std == "1")) %>% 
  # mutate(ASSPraw = as.character(ASSP)) %>% 
  right_join(metadata, by= c("sessionID", "Site")) %>% 
  # filter(min_std > 0) %>%
  mutate(CPUEraw = ASSP/min,
         CPUEstd = ASSPstd/min_std) %>% 
  select(sessionID, Island, Location, Site, Site_Name, Lat, Long, month, day, year, seriesID, App_sunset, std_ending, 
         net_open_1, net_close_1, net_open_2, net_close_2, net_open_3, net_close_3, net_open_4, net_close_4, net_open_5, net_close_5,
         min, min_std, ASSP, ASSPstd, CPUEraw, CPUEstd, Net_mesh:Flagged_notes)

summary(metadata_catches$ASSP)
summary(metadata_catches$ASSPstd)

#### SAVE CPUE DATA FOR ALL NETTING EFFORTS THAT CPUE CAN BE CALCULATED FOR
write.csv(metadata_catches, file = '~/WERC-SC/ASSP_share/ASSP_4_metadata_CPUE_20200306.csv',
          row.names = FALSE)

#### SAVE CPUE DATA 2017-2018 TO SEND TO T TINKER
metadata_catches_2017_2018 <- metadata_catches %>% 
  mutate(year = year(Date)) %>% 
  filter(year > 2016) %>% 
  select(-year)
write.csv(metadata_catches_2017_2018, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE_2017-2018.csv',
          row.names = FALSE)


### SUMMARY OF ALL CATCHES FOR SONGMETER METADATA
catches_std_allSP <- catches_std %>% 
  filter(spp %in% c("ASSP", "LESP", "BLSP")) %>% 
  group_by(spp, nightID) %>% 
  summarise(count = n()) %>% 
  spread(spp, count)

write.csv(catches_std_allSP, file = '~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv',
          row.names = FALSE)

# Site= mosaic::derivedFactor(
#   # ANI
#   "CC" = (island=="ANI" & site=="Cathedral Cove"),
#   "EAI_N" = (island=="ANI" & site=="EAI North side, dock area" | island=="ANI" & site=="Landing Cove Overlook"),
#   # "EAI_S" = (island=="ANI" & site=="EAI South Ridge" | island=="ANI" & site=="EAI South side--near water catchment"),
#   "EAI_SW" = (island=="ANI" & site=="EAI SW end"),
#   "EAI_W" = (island=="ANI" & site=="EAI west of lighthouse"),
#   "FC" = (island=="ANI" & site =="North side Frenchy's Cove, East End, Upper Bench" | island=="ANI" & site =="Frenchy's Beach" | island=="ANI" & site =="Frenchy's Cove"),
#   "GC" = (island=="ANI" & site=="GC"),
#   "RR" = (island=="ANI" & site=="Rat Rock"),
#   "RC" = (island=="ANI" & site=="Rockfall Cove"), 
#   # PI
#   "PI1" = (island=="PI" & site=="1"| island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
#   # SBI
#   "AP" = (island=="SBI" & site=="Arch Point" | island=="SBI" & site=="AP"),
#   "ESP" = (island=="SBI" & site=="Eseal Point" | island=="SBI" & site=="ESP" | island=="SBI" & site=="" | island=="SBI" & site=="UNK"),
#   "NTP" = (island=="SBI" & site=="Nature Trail Plot"), 
#   "NCliffs" = (island=="SBI" & site=="North Peak Cliffs" | island=="SBI" & site=="North Cliffs"), 
#   "SR" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
#   "SP" = (island=="SBI" & site=="SignalPeak" | island=="SBI" & site=="Signal Peak"), 
#   "Sutil" = (island=="SBI" & site=="Sutil Island"), 
#   "WP" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"),
#   "WCliffs" = (island=="SBI" & site=="West Cliffs"), 
#   # SCI
#   "DR" = (island=="SCI" & site=="Diablo Rock"), 
#   "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
#   "SR2" = (island=="SR" & site=="2"),
#   "SR3" = (island=="SR" & site=="3"),
#   "LSH" = (island=="SCI" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
#   "HT" = (island=="SR" & site=="SR High Terrace-East"),
#   .method = "FIRST",
  # .default = "UNK"),

# catches_ID <- catches %>% 
#   group_by(sessionID) %>% 
#   mutate(seq = 1:n(),
#          catchID = paste(sessionID, seq, sep = "_")) %>% 
#   ungroup() %>% 
#   mutate(sessionID = as.factor(sessionID),
#          Site = as.factor(Site)) %>% 
#   filter(TRUE)


# ## test to see if any duplicates were created
# duplicates <- duplicated(catches_metadata) %>%
#   data.table()
# summary(duplicates)
# catches_duplicates <- catches_metadata %>%
#   bind_cols(duplicates) %>%
#   filter(. == "TRUE")