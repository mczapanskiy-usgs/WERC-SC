#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited: March 26, 2020

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
  select(-sunset, -hrs_postSS, -X, -X.1, -X.2, -X.3) %>% 
  filter(TRUE)

# netting site data
sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20200122.csv') %>% 
                select(-Notes, -Site_Name)

# CPUE metadata from "ASSP_CPUE_1"
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_3_CPUE_metadata_Sun_noMoon_minEdit.csv') %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  filter(TRUE)

## update catches/banding datasheet: add catchID, locations, catch date (is diff from 'date' after midnight)
catches <- catches_raw %>% 
  mutate(measurers = measurer.s.,
         recapture = recapture.,
         diet = diet.,
         uncorr_mass = as.numeric(as.character(mass)), # 
         mass_tare = as.numeric(as.character(tare)), 
         mass_corr = uncorr_mass - mass_tare) %>%
  left_join(sites_tbl, by = c("Site", "Location", "Island")) %>% 
  # reorder
  select(catchID:Site, Lat, Long, capture_time:bandno, recapture, diet, BP, uncorr_mass, mass_tare, mass_corr, culmen:R1, measurers, Notes)

# # compare "Site" factors in both tables -> inconsistencies?
# summary(as.factor(catches_raw$Site))
# summary(as.factor(catches$Site))
# summary(sites_tbl$Site)

## bind sunset, net open, net closed, std_ending times with catches dataset
metadata_effort <- metadata %>% 
  select(Site, date, sessionID, net_close_1:net_open_5, App_sunset, std_ending, Lat, Long)

catches_std <- catches %>%
  left_join(metadata_effort, by = c("Site", "sessionID", "Lat", "Long")) %>% 
  mutate(App_sunset = as.POSIXct(App_sunset, format = "%m/%d/%Y %H:%M"),
         std_ending = as.POSIXct(std_ending, format = "%m/%d/%Y %H:%M"),
         std = if_else(std_ending > capture_time, "1", "0"), # if bird was caught before std_ending = 1, after = 0
         # pre_close = if_else(net_close > capture_time, "1", "0"), # if bird was caught before net_close = 1, after = 0
         # post_open = if_else(net_open_1 < capture_time, "1", "0"),  # if bird was caught after net_open = 1, before = 0
         SNR = mosaic::derivedFactor(
           "Y" = (recapture =="SNR" | recapture =="YSN"), # remove same night recaptures
           "N" = (recapture =="N" | recapture =="Y"| recapture =="y"), # recaps from other nights still count 
           .default = "UNK"),
         catchPastSS = capture_time - App_sunset) %>%
  select(catchID, sessionID, App_sunset, std_ending, Month, day, year:release_time, catchPastSS, species:recapture, std, diet:Notes) %>%
  filter(TRUE)

catches_test <- catches_std %>% 
  filter(catchPastSS > 600)

# # compare Sites listed in catches vs. metadata datasets
# summary(as.factor(catches_metadata$Site))
# summary(as.factor(catches$Site))
# 
# summary(catches_metadata$recapture.)
# summary(catches_std$SNR)

### SAVE CATCHES FILE BEFORE IT IS FILTERED
write.csv(catches_std, file = '~/WERC-SC/ASSP_share/ASSP_4_catches_BANDING_20200414.csv',
          row.names = FALSE)

write.csv(catches_test, file = '~/WERC-SC/ASSP_share/ASSP_BANDING_catchTime2change.csv',
          row.names = FALSE)

# missingMETADATA <- anti_join(metadata, catches_std, by = "sessionID")
# missingBANDING <- anti_join(catches_std, metadata, by = "sessionID")

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
  filter(species == "ASSP") %>%
  mutate(assumeBreed = mosaic::derivedFactor(
    "Y" = (BP == "B" | BP == "b" | BP == "2" | BP == "3" | BP == "4"),
    "N" = (BP == "D" | BP == "d" | BP == "0" | BP == "5" | BP == "PD" | BP == "pd" | BP == "1" | BP == "1.5" | BP == "4.5"),
    .default = "ND")) %>% 
  group_by(sessionID, Site) %>%
  summarise(ASSP = n(),
            ASSPstd = sum(std == "1"),
            BPct = n(),
            BPct_std = sum(std == "1"),
            BP_Y = sum(assumeBreed == "Y"),
            BP_Ystd = sum(assumeBreed == "N" | std == "1")) %>%
  right_join(metadata, by= c("sessionID", "Site")) %>%
  # filter(min_std > 0) %>%
  mutate(CPUEraw =  ASSP/min,
         CPUEstd = ASSPstd/min_std,
         BPfreq_Y = BP_Y/BPct,
         BPfreq_Ystd = BP_Ystd/BPct_std) %>%
  select(sessionID, Island, Location, Site, Site_Name, Lat, Long, month, day, year, seriesID, App_sunset, std_ending,
         net_open_1, net_close_1, net_open_2, net_close_2, net_open_3, net_close_3, net_open_4, net_close_4, net_open_5, net_close_5,
         min, min_std, ASSP, ASSPstd, CPUEraw, CPUEstd, BPfreq_Y, BPfreq_N, Net_mesh:Flagged_notes)

summary(metadata_catches$ASSP)
summary(metadata_catches$ASSPstd)

#### SAVE CPUE DATA FOR ALL NETTING EFFORTS THAT CPUE CAN BE CALCULATED FOR
write.csv(metadata_catches, file = '~/WERC-SC/ASSP_share/ASSP_4_metadata_CPUE_20200413.csv',
          row.names = FALSE)
# 
# ## test to see if any duplicates were created
# duplicates <- duplicated(catches_metadata) %>%
#   data.table()
# summary(duplicates)
# catches_duplicates <- catches_metadata %>%
#   bind_cols(duplicates) %>%
#   filter(. == "TRUE")