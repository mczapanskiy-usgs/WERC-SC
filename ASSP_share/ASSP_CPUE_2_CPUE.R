#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited: August 7, 2019

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
read.csv('~/WERC-SC/ASSP_BANDING_08142019.csv') %>% 
  # remove unnecessary rows
  select(-P10, -P09, -P08, -P07, -P06, -P05, -P04, -P03, -P02, -P01, 
         -R6, -R5, -R4, -R3, -R2, -R1, -X, -X.1, -X.2, -X.3, -X.4, -X.5,
         -tail, -sex, -release.time) -> catches_raw 

# netting site data
read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20190802.csv') %>% 
  select(-Notes) -> sites_tbl

# CPUE metadata from "ASSP_CPUE_1"
read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_sum.csv') -> metadata

# for catch data
catches <- catches_raw %>% 
  select(date, island, year, site, capture.time, species, recapture.) %>% 
  filter(island != "ND") %>% 
  mutate(Site= mosaic::derivedFactor(
    # ANI
    "CC" = (island=="ANI" & site=="Cathedral Cove"),
    "EAI_N" = (island=="ANI" & site=="EAI North side, dock area" | island=="ANI" & site=="Landing Cove Overlook"),
    "EAI_S" = (island=="ANI" & site=="EAI South Ridge" | island=="ANI" & site=="EAI South side--near water catchment"),
    "EAI_SW" = (island=="ANI" & site=="EAI SW end"),
    "EAI_W" = (island=="ANI" & site=="EAI west of lighthouse"),
    "FC" = (island=="ANI" & site =="North side Frenchy's Cove, East End, Upper Bench" | island=="ANI" & site =="Frenchy's Beach" | island=="ANI" & site =="Frenchy's Cove"),
    "GC" = (island=="ANI" & site=="GC"),
    "RR" = (island=="ANI" & site=="Rat Rock"),
    "RC" = (island=="ANI" & site=="Rockfall Cove"), 
    # PI
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site=="" | island=="PI" & site=="UNK"), 
    # SBI
    "AP" = (island=="SBI" & site=="Arch Point" | island=="SBI" & site=="AP"),
    "ESP" = (island=="SBI" & site=="Eseal Point" | island=="SBI" & site=="ESP" | island=="SBI" & site=="" | island=="SBI" & site=="UNK"),
    "NTP" = (island=="SBI" & site=="Nature Trail Plot"), 
    "NCliffs" = (island=="SBI" & site=="North Peak Cliffs" | island=="SBI" & site=="North Cliffs"), 
    "SR" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
    "SP" = (island=="SBI" & site=="SignalPeak" | island=="SBI" & site=="Signal Peak"), 
    "Sutil" = (island=="SBI" & site=="Sutil Island"), 
    "WP" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"),
    "WCliffs" = (island=="SBI" & site=="West Cliffs"), 
    # SCI
    "DR" = (island=="SCI" & site=="Diablo Rock"), 
    # SR
    "SR1" = (island=="SR" & site=="1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR" | island=="SR" & site=="UNK" | island=="SR" & site=="AB10"),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LSH" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HT" = (island=="SR" & site=="SR High Terrace-East"),
    .default = ""),
    # create unique netting night ID
    day = substr(date, 9, 10),
    Month = substr(date, 6, 7),
    dateStr = paste(year, Month, day, sep = ""),
    nightID = paste(dateStr,island, Site, sep = "_"),
    ## combine dat and time, adjust date to actual capture date
    capture.time_old = as.POSIXct(paste(date, capture.time), format="%Y-%m-%d %H:%M"),
    # if "capture.time" times were actually after midnight:
    nextDay_capture.time = capture.time_old + 24*60*60, 
    # pull out the hour of capture event, if before 12 then it was after midnight:
    hour_capture = as.numeric(hour(capture.time_old)),
    capture_time = if_else(hour_capture <= 12, nextDay_capture.time, capture.time_old),
    eventDate = ymd(date, tz = "US/Pacific")) %>% 
  select(-site, -capture.time, -capture.time_old, -nextDay_capture.time, -hour_capture) %>% 
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island")) 

catches_nightID <- catches %>% 
  group_by(nightID) %>% 
  mutate(seq = 1:n(),
         catchID = paste(nightID, seq, sep = "_")) %>% 
  ungroup()

### CALCULATE SUNSET AND STD. ENDING
# create catches dataframe to run sunset function on
catch_nights_unique <- catches_nightID %>% 
  na.omit() %>%  
  group_by(eventDate, Lat, Long, Site, nightID) %>%
  count(nightID) %>% 
  ungroup %>% 
  transmute(date = as_date(eventDate), lat = Lat, lon = Long, Site = Site, nightID = nightID)
# run sunset function
sunTimes <- getSunlightTimes(data = catch_nights_unique,
                               keep = c("sunset"), tz = "PST8PDT") %>%
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon,
         App_sunset = sunset,
         eventDate = as.POSIXct(date)) %>%
  select(-date, -lat, -lon, -sunset)

## bind sunset and std_ending times with catches dataset
catches_sun <- catches_nightID %>% 
  inner_join(sunTimes, by = c("Site", "nightID", "Lat", "Long")) %>% # c("Site", "nightID", "eventDate", "Lat", "Long")) 
  mutate(eventDate = eventDate.x) %>% 
  select(-eventDate.x, -eventDate.y) %>% 
  # 56 entries from SR 2005-06-02 were duplicated for some reason
  mutate(duplicate = duplicated(catchID)) %>% 
  filter(duplicate == !TRUE)

# standardize recapture and species
catches_std <- catches_sun %>% 
  mutate(std = if_else(std_ending > capture_time, "1", "0"), # if bird was caught before std_ending = 1, after = 0
         recapture = mosaic::derivedFactor(
           "Y" = (recapture.=="Y" & recapture.=="y" & recapture.=="YSN" & recapture.=="SNR" ),
           "N" = (recapture.=="N"),
           "UNK" = (recapture.=="UNK" & recapture.=="X"),
           .default = "UNK"),
        spp = mosaic::derivedFactor(
           "ASSP" = (species =="ASSP"),
           "LESP" = (species =="LESP" | species =="LHSP"),
           "LSTP" = (species =="LSTP" | species =="LTSP"),
           "BLSP" = (species == "BLSP"),
           "OTHER" = (species == "CAAU" & species =="Jouanin's Petrel" & species=="OTHER" & species =="WEGU" & species=="XAMU"),
           "UNK" = (species == "UNK" & species=="ASSP/LESP"),
           .default = "UNK"))

test <- catches_std %>% 
  filter(spp == "LESP")

### SUM CATCHES BY SPECIES 
catches_std_ASSP <- catches_std %>% 
  filter(recapture == "N",
         spp == "ASSP",
         std == "1") %>% # std = 1 => caught before std_ending
  group_by(nightID) %>% 
  summarise(ASSP = n()) %>% 
  mutate(ASSP_std = as.character(ASSP)) %>% 
  right_join(catch_nights_unique, by= "nightID") %>% 
  select(-ASSP)


### SUMMARY OF ALL CATCHES FOR SONGMETER METADATA
catches_std_allSP <- catches_std %>% 
  filter(spp %in% c("ASSP", "LESP", "BLSP")) %>% 
  group_by(spp, nightID) %>% 
  summarise(count = n()) %>% 
  spread(spp, count)

write.csv(catches_std_allSP, file = '~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv',
          row.names = FALSE)


missingMETADATA <- anti_join(metadata, catches_std_ASSP, by = "nightID")
missingBANDING <- anti_join(catches_std_ASSP, metadata, by = "nightID")
write.csv(missingBANDING, file = '~/WERC-SC/ASSP_share/missingBANDING.csv',
          row.names = FALSE)
write.csv(missingMETADATA, file = '~/WERC-SC/ASSP_share/missingMETADATA.csv',
          row.names = FALSE)


### CPUE
## number of catches for each species and night
# catches_sum <- catches_sun %>%
#   group_by(nightID) %>%
#   count(species)


# metadata_count <- metadata %>%  
#   #  remove multiple open/close events for one netting night
#   group_by(date, Lat, Long, Site, nightID) %>%
#   count(nightID) %>% 
#   # ungroup %>% 
#   left_join(catches_std_ASSP, by = "nightID")

# x <- catches_std$nightID
# y <- as.character(metadata$nightID)

## only birds caught before cutoff point
## Sum spp counts
# divided by species

# # sum up net time for all net open/close intervals
# metadata_SunMoon_sum <- metadata_SunMoon %>% 
#   mutate(minutes_std_raw = as.character(difftime(std_ending, net_open, units="mins")), 
#          minutes_raw = as.character(difftime(net_close, net_open, units="mins")), 
#          minutes_std = if_else(std_ending <= net_open, "0", 
#                                if_else(std_ending <= net_close, minutes_std_raw, minutes_raw)),
#          minutes_raw = as.numeric(minutes_raw),
#          minutes_std = as.numeric(minutes_std)) %>% 
#   group_by(nightID) %>% 
#   mutate(minutes = sum(minutes_std)) %>% 
#   ungroup %>% 
#   select(-minutes_std_raw)

