#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited: Feb 27, 2019

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
# library(mosaic)
# library(oce)
library(suncalc)
# library(foreach)
# library(doParallel)
# library(stats)
# library(circular)
# library(StreamMetabolism)
# library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_catches_1994-2018_03012019.csv') %>% 
  # remove unnecessary rows
  select(-P10, -P09, -P08, -P07, -P06, -P05, -P04, -P03, -P02, -P01, 
         -R6, -R5, -R4, -R3, -R2, -R1, -X, -X.1, -X.2, -X.3, -X.4, -X.5,
         -tail, -sex, -release.time)  -> catches_raw 

read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20190506.csv') %>% 
  select(-Notes) -> sites_tbl

read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_sum.csv') -> metadata_sum

# for catch data
catches <- catches_raw %>% 
  select(date, island, site, capture.time, species, recapture.) %>% 
  mutate(Site= mosaic::derivedFactor(
    "SWcorner" = (island=="SR" & site=="1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR" | island=="SR" & site=="UNK" | island=="SR" & site==""),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LittleScorpionHeadland" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HighTerrace" = (island=="SR" & site=="SR High Terrace-East"),
    "AP" = (island=="SBI" & site=="Arch Point" |island=="SBI" & site=="AP"),
    "ESP" = (island=="SBI" & site=="Eseal Point" |island=="SBI" & site=="ESP"),
    "ShagOverlook" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
    "NatureTrailPlot" = (island=="SBI" & site=="Nature Trail Plot"), 
    "WebstersPoint" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw" | island=="SBI" & site=="WebsterPointDraw"| island=="SBI" & site=="WPDR"), 
    "NCliffs" = (island == "SBI" & site == "North Cliffs" | island == "SBI" & site == "North Peak Cliffs"),
    "WCliffs" = (island == "SBI" & site == "West Cliffs"),
    "SutilI" = (island == "SBI" & site == "Sutil Island"),
    "SignalPeak" = (island == "SBI" & site == "Signal Peak"),
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site==""), 
    "GarbageC" = (island=="ANI" & site=="GC"),
    "Frenchys" = (island=="ANI" & site=="Frenchy's Cove"),
    "NFrenchys" = (island=="ANI" & site=="North side Frenchy's Cove, East End, Upper Bench"),
    "CathedralC" = (island=="ANI" & site=="Cathedral Cove"),
    "RatR" = (island=="ANI" & site=="Rat Rock"),
    .default = ""),
    # create unique netting night ID
    nightID = paste(date,island, Site, sep = "_"),
    ## combine dat and time, adjust date to actual capture date
    capture.time_old = as.POSIXct(paste(date, capture.time), format="%m/%d/%Y %H:%M"),
    # if "capture.time" times were actually after midnight:
    nextDay_capture.time = capture.time_old + 24*60*60, 
    # pull out the hour of capture event, if before 12 then it was after midnight:
    hour_capture = as.numeric(hour(capture.time_old)),
    capture_time = if_else(hour_capture <= 12, nextDay_capture.time, capture.time_old),
    eventDate = mdy(date, tz = "US/Pacific")) %>% 
  select(-site, -capture.time, -capture.time_old, -nextDay_capture.time, -hour_capture) %>% 
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island"))


## calculate sunset
# create catches dataframe to run sunset function on
catches_clip <- catches %>% 
  na.omit() %>%  
  group_by(eventDate, Lat, Long, Site, nightID) %>%
  count(nightID) %>% 
  ungroup %>% 
  transmute(date = as_date(eventDate), lat = Lat, lon = Long, Site = Site, nightID = nightID)
  
# run sunset function
sunTimes <- getSunlightTimes(data = catches_clip,
                               keep = c("sunset"), tz = "PST8PDT") %>%
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon,
         App_sunset = sunset,
         eventDate = as.POSIXct(date)) %>%
  select(-date, -lat, -lon, -sunset)

catches_sun <- catches %>% 
 left_join(sunTimes, by = c("Site", "nightID", "eventDate", "Lat", "Long")) 

# sum up net time for all net open/close intervals
metadata_SunMoon_sum <- metadata_SunMoon %>% 
  mutate(minutes_std_raw = as.character(difftime(std_ending, net_open, units="mins")), 
         minutes_raw = as.character(difftime(net_close, net_open, units="mins")), 
         minutes_std = if_else(std_ending <= net_open, "0", 
                               if_else(std_ending <= net_close, minutes_std_raw, minutes_raw)),
         minutes_raw = as.numeric(minutes_raw),
         minutes_std = as.numeric(minutes_std)) %>% 
  group_by(nightID) %>% 
  mutate(minutes = sum(minutes_std)) %>% 
  ungroup %>% 
  select(-minutes_std_raw)

### CPUE
## number of catches for each species and night
catches_sum <- catches_sun %>%
  group_by(nightID) %>%
  count(species)


## only birds caught before cutoff point
## Sum spp counts
# divided by species

