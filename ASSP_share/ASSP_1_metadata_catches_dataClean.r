#### STORM-PETREL CPUE METADATA
# this script calculates sunset, moon rise and set, moon time
# input data: raw database
# created: Jan 23, 2020
# last edited: Feb 21, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(mosaic)
library(oce)
library(foreach)
library(doParallel)
# library(stats)
library(suncalc)
# library(chron)
# library(rnoaa)

### READ IN DATA
## CPUE DATA (AKA METADATA)
metadata_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE_01232020.csv', na.strings=c("","NA")) 
## BANDING DATA (AKA CATCHES DATA)
catches_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_01232020.csv', na.strings=c("","NA")) %>% 
  select(-X:-X.5)


metadata <- metadata_raw %>% 
  mutate(net_open_old = as.POSIXct(paste(date, net.open), format="%m/%d/%Y %H:%M"),
         net_close_old = as.POSIXct(paste(date, net.close), format="%m/%d/%Y %H:%M"),
         # but some "net_open" and "net_close" times were actually after midnight:
         nextDay_open = net_open_old + 24*60*60, nextDay_close = net_close_old + 24*60*60, 
         # pull out the hour of the open/close event, if before 12 then it was after midnight:
         hour_open = as.numeric(hour(net_open_old)), hour_close = as.numeric(hour(net_close_old)),
         net_open = if_else(hour_open <= 12, nextDay_open, net_open_old),
         net_close = if_else(hour_close <= 12, nextDay_close, net_close_old),
         date = mdy(date),
         # standardize site names
         Site= mosaic::derivedFactor(
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
           "PI1" = (island=="PI" & site=="PI1"), # & site=="1" | island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
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
           "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
           "SR2" = (island=="SR" & site=="2"),
           "SR3" = (island=="SR" & site=="3"),
           "LSH" = (island=="SCI" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
           "HT" = (island=="SR" & site=="SR High Terrace-East"),
           .default = "UNK"),
         # create unique netting night ID
         day = substr(date, 9, 10), month = substr(date, 6, 7),
         dateStr = paste(year, month, day, sep = ""),
         sessionID = paste(dateStr,island, Site, sep = "_"),
         # generate distintion between "island" and "location"
         islandID= mosaic::derivedFactor(
           "ANI" = (island =="ANI"),
           "SMI" = (island == "PI"),
           "SBI" = (island == "SBI"),
           "SCI" = (island == "SCI" | island == "SR"))) %>% 
  select(sessionID, day, month, year, island, Site, App_sunset:mo_period, Net_mesh:notes) %>% 
  filter(TRUE)


catches <- catches_raw %>% 
  filter(island != "ND") %>% 
  mutate(date = mdy(date),
         # standardize site names
         Site= mosaic::derivedFactor(
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
           "PI1" = (island=="PI" & site=="PI1"), # & site=="1" | island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
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
           "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
           "SR2" = (island=="SR" & site=="2"),
           "SR3" = (island=="SR" & site=="3"),
           "LSH" = (island=="SCI" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
           "HT" = (island=="SR" & site=="SR High Terrace-East"),
           .default = "UNK"),
         # create unique netting night ID
         day = substr(date, 9, 10),
         Month = substr(date, 6, 7),
         dateStr = paste(year, Month, day, sep = ""),
         sessionID = paste(dateStr,island, Site, sep = "_"),
         # combine date and time, adjust date to actual capture date
         capture.time_old = as.POSIXct(paste(date, capture.time), format="%Y-%m-%d %H:%M"),
         # if "capture.time" times were actually after midnight:
         nextDay_capture.time = capture.time_old + 24*60*60, 
         # pull out the hour of capture event, if before 12 it would be an early morning capture:
         hour_capture = as.numeric(hour(capture.time_old)),
         capture_time = if_else(hour_capture <= 12, nextDay_capture.time, capture.time_old),
         # do the same for release time
         release.time_old = as.POSIXct(paste(date, release.time), format="%Y-%m-%d %H:%M"),
         nextDay_release.time = release.time_old + 24*60*60, 
         hour_release = as.numeric(hour(release.time_old)),
         release_time = if_else(hour_release <= 12, nextDay_release.time, release.time_old)) %>% 
  group_by(sessionID) %>% 
  mutate(seq = 1:n(),
         catchID = paste(sessionID, seq, sep = "_")) %>% 
  ungroup() %>% 
  mutate(sessionID = as.factor(sessionID),
         Site = as.factor(Site)) %>% 
  select(catchID, sessionID, day, Month, year, island, Site, measurer.s., capture_time, release_time, sunset:Notes) %>% #-capture.time, -capture.time_old, -nextDay_capture.time, -hour_capture, -release.time, -release.time_old, -nextDay_release.time, -hour_release) %>% 
  filter(TRUE)

### save data to manually review
write.csv(metadata, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_METADATA2review.csv', # '~/WERC-SC/ASSP_share/ASSP_CPUE_01242020_JA.csv',
          row.names = FALSE)

write.csv(catches, file = '~/WERC-SC/ASSP_share/ASSP_BANDING_CATCHES2review.csv', # '~/WERC-SC/ASSP_share/ASSP_BANDING_01242020_JA.csv',
            row.names = FALSE)
  
