#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited:

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)
library(oce)
library(suncalc)
library(foreach)
library(doParallel)
library(stats)
library(circular)
library(StreamMetabolism)
library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_catches.csv') -> catches_raw

# for catch data
catches <- catches_raw %>% 
  select(date, island, site, capture.time, species, recapture., Notes) %>% 
  mutate(Site= mosaic::derivedFactor(
    "SWcorner" = (island=="SR" & site=="1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LittleScorpionHeadland" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HighTerrace" = (island=="SR" & site=="SR High Terrace-East"),
    "AP" = (island=="SBI" & site=="Arch Point" |island=="SBI" & site=="AP"),
    "ESP" = (island=="SBI" & site=="Eseal Point" |island=="SBI" & site=="ESP"),
    "ShagOverlook" = (island=="SBI" & site=="Shag Overlook"),
    "NatureTrailPlot" = (island=="SBI" & site=="Nature Trail Plot"), 
    "WebstersPoint" = (island=="SBI" & site=="Webster's Point"), 
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site==""), 
    "GC" = (island=="ANI" & site=="GC"),
    .default = ""),
    Date = mdy(date, tz = "US/Pacific")) %>% 
  select(-date, -site)


### CPUE
## number of catches for each species and night
catches_t <- catches %>% 
  mutate(capture.time2 = hm(capture.time),
         capture_DateTime = as.POSIXct(paste(Date, capture.time2), format="%Y-%m-%d %H:%M:%S"))

# catches_sum <- catches %>% 
#   group_by(Date, Site) %>% 
#   count(species)


## only birds caught before cutoff point
## Sum spp counts
# divided by species

