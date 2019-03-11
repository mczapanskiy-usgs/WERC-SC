#### STORM-PETREL CPUE METADATA
# this script calculates processes the metadata that Amelia created
# created: March 6, 2019 by: E Kelsey
# last edited: 

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
# library(mosaic)
# library(oce)
# library(suncalc)
# library(foreach)
# library(doParallel)
# library(stats)
# library(circular)
# library(StreamMetabolism)
# library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_metadata_1994-2018_03012019.csv') -> catches_metadata
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites_tbl

catches_metadata_rev <- catches_metadata %>% 
  mutate(Site= mosaic::derivedFactor(
    "SWcorner" = (island=="SR" & site=="1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| 
                    island=="SR" & site=="UNK" | island=="SR" & site=="Lower terrace of SE Side of SRK"),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LittleScorpionHeadland" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HighTerrace" = (island=="SR" & site=="SR High Terrace-East"),
    "AP" = (island=="SBI" & site=="Arch Point" |island=="SBI" & site=="AP"),
    "ESP" = (island=="SBI" & site=="Eseal Point" |island=="SBI" & site=="ESP"),
    "ShagOverlook" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
    "NatureTrailPlot" = (island=="SBI" & site=="Nature Trail Plot"), 
    "WebstersPoint" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"), 
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site==""), 
    "GC" = (island=="ANI" & site=="GC"),
    "WestCliffs" = (island=="SBI" & site=="West Cliffs"),
    .default = ""),
    # create unique netting night ID
    day = substr(date, 9, 10),
    Month = substr(date, 6, 7),
    dateStr = paste(year, Month, day, sep = ""),
    nightID = paste(dateStr,island, Site, sep = "_")) %>% 
  select(-day, -Month, -dateStr, -site) %>% 
  # add latitude and longitude to metadata
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island")) %>% 
  select(nightID, date, year, month, island, Site, Lat, Long, net_open, net_close, 
         numRecaps, numBounceOuts, numASSP, numBLSP, numLESP, numLTSP, banders, weather, notes)

write.csv(catches_metadata_rev, file = '~/WERC-SC/ASSP_share/ASSP_MistnetMetadata_1994-2018.csv',
          row.names = FALSE)


