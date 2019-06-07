#### STORM-PETREL CPUE METADATA
# this script calculates processes the metadata that Amelia created
# created: March 6, 2019 by: E Kelsey
# last edited: April 5, 2019 by E Kelsey

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_MistnetMetadata_1994-2018.csv') %>% 
  mutate(Date = as.Date(date, format="%Y-%m-%d")) -> metadata
read.csv('~/WERC-SC/ASSP_share/MistnetMetadata_sumAllSpp.csv') -> catches_std_all
read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locations_032219.csv') %>% 
  select(-Notes) -> sites_tbl
read.csv('~/WERC-SC/ASSP_share/CMI_Deployment_Info_CINP_2011-2018.csv') %>%
  mutate(Deployment_Date = as.Date(Deployment_Date, format="%m/%d/%Y"),
         Retrieval_Date = as.Date(Retrieval_Date, format="%m/%d/%Y"),
         Island = as.character(Island)) %>% 
  select(Sensor_Name, Deployment_Date, Deployment_Year, Retrieval_Date, Island, Location, Latitude, Longitude) -> SM_sites

# standardize sites, create unique ID, add site locations
metadata_2 <- metadata %>% 
  mutate(Site= mosaic::derivedFactor(
    "SR1" = (island=="SR" & site=="1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| 
                    island=="SR" & site=="UNK" | island=="SR" & site=="Lower terrace of SE Side of SRK"),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LScorpionHeadland" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HighTerrace" = (island=="SR" & site=="SR High Terrace-East"),
    "AP" = (island=="SBI" & site=="Arch Point" |island=="SBI" & site=="AP"),
    "ESP" = (island=="SBI" & site=="Eseal Point" |island=="SBI" & site=="ESP"),
    "ShagOverlook" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
    "NatureTr_Plot" = (island=="SBI" & site=="Nature Trail Plot"), 
    "WebstersP" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"), 
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site==""), 
    "GC" = (island=="ANI" & site=="GC"),
    "WCliffs" = (island=="SBI" & site=="West Cliffs"),
    .default = ""),
    # create unique netting night ID
    day = substr(date, 9, 10),
    Month = substr(date, 6, 7),
    dateStr = paste(year, Month, day, sep = ""),
    nightID = paste(dateStr,island, Site, sep = "_")) %>% 
  # add latitude and longitude to metadata
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island")) 

metadata_3 <- metadata_2 %>% 
  left_join(catches_std_all, by = "nightID")

metadata_SM <- metadata_rev %>% 
  select(nightID, Date, year, island, Site, Lat, Long, net_open, net_close_final, notes) %>% #, -day, -Month, -dateStr, -site 
  full_join(SM_sites, by = c("island" = "Island", "year" = "Deployment_Year"))

write.csv(metadata_SM, file = '~/WERC-SC/ASSP_share/ASSP_MistnetMetadata_SM_1994-2018.csv',
          row.names = FALSE)


