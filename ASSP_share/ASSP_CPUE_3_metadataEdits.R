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
read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_sum.csv') -> metadata_SunMoon_sum
read.csv('~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv') -> catches_std_SP
# read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locations_032219.csv') %>% 
#   select(-Notes) -> sites_tbl
read.csv('~/WERC-SC/ASSP_share/CMI_Deployment_Info_CINP_2011-2018.csv') %>%
  mutate(Deployment_Date = as.Date(Deployment_Date, format="%m/%d/%Y"),
         Retrieval_Date = as.Date(Retrieval_Date, format="%m/%d/%Y"),
         Island = as.character(Island),
         Sensor_Location = Location) %>% 
  select(Sensor_Name, Deployment_Date, Deployment_Year, Retrieval_Date, Island, Sensor_Location, Latitude, Longitude) -> SM_sites

### CREATE MISTNETTING METADATA SHEET TO SHARE WITH CMI
metadata_3 <- metadata_SunMoon_sum %>% 
  left_join(catches_std_SP, by = "nightID")

metadata_SM <- metadata_3 %>% 
  select(nightID, Date, year, island, Site, Lat, Long, net_open, net_close, ASSP, LESP, BLSP, notes) %>% #, -day, -Month, -dateStr, -site 
  full_join(SM_sites, by = c("island" = "Island", "year" = "Deployment_Year"))

write.csv(metadata_SM, file = '~/WERC-SC/ASSP_share/ASSP_MistnetMetadata_SM_1994-2018_20190808.csv',
          row.names = FALSE)

### ADD ASSP CATCHES TO METADATA




# catches_std_ASSP <- catches_std %>% 
#   filter(recapture == "N",
#          spp == "ASSP",
#          std == "1") %>% # std = 1 => caught before std_ending
#   group_by(nightID) %>% 
#   summarise(count = n()) %>% 
#   mutate(no_captured_std = as.character(count))
# 
# metadata_count <- metadata %>%  
#   #  remove multiple open/close events for one netting night
#   group_by(date, Lat, Long, Site, nightID) %>%
#   count(nightID) %>% 
#   ungroup %>% 
#   full_join(catches_std_ASSP, by = "nightID")
# 
# ### SUMMARY OF ALL CATCHES FOR SONGMETER METADATA
# catches_std_all <- catches_std %>% 
#   filter(spp %in% c("ASSP", "LESP", "BLSP")) %>% 
#   group_by(spp, nightID) %>% 
#   summarise(count = n()) %>% 
#   spread(spp, count)

