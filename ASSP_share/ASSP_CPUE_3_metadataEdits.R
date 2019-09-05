#### STORM-PETREL CPUE METADATA
# this script calculates processes the metadata that Amelia created
# created: March 6, 2019 by: E Kelsey
# last edited: August 26, 2019 by E Kelsey

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)

### READ IN BANDING CPUE DATA
metadata_SunMoon_sum <- read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_sum.csv')
catches_std_SP <- read.csv('~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv')
# read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20190905.csv') %>% 
#   select(-Notes) -> sites_tbl
SM_sites <- read.csv('~/WERC-SC/ASSP_share/USGS_ASSP_Mistnetting_2019_Deployment_Info.csv') %>%
  mutate(Deployment_Date = as.Date(Deployment_Date, format="%m/%d/%Y"),
         Retrieval_Date = as.Date(Retrieval_Date, format="%m/%d/%Y"),
         Island = as.character(Island)) %>% 
  select(Sensor_Name, SPID, Deployment_Date, Deployment_Year, Retrieval_Date, Island, Site)

### CREATE MISTNETTING METADATA SHEET TO SHARE WITH CMI
metadata_3 <- metadata_SunMoon_sum %>% 
  mutate(island = as.character(island)) %>%
  inner_join(catches_std_SP, by = "nightID")

metadata_SM <- metadata_3 %>% 
  left_join(SM_sites, by = c("island" = "Island", "year" = "Deployment_Year")) %>% 
  mutate(net_notes = notes,
         Site = Site.x) %>%
  select(nightID, Date, year, island, Site, net_open, net_close, ASSP, LESP, BLSP, net_notes, SPID) %>%
  
# ## test to see if any duplicates were created
# duplicates <- duplicated(metadata_SM) %>% 
#   data.table()
# summary(duplicates)
# metadata_duplicates <- metadata_SM %>% 
#   bind_cols(duplicates) %>% 
#   filter(. == "TRUE")
  
  # 22 entries from PI_AB47 and SB_LC duplicated during the bind, remove duplicates
  distinct()
  


write.csv(metadata_SM, file = '~/WERC-SC/ASSP_share/ASSP_MistnetMetadata_SM_1994-2018_20190826.csv',
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

