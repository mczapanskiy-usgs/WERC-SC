#### STORM-PETREL MISTNETTING CPUE METADATA TO COMPARE WITH ACOUSTICS
# this script adds acoustic CPID, adds broodpatch frequency, and finalizes CPUE metadata
# created: April 1, 2019 by: E Kelsey
# last edited: April 10, 2019

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)

### READ IN DATA
# banding catches data 
# CPUE metadata from "ASSP_CPUE_4"
metadata <- read.csv('~/WERC-SC/ASSP_share//ASSP_4_metadata_CPUE_20200325.csv') %>% 
  mutate_at(c("App_sunset", "std_ending"),
              .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  filter(TRUE)

# netting site data
acoustics <- read.csv('~/WERC-SC/ASSP_share/CMI_Deployment_Info_CINP_2011-2018.csv') %>% 
  select(SPID, Island, subIsland, mistnet_site)

# banding catches data 
catches <- read.csv('~/WERC-SC/ASSP_share/ASSP_4_catches_BANDING_20200325.csv') %>%
  mutate(capture_time = as.POSIXct(capture_time, format="%Y-%m-%d %H:%M:%S"),
         release_time = as.POSIXct(release_time, format="%Y-%m-%d %H:%M:%S")) %>% 
  filter(TRUE)

# metadata_SM <- metadata %>% 
#   inner_join(acoustics, by = c("Island" = "Island", "Location" = "subIsland", "Site" = "mistnet_site")) %>% 
#   unique()

## ADD BROOD PATCH FREQUENCY TO METADATA
# translate numerical ranked to B/D/PD
metadata_BPcode <- catches %>%
  mutate(assumeBreed = mosaic::derivedFactor(
    "Y" = (BP == "B" | BP == "b" | BP == "2" | BP == "3" | BP == "4"),
    "N" = (BP == "D" | BP == "d" | BP == "0" | BP == "5" | BP == "PD" | BP == "pd" | BP == "1" | BP == "1.5" | BP == "4.5"),
    .default = "ND")) %>% 
  group_by(sessionID, Site) %>%
  summarise(BPct = n(),
            BP_Y = sum(assumeBreed == "Y"),
            BP_N = sum(assumeBreed == "N")) %>%
  right_join(metadata, by= c("sessionID", "Site")) %>% 
  mutate(BPfreq_Y = BP_Y/BPct,
         BPfreq_N = BP_N/BPct) %>% 
  select(sessionID, Site, Island:CPUEstd, BPfreq_Y:BPfreq_N, Net_mesh:Flagged_notes)

# BPcode <- gather(metadata_BPcode, net_stat, net_time, net_open_1:net_close_5) %>% 
#   mutate(net_time_old = as.POSIXct(paste(date, net_time), format="%m/%d/%Y %H:%M"),
#          net_time_nxt = net_time_old + days(1),
#          net_time_hr = as.numeric(hour(net_time_old)),
#          net_time = if_else(net_time_hr <=12, net_time_nxt, net_time_old)) %>%
#   select(-net_time_old, -net_time_nxt, -net_time_hr) %>% 
#   group_by(sessionID) %>% 
#   spread(net_stat, net_time) %>% 
#   filter(TRUE) 


write.csv(metadata_BPno, file = '~/WERC-SC/ASSP_share/metadata_BPno.csv',
          row.names = FALSE)

write.csv(metadata_BPcode, file = '~/WERC-SC/ASSP_share/metadata_BPcode.csv',
          row.names = FALSE)


#### SAVE CPUE DATA 2015-2018 TO SEND TO T TINKER
metadata_BPno_SM <- metadata_BPno %>%
  filter(year > 2014)

write.csv(metadata_catches_2017_2018, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE_2017-2018.csv',
          row.names = FALSE)

# # analyze just the numerically ranked broodpatches
# metadata_BPno <- catches %>%
#   group_by(sessionID, Site) %>%
#   summarise(BPct = n(),
#             BP0 = sum(BP == "0"),
#             BP1 = sum(BP == "1"),
#             BP1.5 = sum(BP == "1.5"),
#             BP2 = sum(BP == "2"),
#             BP3 = sum(BP == "3"),
#             BP4 = sum(BP == "4"),
#             BP4.5 = sum(BP == "4.5"),
#             BP5 = sum(BP == "5")) %>%
#   right_join(metadata, by= c("sessionID", "Site")) %>% 
#   mutate(BPfreq_0 = BP0/BPct,
#          BPfreq_1 = BP1/BPct,
#          BPfreq_1.5 = BP1.5/BPct,
#          BPfreq_2 = BP2/BPct,
#          BPfreq_3 = BP3/BPct,
#          BPfreq_4 = BP4/BPct,
#          BPfreq_4.5 = BP4.5/BPct,
#          BPfreq_5 = BP5/BPct) # %>% 
# # select(sessionID, Site, Island:CPUEstd, BPfreq_0:BPfreq_5, Net_mesh:Flagged_notes)

# ### SUMMARY OF ALL CATCHES FOR SONGMETER METADATA
# catches_std_allSP <- catches_std %>%
#   filter(spp %in% c("ASSP", "LESP", "BLSP")) %>%
#   group_by(spp, nightID) %>%
#   summarise(count = n()) %>%
#   spread(spp, count)
# write.csv(catches_std_allSP, file = '~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv',
          # row.names = FALSE)

