#### STORM-PETREL MISTNETTING CPUE METADATA TO COMPARE WITH ACOUSTICS
# this script adds acoustic CPID, adds broodpatch frequency, and finalizes CPUE metadata
# created: April 1, 2019 by: E Kelsey
# last edited: July 7, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(readxl)

### READ IN DATA
# read in metadata (CPUE) and standardize date/time fields
metadata <- read_excel("CHIS_ASSP_mistnet_database_07092020.xlsx", sheet = "CPUE", 
                   col_names = TRUE, na = c("NA", "ND")) %>% 
  mutate_at(c("app_sunset", "std_ending"), 
            .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3", 
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>%
  filter(TRUE)

metadata_effort <- metadata %>% 
  select(site_code, session_ID, app_sunset, std_ending, lat, long)

# read in all catch (banding) data
catches <- read_excel("CHIS_ASSP_mistnet_database_07092020.xlsx", sheet = "Banding", 
                      col_names = TRUE, na = c("NA", "ND")) %>% 
  mutate_at(c("capture_time", "release_time"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>%
  filter(species == "ASSP",
         BP != "NR", 
         BP != "NA") %>%
  mutate(assumeBreed = mosaic::derivedFactor(
                "Y" = (BP == "B" | BP == "b" | BP == "2" | BP == "3" | BP == "4"),
                "N" = (BP == "D" | BP == "d" | BP == "0" | BP == "5" | BP == "PD" | BP == "pd" | BP == "1" | BP == "1.5" | BP == "4.5"),
                .default = "ND")) %>% 
  left_join(metadata_effort, by = c("site_code", "session_ID", "lat", "long")) %>% 
  mutate(std = if_else(std_ending > capture_time, "1", "0")) %>%
  select(catch_ID, session_ID, app_sunset, std_ending, month, day, year:release_time, species:recapture, 
         std, diet, BP, assumeBreed, uncorr_mass:notes) %>%
  filter(TRUE)


metadata_BP <- catches %>%
  group_by(session_ID, site_code) %>%
  summarise(BPct = n(),
            BPct_std = sum(std == "1"),
            BP_Y = sum(assumeBreed == "Y"), # number of birds that have a broodpatch (2-4, B)
            BP_Ystd = sum(assumeBreed == "Y" & std == "1")) %>% # number of birds that dont have a broodpatch (1, 1.5, 4.5, 5, PB, D)
  right_join(metadata, by= c("session_ID", "site_code")) %>%
  mutate(BPfreq_Y = BP_Y/BPct, # frequency of birds that have a broodpatch
         BPfreq_Ystd = BP_Ystd/BPct_std,
         CPUEbrd = BP_Y/min,
         CPUEbrd_std = BP_Ystd/min_std) %>% 
  select(session_ID, island_code, subisland_code, site_code, site_name, lat, long, month, day, year, series_ID, app_sunset, std_ending,
         net_open_1, net_close_1, net_open_2, net_close_2, net_open_3, net_close_3, net_open_4, net_close_4, net_open_5, net_close_5,
         min, min_std, ASSP, ASSPstd, CPUEraw, CPUEstd, CPUEbrd, CPUEbrd_std, BP_Y, BP_Ystd, BPfreq_Y, BPfreq_Ystd, net_mesh:flagged_notes)


#### SAVE CPUE DATA 2015-2018 TO SEND TO T TINKER
metadata_BP_SM <- metadata_BP %>%
  filter(year > 2014)

write.csv(metadata_BP_SM, file = '~/WERC-SC/ASSP_share/ASSP_6_CPUE_broodpatch_2015-2018_rev20200630.csv',
          row.names = FALSE)

write.csv(metadata_BP, file = '~/WERC-SC/ASSP_share/ASSP_6_CPUE_broodpatch_20200709.csv',
          row.names = FALSE)
