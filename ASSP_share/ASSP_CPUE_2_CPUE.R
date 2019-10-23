#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: Feb 11, 2019 by: E Kelsey
# last edited: September 13, 2019

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
catches_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_20190913.csv') %>% 
                # remove unnecessary rows
                select(-P10, -P09, -P08, -P07, -P06, -P05, -P04, -P03, -P02, -P01, 
                       -R6, -R5, -R4, -R3, -R2, -R1, -X, -X.1, -X.2, -X.3, -X.4, -X.5,
                       -tail, -sex, -release.time)

# netting site data
sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20190905.csv') %>% 
                select(-Alias, -Notes) 

# CPUE metadata from "ASSP_CPUE_1"
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_new.csv') %>% 
              mutate(Date = as.Date(Date),
                     std_ending = as.POSIXct(std_ending),
                     net_open = as.POSIXct(net_open),
                     net_close = as.POSIXct(net_close),
                     minutes_raw = as.integer(minutes_raw),
                     minutes_std = as.integer(minutes_std)) %>% 
              filter(TRUE)


# for catch data
catches <- catches_raw %>% 
  filter(island != "ND") %>% 
  select(date, island, year, site, capture.time, species, recapture.) %>% 
  mutate(date = mdy(date),
    Site= mosaic::derivedFactor(
    # ANI
    "CC" = (island=="ANI" & site=="Cathedral Cove"),
    "EAI_N" = (island=="ANI" & site=="EAI North side, dock area" | island=="ANI" & site=="Landing Cove Overlook"),
    # "EAI_S" = (island=="ANI" & site=="EAI South Ridge" | island=="ANI" & site=="EAI South side--near water catchment"),
    "EAI_SW" = (island=="ANI" & site=="EAI SW end"),
    "EAI_W" = (island=="ANI" & site=="EAI west of lighthouse"),
    "FC" = (island=="ANI" & site =="North side Frenchy's Cove, East End, Upper Bench" | island=="ANI" & site =="Frenchy's Beach" | island=="ANI" & site =="Frenchy's Cove"),
    "GC" = (island=="ANI" & site=="GC"),
    "RR" = (island=="ANI" & site=="Rat Rock"),
    "RC" = (island=="ANI" & site=="Rockfall Cove"), 
    # PI
    "PI1" = (island=="PI" & site=="1"| island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
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
    "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR" | island=="SR" & site=="UNK" | island=="SR" & site=="AB10"),
    "SR2" = (island=="SR" & site=="2"),
    "SR3" = (island=="SR" & site=="3"),
    "LSH" = (island=="SR" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
    "HT" = (island=="SR" & site=="SR High Terrace-East"),
    .method = "first",
    .default = "UNK"),
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

summary(as.factor(catches$Site))
summary(sites_tbl$Site)

catches_ID <- catches %>% 
  group_by(nightID) %>% 
  mutate(seq = 1:n(),
         catchID = paste(nightID, seq, sep = "_")) %>% 
  ungroup() %>% 
  mutate(nightID = as.factor(nightID),
         Site = as.factor(Site)) %>% 
  filter(TRUE)

## bind sunset, net open, net closed, std_ending times with catches dataset
metadata_effort <- metadata %>% 
  select(Site, Date, nightID, net_open, net_close, App_sunset, std_ending, Lat, Long)

catches_metadata <- catches_ID %>%
  full_join(metadata_effort, by = c("Site", "nightID", "Lat", "Long")) 
# compare Sites listed in catches vs. metadata datasets
summary(metadata_effort$Site)
summary(catches_ID$Site)
  
# ## test to see if any duplicates were created
# duplicates <- duplicated(catches_metadata) %>%
#   data.table()
# summary(duplicates)
# catches_duplicates <- catches_metadata %>%
#   bind_cols(duplicates) %>%
#   filter(. == "TRUE")

# standardize recapture and species
catches_std <- catches_metadata %>% 
  mutate(std = if_else(std_ending > capture_time, "1", "0"), # if bird was caught before std_ending = 1, after = 0
         pre_close = if_else(net_close > capture_time, "1", "0"), # if bird was caught before net_close = 1, after = 0
         post_open = if_else(net_open < capture_time, "1", "0"), # if bird was caught after net_open = 1, before = 0
         recapture = mosaic::derivedFactor(
           "Y" = (recapture.=="SNR" | recapture.=="YSN"), # remove same night recaptures 
           "N" = (recapture.=="N" | recapture.=="Y" | recapture.=="y"), # recaps from other nights still count # 
           # "UNK" = (recapture.=="UNK" & recapture.=="X"),
           .default = "UNK"),
        spp = mosaic::derivedFactor(
           "ASSP" = (species =="ASSP"),
           "LESP" = (species =="LESP" | species =="LHSP"),
           "LSTP" = (species =="LTSP"), # species =="LSTP" | 
           "BLSP" = (species == "BLSP"),
           "OTHER" = (species == "CAAU" & species =="JOPE" & species =="WEGU" & species=="XAMU"), #  & species=="OTHER" 
           # "UNK" = (species == "UNK"), #  & species=="ASSP/LESP"
           .default = "UNK")) %>%
  filter(TRUE)

summary(catches_metadata$recapture.)
summary(catches_std$recapture)

### SAVE CATCHES FILE BEFORE IT IS FILTERED
write.csv(catches_std, file = '~/WERC-SC/ASSP_share/CATCHES_allSpp_unfiltered.csv',
          row.names = FALSE)

missingMETADATA <- anti_join(metadata, catches_std, by = "nightID")
missingBANDING <- anti_join(catches_std, metadata, by = "nightID")

## filter catches, sum by species, than sum by catch effort with metadata
# view what will be filtered
summary(as.factor(catches_std$std)) # catches before vs. after standard ending
summary(as.factor(catches_std$pre_close)) # catches before net opened, also ones duplicated for ea. open/close during one night when merged w/ metadata
summary(as.factor(catches_std$post_open)) # catches after net closed, also ones duplicated for ea. open/close during one night when merged w/ metadata
summary(catches_std$recapture == "N") # recaptures
summary(catches_std$spp == "ASSP")
# filter out other spp, and catches that don't count as part of effort
catches_filtered <- catches_std %>% 
  filter(recapture == "N", 
         pre_close == "1",
         post_open == "1",
         spp == "ASSP") # std == "1",
# sum catches for each species and night
metadata_catches <- catches_filtered %>%
  group_by(island, Site, nightID) %>% # , "net_open"
  # count(species) %>% 
  summarise(ASSP = n(),
            ASSPstd = sum(std == "1")) %>% 
  # mutate(ASSPraw = as.character(ASSP)) %>% 
  left_join(metadata, by= c("nightID", "island", "Site")) %>% # , "net_open"
  filter(minutes_std > 0) %>%
  mutate(CPUEraw = ASSP/minutes_raw,
         CPUEstd = ASSPstd/minutes_std) %>% 
  # drop_na()
  distinct()
write.csv(metadata_catches, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE.csv',
          row.names = FALSE)

summary(metadata_catches$ASSP)
summary(metadata_catches$ASSPstd)


#### SAVE CPUE DATA FOR ALL NETTING EFFORTS THAT CPUE CAN BE CALCULATED FOR
write.csv(metadata_catches, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE.csv',
          row.names = FALSE)

#### SAVE CPUE DATA 2017-2018 TO SEND TO T TINKER
metadata_catches_2017_2018 <- metadata_catches %>% 
  mutate(year = year(Date)) %>% 
  filter(year > 2016) %>% 
  select(-year)
write.csv(metadata_catches_2017_2018, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE_2017-2018.csv',
          row.names = FALSE)







### SUMMARY OF ALL CATCHES FOR SONGMETER METADATA
catches_std_allSP <- catches_std %>% 
  filter(spp %in% c("ASSP", "LESP", "BLSP")) %>% 
  group_by(spp, nightID) %>% 
  summarise(count = n()) %>% 
  spread(spp, count)

write.csv(catches_std_allSP, file = '~/WERC-SC/ASSP_share/MistnetMetadata_sum_SP.csv',
          row.names = FALSE)
