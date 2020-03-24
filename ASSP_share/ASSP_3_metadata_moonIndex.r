#### STORM-PETREL CPUE METADATA
# this script calculates sunset, moon rise and set, moon time
# created: Dec 10, 2018 by: E Kelsey
# last edited: March 5, 2020

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
# netting site locations
sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20200122.csv') %>% 
  select(-Notes)
# metadata from ASSP_2 script
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_2_CPUE_metadata_session.csv', na.strings=c("","NA")) %>% 
  # filter(Site != "RC", Site != "EAI_S") %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format = "%m/%d/%Y %H:%M")) %>% 
  left_join(sites_tbl, by = c("Site", "Location", "island" = "Island")) %>% 
  mutate(Site = as.factor(Site),
         Island = as.factor(island)) %>% 
  select(-App_sunset, -island) %>% 
  filter(TRUE)


### MAKE SUN AND MOON DATAFRAME

## SUNSET
# create dataframe to run sunset function on
sun_vec <- metadata %>%  
  transmute(date = as.Date(date, format = "%m/%d/%Y"), lat = Lat, lon = Long, Site = Site, sessionID = sessionID) %>% #as_date()
  # remove the few rows with unknown locations
  drop_na() %>%  
  filter(TRUE) 

metadata_sunsetTime <- getSunlightTimes(data = sun_vec,
                                    keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon,
         App_sunset = sunset) %>%
  left_join(sun_vec, by = c("date", "lat", "lon")) %>%
  select(-date, -lat, -lon, -sunset) %>%
  left_join(metadata, by = c("sessionID", "Site", "Lat", "Long")) %>% 
  mutate(Site = as.factor(Site),
         Island = as.factor(Island),
         min_1 = net_close_1 - net_open_1,
         min_2 = net_close_2 - net_open_2,
         min_3 = net_close_3 - net_open_3,
         min_4 = net_close_4 - net_open_4,
         min_5 = net_close_5 - net_open_5) %>% 
  mutate_at(c("min_1", "min_2", "min_3", "min_4", "min_5"), .funs = ~replace_na(., 0)) %>%
  mutate(net_close_1_std = if_else(net_close_1 > std_ending, std_ending, net_close_1),
         net_close_2_std = if_else(net_close_2 > std_ending, std_ending, net_close_2),
         net_close_3_std = if_else(net_close_3 > std_ending, std_ending, net_close_3),
         net_close_4_std = if_else(net_close_4 > std_ending, std_ending, net_close_4),
         net_close_5_std = if_else(net_close_5 > std_ending, std_ending, net_close_5),
         min_1_std = as.double(if_else(as.character(net_close_1_std - net_open_1) < 0, "0", 
                                    as.character(net_close_1 - net_open_1))),
         min_2_std = as.double(if_else(as.character(net_close_2_std - net_open_2) < 0, "0", 
                                    as.character(net_close_2 - net_open_2))),
         min_3_std = as.double(if_else(as.character(net_close_3_std - net_open_3) < 0, "0", 
                                    as.character(net_close_3 - net_open_3))),
         min_4_std = as.double(if_else(as.character(net_close_4_std - net_open_4) < 0, "0", 
                                    as.character(net_close_4 - net_open_4))),
         min_5_std = as.double(if_else(as.character(net_close_5_std - net_open_5) < 0, "0", 
                                    as.character(net_close_5 - net_open_5)))) %>% 
  mutate_at(c("min_1_std", "min_2_std", "min_3_std", "min_4_std", "min_5_std"), .funs = ~replace_na(., 0)) %>%
  mutate(min = as.numeric(min_1 + min_2 + min_3 + min_4 + min_5),
         min_std = min_1_std + min_2_std + min_3_std + min_4_std + min_5_std) %>%
  select(-min_1:-min_5_std) %>%
  filter(TRUE) 

write.csv(metadata_sunsetTime, file = '~/WERC-SC/ASSP_share/ASSP_3_CPUE_metadata_Sun_noMoon.csv',
          row.names = FALSE)
                              
## CALCULATE MOON TIME

moonDF <- data.frame(date = seq(ymd("1994-1-1"), ymd("2019-1-1"), by = "days"), 
                     lat = c(33.83128028, rep(NA, 9131)),
                     lon = c(-119.34492248, rep(NA, 9131)), stringsAsFactors=FALSE) %>% 
          fill(lat, lon) %>% 
          filter(TRUE)

# NOW PULL OUT RELEVANT DATES, THEN GET MOON ILLUMINATION DURING THOSE NIGHTS
moonT <- getMoonTimes(data = moonDF, 
                      keep = c("rise", "set"), tz = "PST8PDT") %>% 
          
          filter(TRUE)

as.data.table(moonT)
setkey(moonT, rise)
test = moonT[moonDF, roll = "nearest"]

# create dataframe to run moonCalc function on 
moonT_metadata <- metadata %>%  
  select(sessionID, net_open_1, std_ending) %>% 
  left_join(moonDF, by = c("net_open_1" = "rise"))
  
# moon_vec = metadata %>% 
#   transmute(startDay = net_open_1, #date = as.Date(date, format = "%m/%d/%Y"), # + lubridate::days(1), # mdy(date), # dummy_date = 
#             endDay = startDay + lubridate::days(1),
#             Lat, Long, 
#             # Site = Site, lat = Lat, lon = Long, 
#             sessionID = sessionID) %>% 
#             # startDay = as.POSIXct(date, format="%m/%d/%Y"), #  %h:%m:%s
#             # endDay = startDay + lubridate::days(1)) %>% 
#   # remove the few rows with unknown locations
#   drop_na() %>%  
#   filter(TRUE) 

moonT <- getMoonTimes(data = moon_vec,
                          keep = c("rise", "set"), tz = "PST8PDT") %>% 
  mutate(date = date - lubridate::days(1)) %>%
  left_join(moon_vec, by = c("date", "lat", "lon")) %>%
  # select(-lat, -lon,) %>%
  filter(TRUE) 

moonT_next <- getMoonTimes(data = moon_vec_next,
                          keep = c("rise", "set"), tz = "PST8PDT") %>% 
  mutate(Lat = lat, # nextDay = date,
         Long = lon,
         moonRise_next = rise,
         moonSet_next = set,
         date = date - lubridate::days(2)) %>%
  select(-rise, -set) %>% 
  left_join(moon_test, by = c("date", "Lat", "Long")) %>%
  select(-date, -lat, -lon) %>%
  mutate(moonRise = if_else(rise == "NA", moonRise_next, rise),
         moonSet = if_else(set == "NA", moonSet_next, set)) %>% 
  filter(TRUE) 

### MERGE METADATA WITH SUNSET AND MOONINDEX
## combine moonIndex and sunset
moonIndex_sunset <- moonIndex %>%  
  left_join(sunsetTime, by = c("nightID", "Lat", "Long")) %>% # c("startDates", "Lat", "Long", "Site")
  mutate(Date = date(startDay)) %>% 
  select(nightID, App_sunset, moonFrac, moonMin, moonIndex, Date, Site, std_ending, Lat, Long) %>% 
  filter(TRUE) 
## add combined sun moon dataset to metadata  
metadata_SunMoon_all <- metadata %>% 
  left_join(moonIndex_sunset, by = c("nightID", "Site", "Lat", "Long")) %>% 
  # sum up net time for all net open/close intervals
  mutate(min_std_raw = as.character(difftime(std_ending, net_open, units="mins")), 
         min_raw = as.character(difftime(net_close, net_open, units="mins")), 
         min_std = if_else(std_ending <= net_open, "0", 
                               if_else(std_ending <= net_close, min_std_raw, min_raw)),
         min_raw = as.numeric(min_raw),
         min_std = as.numeric(min_std)) %>% 
  select(-min_std_raw)

## select only new metadata and summarise by each nightID (combining multiple open/close efforts per night)
metadata_SunMoon <- metadata_SunMoon_all %>% 
  group_by(nightID) %>% 
  summarise(Site = first(Site), island = first(island), Date = first(Date), Lat = first(Lat), Long = first(Long), 
            App_sunset = first(App_sunset), moonFrac = first(moonFrac), moonMin = first(moonMin), moonIndex = first(moonIndex),
            net_open = first(net_open), net_close = last(net_close), std_ending = first(std_ending), 
            minutes_raw = sum(min_raw), minutes_std = sum(min_std),
            Net_mesh = first(Net_mesh), Net_dim = first(Net_dim), Audio_file = first(Audio_file), dB_level = first(dB_level), 
            Speaker_system = first(Speaker_system), Data_repository = first(Data_repository), notes = first(notes))

# confirm that metadata_SunMoon is the same length as unique nightID 
metadata_unique <- metadata_SunMoon %>% 
  distinct(nightID) %>% 
  mutate(seq = 1:n()) %>% 
  filter(TRUE)


#### SAVE NEW METADATA (one entry per night, with summed time) AND ALL METADATA
write.csv(metadata_SunMoon, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_new.csv',
          row.names = FALSE)  
write.csv(metadata_SunMoon_all, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_all.csv',
          row.names = FALSE)


# ### READ IN BANDING CPUE DATA
# metadata_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_metadata_session.csv', na.strings=c("","NA")) %>% 
#   mutate(net_open_old = as.POSIXct(paste(date, net_open), format="%m/%d/%Y %H:%M"),
#          net_close_old = as.POSIXct(paste(date, net_close), format="%m/%d/%Y %H:%M"),
#          # but some "net_open" and "net_close" times were actually after midnight:
#          nextDay_open = net_open_old + 24*60*60, nextDay_close = net_close_old + 24*60*60, 
#          # pull out the hour of the open/close event, if before 12 then it was after midnight:
#          hour_open = as.numeric(hour(net_open_old)), hour_close = as.numeric(hour(net_close_old)),
#          net_open = if_else(hour_open <= 12, nextDay_open, net_open_old),
#          net_close = if_else(hour_close <= 12, nextDay_close, net_close_old),
#          date = mdy(date)) %>% 
#   select(-mo_period, -duration:-birds.min.area, -X, -X.1, -net_open_old, -net_close_old,  
#          -nextDay_open, -nextDay_close, -hour_open, -hour_close) %>% # -App_sunset:-WS_midnight, -month, 
#   filter(TRUE) 
# 
# sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20200122.csv') %>% 
#   select(-Site_Name, -Notes)
# # 
# # ### COMBINE ALL METADATA
# # metadata_comb <- metadata_raw %>% 
# #   bind_rows(metadata_raw_AD)
# 
# ### STANDARDIZE LOCATIONS FOR ALL SITES AND REMOVE SUPERFLUOUS COLUMNS
# ## for CPUE metatdata
# metadata <- metadata_raw %>%
#   mutate(Site= mosaic::derivedFactor(
#     # ANI
#     "CC" = (island=="ANI" & site=="Cathedral Cove"),
#     "EAI_N" = (island=="ANI" & site=="EAI North side, dock area" | island=="ANI" & site=="Landing Cove Overlook"),
#     "EAI_S" = (island=="ANI" & site=="EAI South Ridge" | island=="ANI" & site=="EAI South side--near water catchment"),
#     "EAI_SW" = (island=="ANI" & site=="EAI SW end"),
#     "EAI_W" = (island=="ANI" & site=="EAI west of lighthouse"),
#     "FC" = (island=="ANI" & site =="North side Frenchy's Cove, East End, Upper Bench" | island=="ANI" & site =="Frenchy's Beach" | island=="ANI" & site =="Frenchy's Cove"),
#     "GC" = (island=="ANI" & site=="GC"),
#     "RR" = (island=="ANI" & site=="Rat Rock"),
#     "RC" = (island=="ANI" & site=="Rockfall Cove"), 
#     # PI
#     "PI1" = (island=="PI" & site=="PI1"), # & site=="1" | island=="PI" & site=="" | island=="PI" & site=="UNK" | island=="PI" & site=="PI1"), 
#     # SBI
#     "AP" = (island=="SBI" & site=="Arch Point" | island=="SBI" & site=="AP"),
#     "ESP" = (island=="SBI" & site=="Eseal Point" | island=="SBI" & site=="ESP" | island=="SBI" & site=="" | island=="SBI" & site=="UNK"),
#     "NTP" = (island=="SBI" & site=="Nature Trail Plot"), 
#     "NCliffs" = (island=="SBI" & site=="North Peak Cliffs" | island=="SBI" & site=="North Cliffs"), 
#     "SR" = (island=="SBI" & site=="Shag Overlook" | island=="SBI" & site=="Shag Rock Overlook"),
#     "SP" = (island=="SBI" & site=="SignalPeak" | island=="SBI" & site=="Signal Peak"), 
#     "Sutil" = (island=="SBI" & site=="Sutil Island"), 
#     "WP" = (island=="SBI" & site=="Webster's Point" | island=="SBI" & site=="Webster Point Draw"),
#     "WCliffs" = (island=="SBI" & site=="West Cliffs"), 
#     # SCI
#     "DR" = (island=="SCI" & site=="Diablo Rock"), 
#     "SR1" = (island=="SR" & site=="1" | island=="SR" & site=="SR1" | island=="SR" & site==""| island=="SR" & site=="Lower terrace of SE side of SR"| island=="SR" & site=="UNK"),
#     "SR2" = (island=="SR" & site=="2"),
#     "SR3" = (island=="SR" & site=="3"),
#     "LSH" = (island=="SCI" & site=="Little Scorpion Headland" | island=="SR" & site=="Scorpion Bluff"),
#     "HT" = (island=="SR" & site=="SR High Terrace-East"),
#     .default = "UNK"),
#     # create unique netting night ID
#     day = substr(date, 9, 10), month = substr(date, 6, 7),
#     dateStr = paste(year, month, day, sep = ""),
#     sessionID = paste(dateStr,island, Site, sep = "_")) %>%
#   # # rename all old fields
#   # App_sunset_old = App_sunset, Moon_fract_old = Moon_fract, moon_time_old = moon_time, moon_min_old = moon_min, moon_index_old = moon_index, WS_midnight_old = WS_midnight,
#   # std_ending_old = std_ending, minutes_old = minutes, # duration_old = duration, std_captured_old = std_captured, raw_CPUE_old = raw_CPUE, std_CPUE_old = std_CPUE, birds_min_area_old = birds.min.area
#   select(-site, -dateStr) %>%
#   # add latitude and longitude to metadata
#   left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island"))  
# 
# # updated CPUE metadata for publication
# write.csv(metadata, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_metadata.csv',
#           row.names = FALSE)

# 
# # MOON TIME CALCULATION FUNCTION (from MFC "moonIndex_v2.R")
# moonCalc <- function(startDay, endDay, longitude, latitude, interval = 60) { 
#                 startDay <- with_tz(startDay, 'UTC')
#                 endDay <- with_tz(endDay, 'UTC')
#                 interval <- first(interval)
#                 cl <- makeCluster(detectCores())
#                 registerDoParallel(cl)
#                 result <- foreach(s = startDay, e = endDay, 
#                                   x = longitude, y = latitude,
#                                   .combine = bind_rows, 
#                                   .packages = c('oce')) %dopar% {
#                                     t <- seq(from = s, to = e, by = interval)
#                                     period = as.numeric(difftime(e, s, units = 'days'))
#                                     sunAlt <- sunAngle(t, x, y, useRefraction = TRUE)$altitude
#                                     moon <- moonAngle(t, x, y, useRefraction = TRUE)
#                                     moonAlt <- moon$altitude
#                                     moonOnly <- moonAlt > 0 & sunAlt < 0
#                                     moonFrac <- mean(moon$illuminatedFraction[moonOnly])
#                                     moonMin <- sum(moonOnly) * (interval / period)/60
#                                     moonIndex <- moonMin * moonFrac
#                                     data.frame(startDate = s, moonFrac = moonFrac, moonMin = moonMin, moonIndex = moonIndex)
#                                   }
#                 stopCluster(cl)
#                 result
# }


# # run moonCalc function
# moonIndex <- moonCalc(moon_vec$startDay, moon_vec$endDay, moon_vec$Long, moon_vec$Lat) %>% 
#   # add dates and location back into datatable
#   bind_cols(moon_vec) %>% # STOP CODE AFTER THIS LINE AND CHECK THAT DATES LINE UP #
#   # after checking that dates line up, remove "startDate", also remove "endDay" because not needed
#   # select(-startDay, -endDay) %>% 
#   mutate(startDay = as.Date(startDay), 
#          seq = 1:n()) %>% # seq to make sure that tables are joined in correct order 
#   filter(TRUE) 
#account for 