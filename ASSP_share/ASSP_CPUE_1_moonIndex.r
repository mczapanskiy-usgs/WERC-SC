#### STORM-PETREL CPUE METADATA
# this script calculates sunset, moon rise and set, moon time
# created: Dec 10, 2018 by: E Kelsey
# last edited: Feb 2, 2019

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
library(stats)
library(suncalc)
library(chron)
# library(hms)
# library(rnoaa)
# library(maptools)
# library(circular)
# library(StreamMetabolism)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE_2018.csv') %>% # , na.strings=c("","NA")
  mutate(net_open = as.POSIXct(paste(date, net_open), format="%m/%d/%Y %H:%M"),
         net_close = as.POSIXct(paste(date, net_close), format="%m/%d/%Y %H:%M"),
         nextDay = net_close + 24*60*60, # create a new column of the day netting effort ended
         hour = as.numeric(hour(net_close)),
         net_close_new = if_else(hour <= 12, nextDay, net_close)) %>% 
  select(-net_close, -X, -X.1, -nextDay, -hour) -> metadata_raw 

read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_SR_SBI_CPUE_2014-2018.csv', na.strings=c("","NA")) %>% 
  mutate(net_open = as.POSIXct(paste(date, net_open), format="%m/%d/%Y %H:%M"),
         net_close = as.POSIXct(paste(date, net_close), format="%m/%d/%Y %H:%M"),
         nextDay = net_close + 24*60*60, # create a new column of the day netting effort ended
         hour = as.numeric(hour(net_close)),
         net_close_new = if_else(hour <= 12, nextDay, net_close))%>% 
  select(-net_close, -nextDay, -hour) -> metadata_raw_AD 
  
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites_tbl

### COMBINE ALL METADATA AND REMOVE SUPERFLUOUS COLUMNS
metadata_comb <- metadata_raw %>% 
  bind_rows(metadata_raw_AD)

### STANDARDIZE LOCATIONS FOR ALL SITES
## for CPUE metatdata
metadata <- metadata_comb %>%
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
        Date = mdy(date, tz = "US/Pacific"),
        App_sunset_old = App_sunset,
        Moon_fract_old = Moon_fract,
        moon_time_old = moon_time,
        moon_min_old = moon_min,
        moon_index_old = moon_index,
        WS_midnight_old = WS_midnight,
        duration_old = duration,
        minutes_old = minutes,
        std_captured_old = std_captured,
        raw_CPUE_old = raw_CPUE, 
        std_CPUE_old = std_CPUE,
        birds_min_area_old = birds.min.area) %>% 
  select(-site, -std_ending, -std_min, -App_sunset, -Moon_fract, -moon_time, -moon_min, -moon_index, -WS_midnight, 
         -duration, -minutes, -std_captured, -raw_CPUE, -std_CPUE) %>%
  # add latitude and longitude to metadata
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island"))  

# write.csv(metadata, file = '~/WERC-SC/ASSP_share/metadata.csv', row.names = FALSE) 


### MAKE SUN AND MOON DATAFRAME

## Calculate moon time
# FUNCTION (from MFC "moonIndex_v2.R")
moonCalc <- function(startDay, endDay, longitude, latitude, interval = 60) { 
                startDay <- with_tz(startDay, 'UTC')
                endDay <- with_tz(endDay, 'UTC')
                interval <- first(interval)
                cl <- makeCluster(detectCores())
                registerDoParallel(cl)
                result <- foreach(s = startDay, e = endDay, 
                                  x = longitude, y = latitude,
                                  .combine = bind_rows, 
                                  .packages = c('oce')) %dopar% {
                                    t <- seq(from = s, to = e, by = interval)
                                    period = as.numeric(difftime(e, s, units = 'days'))
                                    sunAlt <- sunAngle(t, x, y, useRefraction = TRUE)$altitude
                                    moon <- moonAngle(t, x, y, useRefraction = TRUE)
                                    moonAlt <- moon$altitude
                                    moonOnly <- moonAlt > 0 & sunAlt < 0
                                    moonFrac <- mean(moon$illuminatedFraction[moonOnly])
                                    moonMin = (sum(moonOnly) * interval / period)/60
                                    moonIndex = moonMin * moonFrac
                                    data.frame(startDate = s, moonFrac = moonFrac, moonMin = moonMin, moonIndex = moonIndex)
                                  }
                stopCluster(cl)
                result
              }
# create dataframe to run moonCalc function on 
moon_vec = metadata %>% 
  transmute(startDates = Date, endDates = startDates + lubridate::days(1), Lat, Long, Site = Site)
# run moonCalc function
moonI <- moonCalc(moon_vec$startDates, moon_vec$endDates, moon_vec$Lat, moon_vec$Long) 
# add dates and location back into datatable
moonIndex <- moonI %>% 
  bind_cols(moon_vec) %>% # STOP CODE AFTER THIS LINE AND CHECK THAT DATES LINE UP #
  # after checking that dates line up, remove "startDate", also remove "endDates" because not needed
  select(-startDate, -endDates) %>% 
  mutate(startDates = as.Date(startDates), 
         seq = 1:n()) # seq to make sure that tables are joined in correct order

## calculate sunset
# create datafram to run sunset function on
sun_vec <- metadata %>% 
  transmute(date = as_date(Date), lat = Lat, lon = Long, Site = Site) 
# run sunset function
sunsetTime <- getSunlightTimes(data = sun_vec,
                           keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon) %>% 
  select(-date, -lat, -lon) %>% 
  # make App_sunset by breaking up sunset time and date
  mutate_at(vars(sunset), funs("startDates" = date(.), "App_sunset" = hms::as.hms(.))) %>% 
  mutate(App_sunset = chron::times(App_sunset))


### MERGE METADATA WITH SUNSET AND MOONINDEX
## combine moonIndex and sunset
moonIndex_sunset <- moonIndex %>% 
  left_join(sunsetTime, by = c("startDates", "Lat", "Long", "Site")) %>%
  mutate(Date = date(startDates)) %>% 
  select(seq, App_sunset, moonFrac, moonMin, moonIndex, Date, Site, std_ending, Lat, Long)
## add combined sun moon dataset to metadata  
metadata_SunMoon <- metadata %>% 
  mutate(Date = mdy(date)) %>%
  left_join(moonIndex_sunset, by = c("Date", "Site", "Lat", "Long")) 


### SUM TIME AND CALCULATE CPUE
metadata_SunMoon_CPUE <- metadata_SunMoon %>% 
  mutate(duration = as.numeric(difftime(net_close_new, net_open, units="mins")))

# select(App_sunset, Moon_rise, Moon_Set, moonFrac, moonMin, moonIndex, Date, Island, Site, std_ending, Lat, Long)

