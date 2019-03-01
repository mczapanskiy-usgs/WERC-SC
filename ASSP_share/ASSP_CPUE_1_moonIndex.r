#### STORM-PETREL CPUE METADATA
# this script calculates sunset, moon rise and set, moon time
# created: Dec 10, 2018 by: E Kelsey
# last edited: Feb 27, 2019

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
# library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE_2018.csv', na.strings=c("","NA")) %>% 
  mutate(net_open_old = as.POSIXct(paste(date, net_open), format="%Y-%m-%d %H:%M"),
         net_close_old = as.POSIXct(paste(date, net_close), format="%Y-%m-%d %H:%M"),
         # but some "net_open" and "net_close" times were actually after midnight:
         nextDay_open = net_open_old + 24*60*60, nextDay_close = net_close_old + 24*60*60, 
         # pull out the hour of the open/close event, if before 12 then it was after midnight:
         hour_open = as.numeric(hour(net_open_old)), hour_close = as.numeric(hour(net_close_old)),
         net_open = if_else(hour_open <= 12, nextDay_open, net_open_old),
         net_close = if_else(hour_close <= 12, nextDay_close, net_close_old)) %>% 
  select(-X, -X.1, -nextDay_close, -nextDay_open, -hour_open, -hour_close) -> metadata_raw 

read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_SR_SBI_CPUE_2014-2018.csv', na.strings=c("","NA")) %>% 
  mutate(net_open_old = as.POSIXct(paste(date, net_open), format="%Y-%m-%d %H:%M"),
         net_close_old = as.POSIXct(paste(date, net_close), format="%Y-%m-%d %H:%M"),
         # but some "net_open" and "net_close" times were actually after midnight:
         nextDay_open = net_open_old + 24*60*60, nextDay_close = net_close_old + 24*60*60, 
         # pull out the hour of the open/close event, if before 12 then it was after midnight:
         hour_open = as.numeric(hour(net_open_old)), hour_close = as.numeric(hour(net_close_old)),
         net_open = if_else(hour_open <= 12, nextDay_open, net_open_old),
         net_close = if_else(hour_close <= 12, nextDay_close, net_close_old))%>% 
  select(-nextDay_close, -nextDay_open, -hour_open, -hour_close) -> metadata_raw_AD 
  
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites_tbl

### COMBINE ALL METADATA
metadata_comb <- metadata_raw %>% 
  bind_rows(metadata_raw_AD)

### STANDARDIZE LOCATIONS FOR ALL SITES AND REMOVE SUPERFLUOUS COLUMNS
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
        # create unique netting night ID
        day = substr(date, 9, 10),
        Month = substr(date, 6, 7),
        dateStr = paste(year, Month, day, sep = ""),
        nightID = paste(dateStr,island, Site, sep = "_"),
        # rename all old fields
        App_sunset_old = App_sunset,
        Moon_fract_old = Moon_fract,
        moon_time_old = moon_time,
        moon_min_old = moon_min,
        moon_index_old = moon_index,
        WS_midnight_old = WS_midnight,
        std_ending_old = std_ending,
        # duration_old = duration,
        minutes_old = minutes,
        std_captured_old = std_captured,
        raw_CPUE_old = raw_CPUE, 
        std_CPUE_old = std_CPUE,
        birds_min_area_old = birds.min.area) %>% 
  select(-site, -std_ending, -duration, -std_min, -App_sunset, -Moon_fract, -moon_time, -moon_min, -moon_index, -WS_midnight, 
         -duration, -minutes, -std_captured, -raw_CPUE, -std_CPUE, -Month, -day) %>%
  # add latitude and longitude to metadata
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island"))  


### MAKE SUN AND MOON DATAFRAME

## Calculate moon time
# MOON TIME CALCULATION FUNCTION (from MFC "moonIndex_v2.R")
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
  group_by(date, Lat, Long, Site, nightID) %>%
  count(nightID) %>% 
  ungroup %>% 
  transmute(date = as.Date(date), startDates = lubridate::ymd(date), endDates = startDates + lubridate::days(1), 
            Lat, Long, nightID = nightID) %>% #
  mutate(startDates = as.POSIXct(startDates), endDates = as.POSIXct(endDates))
# run moonCalc function
moon <- moonCalc(moon_vec$startDates, moon_vec$endDates, moon_vec$Lat, moon_vec$Long) 

# add dates and location back into datatable
moonIndex <- moon %>% 
  bind_cols(moon_vec) %>% # STOP CODE AFTER THIS LINE AND CHECK THAT DATES LINE UP #
  # after checking that dates line up, remove "startDate", also remove "endDates" because not needed
  select(-startDate, -endDates) %>% 
  mutate(startDates = as.Date(startDates), 
         seq = 1:n()) # seq to make sure that tables are joined in correct order 

## calculate sunset
# create datafram to run sunset function on
sun_vec <- metadata %>%  
  #  remove multiple open/close events for one netting night
  group_by(date, Lat, Long, Site, nightID) %>%
  count(nightID) %>% 
  ungroup %>% 
  transmute(date = as_date(date), lat = Lat, lon = Long, Site = Site, nightID = nightID) 
# run sunset function
sunsetTime <- getSunlightTimes(data = sun_vec,
                           keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon,
         App_sunset = sunset) %>% 
  select(-date, -lat, -lon, -sunset)  
  # # make App_sunset by breaking up sunset time and date
  # mutate_at(vars(sunset), funs("startDates" = date(.), "App_sunset" = hms::as.hms(.))) %>% 


### MERGE METADATA WITH SUNSET AND MOONINDEX
## combine moonIndex and sunset
moonIndex_sunset <- moonIndex %>%  
  left_join(sunsetTime, by = c("nightID", "Lat", "Long")) %>% # c("startDates", "Lat", "Long", "Site")
  mutate(Date = date(startDates)) %>% 
  select(nightID, App_sunset, moonFrac, moonMin, moonIndex, Date, Site, std_ending, Lat, Long)
## add combined sun moon dataset to metadata  
metadata_SunMoon <- metadata %>% 
  left_join(moonIndex_sunset, by = c("nightID", "Site", "Lat", "Long")) 


### SUM TIME 
# create table with just one row per net night
metadata_unique <- metadata_SunMoon %>% 
  distinct(nightID) %>% 
  mutate(seq = 1:n())
# sum up net time for all net open/close intervals
metadata_SunMoon_sum <- metadata_SunMoon %>% 
  mutate(minutes_std_raw = as.character(difftime(std_ending, net_open, units="mins")), 
         minutes_raw = as.character(difftime(net_close, net_open, units="mins")), 
         minutes_std = if_else(std_ending <= net_open, "0", 
                                if_else(std_ending <= net_close, minutes_std_raw, minutes_raw)),
         minutes_raw = as.numeric(minutes_raw),
         minutes_std = as.numeric(minutes_std)) %>% 
  group_by(nightID) %>% 
  mutate(minutes = sum(minutes_std)) %>% 
  ungroup %>% 
  select(-minutes_std_raw)

write.csv(metadata_SunMoon_sum, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_sum.csv',
          row.names = FALSE)

# test <- metadata_SunMoon_sum %>%
#   select(nightID, net_open_old, std_ending_old, net_open, net_close, std_ending, minutes_old, minutes_raw, minutes_std, minutes)




# moonIndexF <- function(date, lon, lat) {
#   if(class(date) != 'Date') stop('date must be a Date object')
#   
#   d <- data.frame(t = ISOdate(year(date), month(date), day(date), tz = 'UTC') + seq(0, 24*3600, length = 24*3600),
#                   moonalt = oce::moonAngle(t, lon, lat)$altitude,
#                   sunalt = oce::sunAngle(t, lon, lat)$altitude) %>%
#     mutate(absma = abs(moonalt),
#            abssa = abs(sunalt),
#            moonslope = sign(lead(moonalt) - moonalt),
#            sunslope = sign(lead(sunalt) - sunalt))
#   
#   moonrise <- d %>%
#     filter(moonslope > 0) %>%
#     arrange(absma) %>%
#     slice(1) %>%
#     select(t) %>%
#     first
#   
#   moonset <- d %>%
#     filter(moonslope < 0) %>%
#     arrange(absma) %>%
#     slice(1) %>%
#     select(t) %>%
#     first
#   
#   sunrise <- d %>%
#     filter(sunslope > 0) %>%
#     arrange(abssa) %>%
#     slice(1) %>%
#     select(t) %>%
#     first
#   
#   sunset <- d %>%
#     filter(sunslope < 0) %>%
#     arrange(abssa) %>%
#     slice(1) %>%
#     select(t) %>%
#     first
#   
#   # browser()
#   
#   diameter <- moonAngle(d$t[1], lon, lat)$diameter
#   illumination <- moonAngle(d$t[1], lon, lat)$illuminatedFraction
#   moontime <- difftime(max(moonrise, sunset), min(moonset, sunrise), units = 'hours') %>% as.numeric
#   
#   diameter * illumination * moontime
# }
# moonIn <- moonIndexF(moon_vec$date, lon, lat)
