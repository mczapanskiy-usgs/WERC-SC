#### STORM-PETREL CPUE METADATA
# this script calculates sunset, moon rise and set, moon time
# created: Dec 10, 2018 by: E Kelsey
# last edited: Feb 2, 2019

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)
library(oce)
library(foreach)
library(doParallel)
library(stats)
library(suncalc)
# library(chron)
# library(rnoaa)
# library(maptools)
# library(circular)
# library(StreamMetabolism)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE_2018.csv', na.strings=c("","NA")) -> metadata_raw

metadata_raw$net_open <- times(as.numeric(metadata_raw$net_open))
convertToDateTime
        # mutate(net_open = as.character(net_open), net_close = as.character(net_close))
  #        net_open2 = as.character(net_open2), net_close2 = as.character(net_close2),
  #        net_open3 = as.character(net_open3), net_close3 = as.character(net_close3),
  #        net_open4 = as.character(net_open4), net_close4 = as.character(net_close4),
  #        net_open5 = as.character(net_open5), net_close5 = as.character(net_close5))
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_SR_SBI_CPUE_2014-2018.csv', na.strings=c("","NA")) -> metadata_raw_AD  %>% 
  # mutate(net_open = as.character(net_open), net_close = as.character(net_close), 
  #        net_open2 = as.character(net_open2), net_close2 = as.character(net_close2),
  #        net_open3 = as.character(net_open3), net_close3 = as.character(net_close3),
  #        net_open4 = as.character(net_open4), net_close4 = as.character(net_close4),
  #        net_open5 = as.character(net_open5), net_close5 = as.character(net_close5))
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites_tbl

### COMBINE ALL METADATA AND REMOVE SUPERFLUOUS COLUMNS
metadata_comb <- metadata_raw %>% 
  bind_rows(metadata_raw_AD)

### STANDARDIZE LOCATIONS FOR ALL SITES
## for CPUE metatdata
metadata <- metadata_comb %>% 
  mutate(net_open = strftime(net_open, format = "%H:%M"),
         net_close = strftime(net_close, format = "%H:%M"))
         
         Site= mosaic::derivedFactor(
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
        net_open = hms(net_open),
        net_closed = hms(net_closed),
        App_sunset_old = App_sunset,
        Moon_rise_old = Moon_rise,
        Moon_Set_old = Moon_Set,
        Moon_fract_old = Moon_fract,
        moon_time_old = moon_time,
        moon_min_old = moon_min,
        moon_index_old = moon_index,
        WS_midnight_old = WS_midnight,
        std_captured_old = std_captured,
        raw_CPUE_old = raw_CPUE, 
        std_CPUE_old = std_CPUE,
        birds_min_area_old = birds.min.area) %>% 
  select(-site, -std_ending, -std_min) %>%
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
                                    moonIllum <- mean(moon$illuminatedFraction[moonOnly])
                                    moonTime = (sum(moonOnly) * interval / period)/60
                                    moonIndex = moonTime * moonIllum
                                    data.frame(startDate = s, moonTime = moonTime, moonIllum = moonIllum, moonIndex = moonIndex)
                                  }
                stopCluster(cl)
                result
              }
# create dataframe to run moonCalc function on 
moon_vec = metadata %>% 
  transmute(startDates = Date, endDates = startDates + days(1), Lat, Long, Site = Site)
# run moonCalc function
moonI <- moonCalc(moon_vec$startDates, moon_vec$endDates, moon_vec$Lat, moon_vec$Long) 
# add dates and location back into datatable
moonIndex <- moonI %>% 
  bind_cols(moon_vec) %>% # STOP CODE AFTER THIS LINE #
  # after checking that dates line up, remove "startDate", also remove "endDates" because not needed
  select(-startDate, -endDates) %>% 
  mutate(startDates = as.Date(startDates), 
         seq = 1:n())

## calculate sunset
# create datafram to run sunset function on
sun_vec <- metadata %>% 
  transmute(date = as_date(Date), lat = Lat, lon = Long, Site = Site) 
# run sunset function
sunsetTime <- getSunlightTimes(data = sun_vec,
                           keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + hours(5) + minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon) %>% # 
  select(-date, -lat, -lon)
# make sunset dataframe by breaking up sunset time and date
Sunset = sunsetTime %>% 
  mutate_at(vars(sunset), funs("startDates" = date(.), "sunset" = as.hms(.)))
  

### add moonIndex and sunset to metadata
moonIndex_sunset <- moonIndex %>% 
  left_join(Sunset, by = c("startDates", "Lat", "Long", "Site")) %>%
  mutate(Date = date(startDates)) %>% 
  select(seq, sunset, moonIllum, moonTime, moonIndex, Date, Site, std_ending, Lat, Long)
  
metadata_SunMoon <- metadata %>% 
  mutate(Date = mdy(date)) %>%
  left_join(moonIndex_sunset, by = c("Date", "Site", "Lat", "Long")) %>% 
  

# # sunset
# test_sun <- test_vec %>% 
# sunrise.set(latitude, longitude, startDates, timezone = "PST8PDT", num.days = 1)
# # Error in sunrise.set(., latitude, longitude, startDates, timezone = "PST8PDT",  : unused argument (startDates)

# sunrise.set <- function(latitude, longitude, startDates, timezone, num.days=1){
#         #this needs to be longitude latitude#
#         lat.long <- matrix(c(longitude, latitude), nrow=1)
#         day <- as.POSIXct(startDates, tz=timezone)
#        sequence <- seq(from=day, length.out=num.days , by="days")
#         sunrise <- sunriset(lat.long, sequence, direction="sunrise", POSIXct=TRUE)
#         sunset <- sunriset(lat.long, sequence, direction="sunset", POSIXct=TRUE)
#         ss <- data.frame(sunrise, sunset)
#         ss <- ss[,-c(1,3)]
#         colnames(ss)<-c("sunrise", "sunset")
#         return(ss)
# }
# 
# sunrise_vec <- sunrise.set(startDates, longitude, latitude, timezone = "PST8PDT")

# # moon rise and set
# test_moon <- test_vec %>% 
#   getMoonTimes(date = startDates, lat = lat, lon = lon, tz = "PST8PDT", keep = c("rise", "set")) %>% 
#   # account for when moonset is sometimes following day
#   if_else(set == "NA", getMoonTimes(date = endDate, lat = lat, lon = lon, tz = "PST8PDT", keep = c("rise", "set")))  

# # trying to figure out another way to calculate sun and moon rise/set because "suncalc" function seems to be off
# moonrise <- uniroot(SunMoon, moonAngle(t, x, y, useRefraction = TRUE), interval = int, lower = 1, upper = 2)
# sun <- uniroot(t, sunAngle(t, x, y, useRefraction = TRUE)$altitude)
# moon <- moonAngle(t, x, y, useRefraction = TRUE)$altitude
# test2 <- data.frame(sun = sun, moon = moon)
# moonrise <- filter(test2, floor(moon))

# ## create dataframe site lat/long and date
# forSunMoon <- metadata %>%
#   transmute(Site = Site,
#             date = as.Date(Date),
#             lat = Lat, lon = Long)
# ## create new dataframe for moon and sun data
# Moon <- getMoonTimes(data = forSunMoon, keep = c("rise", "set"), tz = "UTC") %>%
#   mutate(moonRise = with_tz(rise, tz = "US/Pacific"),
#          moonSet = with_tz(set, tz = "US/Pacific")) # %>% select(-c(rise, set))
# # test <- suncalc.getTimes(new Date(d.getFullYear(), d.getMonth(), d.getDate(), 12, 0, 0, 0, 0), lat, long)
# 
# Sun <-  getSunlightTimes(data = forSunMoon, keep = "sunset", tz = "UTC") %>%
#   mutate(sunset = with_tz(sunset, tz = "US/Pacific")) %>%
#   select(-c(site, date, lat, lon))
# ## combine sun and moon data for each date and site
# SunMoon <- bind_cols(Moon, Sun, moonPhase) %>%
#   mutate(Date = as.POSIXct(date, tz = "US/Pacific"),
#          j_moonRise = as.numeric(moonRise),
#          j_moonSet = as.numeric(moonSet),
#          j_sunset = as.numeric(sunset))


