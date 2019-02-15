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

# library(hms)
# library(rnoaa)
# library(maptools)
# library(circular)
# library(StreamMetabolism)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE_2018.csv') %>% 
  mutate(net_open = as.character(net.open),
         net_close = as.character(net.close)) %>% 
  select(-net.open, -net.close)-> metadata_raw
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_SR_SBI_CPUE_2014-2018.csv') %>% 
  mutate(net_open = as.character(net.open),
         net_close = as.character(net.close)) %>% 
  select(-net.open, -net.close) -> metadata_raw_AD
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
    Date = mdy(date, tz = "US/Pacific")) %>% 
  select(-date, -site) %>%
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
  transmute(startDates = Date, endDates = startDates + days(1), latitude = Lat, longitude = Long)
# run moonCalc function
moonI <- moonCalc(moon_vec$startDates, moon_vec$endDates, moon_vec$latitude, moon_vec$longitude) 
# add dates and location back into datatable
moonIndex <- moonI %>% 
  bind_cols(moon_vec) %>% # STOP CODE AFTER THIS LINE #
  # after checking that dates line up, remove "startDate", also remove "endDates" because not needed
  select(-startDate, -endDates) %>% 
  mutate(startDates = as.Date(startDates))

## calculate sunset
# create datafram to run sunset function on
sun_vec <- metadata %>% 
  transmute(date = as_date(Date), lat = Lat, lon = Long) 
# run sunset function
sunsetTime <- getSunlightTimes(data = sun_vec,
                           keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + hours(5) + minutes(18)) # 5.3 hours after sunset
# make sunset dataframe by breaking up sunset time and date
sunset = sunsetTime %>% 
  mutate_at(vars(sunset), funs("startDates" = date(.), "sunset" = as.hms(.)))
  

### add moonIndex and sunset to metadata
moonIndex_sunset <- moonIndex %>% 
  left_join(sunset, by = "startDates") %>%
  arrange(sunset, moonIllum, moonTime, moonIndex, startDates, std_ending, latitude, longitude)


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


