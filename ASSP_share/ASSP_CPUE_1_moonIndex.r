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
library(suncalc)
library(foreach)
library(doParallel)
library(stats)
library(circular)
library(StreamMetabolism)
library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE.csv') -> metadata_raw
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites_tbl

### STANDARDIZE LOCATIONS FOR ALL SITES
## for CPUE metatdata
metadata <- metadata_raw %>% 
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
                      moonTime = sum(moonOnly) * interval / period
                      data.frame(moonTime = moonTime, moonIllum = moonIllum)
                    }
  stopCluster(cl)
  result
}

# test function
startDates = metadata$Date[1:10]
endDates = startDates + days(1)
latitude = metadata$Lat[1:10]
longitude = metadata$Long[1:10]

moon_vec = moonCalc(startDates, endDates, longitude, latitude)

# add moon vect to metadata
metadata_2 = metadata %>% 
  mutate(endDate = Date + days(1),
    moon_vec = moonCalc(Date, endDates, Long, Lat))


## calculate sunset, moon rise, and moon set
test_vec <- data.frame(startDate = startDates, endDate = endDates, lat = latitude, lon = longitude)
# sunset
test_sun <- test_vec %>% 
  sunrise.set(lat, lon, startDate, timezone = "PST8PDT", num.days = 1)
# moon rise and set
test_moon <- test_vec %>% 
  getMoonTimes(date = startDates, lat = lat, lon = lon, tz = "PST8PDT", keep = c("rise", "set")) %>% 
  # account for when moonset is sometimes following day
  if_else(set == "NA", getMoonTimes(date = endDate, lat = lat, lon = lon, tz = "PST8PDT", keep = c("rise", "set")))  

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


