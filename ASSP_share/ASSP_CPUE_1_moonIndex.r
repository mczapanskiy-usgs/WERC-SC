#### STORM-PETREL CPUE METADATA
# this script calculates moon phase, net time, and CPUE
# created: Dec 10, 2018 E Kelsey

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)
library(oce)
library(suncalc)
library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE.csv') -> metadata
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites

### STANDARDIZE LOCATIONS FOR ALL SITES
metadata <- metadata %>% 
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
  left_join(sites, by = c("Site" = "Site", "island" = "Island"))  # add latitude and longitude to metadata

### CALCULATE MOON INDEX  
metadata2 <- metadata %>% 
  mutate(App_sunset_2 = getSunlightTimes(date = Date, lat = Lat, long = Long, keep = "sunset", tz = "US/Pacific")),
         Moon_rise_2 = getMoonTimes(date = Date, lat = lat, long = long, keep = c("rise"), tz = "US/Pacific"),
         Moon_Set_2 = getMoonTimes(date = Date, lat = lat, long = long, keep = c("set"), tz = "US/Pacific"), 
         Moon_fract_2 = moonAngle(Date, lat = lat, long = long, tz = "US/Pacific", phase()),
         moon_time_2 = ,
         moon_index_2 = ,
         WS_midnight_2 = ))

# moonIndex4 <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
#   startDay <- with_tz(startDay, 'UTC')
#   endDay <- with_tz(endDay, 'UTC')
#   interval <- first(interval)
#   cl <- makeCluster(detectCores())
#   registerDoParallel(cl)
#   result <- foreach(s = startDay, e = endDay, 
#                     .combine = rbind, 
#                     .packages = c('oce')) %dopar% {
#                       t <- seq(from = s, to = e, by = interval)
#                       period = as.numeric(difftime(e, s, units = 'days'))
#                       sunAlt <- sunAngle(t, longitude, latitude)$altitude
#                       moon <- moonAngle(t, longitude, latitude)
#                       moonAlt <- moon$altitude
#                       moonOnly <- moonAlt > 0 & sunAlt < 0
#                       moonIllum <- mean(moon$illuminatedFraction[moonOnly])
#                       moonTime = sum(moonOnly) * interval / period
#                       data.frame(moonTime = moonTime, moonIllum = moonIllum)
#                     }
#   stopCluster(cl)
#   result
# }
# 
# endDates = seq(from = ymd_hm('2000-01-01 12:00', tz = 'US/Hawaii'),
#                to = ymd_hm('2014-12-31 12:00', tz = 'US/Hawaii'),
#                by = '1 days')
# startDates1dy = endDates - days(1)
# startDates1wk = endDates - days(6)
# startDates2wk = endDates - days(13)
# moonIndex1dy = moonIndex4(startDates1dy, endDates)
# moonIndex1wk = moonIndex4(startDates1wk, endDates)
# moonIndex2wk = moonIndex4(startDates2wk, endDates)
# 
# result <- data.frame(PeriodEnding = endDates,
#                      MoonTime1dy = moonIndex1dy$moonTime,
#                      MoonIllum1dy = moonIndex1dy$moonIllum,
#                      MoonTime1wk = moonIndex1wk$moonTime,
#                      MoonIllum1wk = moonIndex1wk$moonIllum,
#                      MoonTime2wk = moonIndex2wk$moonTime,
#                      MoonIllum2wk = moonIndex2wk$moonIllum)
# 
# write.csv(result, file = '~/WERC-SC/HALE/catch_11.5_moonIndex.csv', row.names = FALSE) # 'WERC-SC/HALE/MoonIndex_v2.csv'
  
#### CPUE
## only birds caught before cutoff point
## Sum spp counts
# divided by species
#
