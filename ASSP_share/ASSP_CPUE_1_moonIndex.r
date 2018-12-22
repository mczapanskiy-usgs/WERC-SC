#### STORM-PETREL CPUE METADATA
# this script calculates moon phase, net time, and CPUE
# created: Dec 10, 2018 E Kelsey
# last edited: 

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
# library(suncalc)
library(circular)
# library(rnoaa)

### READ IN BANDING CPUE DATA
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_CPUE.csv') -> metadata_raw
read.csv('~/WERC-SC/ASSP_share/mistnet_sites_rev.csv') %>% 
  select(-Notes) -> sites
read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_catches.csv') -> catches_raw


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
  left_join(sites, by = c("Site" = "Site", "island" = "Island"))  

write.csv(metadata, file = '~/WERC-SC/ASSP_share/metadata.csv', row.names = FALSE) # 'WERC-SC/ASSP_share/metadata.csv'

# for catch data
catches <- catches_raw %>% 
  select(date, island, site, capture.time, species, recapture., Notes) %>% 
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
  select(-date, -site)

### MAKE SUN AND MOON DATAFRAME
## FUNCTIONS (from MFC "moonIndex_v2.R")
SunMoon <- function(startDay, endDay, longitude, latitude, interval = 60) { 
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

startDates = metadata$Date[1:10]
endDates = startDates + days(1)
latitude = metadata$Lat[1:10]
longitude = metadata$Long[1:10]
sunMoon_vec = SunMoon(startDates, endDates, longitude, latitude)


result <- data.frame(PeriodEnding = endDates,
                     MoonTime1dy = moonIndex1dy$moonTime,
                     MoonIllum1dy = moonIndex1dy$moonIllum,
                     MoonTime1wk = moonIndex1wk$moonTime,
                     MoonIllum1wk = moonIndex1wk$moonIllum,
                     MoonTime2wk = moonIndex2wk$moonTime,
                     MoonIllum2wk = moonIndex2wk$moonIllum)



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
# Moon <- getMoonTimes(data = forSunMoon, keep = c("rise", "set"), tz = "UTC") %>%
#   mutate(moonRise = with_tz(rise, tz = "US/Pacific"),
#          moonSet = with_tz(set, tz = "US/Pacific")) # %>% select(-c(rise, set))
# moonPhase <- getMoonIllumination(date = metadata$Date, keep = c("fraction", "phase")) %>%
#    select(-date)
# Sun <-  getSunlightTimes(data = forSunMoon, keep = "sunset", tz = "UTC") %>%
#   mutate(sunset = with_tz(sunset, tz = "US/Pacific")) %>%
#   select(-c(site, date, lat, lon))
# ## combine sun and moon data for each date and site
# SunMoon <- bind_cols(Moon, Sun, moonPhase) %>%
#   mutate(Date = as.POSIXct(date, tz = "US/Pacific"),
#          j_moonRise = as.numeric(moonRise),
#          j_moonSet = as.numeric(moonSet),
#          j_sunset = as.numeric(sunset))


### CPUE
## number of catches for each species and night
catches_t <- catches %>% 
   mutate(capture.time2 = hm(capture.time),
          capture_DateTime = as.POSIXct(paste(Date, capture.time2), format="%Y-%m-%d %H:%M:%S"))

# catches_sum <- catches %>% 
#   group_by(Date, Site) %>% 
#   count(species)
  
  
## only birds caught before cutoff point
## Sum spp counts
# divided by species

