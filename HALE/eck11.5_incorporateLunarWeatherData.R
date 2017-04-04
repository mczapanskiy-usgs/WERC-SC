library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)

#### WEATHER DATA
trapData <- read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', 
                       stringsAsFactors = FALSE) %>%
  mutate(TrapDate = ymd(Date),
         DummyTrapDate = TrapDate) %>%
  data.table(key = c('WeatherSta', 'DummyTrapDate'))

weatherData <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv', stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(Date),
         DummyWeatherDate = WeatherDate) %>%
  data.table(key = c('Sta_ID', 'DummyWeatherDate'))

cWeekWeather <- trapData[weatherData, roll = -6] %>%
  select(-DummyTrapDate) %>%
  filter(!is.na(catchID)) %>%
  group_by(catchID) %>%
  summarize(TotalRain = sum(Rainfall, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            Tmax = mean(Tmax, na.rm = TRUE),
            relHum = mean(RelativeHumidity, na.rm = TRUE),
            soilMois = mean(SoilMoisture, na.rm = TRUE),
            solRad = mean(SolarRadiation, na.rm = TRUE))

catchWeather <- left_join(trapData, cWeekWeather) %>%
  select(-DummyTrapDate)


#### LUNAR DATA
timez <- ymd_hm('2000-01-13 12:00', tz = 'US/Hawaii')
timezUTC <- with_tz(timez, 'UTC')

bar <- data.frame(t = seq(from = timezUTC - days(3), to = timezUTC, length.out = 10000)) %>%
  mutate(sunAlt = sunAngle(t, -156.1552, 20.7204)$altitude,
         moonAlt = moonAngle(t, -156.1552, 20.7204)$altitude,
         moonOnly = moonAlt > 0 | sunAlt < 0)
# plot to validate sun and moon altitidue values
ggplot(bar,
       aes(x = t)) +
  geom_line(aes(y = sunAlt),
            color = 'red') +
  geom_line(aes(y = moonAlt),
            color = 'blue') +
  geom_vline(xintercept = as.numeric(ymd_hm('2000-01-12 07:04', tz='US/Hawaii')),
             color = 'red') +
  geom_vline(xintercept = as.numeric(ymd_hm('2000-01-12 11:16', tz='US/Hawaii')),
             color = 'blue') +
  geom_vline(xintercept = as.numeric(timezUTC - days(1:3)),
             color = 'black',
             linetype = 'dashed') +
  ylim(0, 90)

#### MOONLIGHT FUNCTION ####
# Value: the time (in seconds) of moonlight without sunlight
# NOTE: Always use noon (local time) for startDay and endDay
moonIndex <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
  startDay <- with_tz(startDay, 'UTC')
  endDay <- with_tz(endDay, 'UTC')
  mapply(FUN = function(startDay, endDay) {
    celestial <- data.frame(t = seq(from = startDay, to = endDay, by = interval)) %>%
      mutate(sunAlt = sunAngle(t, longitude, latitude)$altitude,
             moonAlt = moonAngle(t, longitude, latitude)$altitude,
             moonIllum = moonAngle(t, longitude, latitude)$illuminatedFraction,
             moonOnly = moonAlt > 0 & sunAlt < 0) %>% 
      filter(moonOnly)
    moonTime <- nrow(celestial) * interval
    moonIllum <- mean(celestial$moonIllum)
    moonTime * moonIllum
  },
  startDay, endDay)
}

# average hours of moonlight/week using moonlight function
startDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, catchWeather$Day_, 
                     hour = 12, min = 0, sec = 0, 
                     tz = 'US/Hawaii') - days(6) 
endDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, catchWeather$Day_, 
                  hour = 12, min = 0, sec = 0, 
                  tz = 'US/Hawaii')

catchLunarWeather <- catchWeather %>% 
  mutate(startDay = as.POSIXct(startDay, format = '%Y-%m-%d %H:%M:%S'),
         endDay = as.POSIXct(endDay, format = '%Y-%m-%d %H:%M:%S'), 
         moonIndex = moonIndex(startDay, endDay))

# catchLunarWeather <- catchWeather2 %>% 
#   mutate(moonIndex = moonIndex(startDay, endDay))



#### RUN CPUE ANALYSIS ON SPATIAL DATA WITH WEATHER AND LUNAR DATA
## if there are multiple predEvents in a week, choose the most important one
## make "weeklyCatches" a datatable containing all the factors needed for weekly analysis
weeklyCatchesLW <- catchLunarWeather %>%
  group_by(Trapline, TrapNum, Year, Week) %>%
  filter(predEvent == min(predEvent)) %>%
  slice(1) %>%
  ungroup

## count the number of Trapline events (weeklyCatches) and number of each predEvent per week (aka number of traps in the trapline)
trapsPerLineWeek_sLW <- weeklyCatches_sLW %>%
  group_by(Trapline, Year, Season, Month, Week) %>%
  summarize(NTraps = n()) %>% 
  ungroup

# count number of each predEvent per week per trapline
predEventsPerLineWeek <- weeklyCatches %>%
  group_by(Trapline, Year, Week, Season, Month, predEvent) %>%
  summarise(NEvents = n()) %>% 
  ungroup

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE <- merge(trapsPerLineWeek, predEventsPerLineWeek) %>%
  mutate(CPUE = NEvents/NTraps) %>%
  arrange(Trapline, Year, Week, predEvent)

# trapsPerLineWeek <- weeklyCatchesLW %>%
#   group_by(Trapline, Year, Season, Month, Week) %>%
#   summarize(NTraps = n())
# 
# # count number of each predEvent per week per trapline
# predEventsPerLineWeek <- weeklyCatches %>% 
#   group_by(Trapline, Year, Week, Season, Month, predEvent) %>% 
#   summarise(NEvents = n()) 
# 
# ## number of predEvents per number of traps, for each week on each Trapline
# predEventPUE <- merge(trapsPerLineWeek, predEventsPerLineWeek) %>% 
#   mutate(CPUE = NEvents/NTraps) %>% 
#   arrange(Trapline, Year, Week, predEvent) 
# 
# ## from predEventPUE, need to add back in all predEvents that have a CPUE of 0
# # identify what variables should be consistent for each week
# varFill <- group_by(predEventPUE, Trapline, Week) %>% 
#   summarise(Year = first(Year), 
#             Season = first(Season),
#             Month = first(Month),
#             NTraps = first(NTraps))
# # make a grid of each trapline, week, predEvent
# CPUEgrid <- expand.grid(Trapline = unique(predEventPUE$Trapline), Week = unique(predEventPUE$Week), predEvent = unique(predEventPUE$predEvent)) %>% 
#   mutate(dummyPUE = 0) %>% 
#   merge(predEventPUE, all.x = TRUE) %>% 
#   merge(varFill, all.x = TRUE, by = c('Trapline', 'Week')) %>% 
#   arrange(Trapline, Week, predEvent) %>% 
#   select(Trapline:predEvent, Year = Year.y, Season = Season.y, Month = Month.y, NTraps = NTraps.y, NEvents) %>% 
#   mutate(NEvents = ifelse(is.na(NEvents), 0, NEvents), 
#          CPUE = NEvents/NTraps)
# 
# ## save to GitHub folder
# # predEventPUE data file
# write.csv(predEventPUE, file = '~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
#           row.names = FALSE) 
# # CPUEgrid data file
# write.csv(CPUEgrid, file = '~/WERC-SC/HALE/TraplinePredEventPUE_zeros_11_20161209.csv',
#           row.names = FALSE) 
# # weekly catch for each trap, with season code
# write.csv(weeklyCatches, file = '~/WERC-SC/HALE/catch_11_traploc_baitTypes_predEvent_weeklyCatches_20161209.csv',
#           row.names = FALSE) 
# # final datasheets for spatial analysis
# spatialCatch <- catch %>% 
#   filter(TrapChecked == "TRUE") %>% 
#   select(catchID:Comments, baitType:Season)
# write.csv(spatialCatch, file = '~/WERC-SC/HALE/catch_11_spatialCatches_20161209.csv',
#           row.names = FALSE) 

