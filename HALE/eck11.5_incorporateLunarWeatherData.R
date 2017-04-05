library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)
library(foreach)
library(doParallel)

#### FUNCTION TO GET THE STATISTICAL MODE
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# load spatial data
trapData <- read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', 
                       stringsAsFactors = FALSE) %>% 
  select(-OBJECTID) %>% 
  mutate(TrapDate = lubridate::ymd(Date),
       DummyTrapDate = TrapDate) %>%
  data.table(key = c('WeatherSta', 'DummyTrapDate'))


#### WEATHER DATA ANALYSIS
# trapData2 <- trapData %>%
#   mutate(TrapDate = lubridate::ymd(Date),
#          DummyTrapDate = TrapDate) %>%
#   data.table(key = c('WeatherSta', 'DummyTrapDate'))
# load weather data
weatherData <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv', 
                        stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(Date),
         DummyWeatherDate = WeatherDate) %>%
  data.table(key = c('Sta_ID', 'DummyWeatherDate'))

cWeekWeather <- trapData2[weatherData, roll = -6] %>%
  select(-DummyTrapDate) %>%
  filter(!is.na(catchID)) %>%
  group_by(catchID) %>%
  summarize(TotalRain = sum(Rainfall, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            Tmax = mean(Tmax, na.rm = TRUE),
            relHum = mean(RelativeHumidity, na.rm = TRUE),
            soilMois = mean(SoilMoisture, na.rm = TRUE),
            solRad = mean(SolarRadiation, na.rm = TRUE))

catchWeather <- left_join(trapData2, cWeekWeather) %>%
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
  interval <- first(interval)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  result <- foreach(s = startDay, e = endDay, 
                    .combine = c, 
                    .packages = c('oce')) %dopar% {
                      t <- seq(from = s, to = e, by = interval)
                      sunAlt <- sunAngle(t, longitude, latitude)$altitude
                      moon <- moonAngle(t, longitude, latitude)
                      moonAlt <- moon$altitude
                      moonOnly <- moonAlt > 0 & sunAlt < 0
                      moonIllum <- mean(moon$illuminatedFraction[moonOnly])
                      sum(moonOnly) * interval * moonIllum
                    }
  stopCluster(cl)
  result
}

# average hours of moonlight/week using moonlight function
startDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, catchWeather$Day_, 
                     hour = 12, min = 0, sec = 0, 
                     tz = 'US/Hawaii') - days(6) 
endDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, catchWeather$Day_, 
                  hour = 12, min = 0, sec = 0, 
                  tz = 'US/Hawaii')

catchWeather2 <- catchWeather %>% 
  mutate(startDay = as.POSIXct(startDay, format = '%Y-%m-%d %H:%M:%S'),
         endDay = as.POSIXct(endDay, format = '%Y-%m-%d %H:%M:%S')) %>% 
  slice(1:500)

catchLunarWeather <- catchWeather2 %>%
  mutate(moonIndex = moonIndex(startDay, endDay))

ptm1 <- proc.time()
catchLunarWeather <- mutate(catchWeather2, moonIndex = moonIndex(startDay, endDay))
ptm1 <- proc.time() - ptm1
ptm1

# # final datasheets for spatial analysis
# spatialCatch <- catch %>% 
#   filter(TrapChecked == "TRUE") %>% 
#   select(catchID:Comments, baitType:Season)
# write.csv(spatialCatch, file = '~/WERC-SC/HALE/catch_11_spatialCatches_20161209.csv',
#           row.names = FALSE) 

#### RUN CPUE ANALYSIS ON SPATIAL DATA WITH WEATHER AND LUNAR DATA
## if there are multiple predEvents in a week, choose the most important one
## make "weeklyCatches" a datatable containing all the factors needed for weekly analysis
weeklyCatches_s <- trapData %>%
  group_by(Trapline, TrapNum, Year_, Week) %>%
  filter(predEvent == min(predEvent)) %>%
  slice(1) %>%
  ungroup

## count the number of Trapline events (weeklyCatches) and number of each predEvent per week (aka number of traps in the trapline)
trapsPerLineWeek_s <- weeklyCatches_s %>%
  group_by(Trapline, Year_, Season, Month_, Week) %>%
  dplyr::summarize(NTraps = n(),
            endDay = max(Date),
            startDay = endDay - days(6)) %>% 
  transmute(startDay = ymd(startDay))

# count number of each predEvent per week per trapline
predEventsPerLineWeek_s <- weeklyCatches_s %>%
  group_by(Trapline, Year_, Week, Season, Month_, predEvent) %>%
  dplyr::summarise(NEvents = n(),
                   WeatherSta = Mode(WeatherSta)) 

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE_spatial <- merge(trapsPerLineWeek_s, predEventsPerLineWeek_s) %>%
  mutate(CPUE = NEvents/NTraps,
         cpueID = paste(Trapline, Week,predEvent, sep = "_")) %>% 
  arrange(Trapline, Year_, Week, predEvent)

cpueWeekWeather <- predEventPUE_spatial[weatherData, roll = -6] %>%
  filter(!is.na(cpueID)) %>%
  group_by(cpueID) %>%
  summarize(TotalRain = sum(Rainfall, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            Tmax = mean(Tmax, na.rm = TRUE),
            relHum = mean(RelativeHumidity, na.rm = TRUE),
            soilMois = mean(SoilMoisture, na.rm = TRUE),
            solRad = mean(SolarRadiation, na.rm = TRUE))

predEventPUE_spatial_weather <- left_join(predEventPUE_spatial, cWeekWeather) %>%
  select(-DummyTrapDate)

# 
# ## save to GitHub folder
# # predEventPUE data file
# write.csv(predEventPUE, file = '~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
#           row.names = FALSE) 
# # weekly catch for each trap, with season code
# write.csv(weeklyCatches, file = '~/WERC-SC/HALE/catch_11_traploc_baitTypes_predEvent_weeklyCatches_20161209.csv',
#           row.names = FALSE) 



