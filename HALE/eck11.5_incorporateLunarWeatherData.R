library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)
library(foreach)
library(doParallel)
library(zoo)


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
# load weather data
weatherData <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv', 
                        stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(Date, tz = 'US/Hawaii'))

weatherData.dt <- weatherData %>% 
  mutate(DummyWeatherDate = WeatherDate) %>%
  data.table(key = c('Sta_ID', 'DummyWeatherDate'))

cWeekWeather <- trapData[weatherData.dt, roll = -6] %>%
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

catchLunarWeather <- catchWeather %>% 
  mutate(startDay = as.POSIXct(startDay, format = '%Y-%m-%d %H:%M:%S'),
         endDay = as.POSIXct(endDay, format = '%Y-%m-%d %H:%M:%S')) %>%
  mutate(moonIndex = moonIndex(startDay, endDay))


#### RUN CPUE ANALYSIS ON SPATIAL DATA WITH WEATHER AND LUNAR DATA
# load weather data
weatherIndex <- weatherData %>%
  group_by(Sta_ID) %>%
  mutate(total3monRain = rollapply(Rainfall, 89, sum, na.rm =TRUE, fill = NA, align = 'right'), # wet season (Oct-Mar) and dry season (Apr-Sept) last 6 months, half of that (KRUSHELNYCKY, et al. 2013)
         totalWeekRain = rollapply(Rainfall, 7, sum, na.rm =TRUE, fill = NA, align = 'right'),
         meanRelHum = rollapply(RelativeHumidity, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanSoilMois = rollapply(SoilMoisture, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanSolRad = rollapply(SolarRadiation, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanTmax = rollapply(Tmax, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanTmin = rollapply(Tmin, 7, mean, na.rm = TRUE, fill = NA, align = 'right'))
  
# # test of data is being pulled from the right weather station for each trapline
# trapData_WeatherSta <- trapData %>% 
#   select(Trapline, TrapNum, Year_, Month_, Day_, predEvent, Week, Season, WeatherSta) %>% 
#   mutate(PeriodEnding = ISOdatetime(Year_, Month_, Day_, 12, 0, 0, tz = 'US/Hawaii')) %>% 
#   group_by(Trapline, PeriodEnding) %>% 
#   dplyr::summarise(WeatherSta = Mode(WeatherSta))

# load lunar data
moonIndex <- read.csv('~/WERC-SC/HALE/catch_11.5_moonIndex.csv', 
                      stringsAsFactors = FALSE) %>% 
  mutate(PeriodEnding = ymd_hms(PeriodEnding, tz = 'US/Hawaii'))
## if there are multiple predEvents in a week, choose the most important one
# make "weeklyCatches" a datatable containing all the factors needed for weekly analysis
weeklyCatches_WL <- trapData %>%
  group_by(Trapline, TrapNum, Year_, Week) %>%
  filter(predEvent == min(predEvent)) %>%
  slice(1) %>%
  ungroup

# count the number of Trapline events (weeklyCatches) and number of each predEvent per week (aka number of traps in the trapline)
trapsPerLineWeek_WL <- weeklyCatches_WL %>%
  group_by(Trapline, Year_, Season, Month_, Week) %>%
  dplyr::summarize(NTraps = n()) # 

# count number of each predEvent per week per trapline
predEventsPerLineWeek_WL <- weeklyCatches_WL %>%
  group_by(Trapline, Year_, Week, Season, Month_, predEvent) %>%
  dplyr::summarise(NEvents = n(),
                   WeatherSta = Mode(WeatherSta)) 

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE_WL <- merge(trapsPerLineWeek_WL, predEventsPerLineWeek_WL) %>%
  mutate(CPUE = NEvents/NTraps,
         PeriodEnding = ISOdatetime(Year_, 1, 1, 12, 0, 0, 'US/Hawaii') + weeks(Week)) %>% 
  arrange(Trapline, Year_, Week, predEvent) %>% 
  left_join(select(moonIndex, PeriodEnding, MoonTime1wk, MoonIllum1wk), by = 'PeriodEnding') %>% 
  left_join(select(weatherIndex, WeatherDate, Sta_ID, total3monRain, totalWeekRain, meanRelHum, meanSoilMois, meanSolRad, meanTmin, meanTmax), 
                   by = c('PeriodEnding' = 'WeatherDate', 'WeatherSta' = 'Sta_ID'))


## save predEventPUE_WL data file to GitHub folder
write.csv(predEventPUE_WL, file = '~/WERC-SC/HALE/predEventPUE_climate_11.5.csv',
          row.names = FALSE)




