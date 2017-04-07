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
# load weather data
weatherData <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv', 
                        stringsAsFactors = FALSE) %>%
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
weatherIndex <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv', 
                         stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(Date),
         Week = as.numeric(as.period(lubridate::interval(min(WeatherDate), WeatherDate) %/% weeks(1)))) %>% 
  group_by(Year, Week, Sta_ID) %>%
  summarize(TotalRain = sum(Rainfall, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            Tmax = mean(Tmax, na.rm = TRUE),
            relHum = mean(RelativeHumidity, na.rm = TRUE),
            soilMois = mean(SoilMoisture, na.rm = TRUE),
            solRad = mean(SolarRadiation, na.rm = TRUE)) %>% 
  mutate(PeriodEnding = ISOdatetime(Year, 1, 1, 12, 0, 0, tz = 'US/Hawaii') + weeks(Week))
  
  # select(Trapline, TrapNum, Year_, Month_, Day_, predEvent, Week, Season, WeatherSta, TotalRain:solRad) %>% 
  # mutate(PeriodEnding = ISOdatetime(Year_, Month_, Day_, 12, 0, 0, tz = 'US/Hawaii')) %>% 
  # group_by(Trapline, PeriodEnding) %>%
  # dplyr::summarise(TotalRain = Mode(TotalRain),
  #                  Tmin = Mode(Tmin),
  #                  Tmax = Mode(Tmax),
  #                  relHum = Mode(relHum),
  #                  soilMois = Mode(soilMois),
  #                  solRad = Mode(solRad)) %>% 
  # ungroup

trapData_WeatherSta <- trapData %>% 
  select(Trapline, TrapNum, Year_, Month_, Day_, predEvent, Week, Season, WeatherSta) %>% 
  mutate(PeriodEnding = ISOdatetime(Year_, Month_, Day_, 12, 0, 0, tz = 'US/Hawaii')) %>% 
  group_by(Trapline, PeriodEnding) %>% 
  dplyr::summarise(WeatherSta = Mode(WeatherSta))

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
  # transmute(Year_ = as.numeric(Year_)) %>% 
  mutate(CPUE = NEvents/NTraps,
         # cpueID = paste(Trapline, Week, predEvent, sep = "_"),
         PeriodEnding = ISOdatetime(Year_, 1, 1, 12, 0, 0, 'US/Hawaii') + weeks(Week)) %>% 
  arrange(Trapline, Year_, Week, predEvent) %>% 
  left_join(select(moonIndex, PeriodEnding, MoonTime1wk, MoonIllum1wk), by = 'PeriodEnding') %>% 
  left_join(select(weatherIndex, TotalRain, Tmin, Tmax, relHum, soilMois, solRad), 
                   by = c('PeriodEnding' = 'PeriodEnding', 'WeatherSta' = 'Sta_ID'))


# 
# ## save to GitHub folder
# # predEventPUE data file
# write.csv(predEventPUE, file = '~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
#           row.names = FALSE) 
# # weekly catch for each trap, with season code
# write.csv(weeklyCatches, file = '~/WERC-SC/HALE/catch_11_traploc_baitTypes_predEvent_weeklyCatches_20161209.csv', row.names = FALSE) 



