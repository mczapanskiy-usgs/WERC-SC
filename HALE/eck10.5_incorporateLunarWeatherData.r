library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)

#### WEATHER DATA
trapData <- read.csv('~/WERC-SC/HALE/catch_10.5_spatialCatches_20170109.csv', 
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

startDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, (catchWeather$Day_ - 7), hour = 12, min = 0, sec = 0, tz = 'US/Hawaii')
endDay <- ISOdate(catchWeather$Year_, catchWeather$Month_, catchWeather$Day_, hour = 12, min = 0, sec = 0, tz = 'US/Hawaii')

catchWeather2 <- catchWeather %>% 
  mutate(startDay = as.POSIXct(startDay, format = '%Y-%m-%d %H:%M:%S'),
         endDay = as.POSIXct(endDay, format = '%Y-%m-%d %H:%M:%S')) %>% 
  filter(!is.na(startDay),
         !is.na(endDay))

catchLunarWeather <- catchWeather2 %>% 
  mutate(moonIndex = moonIndex(startDay, endDay))

