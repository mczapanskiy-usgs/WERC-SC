library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)

#### WEATHER DATA
dummyTraps <- read.csv('DummyTraps.csv', stringsAsFactors = FALSE) %>%
  mutate(TrapDate = ymd(Date),
         DummyTrapDate = TrapDate) %>%
  data.table(key = c('WeatherSta', 'DummyTrapDate'))

dummyWeather <- read.csv('DummyWeather.csv', stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(date),
         DummyWeatherDate = WeatherDate) %>%
  data.table(key = c('Sta_ID', 'DummyWeatherDate'))

catchWeather <- dummyTraps[dummyWeather, roll = -6] %>%
  select(-DummyTrapDate) %>%
  filter(!is.na(catchID)) %>%
  group_by(catchID) %>%
  summarize(TotalRain = sum(Rainfall, na.rm = TRUE),
            Tmin = min(Tmin, na.rm = TRUE),
            Tmax = max(Tmax, na.rm = TRUE))

alltogether <- left_join(dummyTraps, catchWeather) %>%
  select(-DummyTrapDate)

View(alltogether)

#### LUNAR DATA
foo <- ymd_hm('2000-01-13 12:00', tz = 'US/Hawaii')
fooUTC <- with_tz(foo, 'UTC')

bar <- data.frame(t = seq(from = fooUTC - days(3), to = fooUTC, length.out = 10000)) %>%
  mutate(sunAlt = sunAngle(t, -156.1552, 20.7204)$altitude,
         moonAlt = moonAngle(t, -156.1552, 20.7204)$altitude,
         moonOnly = moonAlt > 0 | sunAlt < 0)

ggplot(bar,
       aes(x = t)) +
  geom_line(aes(y = sunAlt),
            color = 'red') +
  geom_line(aes(y = moonAlt),
            color = 'blue') +
  # geom_vline(xintercept = as.numeric(ymd_hm('2000-01-12 07:04', tz='US/Hawaii')), 
  #            color = 'red') +
  # geom_vline(xintercept = as.numeric(ymd_hm('2000-01-12 11:16', tz='US/Hawaii')), 
  #            color = 'blue') +
  geom_vline(xintercept = as.numeric(fooUTC - days(1:3)),
             color = 'black',
             linetype = 'dashed') +
  ylim(0, 90)

# Value: the time (in seconds) of moonlight without sunlight
# Always use noon (local time) for startDay and endDay
# interval in seconds
moonIndex <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
  startDay <- with_tz(startDay, 'UTC')
  endDay <- with_tz(endDay, 'UTC')
  celestial <- data.frame(t = seq(from = startDay, to = endDay, by = interval)) %>%
    mutate(sunAlt = sunAngle(t, longitude, latitude)$altitude,
           moonAlt = moonAngle(t, longitude, latitude)$altitude,
           moonIllum = moonAngle(t, longitude, latitude)$illuminatedFraction,
           moonOnly = moonAlt > 0 & sunAlt < 0) %>% 
    filter(moonOnly)
  moonTime <- nrow(celestial) * interval
  moonIllum <- mean(celestial$moonIllum)
  moonTime * moonIllum
}
