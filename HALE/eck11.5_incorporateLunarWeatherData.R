# THIS SCRIPT PULLS IN WEATHER DATA (from HaleNet) AND LUNAR DATA (from Max's "moonIndex_v2" script),
# SUMMARIZED WEATHER AND LUNAR DATA BY WEEK,
# AND TIES IT TO CATCH DATA

library(dplyr)
library(data.table)
library(lubridate)
library(oce)
library(ggplot2)
library(foreach)
library(doParallel)
library(zoo)

setwd("~/WERC-SC/HALE")

## FUNCTION TO GET THE STATISTICAL MODE
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# load weather data
weatherData <- read.csv('~/WERC-SC/HALE/haleNet_data_9.csv',
                        stringsAsFactors = FALSE) %>%
  mutate(WeatherDate = ymd(Date, tz = 'US/Hawaii'))
weatherIndex <- weatherData %>%
  group_by(Sta_ID) %>%
  mutate(total3monRain = rollapply(Rainfall, 89, sum, na.rm =TRUE, fill = NA, align = 'right'), # wet season (Oct-Mar) and dry season (Apr-Sept) last 6 months, half of that (KRUSHELNYCKY, et al. 2013)
         totalWeekRain = rollapply(Rainfall, 7, sum, na.rm =TRUE, fill = NA, align = 'right'),
         meanRelHum = rollapply(RelativeHumidity, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanSoilMois = rollapply(SoilMoisture, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanSolRad = rollapply(SolarRadiation, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanTmax = rollapply(Tmax, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         meanTmin = rollapply(Tmin, 7, mean, na.rm = TRUE, fill = NA, align = 'right'),
         month = month(WeatherDate), day = day(WeatherDate),
         WeatherDate = ISOdatetime(Year, month, day, 12, 0, 0,'US/Hawaii'))

# load lunar data
moonIndex <- read.csv('~/WERC-SC/HALE/catch_11.5_moonIndex.csv', 
                      stringsAsFactors = FALSE) %>% 
  mutate(PeriodEnding = ymd_hms(PeriodEnding, tz = 'US/Hawaii'),
         MoonTime1wk = (MoonTime1wk/3600),
         MoonTime2wk = (MoonTime2wk/3600))

# load spatial data
trapData <- read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', 
                     stringsAsFactors = FALSE) %>% 
  select(-OBJECTID) %>% 
  mutate(TrapDate = lubridate::ymd(Date),
         DummyTrapDate = TrapDate) %>%
  data.table(key = c('WeatherSta', 'DummyTrapDate'))


#### RUN CPUE ANALYSIS ON SPATIAL DATA WITH WEATHER AND LUNAR DATA

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
  dplyr::summarize(NTraps = n()) 
# count number of each predEvent per week per trapline
predEventsPerLineWeek_WL <- weeklyCatches_WL %>%
  group_by(Trapline, Year_, Week, Season, Month_, predEvent) %>%
  dplyr::summarise(NEvents = n(),
                   WeatherSta = Mode(WeatherSta)) 

## number of predEvents per number of traps, for each week on each Trapline
# THEN JOIN WITH WEATHER AND LUNAR DATA
predEventPUE_WL <- merge(trapsPerLineWeek_WL, predEventsPerLineWeek_WL) %>%
  mutate(CPUE = NEvents/NTraps,
         PeriodEnding = ISOdatetime(2000, 1, 1, 12, 0, 0, 'US/Hawaii') + weeks(Week)) %>% 
  arrange(Trapline, Year_, Week, predEvent) %>% 
  left_join(select(moonIndex, PeriodEnding, MoonTime1wk, MoonIllum1wk), by = 'PeriodEnding') %>% 
  left_join(select(weatherIndex, WeatherDate, Sta_ID, total3monRain, totalWeekRain, meanRelHum, meanSoilMois, meanSolRad, meanTmin, meanTmax), 
                   by = c('PeriodEnding' = 'WeatherDate', 'WeatherSta' = 'Sta_ID'))

## save predEventPUE_WL data file to GitHub folder
write.csv(predEventPUE_WL, file = '~/WERC-SC/HALE/predEventPUE_WL_11.5.csv',
          row.names = FALSE)