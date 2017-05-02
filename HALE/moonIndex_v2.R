library(dplyr)
library(oce)
library(lubridate)
library(foreach)
library(doParallel)

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

moonIndex2 <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
  startDay <- with_tz(startDay, 'UTC')
  endDay <- with_tz(endDay, 'UTC')
  interval <- first(interval)
  foreach(s = startDay, e = endDay, .combine = c) %do% {
    t <- seq(from = s, to = e, by = interval)
    sunAlt <- sunAngle(t, longitude, latitude)$altitude
    moon <- moonAngle(t, longitude, latitude)
    moonAlt <- moon$altitude
    moonOnly <- moonAlt > 0 & sunAlt < 0
    moonIllum <- mean(moon$illuminatedFraction[moonOnly])
    sum(moonOnly) * interval * moonIllum
  }
}

moonIndex3 <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
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

moonIndex4 <- function(startDay, endDay, interval = 60, longitude = -156.1552, latitude = 20.7204) {
  startDay <- with_tz(startDay, 'UTC')
  endDay <- with_tz(endDay, 'UTC')
  interval <- first(interval)
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  result <- foreach(s = startDay, e = endDay, 
                    .combine = rbind, 
                    .packages = c('oce')) %dopar% {
    t <- seq(from = s, to = e, by = interval)
    period = as.numeric(difftime(e, s, units = 'days'))
    sunAlt <- sunAngle(t, longitude, latitude)$altitude
    moon <- moonAngle(t, longitude, latitude)
    moonAlt <- moon$altitude
    moonOnly <- moonAlt > 0 & sunAlt < 0
    moonIllum <- mean(moon$illuminatedFraction[moonOnly])
    moonTime = sum(moonOnly) * interval / period
    data.frame(moonTime = moonTime, moonIllum = moonIllum)
  }
  stopCluster(cl)
  result
}

endDates = seq(from = ymd_hm('2000-01-01 12:00', tz = 'US/Hawaii'),
               to = ymd_hm('2014-12-31 12:00', tz = 'US/Hawaii'),
               by = '1 days')
startDates1dy = endDates - days(1)
startDates1wk = endDates - days(6)
startDates2wk = endDates - days(13)
moonIndex1dy = moonIndex4(startDates1dy, endDates)
moonIndex1wk = moonIndex4(startDates1wk, endDates)
moonIndex2wk = moonIndex4(startDates2wk, endDates)

result <- data.frame(PeriodEnding = endDates,
                     MoonTime1dy = moonIndex1dy$moonTime,
                     MoonIllum1dy = moonIndex1dy$moonIllum,
                     MoonTime1wk = moonIndex1wk$moonTime,
                     MoonIllum1wk = moonIndex1wk$moonIllum,
                     MoonTime2wk = moonIndex2wk$moonTime,
                     MoonIllum2wk = moonIndex2wk$moonIllum)

write.csv(result, 'WERC-SC/HALE/MoonIndex_v2.csv', row.names = FALSE)

catch <- read.csv('catchLunarWeather_slice.csv', stringsAsFactors = FALSE) %>%
  mutate(startDay = ymd_hms(startDay, tz = 'US/Hawaii'),
         endDay = ymd_hms(endDay, tz = 'US/Hawaii')) %>%
  slice(1:500)

ptm1 <- proc.time()
catch1 <- mutate(catch, moonIndex = moonIndex(startDay, endDay))
ptm1 <- proc.time() - ptm1
ptm1

ptm2 <- proc.time()
catch2 <- mutate(catch, moonIndex = moonIndex2(startDay, endDay))
ptm2 <- proc.time() - ptm2
ptm2

ptm3 <- proc.time()
catch3 <- mutate(catch, moonIndex = moonIndex3(startDay, endDay))
ptm3 <- proc.time() - ptm3
ptm3