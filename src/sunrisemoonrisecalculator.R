library(oce)
library(lubridate)

lat <- 20.7204
lon <- -156.1552
utc <- ymd_hms('2016-11-09 12:00:00', tz = 'UTC')


plot(function(t) moonAngle(t, lon, lat)$altitude, 
     from = ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), #noon in Hawaii
     to = ymd_hms('2016-11-10 22:00:00', tz = 'UTC'),
     col = 'blue',
     ylim = c(-90, 90),
     axes = FALSE,
     main = 'Moon angle at Haleakala Nov. 9-10',
     xlab = 'Time',
     ylab = 'Moon angle')
abline(h = 0, lty = 2)
# Labels in Hawaii time
axis(1, 
     at = seq(from = ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), 
              to = ymd_hms('2016-11-10 22:00:00', tz = 'UTC'),
              by = '4 hours'),
     labels = format(seq(from = ymd_hms('2016-11-09 12:00:00', tz = 'UTC'), 
                         to = ymd_hms('2016-11-10 12:00:00', tz = 'UTC'),
                         by = '4 hours'), 
                     '%m/%d %H:%M'))
axis(2, at = c(-90, -45, 0, 45, 90))

moonrise <- optimize(function(t) {
    t <- as.POSIXct(t, origin = ymd_hms('1970-01-01 00:00.00', tz = 'UTC'), tz = 'UTC') 
    moonAngle(t, lon, lat)$altitude
  },
  as.numeric(c(ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), 
               ymd_hms('2016-11-10 22:00:00', tz = 'UTC'))))$minimum

abline(v = moonrise, col = 'blue')

moonset <- optimize(function(t) {
  t <- as.POSIXct(t, origin = ymd_hms('1970-01-01 00:00.00', tz = 'UTC'), tz = 'UTC') 
  moonAngle(t, lon, lat)$altitude
},
as.numeric(c(ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), 
             ymd_hms('2016-11-10 22:00:00', tz = 'UTC'))))$minimum

abline(v = moonset, col = 'blue')

sunrise <- optimize(function(t) {
  t <- as.POSIXct(t, origin = ymd_hms('1970-01-01 00:00.00', tz = 'UTC'), tz = 'UTC') 
  -sunAngle(t, lon, lat)$altitude
},
as.numeric(c(ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), 
             ymd_hms('2016-11-10 22:00:00', tz = 'UTC'))))$minimum

abline(v = sunrise, col = 'red')

sunset <- optimize(function(t) {
  t <- as.POSIXct(t, origin = ymd_hms('1970-01-01 00:00.00', tz = 'UTC'), tz = 'UTC') 
  sunAngle(t, lon, lat)$altitude
},
as.numeric(c(ymd_hms('2016-11-09 22:00:00', tz = 'UTC'), 
             ymd_hms('2016-11-10 22:00:00', tz = 'UTC'))))$minimum

abline(v = sunset, col = 'red')

curve(sunAngle(x, lon, lat)$altitude, col = 'red', add = TRUE)
