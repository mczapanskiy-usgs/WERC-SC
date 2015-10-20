library('data.table')
library('dplyr')

gps <- data.table(gpsUTC = as.POSIXct('2015-07-07 12:12:12', tz = 'UTC') + 120*(0:9),
                  lat = 1:10,
                  lon = 1:10) %>%
  mutate(nextUTC = lead(gpsUTC, default = Inf)) %>%
  setkey(gpsUTC, nextUTC)

tdr <- data.table(tdrUTC = as.POSIXct('2015-07-07 12:15:12', tz = 'UTC') + 0:179,
                  depth = sin(1:180) + 1) %>%
  mutate(dummyUTC = tdrUTC) %>%
  setkey(tdrUTC, dummyUTC)

foverlaps(x = gps,
          y = tdr,
          nomatch = NA) %>%
  select(gpsUTC, 
         lat, 
         lon,
         tdrUTC, 
         depth)
