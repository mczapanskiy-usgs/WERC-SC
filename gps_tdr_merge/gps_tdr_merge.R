library('data.table')
library('dplyr')

# Dummy Data Prototype
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


# Sample Data Prototype
gps <- read.csv('trackcode/gps/RTTR_trips.csv') %>%
  filter(Deploy_ID == 290) %>%
  mutate(gpsUTC = as.POSIXct(UTC, tz ='UTC'),
         nextUTC = lead(gpsUTC, default = Inf)) %>%
  data.table %>%
  setkey(gpsUTC, nextUTC)

tdr <- read.csv('dive_identification/3_dive_data/290.CSV') %>%
  mutate(diveUTC = as.POSIXct(Begin, tz = 'UTC'),
         dummyUTC = diveUTC) %>% 
  data.table %>%
  setkey(diveUTC, dummyUTC)

foverlaps(x = gps,
          y = tdr,
          nomatch = NA) %>%
  select(gpsUTC,
         Latitude,
         Longitude,
         trip_no,
         diveUTC,
         DiveID,
         DiveDuration = Duration,
         MaxDepth)
