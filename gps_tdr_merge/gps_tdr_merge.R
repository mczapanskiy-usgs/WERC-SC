library('data.table')
library('dplyr')
library('adehabitatLT')

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

dives <- read.csv('dive_identification/3_dive_data/290.CSV') %>%
  mutate(diveUTC = as.POSIXct(Begin, tz = 'UTC'),
         dummyUTC = diveUTC) %>% 
  data.table %>%
  setkey(diveUTC, dummyUTC)

wetdry <- read.csv('dive_identification/5_wetdry_data/290.CSV') %>%
  mutate(wetdryUTC = as.POSIXct(Begin, tz = 'UTC'),
         dummyUTC = wetdryUTC) %>% 
  data.table %>%
  setkey(wetdryUTC, dummyUTC)

foverlaps(x = gps,
          y = dives,
          nomatch = NA) %>%
  foverlaps(y = wetdry,
            nomatch = NA) %>%
  transmute(gpsUTC,
            Latitude,
            Longitude,
            trip_no,
            Wet = !is.na(wetdryUTC),
            diveUTC,
            DiveID,
            DiveDuration = Duration,
            MaxDepth)


# Rediscretize location data
gpstraj <- with(gps, as.ltraj(xy = cbind(Longitude, Latitude), date = gpsUTC, id = trip_no)) %>%
  redisltraj(u = 120, samplex0 = TRUE, type = 'time') %>%
  lapply(function(trip) {
    select <- dplyr::select
    trip <- trip %>%
      select(gpsUTC = date,
             Latitude = y,
             Longitude = x) %>%
      mutate(nextUTC = lead(gpsUTC, default = Inf)) %>%
      data.table %>%
      setkey(gpsUTC, nextUTC)
    
    tdrtrip <- filter(tdr, diveUTC %between% range(trip$gpsUTC))
    
    foverlaps(x = trip,
              y = tdrtrip,
              nomatch = NA) %>%
      select(gpsUTC,
             Latitude,
             Longitude,
             diveUTC,
             DiveID,
             DiveDuration = Duration,
             MaxDepth)
  })

sapply(1:6,
       function(trip_no) {
         gpstraj[[trip_no]] %>%
           group_by(gpsUTC, Latitude, Longitude) %>%
           summarize(Dives = sum(!is.na(DiveID))) %>%
           write.csv(sprintf('gps_tdr_merge/merged_trips/%i_%i.csv', 290, trip_no), row.names = FALSE)
       })
