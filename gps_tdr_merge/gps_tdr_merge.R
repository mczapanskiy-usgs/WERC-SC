library('dplyr')

gps <- data.frame(gpsUTC = as.POSIXct('2015-07-07 12:12:12', tz = 'UTC') + 120*(0:9),
                  lat = 1:10,
                  lon = 1:10) 

tdr <- data.frame(tdrUTC = as.POSIXct('2015-07-07 12:15:12', tz = 'UTC') + 0:179,
                  depth = sin(1:180) + 1)

merge(gps%>%
        mutate(nextUTC = lead(gpsUTC)), 
      tdr, all = TRUE) %>%
  filter(tdrUTC >= gpsUTC,
         tdrUTC < nextUTC) %>%
  select(-nextUTC) %>% View


gps_tdr <- rbind(tdr[gps, roll = 1], gps[tdr, roll = TRUE])
setkey(gps_tdr, gpsUTC, tdrUTC)
gps_tdr %>%
  select(gpsUTC, tdrUTC, lat, lon, depth)

gps[tdr, roll = TRUE] %>%
  select(gpsUTC, tdrUTC, lat, lon, depth) %>%
  rbind(gps %>%
          select)

rbindlist(list(gps, tdr[gps, roll = TRUE]), use.names = TRUE, fill = TRUE) %>%
  setkey(gpsUTC, tdrUTC) %>% View

gps[tdr, roll = TRUE] %>% setkey(UTC) %>% View
tdr[gps] %>% setkey(UTC) %>% View
