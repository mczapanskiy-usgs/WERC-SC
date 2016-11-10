library(dplyr)
library(lubridate)
library(RNetCDF)
library(abind)

winds20140715 <- open.nc('../../ArcGIS/ROSES/Data/Environmental/Wind/uv20140715rt.nc')
winds20140716 <- open.nc('../../ArcGIS/ROSES/Data/Environmental/Wind/uv20140716rt.nc')

lon_w <- var.get.nc(winds20140715, 'lon')
lat_w <- var.get.nc(winds20140715, 'lat')
time_w <- c(var.get.nc(winds20140715, 'time'),
            var.get.nc(winds20140716, 'time'))
u <- abind(var.get.nc(winds20140715, 'u'),
           var.get.nc(winds20140716, 'u'),
           along = 3)
v <- abind(var.get.nc(winds20140715, 'v'),
           var.get.nc(winds20140716, 'v'),
           along = 3)

wind.from.point <- function(time, lon, lat) {
  lon2 <- lon %% 360
  lonIdx <- sapply(lon2, function(l) max(which(lon_w < l)))
  latIdx <- sapply(lat, function(l) max(which(lat_w < l)))
  time2 <- (time - ymd_hms('1978-01-01 00:00:00')) / dhours(1)
  timeIdx <- sapply(time2, function(t) max(which(time_w < t)))
  
  data.frame(u = u[lonIdx, latIdx, timeIdx], v = v[lonIdx, latIdx, timeIdx])
}

rfbo_trip.0 <- read.csv('trackcode/gps/All_tracks/RFBO_1.5_trips_annotated.csv') %>%
  filter(Deploy_ID == 377,
         trip_no == 1) %>%
  mutate(UTC = ymd_hms(UTC, tz = 'UTC')) 

rfbo_trip <- merge(rfbo_trip.0,
                   group_by(rfbo_trip.0, UTC, Longitude, Latitude) %>%
                     do(wind.from.point(.$UTC, .$Longitude, .$Latitude)))

wtsh_trip.0 <- read.csv('trackcode/gps/All_tracks/WTSH_1_trips_annotated.csv')  %>%
  filter(Deploy_ID == 377,
         trip_no == 1) %>%
  mutate(UTC = ymd_hms(UTC, tz = 'UTC')) 
                   
                   