library(dplyr)
library(RSQLite)
library(oce)
library(geosphere)
library(adehabitatLT)
library(foreach)
library(iterators)

POSIXct.from.db <- function(x) as.POSIXct(x, tz = 'UTC', origin = '1970-01-01 00:00.00 UTC')
MHIdb <- dbConnect(dbDriver('SQLite'), 'Hawaii_data/MHI_GPS_TDR.sqlite')

trips <- dbGetQuery(MHIdb,
"SELECT T.DeployID, T.TripID, T.Begin, T.End
FROM Trip T
  JOIN DeploymentMEtadata DM
    ON DM.DeployID = T.DeployID
  JOIN BirdMetadata BM
    ON BM.DeployID = T.DeployID
WHERE DM.TDRRecovered = 1
  AND DM.GPSRecovered = 1
  AND T.BeginComplete = 1
  AND T.EndComplete = 1
  AND BM.Species IN ('BRBO', 'RFBO')
  AND BM.SubColonyCode = 'LEH'")

foreach(trip = iter(trips[1,], by = 'row'), .combine = rbind) %do% {
  track <- dbGetPreparedQuery(MHIdb,
    "SELECT *
    FROM RediscretizedTrack T
    WHERE T.DeployID = ?
      AND T.TripID = ?",
    dplyr::select(trip, DeployID, TripID))
  
  x0 <- mean(track$Longitude)
  y0 <- mean(track$Latitude)
  origin = c(x0, y0)
  
  track <- mutate(track,
                  d = distGeo(origin, cbind(Longitude, Latitude)),
                  b = bearing(cbind(Longitude, Latitude), origin),
                  x = d * cos(b * pi / 180),
                  y = d * sin(b * pi / 180))
} 