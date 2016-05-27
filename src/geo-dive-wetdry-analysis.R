library(dplyr)
library(RSQLite)
library(oce)
library(foreach)
library(iterators)
library(data.table)

POSIXct.from.db <- function(x) as.POSIXct(x, tz = 'UTC', origin = '1970-01-01 00:00.00 UTC')
MHIdb <- dbConnect(dbDriver('SQLite'), 'Hawaii_data/MHI_GPS_TDR.sqlite')

trips <- dbGetQuery(MHIdb,
                    "SELECT T.DeployID, T.TripID, T.Begin, T.End, T.Duration,
                    BM.FieldID, BM.Species
                    FROM Trip T
                    JOIN DeploymentMetadata DM
                    ON DM.DeployID = T.DeployID
                    JOIN BirdMetadata BM
                    ON BM.DeployID = T.DeployID
                    WHERE DM.TDRRecovered = 1
                    AND DM.GPSRecovered = 1
                    AND T.Flag = 0
                    AND T.End - T.Begin > 3600 
                    AND BM.Species IN ('BRBO', 'RFBO')
                    AND BM.SubColonyCode = 'LEH'")

# 268 trips
ntrips <- nrow(trips)

get.track <- function(DeployID, TripID) {
  track <- dbGetPreparedQuery(MHIdb,
                              "SELECT T.*, BM.Species, W.DeployID IS NOT NULL AS 'Wet', COUNT(D.DiveID) AS 'Dives'
                              FROM RediscretizedTrack T
                                JOIN BirdMetadata BM
                                  ON T.DeployID = BM.DeployID
                                LEFT JOIN WetDry W
                                  ON T.DeployID = W.DeployID
                                    AND W.End > T.UTC
                                    AND W.Begin < T.UTC + 120
                              	LEFT JOIN Dive D
                              		ON T.DeployID = D.DeployID
                              			AND D.End > T.UTC
                              			AND D.Begin < T.UTC + 120
                              WHERE T.DeployID = ?
                                AND T.TripID = ?
                              GROUP BY T.DeployID, T.TripID, T.UTC",
                              dplyr::select(trip, DeployID, TripID))
  
  mutate(track,
         UTC = POSIXct.from.db(UTC),
         SunAltitude = sunAngle(UTC, Longitude, Latitude)$altitude) %>%
    filter(SunAltitude > 0)
}

DIVE_TRACKS <- foreach(trip = iter(trips, by = 'row'), .combine = rbind) %do% get.track(trip$DeployID, trip$TripID)

DTid <- DIVE_TRACKS %>%
  group_by(DeployID,
           TripID) %>%
  summarize %>%
  ungroup %>%
  mutate(DTid = row_number())

DIVE_TRACKS <-
  merge(DIVE_TRACKS, DTid)

write.csv(DIVE_TRACKS, 'Hawaii_data/Lehua/diving/dive_tracks.csv', row.names = FALSE, na = "")


