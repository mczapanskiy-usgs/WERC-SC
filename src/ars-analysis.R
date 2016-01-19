library(dplyr)
library(RSQLite)
library(oce)
library(geosphere)
library(adehabitatLT)
library(foreach)
library(iterators)
library(ggplot2)
library(data.table)

POSIXct.from.db <- function(x) as.POSIXct(x, tz = 'UTC', origin = '1970-01-01 00:00.00 UTC')
MHIdb <- dbConnect(dbDriver('SQLite'), 'Hawaii_data/MHI_GPS_TDR.sqlite')

trips <- dbGetQuery(MHIdb,
"SELECT T.DeployID, T.TripID, T.Begin, T.End, T.Duration,
  BM.FieldID, BM.Species
FROM Trip T
  JOIN DeploymentMEtadata DM
    ON DM.DeployID = T.DeployID
  JOIN BirdMetadata BM
    ON BM.DeployID = T.DeployID
WHERE DM.TDRRecovered = 1
  AND DM.GPSRecovered = 1
  AND T.BeginComplete = 1
  AND T.EndComplete = 1
  AND T.End - T.Begin > 3600 
  AND BM.Species IN ('BRBO', 'RFBO')
  AND BM.SubColonyCode = 'LEH'")

trips <- trips %>%
  mutate(EXCLUDE = DeployID == 397 & TripID == 2) %>%
  filter(!EXCLUDE) %>%
  dplyr::select(-EXCLUDE)

foreach(trip = iter(trips, by = 'row')) %do% {
  track <- dbGetPreparedQuery(MHIdb,
                              "SELECT *
                              FROM RediscretizedTrack T
                              WHERE T.DeployID = ?
                              AND T.TripID = ?",
                              dplyr::select(trip, DeployID, TripID))
  
  x0 <- mean(track$Longitude)
  y0 <- mean(track$Latitude)
  origin = c(x0, y0)
  
  trackxy <- mutate(track,
                    UTC = POSIXct.from.db(UTC),
                    d = distGeo(origin, cbind(Longitude, Latitude)),
                    b = bearing(cbind(Longitude, Latitude), origin),
                    x = d * -sin(b * pi / 180),
                    y = d * -cos(b * pi / 180),
                    step = sqrt((lead(x) - x)^2 + (lead(y) - y)^2))
  
  tracklt <- with(trackxy, as.ltraj(cbind(x, y), 
                                    date = UTC, 
                                    id = paste(trip$DeployID, trip$TripID, sep = '_')))
  fptresults <- fpt(tracklt, radii = seq(from = 1e3, to = 500e3, by = 1e3), units = 'seconds')
  
  rv <- data.frame(radius = attr(fptresults, 'radii'),
                   variance = apply(fptresults[[1]], 2, function(f) var(log(f), na.rm = TRUE))) %>%
    na.omit
  
  smoother <- function(vec, fil) {
    wing <- (length(fil) - 1) / 2
    vec2 <- c(rep(vec[1], wing),
              vec,
              rep(vec[length(vec)], wing))
    fil2 <- fil / sqrt(sum(fil)^2)
    convolve(vec2, fil2, type = 'filter')
  }
  
  rv <- rv %>%
    mutate(log_var = log(variance),
           log_var_smooth = smoother(log_var, c(2,3,5,3,2)),
           dv_dr = (lead(log_var_smooth) - log_var_smooth) / (lead(radius) - radius),
           d2v_dr2 = (lead(dv_dr) - dv_dr) / (lead(radius) - radius))
  
  x11()
  plot(log_var ~ radius, filter(rv, log_var >= -10))
  lines(log_var_smooth ~ radius, rv)
  ars_radii <- rv$radius[with(rv, identify(radius, log_var, labels = radius))]
  dev.off()

  list(DeployID = trip$DeployID, 
       TripID = trip$TripID,
       Nfpt = nrow(rv),
       ARS_radii = ars_radii)
} %>%
  lapply(function(trip) {
    data.frame(DeployID = trip$DeployID, 
               TripID = trip$TripID, 
               Nfpt = trip$Nfpt, 
               ARS_radii = if(length(trip$ARS_radii) == 0) NA else trip$ARS_radii)
  }) %>%
  rbindlist -> ars_scales
