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

smoother <- function(vec, fil) {
  wing <- (length(fil) - 1) / 2
  vec2 <- c(rep(vec[1], wing),
            vec,
            rep(vec[length(vec)], wing))
  fil2 <- fil / sqrt(sum(fil)^2)
  convolve(vec2, fil2, type = 'filter')
}

get.track <- function(DeployID, TripID) {
  track <- dbGetPreparedQuery(MHIdb,
                              "SELECT *
                              FROM RediscretizedTrack T
                              WHERE T.DeployID = ?
                                AND T.TripID = ?",
                              dplyr::select(trip, DeployID, TripID))
  
  x0 <- mean(track$Longitude)
  y0 <- mean(track$Latitude)
  origin = c(x0, y0)
  
  mutate(track,
         UTC = POSIXct.from.db(UTC),
         d = distGeo(origin, cbind(Longitude, Latitude)),
         b = bearing(cbind(Longitude, Latitude), origin),
         x = d * -sin(b * pi / 180),
         y = d * -cos(b * pi / 180),
         step = sqrt((lead(x) - x)^2 + (lead(y) - y)^2),
         SunAltitude = sunAngle(UTC, Longitude, Latitude)$altitude) %>%
    filter(SunAltitude > 0) %>%
    mutate(UTC2 = first(UTC) + seq_along(UTC)) 
  # Times no longer accurate, but they are sequential. 
  # Necessary for excluding night periods from fpt analysis
}

ars_scale_file <- 'Hawaii_data/Lehua/ars/scales.csv'
ars_scales <- if(file.exists(ars_scale_file)) {
  read.csv(ars_scale_file)
} else {
  manual_results <- foreach(trip = iter(trips, by = 'row'), i = 1:ntrips) %do% {
    trackxy <- get.track(trip$DeployID, trip$TripID)
    
    tracklt <- with(trackxy, as.ltraj(cbind(x, y), 
                                      date = UTC, 
                                      id = paste(trip$DeployID, trip$TripID, sep = '_')))
    fptresults <- fpt(tracklt, radii = seq(from = .5e3, to = 500e3, by = .25e3), units = 'seconds')
    
    rv <- data.frame(radius = attr(fptresults, 'radii'),
                     varlog = apply(fptresults[[1]], 2, function(f) var(log(f), na.rm = TRUE))) %>%
      na.omit
    
    rv <- rv %>%
      mutate(varlog2 = smoother(varlog, c(1, 2, 4, 8, 4, 2, 1)),
             dv_dr = (lead(varlog2) - varlog2) / (lead(radius) - radius),
             d2v_dr2 = (lead(dv_dr) - dv_dr) / (lead(radius) - radius))
    
    x11(1200,800, title = as.character(i))
    plot(varlog ~ radius, rv)
    lines(varlog2 ~ radius, rv)
    ars_radii <- rv$radius[with(rv, identify(radius, varlog, labels = radius))]
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
    rbindlist
  write.csv(manual_results, 'Hawaii_data/Lehua/ars/scales.csv', row.names = FALSE)
  manual_results
}

metadata <- dbGetQuery(MHIdb,
"SELECT BM.DeployID, BM.Species
FROM BirdMetadata BM 
WHERE BM.Species IN ('BRBO', 'RFBO')") %>%
  as.data.table

ars_scales <- left_join(ars_scales, metadata) %>%
  as.data.frame

ars_scales %>%
  ggplot(aes(ARS_radii,
             color = Species)) +
  geom_density(size = 1) +
  ggtitle('Foraging scale distribution across species')

species_scales <- ars_scales %>%
  group_by(Species) %>%
  summarize(mean_radius = mean(ARS_radii, na.rm=TRUE),
            median_radius = median(ARS_radii, na.rm = TRUE),
            sd_radius = sd(ARS_radii, na.rm = TRUE))

ars_scales <- merge(ars_scales, dplyr::select(species_scales, Species, median_radius), by = 'Species')

ARS_zones <- foreach(trip = iter(ars_scales, by = 'row'), .combine = rbind) %do% {
  trackxy <- get.track(trip$DeployID, trip$TripID)
  
  tracklt <- with(trackxy, as.ltraj(cbind(x, y), 
                                    date = UTC2, 
                                    id = paste(trip$DeployID, trip$TripID, sep = '_')))
  
  trackxy %>% 
    merge(species_scales)
    mutate(fpt = fpt(tracklt, radii = trip$median_radius, units = 'seconds')[[1]]$r1) %>%
    filter(percent_rank(fpt) >= .95) %>%
    transmute(DeployID,
              TripID,
              Longitude,
              Latitude,
              FPT = fpt,
              Radius = trip$median_radius)
}
write.csv(ARS_zones, 'Hawaii_data/Lehua/ars/zones.csv', row.names = FALSE)

ARS_scale_freq <- ars_scales %>% 
  group_by(Species, DeployID, TripID) %>% 
  summarize(Nscales = n_distinct(ARS_radii, na_rm = TRUE)) %>% 
  ungroup %>% 
  group_by(Species, Nscales) %>% 
  summarize(Ntrips = n()) %>% 
  group_by(Species) %>% 
  mutate(Ftrips = Ntrips / sum(Ntrips)) %>%
  ungroup %>%
  arrange(Species, Nscales)

ARS_scale_freq %>%
  ggplot(aes(Nscales, 
             Ftrips, 
             fill = Species)) + 
  geom_bar(stat='identity', 
           position = position_dodge())

FPT_TRACKS <- foreach(trip = iter(trips, by = 'row'), .combine = rbind) %do% {
  trackxy <- get.track(trip$DeployID, trip$TripID) %>%
    merge(trip) %>%
    merge(dplyr::select(species_scales, Species, radius = median_radius), by = 'Species')
  
  tracklt <- with(trackxy, as.ltraj(cbind(x, y), 
                                    date = UTC, 
                                    id = sprintf('%40d%20d', DeployID, TripID)))
  
  trackxy %>% 
    mutate(fpt = fpt(tracklt, radii = radius, units = 'seconds')[[1]]$r1,
           ars_zone = percent_rank(fpt) >= .75)
}

DTid <- FPT_TRACKS %>%
  group_by(DeployID,
           TripID) %>%
  summarize %>%
  ungroup %>%
  mutate(DTid = row_number())

FPT_TRACKS <-
  merge(FPT_TRACKS, DTid)

write.csv(FPT_TRACKS, 'Hawaii_data/Lehua/ars/fpttracks.csv', row.names = FALSE, na = "")
