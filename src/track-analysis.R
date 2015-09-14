library('dplyr')
library('adehabitatLT')

# Create ltraj object
metadata <- read.csv('metadata/gps_metadata.csv', stringsAsFactors = FALSE) %>%
  select(Deploy_ID, FieldID) %>%
  unique() 
track.data.0 <- read.csv('data/tracks/BRBO_colrad_2_tracks_trips.csv', stringsAsFactors = FALSE) %>%
  mutate(UTC = as.POSIXct(UTC, format = '%Y/%m/%d %H:%M:%S', tz = 'UTC')) %>%
  select(UTC, Latitude, Longitude, trip_no, trip_start, trip_end, Deploy_ID) %>%
  filter(trip_no > 0, Deploy_ID == 254)
longlat <- as.matrix(track.data.0[,c('Longitude', 'Latitude')])
sp.longlat <- SpatialPoints(longlat, proj4string = CRS('+proj=longlat +datum=WGS84'))
sp.utm <- spTransform(sp.longlat, CRS('+proj=utm +zone=4 +datum=WGS84'))
track.data.0[,c('Longitude', 'Latitude')] <- sp.utm@coords
track.data <- merge(track.data.0, metadata)
BRBO.tracks.0 <- with(track.data, as.ltraj(xy = cbind(Longitude, Latitude), date = UTC, id = FieldID, burst = trip_no))
BRBO.tracks <- redisltraj(BRBO.tracks.0, 125, type = 'time')
rm(longlat, sp.longlat, sp.utm, track.data.0, BRBO.tracks.0)

# Plot bursts
plot(BRBO.tracks, perani = FALSE)
plotltr(BRBO.tracks, which = 'dist')
plotltr(BRBO.tracks, which = 'rel.angle')

# Use autocorrelation to identify changes in behavior
acfdist.ltraj(BRBO.tracks, which = 'dist', lag = 30)
acfang.ltraj(BRBO.tracks, which = 'relative', lag = 30)

# Visualize First Passage Time (FPT)
fpt.radii <- signif(seq(0, 500, length=20), 2)
BRBO.fpt <- fpt(BRBO.tracks, fpt.radii, units = 'seconds') 
burst.idx <- 3
track <- BRBO.tracks[[burst.idx]]
fpt.idx <- 10
fpt <- BRBO.fpt[[burst.idx]][,fpt.idx]
plot(track$date, fpt, xlab = 'Date', ylab = 'FPT', main = sprintf('FPT at radius %im', attr(BRBO.fpt, 'radii')[fpt.idx]))
hist(fpt)

lapply(BRBO.fpt, function(burst) plot(fpt.radii, sapply(burst, mean, na.rm = TRUE)))
lapply(BRBO.fpt, function(burst) plot(fpt.radii, sapply(burst, var, na.rm = TRUE)))


plot(BRBO.tracks, burst = 3, col = brewer.pal(4, 'RdYlGn')[findInterval(fpt, quantile(fpt, na.rm = TRUE))])
plot(track$date, fpt, 
     color = brewer.pal(4, 'RdYlGn')[findInterval(fpt, quantile(fpt, na.rm = TRUE))],
     xlab = 'Date', ylab = 'FPT', main = sprintf('FPT at radius %im', attr(BRBO.fpt, 'radii')[fpt.idx]))

