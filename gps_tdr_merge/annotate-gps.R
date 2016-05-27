library('data.table')
library('dplyr')
library('adehabitatLT')

# Load metadata
metadata <- read.csv(file.path('trackcode', 'gps', 'metadata_all_GPS.csv')) %>%
  mutate(UTC = as.POSIXct(UTC, format = '%m/%d/%Y %H:%M', tz = 'UTC')) %>%
  # Aggregate by Deploy_ID to get deployment and recovery times
  group_by(Deploy_ID,
           Species) %>%
  summarize(Deployed = min(UTC),
            Recovered = max(UTC))

# Annotate location data with wet/dry and dive data
annotate.gps <- function(deployid, resample = 120) {
  metadata <- filter(metadata, Deploy_ID == deployid)
  
  # Load dive data
  dives <- file.path('dive_identification',
                     '3_dive_data',
                     sprintf('%s.CSV', deployid)) %>%
    read.csv %>%
    mutate(diveUTC = as.POSIXct(Begin, tz = 'UTC'),
           # Dummy variable necessary for overlap join
           dummyUTC = diveUTC) %>% 
    data.table %>%
    setkey(diveUTC, dummyUTC)
  
  # Load wetdry data
  wetdry <- file.path('dive_identification',
                      '5_wetdry_data',
                      sprintf('%s.CSV', deployid)) %>%
    read.csv %>%
    mutate(wetdryUTC = as.POSIXct(Begin, tz = 'UTC'),
           # Dummy variable necessary for overlap join
           dummyUTC = wetdryUTC) %>% 
    data.table %>%
    setkey(wetdryUTC, dummyUTC)

  # Load GPS locations
  file.path('trackcode',
            'gps',
            # Tracks are in files grouped by species
            sprintf('%s_trips.csv', metadata$Species)) %>%
    read.csv %>%
    filter(Deploy_ID == deployid,
           # Only analyze complete trips
           tripStComp == 1,
           tripEndComp == 1) %>%
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC')) %>%
    # Split into trips and rediscretize in time
    (function(gps) as.ltraj(xy = gps[,c('Longitude', 'Latitude')], date = gps$UTC, id = gps$trip_no)) %>%
    redisltraj(u = resample, samplex0 = TRUE, type = 'time') %>%
    # Annotate locations with wetdry and dive data
    lapply(function(trip) {
      trip %>%
        transmute(DeployID = deployid,
                  gpsUTC = date,
                  Latitude = y,
                  Longitude = x,
                  Distance = distHaversine(cbind(x, y), cbind(lead(x), lead(y))),
                  Speed = Distance / as.numeric(difftime(lead(gpsUTC), gpsUTC, units = 'secs')),
                  trip_no = attr(., 'burst'),
                  # Calculate window bounds for overlap join
                  nextUTC = lead(gpsUTC, default = Inf)) %>%
        data.table %>%
        setkey(gpsUTC, nextUTC) %>%
        # Overlap join GPS locations with dive data
        foverlaps(x = .,
                  y = dives,
                  nomatch = NA) %>%
        # Overlap join GPS locations with wetdry data
        foverlaps(x = .,
                  y = wetdry,
                  nomatch = NA) %>%
        # Aggregate annotations by GPS location
        group_by(DeployID,
                 trip_no,
                 gpsUTC,
                 Latitude,
                 Longitude,
                 Speed) %>%
        summarize(WetDry = all(!is.na(wetdryUTC)),
                  DiveN = sum(!is.na(DiveID)),
                  TotalDiveDuration = sum(Duration),
                  MaxDepth = max(MaxDepth))
    }) %>%
    # Rejoin annotated data
    rbindlist
}
