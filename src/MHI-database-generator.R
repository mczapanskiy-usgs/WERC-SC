library(dplyr)
library(RSQLite)
library(foreach)
library(data.table)

## Create SQLite Database for MHI data
# Read metadata CSV file
Metadata <- read.csv('metadata/TrackingMetadata.csv', 
                     stringsAsFactors = FALSE,
                     na.strings = '') %>%
  mutate(HatchDate = as.POSIXct(HatchDate, tz = 'UTC'),
         ChickFateDate = as.POSIXct(ChickFateDate, tz = 'UTC'),
         DeployCaptureTime = as.POSIXct(DeployCaptureTime, tz = 'UTC'),
         DeployReleaseTime = as.POSIXct(DeployReleaseTime, tz = 'UTC'),
         RecoverCaptureTime = as.POSIXct(RecoverCaptureTime, tz = 'UTC'),
         RecoverReleaseTime = as.POSIXct(RecoverReleaseTime, tz = 'UTC'))

# Read TDR settings from CEFAS files
TDRSettings <- foreach(deployid = Metadata$DeployID, 
                       tdrfile = Metadata$TDR_filename, 
                       .combine = rbind) %do% {
  if(tdrfile == '' || is.na(tdrfile)) return(NULL)

  cefas_filename <- sprintf('Hawaii_data/5Dives/1_CEFAS_output/%s.CSV', tdrfile)
  if(file.exists(cefas_filename)) {
    cefas_file <- readLines()
    
    background.rate <- cefas_file %>%
      grep(pattern = 'Logging rate = [0-9]+', x = ., value = TRUE) %>%
      first %>%
      sub('Logging rate = ([0-9]+)', '\\1', x = .) %>%
      as.numeric
    
    fastlog.rate <- cefas_file %>%
      grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
      first %>%
      sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
      as.numeric
    
    threshold <- cefas_file %>%
      grep(pattern = 'Dive Termination Depth = [01]m', x = ., value = TRUE) %>%
      first %>%
      sub('Dive Termination Depth = ([01])m', '\\1', x = .) %>%
      as.numeric
    
    wetdry <- cefas_file %>%
      grep(pattern = 'Table of Wet Times follows', x = .) %>%
      length > 0
  } else {
    background.rate <- NA
    fastlog.rate <- NA
    threshold <- NA
    wetdry <- NA
  }
  
  data.frame(DeployID = deployid,
             BackgroundRate = background.rate,
             FastlogRate = fastlog.rate,
             Threshold = threshold,
             WetDry = wetdry)
}

# Gather dive data
# Start with list of species-specific dive data folders...
Dives <- dir('Hawaii_data/5Dives/', pattern = '4[a-z]', full.names = TRUE) %>%
  # List dive data CSVs for each folder...
  lapply(dir, full.names = TRUE) %>%
  # Flatten
  unlist %>%
  # Read dive data CSVs
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  # Bind all files
  bind_rows %>%
  # Tidy up
  transmute(DeployID,
            EventID,
            DiveID,
            DiveStartUTC = as.POSIXct(Begin, tz = 'UTC'),
            DiveEndUTC = as.POSIXct(End, tz = 'UTC'),
            Duration,
            MaxDepth,
            Nrecords = N)

# Dive QAQC results
# WTSH
WTSHqaqc <- dir('www', pattern = 'WTSH_QAQC_', full.names = TRUE) %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows %>%
  # TDRs from 751 and 822 were buggy
  filter(DeployID != 751,        
         DeployID != 822) %>%
  # Order ValidDive from false to unclear to true
  mutate(ValidDive2 = recode(ValidDive,
                             f = 0,
                             u = 1,
                             t = 2)) %>%
  group_by(DeployID, DiveID) %>%
  # Assumes 3 QAQC files
  summarize(DiveQuality = sum(ValidDive2) / 3,
            PlungeErr = any(PlungeErr),
            SplitErr = any(SplitErr))

# RFBO

  

# Gather track and trip data
GPSTracksQAQC <- read.csv('Hawaii_data/4TripBreaker/Trips/AllSpecies_tripInfo_QAQC2016.csv',
                          stringsAsFactors = FALSE) %>%
  select(DeployID = Deploy_ID,
         TripNumber = trip_no,
         TripFlag = Flag,
         DistanceOK:Notes)

GPSTracks <- c("Hawaii_data/4TripBreaker/Tracks/BRBO_1.5_trips_annotated.csv",
            "Hawaii_data/4TripBreaker/Tracks/PFSH_2.5_trips_annotated.csv",          
            "Hawaii_data/4TripBreaker/Tracks/RFBO_1.5_trips_annotated.csv",          
            "Hawaii_data/4TripBreaker/Tracks/RTTR_1.5_trips_annotated.csv",          
            "Hawaii_data/4TripBreaker/Tracks/WTSH_1_trips_annotated.csv") %>%
  lapply(read.csv, stringsAsFactors = FALSE) %>%
  bind_rows %>%
  # Drop duplicate points created by trip breaker
  distinct(Deploy_ID, UTC, .keep_all = TRUE) %>%
  transmute(TimestampUTC = as.POSIXct(UTC, tz = 'UTC'),
            Longitude,
            Latitude,
            Altitude = NA,
            Velocity = NA,
            TripNumber = trip_no,
            TripStartUTC = as.POSIXct(tripSt, tz = 'UTC'),
            TripEndUTC = as.POSIXct(tripEnd, tz = 'UTC'),
            TripStartStatus = tripStComp,
            TripEndStatus = tripEndComp,
            DeployID = Deploy_ID) %>%
  merge(GPSTracksQAQC,
        by = c('DeployID', 'TripNumber'))

DID.lookup <- function(eobs_id) {
  eobs_ids <- data.frame(oldDeployID = substr(eobs_id, 1, nchar(eobs_id)-3) %>% as.numeric)
  deploy_ids <- select(Metadata, DeployID, oldDeployID) %>%
    filter(DeployID != oldDeployID)
  
  merge(eobs_ids, deploy_ids) %>%
    select(DeployID) %>%
    `[[`(1)
}

EOBSTracks <- read.csv('Hawaii_data/4TripBreaker/Tracks/LAAL_rad_1.5_locs.behav.annotation.csv',
                       stringsAsFactors = FALSE) %>%
  transmute(TimestampUTC = as.POSIXct(date, tz = 'UTC'),
            Longitude = longitude,
            Latitude = latitude,
            Altitude = NA,
            Velocity = vel,
            TripNumber = tracks.trip_no,
            TripStartStatus = tracks.tripStComp,
            TripEndStatus = tracks.tripEndComp,
            DeployID = DID.lookup(id))

Tracks <- bind_rows(GPSTracks, EOBSTracks)

# Test primary keys
pkeys <- list(DeploymentMetadata = 'DeployID',
     BirdMetadata = 'DeployID',
     Dive = c('DeployID', 'DiveID'),
     WetDry = c('DeployID', 'Begin'),
     Track = c('DeployID', 'UTC'),
     RediscretizedTrack = c('DeployID', 'UTC'),
     Trip = c('DeployID', 'TripID'))
valid.pkeys <- sapply(seq_along(pkeys), function(i) {
  data <- names(pkeys)[i] %>% get
  keys <- pkeys[[i]]
  values.per.key <- data %>%
    group_by_(.dots = keys) %>%
    summarize(N = n())
  all(values.per.key$N == 1)
})

# Create SQLite database
if(all(valid.pkeys)) {
  MHIdb <- dbConnect(SQLite(), 'Hawaii_data/MHI_GPS_TDR.sqlite')
  dbWriteTable(MHIdb, 'DeploymentMetadata', DeploymentMetadata, overwrite = TRUE)
  dbWriteTable(MHIdb, 'BirdMetadata', BirdMetadata, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Dive', Dive, overwrite = TRUE)
  dbWriteTable(MHIdb, 'WetDry', WetDry, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Track', Track, overwrite = TRUE)
  dbWriteTable(MHIdb, 'RediscretizedTrack', RediscretizedTrack, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Trip', Trip, overwrite = TRUE)
  dbDisconnect(MHIdb)
} else {
  error("Error in primary keys. Check valid.pkeys")
}

