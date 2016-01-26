library(dplyr)
library(RSQLite)
library(foreach)
library(data.table)
library(adehabitatLT)

## Create SQLite Database for MHI data from CSV files
# Read metadata CSV file
OriginalMetadata <- read.csv('trackcode/gps/metadata_all_GPS.csv', stringsAsFactors = FALSE) %>%
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric,
         GPS_programmed_start_datetime_local2 = mapply(FUN = as.POSIXct, GPS_programmed_start_datetime_local, tz = ifelse(UTC_LocalTime_offset_hours == -10, 'US/Hawaii', 'Chile/Continental'), format = '%m/%d/%Y %H:%M') %>%
           as.POSIXct(origin = '1970-01-01 00:00.00 UTC', tz = 'UTC') %>% as.numeric)

# Reformat metadata to one row per deployment
DeploymentMetadataMinusTDRSettings <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           Year = Year, 
           DeploySession = DeplSess,
           GPSStart = GPS_programmed_start_datetime_local2,
           GPSInterval = GPS_Interval_seconds) %>%
  summarize(Recovered = any(Tagging_Event == 'R'),
            GPSDeployed = any(GPS_Y_N == 'Y'),
            GPSNotes = paste(GPS_notes, collapse = ''),
            TDRDeployed = any(TDR_Y_N == 'Y'),
            Processor = paste(Processor, collapse = '; '),
            Notes = paste(Notes, collapse = '; ')) %>%
  ungroup %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'D') %>%
              dplyr::select(DeployID = Deploy_ID, 
                     # NOTE: some entries have missing and/or different GPS/TDR IDs in deployment and 
                     # recovery. Since device ID has no impact on analysis, we will default to choosing
                     # the ID as recorded on deployment
                     GPSID = GPS_ID,
                     TDRID = TDR_ID,
                     UTCDeployed = UTC)) %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'R') %>%
              dplyr::select(DeployID = Deploy_ID, 
                     UTCRecovered = UTC,
                     GPSRecovered = GPS_TagRecov,
                     GPSFile = GPS_Track_File,
                     TDRRecovered = TDR_TagRecov,
                     TDRFile = TDR_File))

# Read TDR settings from CEFAS files
TDRSettings <- foreach(deployid = DeploymentMetadataMinusTDRSettings$DeployID, tdrfile = DeploymentMetadataMinusTDRSettings$TDRFile, .combine = rbind) %do% {
  if(tdrfile == '' || is.na(tdrfile)) return(NULL)
  
  cefas_file <- file.path('dive_identification',
                          '1_CEFAS_output',
                          sprintf('%s.CSV', tdrfile))
  cefas_contents <- readLines(cefas_file)
  
  start <- cefas_contents[grep(pattern = 'Start Date,Start Time,Stop Date,Stop Time,Logging Rate,sensors,Resolution,Fast Rate', x = cefas_contents) + 1] %>%
    strsplit(',') %>% 
    unlist %>% 
    `[`(1:2) %>%
    paste(collapse = ' ') %>%
    as.POSIXct(format = '%d/%m/%y %H:%M:%S', tz = 'US/Hawaii')
  
  fastlog.rate <- cefas_contents %>%
    grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
    first %>%
    sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
    as.numeric
  
  threshold <- cefas_contents %>%
    grep(pattern = 'Dive Termination Depth = [01]m', x = ., value = TRUE) %>%
    first %>%
    sub('Dive Termination Depth = ([01])m', '\\1', x = .) %>%
    as.numeric
  
  wetdry <- cefas_contents %>%
    grep(pattern = 'Table of Wet Times follows', x = .) %>%
    length > 0
  
  data.frame(DeployID = deployid,
             TDRStart = start,
             TDRInterval = fastlog.rate,
             TDRThreshold = threshold,
             TDRWetDry = wetdry)
}

# Join metadata with TDR settings and rearrange columns
DeploymentMetadata <- left_join(DeploymentMetadataMinusTDRSettings, TDRSettings) %>%
  dplyr::select(DeployID, DeploySession, Recovered, UTCDeployed, UTCRecovered, GPSDeployed, GPSRecovered, GPSID, GPSStart, GPSInterval, GPSNotes, GPSFile, TDRDeployed, TDRRecovered, TDRID, TDRStart, TDRInterval, TDRWetDry, TDRThreshold, TDRFile, Notes) %>%
  as.data.frame

# Extract bird metadata
BirdMetadata <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           FieldID = FieldID,
           BandNumber = BandNo,
           Country = Country,
           Island = Island,
           Site = Site,
           SubColony = SubCol,
           SubColonyCode = SubCol_Code,
           Species = Species,
           NestNumber = NestNo,
           Age = Age,
           NestLatitude = Nest_Lat_DD,
           NestLongitude = Nest_Long_DD,
           ColonyLatitude = Col_Lat_DD,
           ColonyLongitude = Col_Long_DD) %>%
  summarize(BloodCard = any(Blood_FTA == 'Y'),
            BloodVial = any(Blood_vial == 'Y'),
            Feathers = any(Feathers == 'Y'),
            Diet = any(Diet == 'Y'),
            Sex = first(Sex[Sex != 'U']),
            HowSexed = first(How_Sexed[Sex != 'U']),
            RLBand = paste(R_L, collapse = ''),
            Notes = paste(Notes, collapse = '; ')) %>%
  ungroup %>%
  mutate(Sex = ifelse(is.na(Sex), 'U', Sex),
         HowSexed = ifelse(is.na(HowSexed), 'N', HowSexed)) %>%
  as.data.frame

# Gather dive data
ValidBRBODives <- dir('dive_identification/5a_brbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

BRBODive <- lapply(dir('dive_identification/4a_brbo_dive_data/', full.names = TRUE),
                   read.csv,
                   stringsAsFactors = FALSE) %>%
  rbindlist %>%
  semi_join(ValidBRBODives) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC') %>% as.numeric) %>%
  as.data.frame

ValidRFBODives <- dir('dive_identification/5b_rfbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

RFBODive <- lapply(dir('dive_identification/4b_rfbo_dive_data/', full.names = TRUE),
                   read.csv,
                   stringsAsFactors = FALSE) %>%
  rbindlist %>%
  semi_join(ValidRFBODives) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC') %>% as.numeric) %>%
  as.data.frame

Dive <- rbind(BRBODive, RFBODive)

# Gather wet/dry data
WetDry <- foreach(DeployID = DeploymentMetadata$DeployID,
                  TDRwd = DeploymentMetadata$TDRWetDry,
                  TDRfile = DeploymentMetadata$TDRFile,
                  .combine = rbind) %do% {
  if(is.na(TDRwd)) { 
    NULL
  } else if(TDRwd == TRUE) {
    wetdrypattern <- '[0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}       [0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}'
    readLines(file.path('dive_identification',
                        '1_CEFAS_output',
                        sprintf('%s.CSV', TDRfile))) %>%
      grep(pattern = wetdrypattern, x = ., value = TRUE) %>%
      strsplit('[[:space:]]{2,}') %>%
      unlist %>%
      matrix(ncol = 2,
             byrow = TRUE,
             dimnames = list(NULL, c('Begin', 'End'))) %>%
      data.frame %>%
      transmute(DeployID,
                Begin = as.POSIXct(Begin, format = '%d/%m/%y %H:%M:%OS', tz = 'US/Hawaii') %>% as.numeric + .05,
                End = as.POSIXct(End, format = '%d/%m/%y %H:%M:%OS', tz = 'US/Hawaii') %>% as.numeric + .05)
  } else if(TDRwd == FALSE && file.exists(file.path('dive_identification',
                                                    '2_tdr_data',
                                                    sprintf('%s.CSV', TDRfile)))) {
    read.csv(file.path('dive_identification',
                       '2_tdr_data',
                       sprintf('%s.CSV', TDRfile))) %>%
      filter(EventId > 0) %>%
      mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05) %>%
      group_by(EventId) %>%
      summarize(Begin = min(UTC),
                End = max(UTC)) %>%
      ungroup %>%
      transmute(DeployID = DeployID,
                Begin = as.numeric(Begin),
                End = as.numeric(End))
  } else {
    NULL
  }
}

# Gather track and trip data
BRBOTracks <- read.csv('trackcode/gps/All_tracks/BRBO_1.5_trips_annotated.csv') %>%
  transmute(DeployID = Deploy_ID,
            UTC = as.POSIXct(UTC, tz = 'UTC'),
            Latitude,
            Longitude,
            TripID = trip_no)

RFBOTracks <- read.csv('trackcode/gps/All_tracks/RFBO_1.5_trips_annotated.csv') %>%
  transmute(DeployID = Deploy_ID,
            UTC = as.POSIXct(UTC, tz = 'UTC'),
            Latitude,
            Longitude,
            TripID = trip_no)

Track <- rbind(BRBOTracks, RFBOTracks)

## Remove duplicate track points. Temporary workaround until trip breaker interpolation bug is fixed.
DropTrackPoints <- Track %>% 
  group_by(DeployID, UTC) %>% 
  summarize(N = n()) %>% 
  ungroup %>% 
  filter(N == 2) %>% 
  merge(Track, by = c('DeployID', 'UTC')) %>% 
  arrange(DeployID, UTC, TripID) %>% 
  mutate(Drop = row_number() %% 2 == 0) %>% 
  filter(Drop) %>% 
  dplyr::select(-Drop)
Track <- anti_join(Track,
                   DropTrackPoints) %>%
  arrange(DeployID, UTC)
Trip <- read.csv('trackcode/gps/All_tracks/AllSpecies_tripInfo_QAQC.csv',
                 stringsAsFactors = FALSE) %>%
  dplyr::select(DeployID = Deploy_ID,
         TripID = trip_no,
         Begin = tripSt,
         End = tripEnd,
         BeginComplete = tripStComp, 
         EndComplete = tripEndComp,
         Duration = duration.hrs,
         Flag:Notes) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric) %>%
  as.data.frame

# Rediscretize track data
RediscretizedTrack <- as.ltraj(xy = Track[,c('Longitude', 'Latitude')],
                               date = Track$UTC,
                               id = Track$DeployID,
                               burst = paste(Track$DeployID, Track$TripID, sep = '-')) %>%
  redisltraj(u = 120, 
             samplex0 = TRUE, 
             type = 'time') %>% 
  lapply(function(burst) {
    cbind(DeployID = attr(burst, 'id') %>% as.numeric,
          TripID = attr(burst, 'burst') %>% sub('.*-([0-9]+)', '\\1', .) %>% as.numeric,
          transmute(burst, 
                    UTC = date %>% as.numeric, 
                    Latitude = y, 
                    Longitude = x))
  }) %>%
  rbindlist

Track <- mutate(Track, UTC = as.numeric(UTC))  

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

