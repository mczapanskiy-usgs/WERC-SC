library(dplyr)
library(RSQLite)
library(foreach)
library(data.table)

## Create SQLite Database for MHI data from CSV files
# Read metadata CSV file
OriginalMetadata <- read.csv('trackcode/gps/metadata_all_GPS.csv', stringsAsFactors = FALSE) %>%
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M'),
         GPS_programmed_start_datetime_local2 = mapply(FUN = as.POSIXct, GPS_programmed_start_datetime_local, tz = paste0('Etc/GMT', UTC_LocalTime_offset_hours), format = '%m/%d/%Y %H:%M') %>%
           as.POSIXct(origin = '1970-01-01 00:00.00 UTC', tz = 'UTC'))

# Reformat metadata to one row per deployment
DeploymentMetadataMinusTDRSettings <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           Year = Year, 
           DeploySession = DeplSess,
           GPSID = GPS_ID,
           GPSStart = GPS_programmed_start_datetime_local2,
           GPSInterval = GPS_Interval_seconds,
           TDRID = TDR_ID) %>%
  summarize(Recovered = any(Tagging_Event == 'R'),
            GPSDeployed = any(GPS_Y_N == 'Y'),
            GPSNotes = paste(GPS_notes, collapse = ''),
            TDRDeployed = any(TDR_Y_N == 'Y'),
            Processor = paste(Processor, collapse = '; '),
            Notes = paste(Notes, collapse = '; ')) %>%
  ungroup %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'D') %>%
              select(DeployID = Deploy_ID, 
                     UTCDeployed = UTC)) %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'R') %>%
              select(DeployID = Deploy_ID, 
                     UTCRecovered = UTC,
                     GPSRecovered = GPS_TagRecov,
                     GPSFile = GPS_Track_File,
                     TDRRecovered = TDR_TagRecov,
                     TDRFile = TDR_File))

# Read TDR settings from CEFAS files
TDRSettings <- foreach(deployid = DeploymentMetadata$DeployID, tdrfile = DeploymentMetadata$TDRFile, .combine = rbind) %do% {
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
    as.POSIXct(format = '%d/%m/%y %H:%M:%S', tz = 'Etc/GMT-10')
  
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
  select(DeployID, DeploySession, Recovered, UTCDeployed, UTCRecovered, GPSDeployed, GPSRecovered, GPSID, GPSStart, GPSInterval, GPSNotes, GPSFile, TDRDeployed, TDRRecovered, TDRID, TDRStart, TDRInterval, TDRWetDry, TDRThreshold, TDRFile, Notes)

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
         HowSexed = ifelse(is.na(HowSexed), 'N', HowSexed))

# Gather dive data
ValidBRBODives <- dir('dive_identification/5a_brbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

BRBODive <- lapply(dir('dive_identification/4a_brbo_dive_data/', full.names = TRUE),
                   read.csv) %>%
  rbindlist %>%
  semi_join(ValidBRBODives)

ValidRFBODives <- dir('dive_identification/5b_rfbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

RFBODive <- lapply(dir('dive_identification/4b_rfbo_dive_data/', full.names = TRUE),
                   read.csv) %>%
  rbindlist %>%
  semi_join(ValidRFBODives)

Dive <- rbind(BRBODive, RFBODive)

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

## Remove duplicate track points. Temporary fix.
DropTrackPoints <- Track %>% 
  group_by(DeployID, UTC) %>% 
  summarize(N = n()) %>% 
  ungroup %>% 
  filter(N == 2) %>% 
  merge(Track, by = c('DeployID', 'UTC')) %>% 
  arrange(DeployID, UTC, TripID) %>% 
  mutate(Drop = row_number() %% 2 == 0) %>% 
  filter(Drop) %>% 
  select(-Drop)
Track <- anti_join(Track,
                   DropTrackPoints)

BRBOTrips <- read.csv('trackcode/gps/All_tracks/BRBO_1.5_trips_annotated.csv') %>%
  group_by(DeployID = Deploy_ID,
           TripID = trip_no,
           Begin = tripSt,
           End = tripEnd,
           BeginComplete = tripStComp,
           EndComplete = tripEndComp,
           Duration = duration.hrs) %>%
  summarize %>%
  ungroup

RFBOTrips <- read.csv('trackcode/gps/All_tracks/RFBO_1.5_trips_annotated.csv') %>%
  group_by(DeployID = Deploy_ID,
           TripID = trip_no,
           Begin = tripSt,
           End = tripEnd,
           BeginComplete = tripStComp,
           EndComplete = tripEndComp,
           Duration = duration.hrs) %>%
  summarize %>%
  ungroup

Trip <- rbind(BRBOTrips, RFBOTrips)

# Create SQLite database
MHIdb <- dbConnect(SQLite(), 'Hawaii_data/MHI_GPS_TDR.sqlite')
dbWriteTable(MHIdb, 'DeploymentMetadata', DeploymentMetadata %>% as.data.frame, overwrite = TRUE)
dbWriteTable(MHIdb, 'BirdMetadata', BirdMetadata %>% as.data.frame, overwrite = TRUE)
dbWriteTable(MHIdb, 'Dive', Dive %>% as.data.frame, overwrite = TRUE)
dbWriteTable(MHIdb, 'Track', Track %>% as.data.frame, overwrite = TRUE)
dbWriteTable(MHIdb, 'Trip', Trip %>% as.data.frame, overwrite = TRUE)
dbDisconnect(MHIdb)
