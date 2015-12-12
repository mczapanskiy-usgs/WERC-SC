library(dplyr)
library(RSQLite)
library(foreach)

## Create SQLite Database for MHI data from CSV files
OriginalMetadata <- read.csv('trackcode/gps/metadata_all_GPS.csv') %>%
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M'),
         GPS_programmed_start_datetime_local2 = mapply(FUN = as.POSIXct, GPS_programmed_start_datetime_local, tz = paste0('Etc/GMT', UTC_LocalTime_offset_hours), format = '%m/%d/%Y %H:%M') %>%
           as.POSIXct(origin = '1970-01-01 00:00.00 UTC', tz = 'UTC'))

DeploymentMetadata <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           DeploySession = DeplSess,
           GPSID = GPS_ID,
           GPSStart = GPS_programmed_start_datetime_local,
           GPSInterval = GPS_Interval_seconds,
           TDRID = TDR_ID) %>%
  summarize(Recovered = any(Tagging_Event == 'R'),
            GPSDeployed = any(GPS_Y_N == 'Y'),
            GPSNotes = paste(GPS_notes, collapse = ''),
            TDRDeployed = any(TDR_Y_N == 'Y'),
            Notes = paste(Notes, collapse = '')) %>%
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


