library('dplyr')

metadata <- read.csv('dive_identification/TDRmetadata.csv') %>%
  mutate(Deployed = as.POSIXct(Deployed, tz = 'UTC'),
         Recovered = as.POSIXct(Recovered, tz = 'UTC'))

summarize.wetdry <- function(deployid) {
  metadata <- filter(metadata, DeployID == deployid)
  if(metadata$WetDry) {
    wetdrypattern <- '[0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}       [0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}'
    readLines(file.path('dive_identification',
                        '1_CEFAS_output',
                        sprintf('%s.CSV', metadata$TDRFilename))) %>%
      grep(pattern = wetdrypattern, x = ., value = TRUE) %>%
      (function(wetdrylines) {
        strsplit(wetdrylines, '[[:space:]]{2,}') %>%
          unlist %>%
          matrix(nrow = length(wetdrylines), ncol = 2, byrow = TRUE) %>%
          data.frame %>%
          transmute(Begin = as.POSIXct(X1, format = '%m/%d/%y %H:%M:%OS', tz = 'UTC') + .05, 
                    End = as.POSIXct(X2, format = '%m/%d/%y %H:%M:%OS', tz = 'UTC') + .05)
      })
  } else {
    read.csv('dive_identification/2_tdr_data/LEH2014RTTR094_A10373_061614.CSV') %>%
      filter(EventId > 0) %>%
      mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05) %>%
      group_by(EventId) %>%
      summarize(Begin = min(UTC),
                End = max(UTC)) %>%
      select(-EventId)
  } %>%
    filter(Begin > metadata$Deployed,
           End < metadata$Recovered) %>%
    mutate(DeployID = deployid) %>%
    select(DeployID, Begin, End) %>%
    write.csv(file.path('dive_identification',
                        '5_wetdry_data',
                        sprintf('%s.CSV', deployid)),
              row.names = FALSE)
  
  deployid
}
