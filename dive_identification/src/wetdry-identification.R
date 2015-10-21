library('dplyr')

metadata <- read.csv('dive_identification/TDRmetadata.csv') %>%
  mutate(Deployed = as.POSIXct(Deployed, tz = 'UTC'),
         Recovered = as.POSIXct(Recovered, tz = 'UTC'))

samplewetdryline <- '01/10/14 20:03:49.600       01/10/14 20:05:16.000'
wetdrypattern <- '[0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}       [0-9]{2}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}'
readLines('dive_identification/1_CEFAS_output/LAW_WTSH_021_30018_A10348_100114.CSV') %>%
  grep(pattern = wetdrypattern, x = ., value = TRUE) %>%
  (function(wetdrylines) {
    strsplit(wetdrylines, '[[:space:]]{2,}') %>%
      unlist %>%
      matrix(nrow = length(wetdrylines), ncol = 2, byrow = TRUE) %>%
      data.frame %>%
      transmute(Begin = as.POSIXct(X1, format = '%m/%d/%y %H:%M:%OS', tz = 'UTC') + .05, 
                End = as.POSIXct(X2, format = '%m/%d/%y %H:%M:%OS', tz = 'UTC') + .05)
  }) -> wetdry
