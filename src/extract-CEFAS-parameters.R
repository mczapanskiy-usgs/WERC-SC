library('dplyr')

get.datablock0.rate <- function(filepath = 'dive_identification/CEFAS_output/HAW_WTSH_007_02182_A10346_101414.CSV') {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Logging rate = \\d+', x = ., value = TRUE) %>%
      sub('Logging rate = (\\d+)', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
}

get.fastlog.rate <- function(filepath = 'dive_identification/CEFAS_output/HAW_WTSH_007_02182_A10346_101414.CSV') {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
      first %>%
      sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
}

get.threshold <- function(filepath = 'dive_identification/CEFAS_output/HAW_WTSH_007_02182_A10346_101414.CSV') {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Dive Termination Depth = [0-9\\.]+m', x = ., value = TRUE) %>%
      sub('Dive Termination Depth = ([0-9\\.]+)m', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
}

get.wetdry <- function(filepath = 'dive_identification/CEFAS_output/LEH2014RFBO020_A10367_061614.CSV') {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Wet dry logging -  Active', x = ., value = TRUE) %>%
      length > 0
  } else {
    NA
  }
}

library('foreach')
library('iterators')

get.filepath <- function(species, tdrfile) {
  file.path('dive_identification',
            'CEFAS_output',
           sprintf('%s.CSV', tdrfile))
}

tdr_metadata <- read.csv('dive_identification/TDRmetadata.csv', stringsAsFactors = FALSE)[,1:7]
foreach(deployment = iter(tdr_metadata, by = 'row'), .combine = rbind) %do% {
  tdrfile <- get.filepath(deployment$Species, deployment$TDRFilename)
  data.frame(DataBlock0Rate = get.datablock0.rate(tdrfile),
             FastLogRate = get.fastlog.rate(tdrfile),
             Threshold = get.threshold(tdrfile),
             WetDry = get.wetdry(tdrfile))
} %>% 
  cbind(tdr_metadata, .) %>%
  write.csv('dive_identification/TDRmetadata2.csv', row.names = FALSE)
