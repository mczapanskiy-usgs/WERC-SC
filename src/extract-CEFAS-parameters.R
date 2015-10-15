library('dplyr')

get.datablock0.rate <- function(filepath = 'dive_identification/rttr_data/LEH2014RTTR036B_A10372_071814.CSV') {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Logging rate = \\d+', x = ., value = TRUE) %>%
      sub('Logging rate = (\\d+)', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
}

get.fastlog.rate <- function(filepath = 'dive_identification/rttr_data/LEH2014RTTR036B_A10372_071814.CSV') {
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

library('foreach')
library('iterators')

get.filepath <- function(species, tdrfile) {
  file.path('dive_identification', 
           sprintf('%s_data', tolower(species)),
           sprintf('%s.CSV', tdrfile))
}

tdr_metadata <- read.csv('dive_identification/TDRmetadata.csv', stringsAsFactors = FALSE)[,1:7]
foreach(deployment = iter(tdr_metadata, by = 'row'), .combine = rbind) %do% {
  data.frame(DataBlock0Rate = get.datablock0.rate(get.filepath(deployment$Species, deployment$TDRFilename)),
             FastLogRate = get.fastlog.rate(get.filepath(deployment$Species, deployment$TDRFilename)))
} %>% 
  cbind(tdr_metadata, .) %>%
  write.csv('dive_identification/TDRmetadata2.csv', row.names = FALSE)
