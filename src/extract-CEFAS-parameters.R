library('dplyr')

get.datablock0.rate <- Vectorize(function(filepath) {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Logging rate = \\d+', x = ., value = TRUE) %>%
      sub('Logging rate = (\\d+)', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
})

get.fastlog.rate <- Vectorize(function(filepath) {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
      first %>%
      sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
})

get.threshold <- Vectorize(function(filepath) {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Dive Termination Depth = [0-9\\.]+m.*', x = ., value = TRUE) %>%
      sub('Dive Termination Depth = ([0-9\\.]+)m.*', '\\1', x = .) %>%
      as.numeric
  } else {
    NA
  }
})

get.wetdry <- Vectorize(function(filepath) {
  if(file.exists(filepath)) {
    readLines(filepath) %>%
      grep(pattern = 'Wet dry logging -  Active', x = ., value = TRUE) %>%
      length > 0
  } else {
    NA
  }
})
