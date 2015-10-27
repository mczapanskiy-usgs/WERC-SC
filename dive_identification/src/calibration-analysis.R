library('dplyr')
library('ggplot2')

# Create list of all fastlogs
get.fastlogs <- function(deployid) {
  tryCatch({
    read.csv('dive_identification/TDRmetadata.csv') %>%
      filter(DeployID == deployid) %>%
      (function(deployment) {
        file.path('dive_identification',
                  '2_tdr_data',
                  sprintf(fmt = '%s.CSV', deployment$TDRFilename))
      }) %>%
      read.csv %>%
      filter(EventId > 0) %>%
      group_by(EventId) %>%
      summarize(DeployId = deployid,
                N = n(),
                maxDepth = max(Pressure),
                depthRange = maxDepth - min(Pressure),
                speed = (lead(Pressure) - Pressure) / (lead(UTC) - UTC))
  }, error = function(e) {
    browser()
  })
}

fastlogs <- read.csv('dive_identification/TDRmetadata.csv') %>%
  #slice(1:10) %>%
  rowwise %>%
  do(get.fastlogs(.$DeployID)) %>%
  filter(N > 4,
         depthRange >= .5) %>%
  ungroup %>%
  mutate(nquintile = ntile(N, 5),
         depthquintile = ntile(maxDepth, 5),
         rangequintile = ntile(depthRange, 5),
         cluster1 = interaction(nquintile, depthquintile),
         cluster2 = interaction(nquintile, rangequintile),
         duration = difftime(max(UTC), min(UTC), units = 'secs') %>% as.numeric,
         avgspeed = mean(abs(speed)),
         magspeed = sqrt(speed^2))

samplebursts <- fastlogs %>% 
  group_by(cluster2) %>%
  sample_n(5) %>%
  ungroup

plot.fastlog <- function(did, eid) {
  png(width = 1800, height = 900, filename = sprintf('dive_identification/7_calibration_plots/%i_%i.png', did, eid))
  metadata <- read.csv('dive_identification/TDRmetadata.csv') %>%
    filter(DeployID == did)
  
  fastlog <- metadata %>%
    (function(deployment) {
      file.path('dive_identification',
                '2_tdr_data',
                sprintf(fmt = '%s.CSV', deployment$TDRFilename))
    }) %>%
    read.csv %>%
    filter(EventId == eid) %>%
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05,
           PressureCalibMin = Pressure - min(Pressure),
           PressureCalibLast = Pressure - last(Pressure),
           PressureCalibMedian = Pressure - median(Pressure))
  
  utc.breaks <- function(limits) {
    ORIGIN <- as.POSIXct('1970-01-01 00:00.00', tz = 'UTC')
    seq(from = limits[1] %>% as.numeric %>% floor,
        to = limits[2] %>% as.numeric %>% ceiling,
        length = 6) %>%
      as.POSIXct(tz = 'UTC', origin = ORIGIN) + .05
  }
  
  rawplot <- ggplot(fastlog,
                    aes(x = UTC,
                        y = Pressure)) +
    geom_line(size = .75,
              color = '#e41a1c') +
    geom_point(size = 4,
               color = '#e41a1c') +
    scale_x_datetime(breaks = utc.breaks) +
    scale_y_reverse() +
    guides(color = FALSE, size = FALSE) +
    labs(title = sprintf('%s\nDeploy ID: %i Event ID: %i', metadata$FieldID, did, eid))
  
  minplot <- ggplot(fastlog,
                    aes(x = UTC,
                        y = PressureCalibMin)) +
    geom_line(size = .75,
              color = '#377eb8') +
    geom_point(size = 4,
               color = '#377eb8') +
    scale_x_datetime(breaks = utc.breaks) +
    scale_y_reverse()
    
  lastplot <- ggplot(fastlog,
                     aes(x = UTC,
                         y = PressureCalibLast)) +
    geom_line(size = .75,
              color = '#4daf4a') +
    geom_point(size = 4,
               color = '#4daf4a') +
    scale_x_datetime(breaks = utc.breaks) +
    scale_y_reverse() +
    guides(color = FALSE, size = FALSE)
  
  medianplot <- ggplot(fastlog,
                     aes(x = UTC,
                         y = PressureCalibMedian)) +
    geom_line(size = .75,
              color = '#984ea3') +
    geom_point(size = 4,
               color = '#984ea3') +
    scale_x_datetime(breaks = utc.breaks) +
    scale_y_reverse() +
    guides(color = FALSE, size = FALSE)
  
  grid.arrange(rawplot, minplot, lastplot, medianplot, nrow = 2, ncol = 2)
  
  dev.off()
  
  sprintf('dive_identification/7_calibration_plots/%i_%i.png', did, eid)
}

plot.samples <- function() {
  samplebursts %>%
  rowwise %>%
  do(plot.fastlog(.$DeployId, .$EventId))
}
