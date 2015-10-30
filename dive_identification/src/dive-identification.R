library('dplyr')
library('foreach')
library('diveMove')

metadata <- read.csv('dive_identification/TDRmetadata.csv') %>%
  mutate(Deployed = as.POSIXct(Deployed, tz = 'UTC'),
         Recovered = as.POSIXct(Recovered, tz = 'UTC'))

initialize.tdr <- function(deployid) {
  metadata <- filter(metadata, DeployID == deployid)
  
  tdr.path <- function(deployid) {
    with(metadata %>%
           filter(DeployID == deployid),
         file.path('dive_identification',
                   '2_tdr_data',
                   sprintf('%s.CSV', TDRFilename)))
  }
  
  tdr <- read.csv(tdr.path(deployid)) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05) %>%
    filter(UTC > metadata$Deployed,
           UTC < metadata$Recovered) %>%
    # Apply Speed and Acceleration filters (from calibration-analysis.R)
    mutate(Speed = (lead(Pressure) - Pressure) / as.numeric(difftime(lead(UTC), UTC, units = 'secs')),
           Acceleration = (lead(Speed) - Speed) / as.numeric(difftime(lead(UTC), UTC, units = 'secs')),
           RawPressure = Pressure,
           Pressure = ifelse((!is.na(Speed) & abs(Speed) > 10) | (!is.na(Acceleration) & Acceleration > 100), 
                             NA,
                             Pressure))
  attr(tdr, 'DeployID') <- deployid
  tdr
}

calibrate.tdr <- function(tdr, depth_thr = .5) {
  if(nrow(tdr) == 0) return(tdr)
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  
  # Choose offset method 
  # Short fastlogs (<2s) always use min 
  # Long fastlogs (>10s) always use median
  # Medium fastlogs (>2s, <10s) use a combination of duration and slope to determine method
  surface.offset <- function(fastlog) {
    fastlog$UTC2 <- seq_along(fastlog$UTC)
    minoffset <- min(fastlog$Pressure, na.rm = TRUE)
    medianoffset <- median(fastlog$Pressure, na.rm = TRUE)
    duration <- difftime(max(fastlog$UTC), min(fastlog$UTC), units = 'secs') %>% as.numeric
    if(duration < 2) {
      minoffset
    } else if(duration > 10) {
      medianoffset
    } else {
      tryCatch({
        pressurelm <- lm(Pressure ~ UTC2, fastlog)
        slope <- coefficients(pressurelm)[2]
        # slope_duration threshold from calibration-analysis.R
        if(slope > slope_duration_threshold[1] * duration + slope_duration_threshold[2]) {
          medianoffset
        } else {
          minoffset
        }
      }, error = function(e) minoffset)
    }
  }
  
  # Filter out FastLogs with only 4 points or depth range less than dive threshold
  valid_fastlogs <- tdr %>%
    filter(EventId > 0) %>%
    group_by(EventId) %>%
    summarize(N = n(),
              depthRange = max(Pressure, na.rm = TRUE) - min(Pressure, na.rm = TRUE)) %>%
    filter(N > 4,
           depthRange > depth_thr)
  
  calibrate.fastlog <- function(fastlog) {
    surface <- surface.offset(fastlog)
    # Create a TDR object
    createTDR(fastlog$UTC,
              fastlog$Pressure,
              dtime = metadata$FastLogRate,
              file = tdr.path(deployid)) %>%
      # Calibrate TDR using min or median pressure as offset
      calibrateDepth(wet.thr = 0,
                     dive.thr = depth_thr,
                     zoc.method = 'offset',
                     offset = surface) %>%
      # Pull calibrated pressures, dive ids, and phases from calibrated TDR object
      (function(calibrated.tdr) {
        data.frame(CalibPressure = calibrated.tdr@tdr@depth, 
                   Surface = surface,
                   DiveIdInit = calibrated.tdr@dive.activity$dive.id, 
                   DivePhase = calibrated.tdr@dive.phases)
      }) %>%
      # Rejoin to original data
      cbind(fastlog, .) %>%
      arrange(UTC)
  }
  
  #Recalibrate DiveIds
  recalibrate.diveids <- function(tdr) {
    # Calculate DiveIds across all records
    diveTable <- tdr %>%
      filter(DiveIdInit > 0) %>%
      group_by(EventId, DiveIdInit) %>%
      summarize %>%
      as.data.frame
    diveTable$DiveId <- seq_along(diveTable[,1])
    # Merge tdr with dive table
    merge(tdr, diveTable, all.x = TRUE) %>%
      select(-DiveIdInit) %>%
      mutate(DiveId = ifelse(is.na(DiveId), 0, DiveId)) %>%
      arrange(UTC)
  }
  
  # Split by EventId and process FastLogs individually using diveMove
  tdr %>%
    filter(EventId %in% valid_fastlogs$EventId) %>%
    group_by(EventId) %>%
    do(calibrate.fastlog(.)) %>%
    recalibrate.diveids -> result
  attr(result, 'DeployID') <- deployid
  result
}

plot.dive <- function(tdr, diveid, dive_thr = .5) {
  expand.range <- function(rng, delta) rng + c(-delta, delta)
  `%between%` <- function(x, rng) x >= rng[1] & x <= rng[2]
  pressure.range <- function(pressure) range(pressure, na.rm = TRUE) %>% pmin(c(0, Inf)) %>% rev
  dive_UTC_rng <- range(filter(tdr, DiveId == diveid)$UTC)
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  eventid <- filter(tdr, DiveId == diveid)$EventId[1]
  png(filename = file.path('dive_identification',
                           '4_dive_plots',
                           sprintf('%i_%i_%i.png', deployid, eventid, diveid)),
      width = 1600, height = 900)
  par(ps = 20, cex = 1)
  layout(matrix(c(1, 1, 1, 2, 2), 1, 5))
  with(filter(tdr, EventId == eventid, UTC %between% expand.range(dive_UTC_rng, 1)), {
    plot(UTC, CalibPressure,
         ylim = pressure.range(CalibPressure),
         pch = 16,
         cex = 2,
         col = ifelse(DiveId == diveid, 'blue', 'red'),
         xaxt = 'n',
         main = sprintf('Field ID = %s, Deploy ID = %i\nEvent ID = %i, Dive ID = %i', 
                        metadata$FieldID, deployid, eventid, diveid),
         sub = sprintf('Threshold = %im', metadata$Threshold),
         xlab = '',
         ylab = 'Depth (m)')
    lines(UTC, CalibPressure, col = '#888888')
    abline(h = dive_thr, col = 'blue', lty = 2)
    axis(4)
    axis.POSIXct(1, 
                 at = seq(from = min(UTC), to = max(UTC), length.out = 6), 
                 format = '%m/%d %H:%M:%OS1')
    grid()
  })
  with(filter(tdr, EventId == eventid), {
    plot(UTC, RawPressure,
         ylim = pressure.range(c(CalibPressure, RawPressure)),
         pch = 16,
         col = 'red',
         cex = 2,
         xaxt = 'n',
         xlab = '',
         ylab = '')
    lines(UTC, RawPressure, col = 'red')
    points(UTC, CalibPressure, pch = 16, cex = 2, col = 'green')
    lines(UTC, CalibPressure, col = 'green')
    abline(h = dive_thr, col = 'blue', lty = 2)
    axis(4)
    axis.POSIXct(1, 
                 at = seq(from = min(UTC), to = max(UTC), length.out = 6), 
                 format = '%m/%d %H:%M:%OS1')
    grid()
    x_range <- dive_UTC_rng %>% as.numeric
    if(diff(x_range) < .05 * diff(par('usr')[1:2]))
      x_range <- expand.range(x_range, .025 * diff(par('usr')[1:2]))
    rect(x_range[1], par('usr')[3], x_range[2], par('usr')[4], col = '#AAAAAA50', border = NA)
  })
  dev.off()
  
  sprintf('%i_%i', deployid, diveid)
}

analyze.dives <- function(tdr) {
  tdr %>%
    filter(DiveId > 0) %>%
    group_by(DiveId) %>%
    summarize(DeployID = attr(tdr, 'DeployID'),
              EventID = first(EventId),
              Begin = first(UTC),
              End = last(UTC),
              Duration = difftime(last(UTC), first(UTC), units = 'secs') %>% as.numeric %>% round(digits = 1),
              MaxDepth = max(Pressure),
              N = n()) %>%
    select(DeployID:EventID, DiveID = DiveId, Begin:N)
}

kitandkaboodle <- function() {
  foreach(did = metadata$DeployID[6:64], .combine = c) %do% {
    # Initialize and calibrate TDR data
    calib.tdr <- initialize.tdr(did) %>%
      calibrate.tdr
    
    if(nrow(calib.tdr) > 0) {
      # Analyze dives and write to file
      dives <- analyze.dives(calib.tdr)
      write.csv(dives,
                file.path('dive_identification',
                          '3_dive_data',
                          sprintf('%i.CSV', did)),
                row.names = FALSE)
      
      # Plot dive
      plotted <- sapply(dives$DiveID, function(diveid) plot.dive(tdr = calib.tdr, diveid = diveid))
      
      paste(plotted, collapse = '_')
    } else {
      sprintf('ERRORNOROWS:%i', did) 
    }
  }
}
