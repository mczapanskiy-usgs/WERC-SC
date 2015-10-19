library('dplyr')
library('foreach')
library('diveMove')

initilize.tdr <- function(deployid) {
  metadata <- filter(metadata, DeployID == deployid)
  tdr <- read.csv(tdr.path(deployid), stringsAsFactors = FALSE) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC')) %>%
    filter(UTC > metadata$Deployed,
           UTC < metadata$Recovered) %>%
    rename(PressureInit = Pressure)
  attr(tdr, 'DeployID') <- deployid
  tdr
}
tdr.path <- function(deployid) {
  with(metadata %>%
         filter(DeployID == deployid),
       file.path('dive_identification',
                 '2_tdr_data',
                 sprintf('%s.CSV', TDRFilename)))
}

calibrate.tdr <- function(tdr, depth_thr = .5) {
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  # Split by EventId and process FastLogs individually using diveMove
  tdr %>%
    filter(EventId > 0) %>%
    group_by(EventId) %>%
    (function(fastlog) {
      surface <- if(metadata$Threshold == 0) median(fastlog$PressureInit) else 0
      # Create a TDR object
      createTDR(fastlog$UTC,
                fastlog$PressureInit,
                dtime = metadata$FastLogRate,
                file = tdr.path(deployid)) %>%
        # Calibrate TDR using median pressure as offset
        calibrateDepth(wet.thr = 0,
                       dive.thr = depth_thr,
                       zoc.method = 'offset',
                       offset = surface) %>%
        # Pull calibrated pressures, dive ids, and phases from calibrated TDR object
        (function(calibrated.tdr) {
          data.frame(Pressure = calibrated.tdr@tdr@depth, 
                     Surface = surface,
                     DiveIdInit = calibrated.tdr@dive.activity$dive.id, 
                     DivePhase = calibrated.tdr@dive.phases)
        }) %>%
        # Rejoin to original data
        cbind(fastlog, .) %>%
        #Recalibrate DiveIds
        (function(tdr) {
          # Calculate DiveIds across all records
          diveTable <- tdr %>%
            filter(EventId > 0,
                   DiveIdInit > 0) %>%
            group_by(EventId, DiveIdInit) %>%
            summarize %>%
            as.data.frame
          diveTable$DiveId <- seq_along(diveTable[,1])
          # Merge tdr with dive table
          merge(tdr, diveTable, all.x = TRUE) %>%
            select(-DiveIdInit) %>%
            mutate(DiveId = ifelse(is.na(DiveId), 0, DiveId))
        }) %>%
        arrange(UTC) -> result
      attr(result, 'DeployID') <- deployid
      result
    }) 
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

plot.dive <- function(tdr, diveid, dive_thr = .5) {
  expand.range <- function(rng, delta) rng + c(-delta, delta)
  `%between%` <- function(x, rng) x >= rng[1] & x <= rng[2]
  pressure.range <- function(pressure) range(pressure) %>% pmin(c(0, Inf)) %>% rev
  dive_UTC_rng <- range(filter(tdr, DiveId == diveid)$UTC)
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  eventid <- filter(tdr, DiveId == diveid)$EventId[1]
  png(filename = file.path('dive_identification',
                           '4_plots',
                           sprintf('%i_%i_%i.png', deployid, eventid, diveid)),
      width = 1600, height = 900)
  par(ps = 20, cex = 1)
  layout(matrix(c(1, 1, 1, 2, 2), 1, 5))
  with(filter(tdr, EventId == eventid, UTC %between% expand.range(dive_UTC_rng, 1)), {
    plot(UTC, Pressure,
         ylim = pressure.range(Pressure),
         pch = 16,
         cex = 2,
         col = ifelse(DiveId == diveid, 'blue', 'red'),
         xaxt = 'n',
         main = sprintf('Field ID = %s, Deploy ID = %i\nEvent ID = %i, Dive ID = %i', 
                        metadata$FieldID, deployid, eventid, diveid),
         sub = sprintf('Threshold = %im', metadata$Threshold),
         xlab = '',
         ylab = 'Depth (m)')
    lines(UTC, Pressure, col = '#888888')
    abline(h = dive_thr, col = 'blue', lty = 2)
    axis(4)
    axis.POSIXct(1, 
                 at = seq(from = min(UTC), to = max(UTC), length.out = 6), 
                 format = '%m/%d %H:%M:%OS1')
    grid()
  })
  with(filter(tdr, EventId == eventid), {
    plot(UTC, PressureInit,
         ylim = pressure.range(c(Pressure, PressureInit)),
         pch = 16,
         col = 'red',
         cex = 2,
         xaxt = 'n',
         xlab = '',
         ylab = '')
    lines(UTC, PressureInit, col = 'red')
    points(UTC, Pressure, pch = 16, cex = 2, col = 'green')
    lines(UTC, Pressure, col = 'green')
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
}

kitandkaboodle <- function() {
  foreach(did = metadata$DeployID, .combine = c) %do% {
    # Initialize and calibrate TDR data
    calib.tdr <- initialize.tdr(did) %>%
      calibrate.tdr
    
    # Analyze dives and write to file
    dives <- analyze.dives(calib.tdr)
    write.csv(dives,
              file.path('dive_identification',
                        '3_dive_data',
                        sprintf('%i.CSV', did)),
              row.names = FALSE)
    
    # Plot dive
    sapply(dives$DiveID, function(diveid) plot.dive(tdr = calib.tdr, diveid = diveid))
    
    did
  }
}