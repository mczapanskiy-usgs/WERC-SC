library('dplyr')
library('foreach')
library('diveMove')
library('zoo')
library('ggplot2')
library('data.table')

metadata <- read.csv('trackcode/gps/metadata_all_GPS.csv') %>%
  filter(Species == 'PFSH') %>%
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M')) %>% 
  group_by(DeployID = Deploy_ID,
           FieldID,
           Species,
           Site = SubCol_Code) %>%
  summarize(Deployed = min(UTC, na.rm = TRUE),
            Recovered = max(UTC, na.rm = TRUE),
            TDRFile = paste(TDR_File, collapse = ''),
            ValidTDR = any(TDR_TagRecov %in% c(1, 3))) %>%
  ungroup %>%
  filter(ValidTDR) %>%
  select(-ValidTDR)

tdr.path <- function(deployid) {
  with(metadata %>%
         filter(DeployID == deployid),
       file.path('dive_identification',
                 '2_tdr_data',
                 sprintf('%s.CSV', TDRFile)))
}

initialize.tdr <- function(deployid) {
  metadata <- filter(metadata, DeployID == deployid)
  
  tdr <- read.csv(tdr.path(deployid)) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05) %>%
    filter(UTC > metadata$Deployed,
           UTC < metadata$Recovered)
  attr(tdr, 'DeployID') <- deployid
  tdr
}

calibrate.tdr <- function(tdr, surface_thr = .1, depth_thr = .2, dur_thr = .5) {
  if(nrow(tdr) == 0) return(tdr)
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  
  # Calibrate surface to minimum pressure
  surface.offset <- function(fastlog) {
    exception <- (read.csv('dive_identification/BRBO_surface_exceptions.csv') %>%
      filter(DID == deployid, EID == fastlog$EventId[1]) %>%
      nrow) == 1
    minoffset <- min(fastlog$Pressure, na.rm = TRUE)
    medianoffset <- if(any(fastlog$Pressure >= -1 & fastlog$Pressure <= 1, na.rm = TRUE)) {
      median(fastlog$Pressure[fastlog$Pressure >= -1 & fastlog$Pressure <= 1], na.rm = TRUE)
    } else {
      median(fastlog$Pressure, na.rm = TRUE)
    }
    if(exception) {
      medianoffset
    } else {
      minoffset
    }
  }
  
  # Filter out FastLogs shorter than duration threshold or depth range less than dive threshold
  valid_fastlogs <- tdr %>%
    filter(EventId > 0) %>% 
    group_by(EventId) %>%
    summarize(duration = difftime(max(UTC, na.rm = TRUE), min(UTC, na.rm = TRUE), units = 'secs') %>% as.numeric,
              depthRange = max(Pressure, na.rm = TRUE) - min(Pressure, na.rm = TRUE),
              N = n()) %>%
    filter(duration >= dur_thr,
           depthRange >= depth_thr,
           N > 4)
  
  get.fastlog.rate <- function() {
    cefas_file <- file.path('dive_identification',
                            '1_CEFAS_output',
                            sprintf('%s.CSV', metadata$TDRFile))
    readLines(cefas_file) %>%
      grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
      first %>%
      sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
      as.numeric
  }
  
  calibrate.fastlog <- function(fastlog) {
    surface <- surface.offset(fastlog)
    # Create a TDR object
    createTDR(fastlog$UTC,
              fastlog$Pressure,
              dtime = get.fastlog.rate(),
              file = tdr.path(deployid)) %>%
      # Calibrate TDR using min or median pressure as offset
      calibrateDepth(wet.thr = 0,
                     dive.thr = surface_thr,
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
      summarize(duration = difftime(max(UTC, na.rm = TRUE), min(UTC, na.rm = TRUE), units = 'secs') %>% as.numeric,
                maxdepth = max(CalibPressure, na.rm = TRUE)) %>%
      filter(duration >= dur_thr,
             maxdepth >= depth_thr) %>%
      as.data.frame
    diveTable$DiveId <- seq_along(diveTable[,1])
    # Merge tdr with dive table
    merge(tdr, diveTable, all.x = TRUE) %>%
      select(-DiveIdInit) %>%
      mutate(DiveId = ifelse(is.na(DiveId), 0, DiveId)) %>%
      arrange(UTC)
  }
  
  # Split by EventId and process FastLogs individually using diveMove
  if(nrow(valid_fastlogs) > 0) {
    result <- tdr %>%
      filter(EventId %in% valid_fastlogs$EventId) %>%
      group_by(EventId) %>%
      do(calibrate.fastlog(.)) %>%
      recalibrate.diveids
    attr(result, 'DeployID') <- deployid
    result
  } else {
    NULL
  }
}

plot.dive <- function(tdr, diveid, surface_thr = .1, depth_thr = .2) {
  expand.range <- function(rng, delta) rng + c(-delta, delta)
  `%between%` <- function(x, rng) x >= rng[1] & x <= rng[2]
  pressure.range <- function(pressure) range(pressure, na.rm = TRUE) %>% pmin(c(0, Inf)) %>% rev
  dive_UTC_rng <- range(filter(tdr, DiveId == diveid)$UTC)
  deployid <- attr(tdr, 'DeployID')
  metadata <- filter(metadata, DeployID == deployid)
  eventid <- filter(tdr, DiveId == diveid)$EventId[1]
  png(filename = file.path('dive_identification',
                           '5d_pfsh_dive_plots',
                           sprintf('%i_%i_%i.png', deployid, eventid, diveid)),
      width = 1200, height = 600)
  par(ps = 20, cex = 1)
  layout(matrix(c(1, 1, 1, 2, 2), 1, 5))
  with(filter(tdr, EventId == eventid, UTC %between% expand.range(dive_UTC_rng, 5)), {
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
    lines(UTC, CalibPressure,
          col = 'blue')
    abline(h = surface_thr, col = 'blue', lty = 2)
    axis(4)
    axis.POSIXct(1, 
                 at = seq(from = min(UTC), to = max(UTC), length.out = 6), 
                 format = '%m/%d %H:%M:%OS1')
    grid()
  })
  with(filter(tdr, EventId == eventid), {
    plot(UTC, Pressure,
         ylim = pressure.range(c(CalibPressure, Pressure)),
         pch = 16,
         col = 'red',
         cex = 2,
         xaxt = 'n',
         xlab = '',
         ylab = '')
    lines(UTC, Pressure, col = 'red')
    points(UTC, CalibPressure, pch = 16, cex = 2, col = 'green')
    lines(UTC, CalibPressure, col = 'green')
    abline(h = surface_thr, col = 'blue', lty = 2)
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
              MaxDepth = max(CalibPressure),
              N = n()) %>%
    select(DeployID:EventID, DiveID = DiveId, Begin:N)
}

kitandkaboodle <- function() {
  foreach(did = metadata$DeployID, .combine = c) %do% {
    tryCatch({
      # Initialize and calibrate TDR data
      calib.tdr <- initialize.tdr(did) %>%
        calibrate.tdr
    }, error = function(e) browser())
    
    if(!is.null(calib.tdr) && nrow(calib.tdr) > 0 ) {
      
      write.csv(calib.tdr %>%
                  mutate(UTC = format(UTC, '%Y-%m-%d %H:%M:%OS1')),
                file.path('dive_identification',
                          '3d_pfsh_calibrated_data',
                          sprintf('%i.CSV', did)))
    
      # Analyze dives and write to file
      dives <- analyze.dives(calib.tdr)
      write.csv(dives %>%
                  mutate(Begin = format(Begin, '%Y-%m-%d %H:%M:%OS1'),
                         End = format(End, '%Y-%m-%d %H:%M:%OS1')),
                file.path('dive_identification',
                          '4d_pfsh_dive_data',
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

post.analysis <- function() {
  valid_dives <- dir('dive_identification/5d_pfsh_dive_data_dive_plots/Dives/') %>% 
    sub(pattern = '.png', replacement = '', x = ., fixed = TRUE) %>%
    strsplit('_') %>% 
    unlist %>% 
    matrix(ncol = 3, byrow = TRUE) %>% 
    data.frame %>% 
    transmute(DeployID = X1, EventID = X2, DiveID = X3, Valid = TRUE)
  invalid_dives <- dir('dive_identification/5d_pfsh_dive_plots/NonDives/') %>% 
    sub(pattern = '.png', replacement = '', x = ., fixed = TRUE) %>%
    strsplit('_') %>% 
    unlist %>% 
    matrix(ncol = 3, byrow = TRUE) %>% 
    data.frame %>% 
    transmute(DeployID = X1, EventID = X2, DiveID = X3, Valid = FALSE)
  dive_validity <- rbind(valid_dives, invalid_dives) %>%
    arrange(DeployID, DiveID)
  dive_stats <- lapply(dir('dive_identification/4d_pfsh_dive_data/', full.names = TRUE), 
                       function(file) read.csv(file) %>% 
                         mutate(Begin = as.POSIXct(Begin, tz = 'UTC'), 
                                End = as.POSIXct(End, tz = 'UTC'))) %>% 
    rbindlist %>%
    merge.data.frame(dive_validity)
  dive_summary <- dive_stats %>%
    group_by(Valid) %>%
    summarize(N = n(),
              MeanDur = mean(Duration),
              MaxDur = max(Duration),
              MeanMaxDepth = mean(MaxDepth),
              MaxMaxDepth = max(MaxDepth))
  
  # Duration distribution
  ggplot(dive_stats, 
         aes(x = Duration + 1,
             color = Valid)) +
    geom_density() + 
    scale_x_continuous(name = 'log2(Duration)',
                       trans = 'log2', 
                       breaks = c(1.5, 2, 4, 8, 16))
  
  # Depth distribution
  ggplot(dive_stats, 
         aes(x = MaxDepth,
             color = Valid)) +
    geom_density() + 
    scale_x_continuous(name = 'MaxDepth')
  
  # Depth vs Duration
  ggplot(dive_stats, 
         aes(x = Duration + 1,
             y = MaxDepth,
             color = Valid)) +
    geom_point() +
    stat_density2d() + 
    scale_x_continuous(name = 'log2(Duration + 1)',
                       trans = 'log2', 
                       breaks = c(1, 2, 4, 8),
                       limits = c(1, 10)) +
    scale_y_continuous(limits = c(0, 6))
}
