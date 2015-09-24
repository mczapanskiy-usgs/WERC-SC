library('dplyr')
library('foreach')
library('diveMove')
library('iterators')

### RFBO ###

# Read RFBO metadata
read.csv('dive_identification/metadata_all_GPS_05.28.15_working.csv', stringsAsFactors = FALSE) %>% 
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M')) %>% 
  filter(Species == 'RFBO', Island == 'Kauai') %>%
  group_by(Deploy_ID, FieldID, Species) %>%
  summarize(deployed = min(UTC), 
            recovered = max(UTC),
            period = difftime(max(UTC), min(UTC), units = 'days') %>% as.numeric,
            ValidGpsTdr = any(TDR_TagRecov == 1 && GPS_TagRecov == 1), 
            TDR_File = paste(TDR_File, collapse = '')) %>% 
  filter(ValidGpsTdr) -> RFBO_metadata

# Iterate through each TDR file and plot dives using both surface detection methods (median vs. linear model)
foreach(rfbo_tdr = dir('dive_identification/rfbo_data2/', full.names = TRUE)) %do% {
  deploy_metadata <- filter(RFBO_metadata, TDR_File == rfbo_tdr %>% basename %>% sub('.CSV', '', .))
  
  # Set up folder for plots
  plot_path <- file.path('dive_identification', 'rfbo_dive_plots', paste0(deploy_metadata$Deploy_ID, deploy_metadata$FieldID))
  dir.create(plot_path, showWarnings = FALSE)
  
  # Read TDR CSV
  tdr_data <- read.csv(rfbo_tdr, stringsAsFactors = FALSE) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC')) %>%
    filter(UTC > deploy_metadata$deployed,
           UTC < deploy_metadata$recovered)
  
  # Find linear model for background pressure sampling
  drift_lm <- lm(Pressure ~ UTC, filter(tdr_data, EventId == 0))
  
  # Find dives in each Fast Log
  foreach(event = unique(tdr_data$EventId), .combine = rbind) %do% {
    fast_log <- filter(tdr_data, EventId == event)
    tdr <- with(fast_log, 
                createTDR(UTC, 
                          Pressure, 
                          dtime = .1, 
                          file = rfbo_tdr))
    
    # Calculate surface offsets
    surface_offset_median <- median(fast_log$Pressure)
    surface_offset_lm <- predict(drift_lm, data.frame(UTC = mean(fast_log$UTC)))
    
    # Use diveMove to find dives
    calib_median <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .5,
                             zoc.method = 'offset', offset = surface_offset_median)
    calib_lm <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .5,
                                zoc.method = 'offset', offset = surface_offset_lm)
    
    pressure_range <- range(c(fast_log$Pressure, calib_median@tdr@depth, calib_lm@tdr@depth))
    
    plot_dives <- function(tdr_data, zoc_method, zoc) {
      with(tdr_data, {
        plot(UTC,
             Pressure,
             col = ifelse(Dive, 'blue', 'red'),
             sub = sprintf('%s ZOC: %.3f',zoc_method, zoc),
             ylim = rev(pressure_range))
      })
    }
    png(file.path(plot_path, paste0(event, '.png')), width = 1080, height = 1080)
    par(mfrow=c(3,1)) 
    with(fast_log, {
         plot(UTC,
              Pressure,
              main = sprintf('Fast Log %i\nFID: %s DID: %s', event, deploy_metadata$FieldID, deploy_metadata$Deploy_ID),
              sub = 'No ZOC',
              ylim = rev(pressure_range))
    })
    plot_dives(data.frame(UTC = calib_median@tdr@time, 
                          Pressure = calib_median@tdr@depth,
                          Dive = calib_median@dive.activity$dive.id >0), 
               'Median surface', 
               surface_offset_median)
    plot_dives(data.frame(UTC = calib_lm@tdr@time, 
                          Pressure = calib_lm@tdr@depth,
                          Dive = calib_lm@dive.activity$dive.id >0), 
               'Linear regression', 
               surface_offset_lm)
    dev.off()
  }
}

### End RFBO ###


# Iterate through data folder
foreach(tdr_file = dir('dive_identification/data2/', full.names = TRUE)) %do% {
  # Read TDR CSV
  tdr_data <- read.csv(tdr_file, stringsAsFactors = FALSE) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC'))
  
  # Extract species and band suffix from file name
  species <- regmatches(tdr_file, regexpr('BRBO|RFBO|RTTR', tdr_file))
  bandno <- sub('.*([0-9]{6}).CSV', '\\1', tdr_file)
  
  # Create folder for plots
  plot_path <- file.path('dive_identification', 'plots2', paste(species, bandno, sep = '_'))
  dir.create(plot_path)
  
  # Iterate through valid fast log events (more than zero points)
  foreach(event = unique(tdr_data$EventId)) %do% {
    # Create a png to save to
    png(filename = file.path(plot_path, paste0(event, '.PNG')), 
        width = 900,
        height = 600)
    
    # Create the plot
    with(filter(tdr_data, EventId == event), {
      plot(UTC, Pressure,
           ylim = c(4,-1),
           cex = .5,
           main = sprintf('%s %s\nFast Log %i', species, bandno, event))
      lines(UTC, Pressure)
      abline(h = 1, col = 'red', lty = 2)
      abline(h = median(Pressure), col = 'green', lty = 2)
      abline(h = 1 + median(Pressure), col = 'blue', lty = 2)
    })
    dev.off()
  }
}

foreach(tdr_file = dir('dive_identification/data2/', pattern = 'WTSH', full.names = TRUE)) %do% {
  WTSH_csv <- read.csv(tdr_file, stringsAsFactors = FALSE) %>% mutate(UTC = as.POSIXct(UTC, tz = 'UTC'))
  foreach(event = unique(WTSH_csv$EventId), .combine = rbind) %do% {
    #eventid  records	max_depth	surface_offset	adj_max_depth	1m_dives	1m_adj_dives	.75m_adj_dives	.5m_adj_dives
    burst <- filter(WTSH_csv, EventId == event)
    if(nrow(burst) == 4) return(NULL)
    tdr <- with(burst, 
                createTDR(UTC, 
                          Pressure, 
                          dtime = .1, 
                          file = tdr_file))
    surface_offset <- median(burst$Pressure)
    calib1 <- calibrateDepth(tdr, wet.thr = 0, dive.thr = 1,
                             zoc.method = 'offset', offset = 0)
    calib1adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = 1,
                             zoc.method = 'offset', offset = surface_offset)
    calib.75adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .75,
                               zoc.method = 'offset', offset = surface_offset)
    calib.5adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .5,
                              zoc.method = 'offset', offset = surface_offset)
    
    data.frame(EventId = event,
               records = nrow(burst), 
               max_depth = max(burst$Pressure),
               surface_offset = surface_offset,
               adj_max_depth = max(burst$Pressure) + surface_offset,
               dives_1 = length(calib1@dive.models),
               dives_1adj = length(calib1adj@dive.models),
               dives_.75adj = length(calib.75adj@dive.models),
               dives_.5adj = length(calib.5adj@dive.models))
  } -> WTSH_breakdown
  
  plot_path <- file.path('dive_identification', 'plots3')
  png(file.path(plot_path, tdr_file %>% basename %>% sub('.CSV', '.png', .)),
      height = 400,
      width = 600)
  barplot(colSums(BRBO010_breakdown[6:9]),
          beside = TRUE,
          main = tdr_file %>% basename %>% sub('.CSV', '', .))
  dev.off()
}

tdr_file <- 'dive_identification/data2/HOO_WTSH_029_28077_A10369_091914.CSV'
WTSH_csv <- read.csv(tdr_file, stringsAsFactors = FALSE) %>% mutate(UTC = as.POSIXct(UTC, tz = 'UTC'))
WTSH_breakdown <- WTSH_csv %>%
  group_by(EventId) %>%
  summarize(records = n(),
            max_depth = max(Pressure),
            suface_offset = median(Pressure)) %>%
  tail(-1)

# WTSH Metadata
read.csv('dive_identification/metadata_all_GPS_05.28.15_working.csv', stringsAsFactors = FALSE) %>% 
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M')) %>% 
  group_by(Deploy_ID, FieldID, Species) %>% 
  summarize(deployed = min(UTC), 
            recovered = max(UTC),
            period = difftime(max(UTC), min(UTC), units = 'days') %>% as.numeric,
            ValidTDR = any(TDR_TagRecov == 1), 
            TDR_File = paste(TDR_File, collapse = '')) %>% 
  filter(ValidTDR) -> WTSH_metadata

# WTSH Pressure Drift
foreach(wtsh_file = dir('dive_identification/wtsh_data2/', full.names = TRUE), .combine = rbind) %do% {
  metadata <- WTSH_metadata %>%
    filter(TDR_File == wtsh_file %>% basename %>% sub('.csv', '', ., ignore.case = TRUE))
  tdr_data <- read.csv(wtsh_file, stringsAsFactors = FALSE) %>%
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC'),
           tad = difftime(UTC, metadata$deployed, units = 'days')) %>%
    filter(EventId == 0,
           UTC > metadata$deployed,
           UTC < metadata$recovered)
  drift_model <- lm(Pressure ~ tad, data = tdr_data)
  data.frame(Deploy_ID = metadata$Deploy_ID,
             deployed = metadata$deployed, 
             recovered = metadata$recovered, 
             duration = difftime(metadata$recovered, metadata$deployed, units = 'days'),
             min_pressure = min(tdr_data$Pressure),
             max_pressure = max(tdr_data$Pressure),
             b = coef(drift_model)[1],
             m = coef(drift_model)[2])
} -> WTSH_pressure_drift
with(WTSH_pressure_drift, {
  plot(x = 0,
       y = 0,
       xlim = c(0, max(duration)),
       ylim = rev(range(c(min_pressure, max_pressure))),
       pch = NA_integer_,
       main = 'Pressure drift in WTSH TDRs',
       xlab = 'Days after deployment',
       ylab = 'Pressure')
  axis(side = 4)
  grid()
  })
foreach(WTSH = iter(WTSH_pressure_drift, by = 'row')) %do% {
  with(WTSH, {
    lines(c(0, duration), 
          m * c(0, duration) + b,
          lwd = 1)
  })
}

# WTSH Depth Distribution Over Time
foreach(wtsh_file = dir('dive_identification/wtsh_data2/', full.names = TRUE), .combine = rbind) %do% {
  metadata <- WTSH_metadata %>%
    filter(TDR_File == wtsh_file %>% basename %>% sub('.csv', '', ., ignore.case = TRUE))
  read.csv(wtsh_file, stringsAsFactors = FALSE) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC')) %>%
    filter(EventId > 0,
           UTC > metadata$deployed,
           UTC < metadata$recovered) %>%
    group_by(EventId) %>%
    summarize(records = n(),
              max_depth = max(Pressure),
              UTC = min(UTC),
              TDR_File = basename(wtsh_file) %>% sub('.csv', '', ., ignore.case = TRUE)) %>%
    filter(records > 4,
           max_depth >= 1) %>%
    mutate(tad = difftime(UTC, metadata$deployed, units = 'days') %>% as.numeric)
} -> WTSH_depth_distribution
depth_dist_model <- lm(max_depth ~ tad, WTSH_depth_distribution)
plot(max_depth ~ tad, 
     data = WTSH_depth_distribution,
     main = 'WTSH Dive Detection Bias',
     sub = with(summary(depth_dist_model), sprintf('R^2 = %.5f  p-value = %.5f', r.squared, pf(fstatistic[1], fstatistic[2], fstatistic[3], lower.tail = FALSE))),
     xlab = 'Days after deployment',
     ylab = 'Max depth (m)')
abline(depth_dist_model)

# WTSH Dive Distribution Over Time
dive_list <- lapply(unique(WTSH_depth_distribution$TDR_File), 
                    function(tdr) filter(WTSH_depth_distribution, TDR_File == tdr))
dive_list_order <- sapply(dive_list, function(dives) max(dives$tad)) %>% order
dive_list <- dive_list[dive_list_order]
boxplot(dive_list,
        horizontal = TRUE,
        main = 'Dive distribution per tag',
        xlab = 'Days after deployment',
        ylab = 'Tag')
WTSH_depth_distribution %>%
  #filter(tad < 6) %>%
  mutate(bucket = tad %/% .5) %>% 
  group_by(bucket) %>%
  summarize(dives_per_tag = n() / n_distinct(TDR_File)) -> WTSH_dive_distribution

foreach(event = unique(WTSH_csv$EventId)[-1], .combine = rbind) %do% {
  #eventid  records  max_depth	surface_offset	adj_max_depth	1m_dives	1m_adj_dives	.75m_adj_dives	.5m_adj_dives
  burst <- filter(WTSH_csv, EventId == event)
  if(nrow(burst) == 4) return(NULL)
  tdr <- with(burst, 
              createTDR(UTC, 
                        Pressure, 
                        dtime = .1, 
                        file = tdr_file))
  surface_offset <- median(burst$Pressure)
  calib1 <- calibrateDepth(tdr, wet.thr = 0, dive.thr = 1,
                           zoc.method = 'offset', offset = 0)
  calib1adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = 1,
                              zoc.method = 'offset', offset = surface_offset)
  calib.75adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .75,
                                zoc.method = 'offset', offset = surface_offset)
  calib.5adj <- calibrateDepth(tdr, wet.thr = 0, dive.thr = .5,
                               zoc.method = 'offset', offset = surface_offset)
  
  browser()
  
  data.frame(EventId = event,
             records = nrow(burst), 
             max_depth = max(burst$Pressure),
             surface_offset = surface_offset,
             adj_max_depth = max(burst$Pressure) + surface_offset,
             dives_1 = length(calib1@dive.models),
             dives_1adj = length(calib1adj@dive.models),
             dives_.75adj = length(calib.75adj@dive.models),
             dives_.5adj = length(calib.5adj@dive.models))
} -> WTSH_breakdown

dive_of_interest <- 152
BRBO010_tdr <- with(filter(BRBO010_csv, EventId == dive_of_interest), 
                    createTDR(UTC, 
                              Pressure, 
                              dtime = .1, 
                              file = 'dive_identification/data2/LEH2014BRBO010_A10343_061614.CSV'))
BRBO010_calib <- calibrateDepth(BRBO010_tdr, zoc.method = 'offset', offset = median(BRBO010_csv$Pressure))
