library('dplyr')
library('ggplot2')
library('gridExtra')

# Create list of all fastlogs
get.fastlogs <- function(did) {
  tryCatch({
    read.csv('dive_identification/TDRmetadata.csv') %>%
      filter(DeployID == did) %>%
      (function(deployment) {
        file.path('dive_identification',
                  '2_tdr_data',
                  sprintf(fmt = '%s.CSV', deployment$TDRFilename))
      }) %>%
      read.csv %>%
      filter(EventId > 0) %>%
      group_by(EventId) %>%
      mutate(UTC = as.POSIXct(UTC, tz = 'UTC') + .05,
             Speed = (lead(Pressure) - Pressure) / as.numeric(difftime(lead(UTC), UTC, units = 'secs')),
             Acceleration = (lead(Speed) - Speed) / as.numeric(difftime(lead(UTC), UTC, units = 'secs')),
             RawPressure = Pressure,
             Pressure = ifelse((!is.na(Speed) & abs(Speed) > 10) | (!is.na(Acceleration) & Acceleration > 100), 
                               NA,
                               RawPressure))
  }, error = function(e) {
    browser()
  })
}
get.fastlog <- function(did, eid) {
  get.fastlogs(did) %>%
    ungroup %>%
    filter(EventId == eid)
}
fastlog.summary <- function(deployid) {
  get.fastlogs(deployid) %>%
      summarize(DeployId = deployid,
                N = n(),
                maxDepth = max(Pressure, na.rm = TRUE),
                depthRange = maxDepth - min(Pressure, na.rm = TRUE),
                avgspeed = mean(abs(Speed), na.rm = TRUE),
                magspeed = sqrt(sum(Speed^2, na.rm = TRUE)),
                duration = difftime(max(UTC), min(UTC), units = 'secs') %>% as.numeric)
}

fastlogs <- if(exists('fastlogs')) { 
  fastlogs
} else {
  read.csv('dive_identification/TDRmetadata.csv') %>%
    rowwise %>%
    do(fastlog.summary(.$DeployID)) %>%
    filter(N > 4,
           depthRange >= .5) %>%
    ungroup %>%
    mutate(durationgroup = cut(duration, breaks = c(0, 2, 10, Inf), labels = c('short', 'medium', 'long')),
           avgspeedgroup = cut(avgspeed, breaks = c(0, 1, 10, Inf), labels = c('a', 'b', 'c')),
           magspeedgroup = cut(magspeed, breaks = c(0, 10, 50, Inf), labels = c('x', 'y', 'z')))
}

samplebursts <- if(exists('samplebursts')) {
  samplebursts
} else {
  fastlogs %>% 
    filter(duration < 1e3) %>%
    group_by(durationgroup, avgspeedgroup) %>%
    sample_n(10) %>%
    ungroup
}

plot.fastlog <- function(fl) {
  did <- fl$DeployId
  eid <- fl$EventId
  
  fastlog <- get.fastlog(did, eid) %>%
    mutate(PressureCalibMin = Pressure - min(Pressure, na.rm = TRUE),
           PressureCalibMedian = Pressure - median(Pressure, na.rm = TRUE),
           RawPressureCalibMin = RawPressure - min(Pressure, na.rm = TRUE),
           RawPressureCalibMedian = RawPressure - median(Pressure, na.rm = TRUE))
  
  plot.subset <- function(lower, upper, i) {
    limits <- c(lower, upper)
    attr(limits, 'tzone') <- 'UTC'
    
    sprintf('dive_identification/7_calibration_plots/%s_%s_%i_%i(%i).png', fl$durationgroup, fl$magspeedgroup, did, eid, i) %>%
     png(width = 1800, height = 900, filename = .)
    
    metadata <- read.csv('dive_identification/TDRmetadata.csv') %>%
      filter(DeployID == did)
    
    utc.breaks <- function(limits) {
      ORIGIN <- as.POSIXct('1970-01-01 00:00.00', tz = 'UTC')
      seq(from = limits[1] %>% as.numeric %>% floor,
          to = limits[2] %>% as.numeric %>% ceiling,
          length = 6) %>%
        as.POSIXct(tz = 'UTC', origin = ORIGIN) + .05
    }
    
    rawplot <- ggplot(filter(fastlog, !is.na(Pressure)),
                      aes(x = UTC,
                          y = Pressure)) +
      geom_line(size = .75,
                color = '#e41a1c') +
      geom_point(size = 4,
                 color = '#e41a1c') +
      geom_point(data = fastlog,
                 aes(x = UTC,
                     y = RawPressure),
                 size = 2,
                 color = '#e41a1c50') +
      geom_line(data = fastlog,
                aes(x = UTC,
                    y = RawPressure),
                size = .75,
                color = '#e41a1c50') +
      scale_x_datetime(breaks = utc.breaks, limits = limits) +
      scale_y_reverse() +
      guides(color = FALSE, size = FALSE) +
      labs(title = sprintf('%s (%i)\nDeploy ID: %i Event ID: %i\n%s: %f3;%f3', metadata$FieldID, i, did, eid, fl$cluster2, fl$duration, fl$magspeed))
    
    minplot <- ggplot(filter(fastlog, !is.na(Pressure)),
                      aes(x = UTC,
                          y = PressureCalibMin)) +
      geom_line(size = .75,
                color = '#377eb8') +
      geom_point(size = 4,
                 color = '#377eb8') +
      geom_point(data = fastlog,
                 aes(x = UTC,
                     y = RawPressureCalibMin),
                 size = 2,
                 color = '#377eb850') +
      geom_line(data = fastlog,
                 aes(x = UTC,
                     y = RawPressureCalibMin),
                 size = .75,
                 color = '#377eb850') +
      scale_x_datetime(breaks = utc.breaks, limits = limits) +
      scale_y_reverse()
    
    fastlog$UTC2 <- seq_along(fastlog$UTC)
    pressurelm <- lm(PressureCalibMedian ~ UTC2, fastlog)
    medianplot <- ggplot(filter(fastlog, !is.na(Pressure)),
                         aes(x = UTC2,
                             y = PressureCalibMedian)) +
      geom_line(size = .75,
                color = '#377eb8') +
      geom_point(size = 4,
                 color = '#377eb8') +
      geom_point(data = fastlog,
                 aes(x = UTC2,
                     y = RawPressureCalibMedian),
                 size = 2,
                 color = '#377eb850') +
      geom_line(data = fastlog,
                 aes(x = UTC2,
                     y = RawPressureCalibMedian),
                 size = .75,
                 color = '#377eb850') +
      stat_smooth(method = 'lm') +
      #scale_x_datetime(breaks = utc.breaks, limits = limits) +
      scale_y_reverse() +
      guides(color = FALSE, size = FALSE) +
      labs(title = sprintf('Slope = %3f, R^2 = %3f', coefficients(pressurelm)[2], summary(pressurelm)$r.squared))
    
    speedplot <- ggplot(filter(fastlog, !is.na(Speed)),
                         aes(x = UTC,
                             y = Speed)) +
      geom_line(size = .75,
                color = '#984ea3') +
      geom_point(size = 4,
                 color = '#984ea3') +
      geom_point(data = fastlog,
                 aes(x = UTC,
                     y = Speed),
                 size = 2,
                 color = '#984ea350') +
      geom_line(data = fastlog,
                aes(x = UTC,
                    y = Speed),
                size = .75,
                color = '#984ea350') +
      scale_x_datetime(breaks = utc.breaks, limits = limits) +
      scale_y_reverse() +
      guides(color = FALSE, size = FALSE)
    
    accelplot <- ggplot(filter(fastlog, !is.na(Acceleration)),
                        aes(x = UTC,
                            y = Acceleration)) +
      geom_line(size = .75,
                color = '#4daf4a') +
      geom_point(size = 4,
                 color = '#4daf4a') +
      geom_point(data = fastlog,
                 aes(x = UTC,
                     y = Acceleration),
                 size = 2,
                 color = '#4daf4a50') +
      geom_line(data = fastlog,
                aes(x = UTC,
                    y = Acceleration),
                size = .75,
                color = '#4daf4a50') +
      scale_x_datetime(breaks = utc.breaks, limits = limits) +
      scale_y_reverse() +
      guides(color = FALSE, size = FALSE)
    
    grid.arrange(rawplot, 
                 if(fl$durationgroup == 'short') { minplot } else { medianplot }, 
                 speedplot,
                 accelplot,
                 nrow = 2, ncol = 2)
    
    dev.off()
  }
  
  all.limits <- seq(min(fastlog$UTC), max(fastlog$UTC), by = '10 secs') %>% c(max(fastlog$UTC))
  attr(all.limits, 'tzone') <- 'UTC'
  sapply(seq_along(all.limits)[-1], function(i) plot.subset(all.limits[i - 1], all.limits[i], i - 1))
  
  data.frame(DeployID = did, EventID = eid, Duration = fl$durationgroup, MagSpeed = fl$magspeedgroup)
}

plot.samples <- function() {
  samplebursts %>%
    rowwise %>%
    do(plot.fastlog(.))
}

plot.mediums <- function() {
  fastlogs %>%
    filter(durationgroup == 'medium') %>%
    rowwise %>%
    do(plot.fastlog(.))
}

fastlog.summary2 <- function(did, eid) {
  fastlog <- get.fastlog(did, eid) %>%
    mutate(DeployID = did)
  
  fastlog$UTC2 <- seq_along(fastlog$UTC)
  pressurelm <- lm(Pressure ~ UTC2, fastlog)
  
  fastlog %>%
    group_by(DeployID, EventId) %>%
    summarize(duration = difftime(max(UTC), min(UTC), units = 'secs') %>% as.numeric,
              slope = coefficients(pressurelm)[2], 
              r.squared = summary(pressurelm)$r.squared,
              medianpressure = median(Pressure, na.rm = TRUE),
              minpressure = min(Pressure, na.rm = TRUE),
              offsetdiff = medianpressure - minpressure)
}

min.vs.median <- function() {
  min_events <- dir('dive_identification/7_calibration_plots/min or median/min/') %>%
    sub('medium_[xyz]_([0-9]+_[0-9]+).*', '\\1', .) %>%
    strsplit('_') %>%
    unlist %>%
    matrix(nrow = 101, ncol = 2, byrow = TRUE) %>%
    as.data.frame %>%
    rowwise %>%
    do(fastlog.summary2(did = .$V1, eid = .$V2)) %>%
    ungroup %>%
    mutate(CalibType = 'Min')
  
  median_events <- dir('dive_identification/7_calibration_plots/min or median/median/') %>%
    sub('medium_[xyz]_([0-9]+_[0-9]+).*', '\\1', .) %>%
    strsplit('_') %>%
    unlist %>%
    matrix(nrow = 78, ncol = 2, byrow = TRUE) %>%
    as.data.frame %>% 
    rowwise %>%
    do(fastlog.summary2(did = .$V1, eid = .$V2)) %>%
    ungroup %>%
    mutate(CalibType = 'Median')
  
  CalibTypeOracle <- rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType))
  
  ggplot(CalibTypeOracle,
           aes(x = duration,
               color = CalibType)) +
    geom_density()
  
  rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType)) %>% 
    ggplot(aes(x = slope,
               color = CalibType)) +
    geom_density()
  
  rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType)) %>% 
    ggplot(aes(CalibType,
               duration)) +
    geom_boxplot()
  
  rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType)) %>% 
    ggplot(aes(CalibType, 
               slope)) +
    geom_boxplot()
  
  rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType)) %>% 
    ggplot(aes(CalibType,
               r.squared)) +
    geom_boxplot()
  
  rbind(min_events, median_events) %>% 
    mutate(CalibType = factor(CalibType)) %>% 
    select(duration, slope, r.squared) %>%
    plotmatrix
}

duration.overlap <- function(threshold) {
  CalibTypeOracle %>%
    group_by(CalibType) %>%
    summarize(sum(duration > threshold) / n()) %>%
    (function(tbl) 1 - tbl[1, 2] + tbl[2, 2]) %>%
    as.numeric
}

slope.overlap <- function(threshold) {
  CalibTypeOracle %>%
    group_by(CalibType) %>%
    summarize(sum(slope > threshold) / n()) %>%
    (function(tbl) 1 - tbl[1, 2] + tbl[2, 2]) %>%
    as.numeric
}

threshold.fit <- function(m.b) {
  m <- m.b[1]
  b <- m.b[2]
  CalibTypeOracle %>%
    group_by(CalibType) %>%
    summarize(sum(slope > m * duration + b) / n()) %>%
    (function(tbl) 1 - tbl[1, 2] + tbl[2, 2]) %>%
    as.numeric
}
slope_duration_threshold <- optim(c(-.02, .05), threshold.fit)$par
ggplot(CalibTypeOracle, aes(x = duration, 
                            y = slope, 
                            color = CalibType)) + 
  geom_point() + 
  geom_abline(slope = slope_duration_threshold[1], 
              intercept = slope_duration_threshold[2])

CalibTypeOracle %>%
  mutate(y = slope_duration_threshold[1] * duration + slope_duration_threshold[2],
         invalid = (CalibType == 'Median' & slope < y) | (CalibType == 'Min' & slope > y)) %>%
  filter(invalid) %>%
  View

duration.threshold <- optimize(duration.overlap, c(3, 6))$minimum
slope.threshold <- optimize(slope.overlap, c(-.05, 0))$minimum
