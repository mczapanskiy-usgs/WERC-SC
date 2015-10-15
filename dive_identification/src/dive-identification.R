library('plyr')
library('dplyr')

test_deploy_id = 341 # LEH2014RFBO004A

# TDR Initialization
# Read metadata
# Read TDR CSV
# Truncate by deploy and recover times
initialize.tdr <- function(deploy_id) {
  metadata <- read.csv('dive_identification/metadata_all_GPS_05.28.15_working.csv', stringsAsFactors = FALSE) %>% 
    filter(Deploy_ID == deploy_id) %>%
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M')) %>% 
    group_by(Deploy_ID, FieldID, Species) %>%
    summarize(deployed = min(UTC), 
              recovered = max(UTC),
              period = difftime(max(UTC), min(UTC), units = 'days') %>% as.numeric,
              TDR_File = paste(TDR_File, collapse = ''))

  tdr.folder <- sprintf("%s_data2", tolower(metadata$Species))
  tdr.file <- sprintf("%s.CSV", metadata$TDR_File)
  tdr.path <- file.path('dive_identification', tdr.folder, tdr.file)
  tdr <- read.csv(tdr.path, stringsAsFactors = FALSE) %>% 
    mutate(UTC = as.POSIXct(UTC, tz = 'UTC')) %>%
    filter(UTC > metadata$deployed,
           UTC < metadata$recovered)
}

# TDR Calibration
# ZOC pressure using median pressure as surface (per fast log)
# Assign phases based on dive threshold (defaults to 1m)
calibrate.tdr <- function(tdr, depth_thr = 1) {
  tdr.calib <- tdr %>%
    rename(PressureInit = Pressure) %>%
    filter(EventId > 0) %>%
    ddply(.variables = .(EventId),
          .fun = function(fastlog) {
            mutate(fastlog, 
                   Surface = median(PressureInit),
                   Pressure = pmax(PressureInit - Surface, 0),
                   Submerged = Pressure >= depth_thr,
                   Speed = c(diff(Pressure) / (UTC %>% diff %>% round(1) %>% as.numeric), NA))
          })
}

# Dive Identification
# A dive is two or more successive points beneath the dive threshold (1m)
identify.dives <- function(tdr) {
  dive_match <- gregexpr("1{2,}", fastlog$Submerged %>% ifelse(1, 2) %>% paste(collapse = ''))
  tdr$DiveIdx <- 0
  mapply(function(i, l, d) fastlog$DiveIdx[i:(i+l-1)] <<- d, 
         dive_match[[1]], 
         attr(dive_match[[1]], 'match.length'), 
         seq_along(dive_match[[1]]))
}

test_deploy_id %>% initialize.tdr() %>% calibrate.tdr() -> test_tdr
test_tdr %>%
  group_by(EventId) %>%
  summarize(records = n(),
            max_depth = max(Pressure)) %>%
  arrange(desc(records)) %>%
  View

fastlog <- filter(test_tdr, EventId == 302)

with(filter(test_tdr, EventId == 302), {
  # FastLog plot
  plot(UTC,
       Pressure,
       col = ifelse(Submerged, 'blue', 'red'),
       pch = 16,
       ylim = rev(range(Pressure)),
       xaxt="n",
       xlab = "") 
  lines(UTC, Pressure)
  axis.POSIXct(1, UTC, format = "%m/%d %H:%M:%OS1")
  axis(4)
  grid()
  abline(h = 0, col = '#AAAAAA', lty = 2)
  abline(h = 1, col = '#AAAAAA', lty = 2)
})

x11_scale <- 1.5
x11(width = 11 * x11_scale, height = 7 * x11_scale)
fastlog <- 
with(fastlog, {
  plot_fastlog(fastlog) 
  
  # Zoom 
  xlim_orig <- par("usr")[1:2]
  flags <- NULL
  is_zoomed <- FALSE
  usrxy <- function(x, y) {
    origin <- as.POSIXct('1970-01-01 00:00.00', tz = 'UTC')
  }
  newxlim <- function(x) {
    origin <- as.POSIXct('1970-01-01 00:00.00', tz = 'UTC')
    clickx <- as.POSIXct(xlim_orig[1] + x * diff(xlim_orig), origin = origin, tz = 'UTC')
    c(clickx - 10, clickx + 10)
  }
  setGraphicsEventHandlers(prompt = "Click to zoom, right click to flag, q to quit",
                           onMouseUp = function(buttons, x, y) {
                             if(buttons == 0) {
                               if(is_zoomed) {
                                 plot_fastlog(fastlog, newxlim(x))
                                 is_zoomed <<- TRUE
                               } else {
                                 plot_fastlog(fastlog)
                                 is_zoomed <<- FALSE
                               }
                             } else if(buttons == 2) {
                               
                               
                                        xlim_new[1] <<- newx(x, xlim_orig)
                                        click_mode <<- 2
                                      },
                                      {
                                        xlim_new[2] <<- newx(x, xlim_orig)
                                        click_mode <<- 1
                                        plot_fastlog(fastlog, xlim_new)
                                        xlim_orig <<- xlim_new
                                      })
                             else {
                               
                             }
                           },
                           onKeybd = function(key) {
                             
                           }
  locator(n = 2)
})
dev.off()
