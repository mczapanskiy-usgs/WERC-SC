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
calibrate.tdr <- function(tdr) {
  tdr.calib <- tdr %>%
    rename(PressureInit = Pressure) %>%
    filter(EventId > 0) %>%
    ddply(.variables = .(EventId),
          .fun = function(fastlog) {
            mutate(fastlog, 
                   Surface = median(PressureInit),
                   Pressure = pmax(PressureInit - Surface, 0),
                   Dive = Pressure >= 1,
                   Speed = c(diff(Pressure) / (UTC %>% diff %>% round(1) %>% as.numeric), NA))
          })
}

test_deploy_id %>% initialize.tdr() %>% calibrate.tdr() -> test_tdr
test_tdr %>%
  group_by(EventId) %>%
  summarize(records = n(),
            max_depth = max(Pressure)) %>%
  arrange(desc(records)) %>%
  View

verify.tdr <- function()

x11_scale = 1.5
x11(width = 11 * x11_scale, height = 7 * x11_scale)
par(mfrow = c(2,1))
with(filter(test_tdr, EventId == 322), {
  plot(UTC,
       Pressure,
       col = ifelse(Dive, 'blue', 'red'),
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
with(filter(test_tdr, EventId == 322), {
  plot(UTC,
       Speed,
       pch = 16,
       xaxt = "n",
       xlab = "") 
  lines(UTC, Speed)
  axis.POSIXct(1, UTC, format = "%m/%d %H:%M:%OS1")
  axis(4)
  grid()
  abline(h = 0, col = '#AAAAAA', lty = 2)
  abline(h = 1, col = '#AAAAAA', lty = 2)
})
dev.off()
