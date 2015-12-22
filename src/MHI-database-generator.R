library(dplyr)
library(RSQLite)
library(foreach)
library(data.table)
library(adehabitatLT)
library(geosphere)

## Create SQLite Database for MHI data from CSV files
# Read metadata CSV file
OriginalMetadata <- read.csv('trackcode/gps/metadata_all_GPS.csv', stringsAsFactors = FALSE) %>%
  mutate(UTC = as.POSIXct(UTC, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric,
         GPS_programmed_start_datetime_local2 = mapply(FUN = as.POSIXct, GPS_programmed_start_datetime_local, tz = paste0('Etc/GMT', UTC_LocalTime_offset_hours), format = '%m/%d/%Y %H:%M') %>%
           as.POSIXct(origin = '1970-01-01 00:00.00 UTC', tz = 'UTC') %>% as.numeric)

# Reformat metadata to one row per deployment
DeploymentMetadataMinusTDRSettings <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           Year = Year, 
           DeploySession = DeplSess,
           GPSStart = GPS_programmed_start_datetime_local2,
           GPSInterval = GPS_Interval_seconds) %>%
  summarize(Recovered = any(Tagging_Event == 'R'),
            GPSDeployed = any(GPS_Y_N == 'Y'),
            GPSNotes = paste(GPS_notes, collapse = ''),
            TDRDeployed = any(TDR_Y_N == 'Y'),
            Processor = paste(Processor, collapse = '; '),
            Notes = paste(Notes, collapse = '; ')) %>%
  ungroup %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'D') %>%
              select(DeployID = Deploy_ID, 
                     # NOTE: some entries have missing and/or different GPS/TDR IDs in deployment and 
                     # recovery. Since device ID has no impact on analysis, we will default to choosing
                     # the ID as recorded on deployment
                     GPSID = GPS_ID,
                     TDRID = TDR_ID,
                     UTCDeployed = UTC)) %>%
  left_join(OriginalMetadata %>%
              filter(Tagging_Event == 'R') %>%
              select(DeployID = Deploy_ID, 
                     UTCRecovered = UTC,
                     GPSRecovered = GPS_TagRecov,
                     GPSFile = GPS_Track_File,
                     TDRRecovered = TDR_TagRecov,
                     TDRFile = TDR_File))

# Read TDR settings from CEFAS files
TDRSettings <- foreach(deployid = DeploymentMetadataMinusTDRSettings$DeployID, tdrfile = DeploymentMetadataMinusTDRSettings$TDRFile, .combine = rbind) %do% {
  if(tdrfile == '' || is.na(tdrfile)) return(NULL)
  
  cefas_file <- file.path('dive_identification',
                          '1_CEFAS_output',
                          sprintf('%s.CSV', tdrfile))
  cefas_contents <- readLines(cefas_file)
  
  start <- cefas_contents[grep(pattern = 'Start Date,Start Time,Stop Date,Stop Time,Logging Rate,sensors,Resolution,Fast Rate', x = cefas_contents) + 1] %>%
    strsplit(',') %>% 
    unlist %>% 
    `[`(1:2) %>%
    paste(collapse = ' ') %>%
    as.POSIXct(format = '%d/%m/%y %H:%M:%S', tz = 'Etc/GMT-10')
  
  fastlog.rate <- cefas_contents %>%
    grep(pattern = 'Fast rate [0-9\\.]+', x = ., value = TRUE) %>%
    first %>%
    sub('Fast rate ([0-9\\.]+)', '\\1', x = .) %>%
    as.numeric
  
  threshold <- cefas_contents %>%
    grep(pattern = 'Dive Termination Depth = [01]m', x = ., value = TRUE) %>%
    first %>%
    sub('Dive Termination Depth = ([01])m', '\\1', x = .) %>%
    as.numeric
  
  wetdry <- cefas_contents %>%
    grep(pattern = 'Table of Wet Times follows', x = .) %>%
    length > 0
  
  data.frame(DeployID = deployid,
             TDRStart = start,
             TDRInterval = fastlog.rate,
             TDRThreshold = threshold,
             TDRWetDry = wetdry)
}

# Join metadata with TDR settings and rearrange columns
DeploymentMetadata <- left_join(DeploymentMetadataMinusTDRSettings, TDRSettings) %>%
  select(DeployID, DeploySession, Recovered, UTCDeployed, UTCRecovered, GPSDeployed, GPSRecovered, GPSID, GPSStart, GPSInterval, GPSNotes, GPSFile, TDRDeployed, TDRRecovered, TDRID, TDRStart, TDRInterval, TDRWetDry, TDRThreshold, TDRFile, Notes) %>%
  as.data.frame

# Extract bird metadata
BirdMetadata <- OriginalMetadata %>%
  filter(!is.na(Deploy_ID),
         Tagging_Event != 'N') %>%
  group_by(DeployID = Deploy_ID,
           FieldID = FieldID,
           BandNumber = BandNo,
           Country = Country,
           Island = Island,
           Site = Site,
           SubColony = SubCol,
           SubColonyCode = SubCol_Code,
           Species = Species,
           NestNumber = NestNo,
           Age = Age,
           NestLatitude = Nest_Lat_DD,
           NestLongitude = Nest_Long_DD,
           ColonyLatitude = Col_Lat_DD,
           ColonyLongitude = Col_Long_DD) %>%
  summarize(BloodCard = any(Blood_FTA == 'Y'),
            BloodVial = any(Blood_vial == 'Y'),
            Feathers = any(Feathers == 'Y'),
            Diet = any(Diet == 'Y'),
            Sex = first(Sex[Sex != 'U']),
            HowSexed = first(How_Sexed[Sex != 'U']),
            RLBand = paste(R_L, collapse = ''),
            Notes = paste(Notes, collapse = '; ')) %>%
  ungroup %>%
  mutate(Sex = ifelse(is.na(Sex), 'U', Sex),
         HowSexed = ifelse(is.na(HowSexed), 'N', HowSexed)) %>%
  as.data.frame

# Gather dive data
ValidBRBODives <- dir('dive_identification/5a_brbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

BRBODive <- lapply(dir('dive_identification/4a_brbo_dive_data/', full.names = TRUE),
                   read.csv,
                   stringsAsFactors = FALSE) %>%
  rbindlist %>%
  semi_join(ValidBRBODives) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC') %>% as.numeric) %>%
  as.data.frame

ValidRFBODives <- dir('dive_identification/5b_rfbo_dive_plots/Dives') %>%
  sub('.png', '', ., fixed = TRUE) %>%
  strsplit('_') %>%
  unlist %>%
  as.numeric %>%
  matrix(ncol = 3, byrow = TRUE, dimnames = list(NULL, c('DeployID', 'EventID', 'DiveID'))) %>%
  data.table

RFBODive <- lapply(dir('dive_identification/4b_rfbo_dive_data/', full.names = TRUE),
                   read.csv,
                   stringsAsFactors = FALSE) %>%
  rbindlist %>%
  semi_join(ValidRFBODives) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC') %>% as.numeric) %>%
  as.data.frame

Dive <- rbind(BRBODive, RFBODive)

# Gather track and trip data
BRBOTracks <- read.csv('trackcode/gps/All_tracks/BRBO_1.5_trips_annotated.csv') %>%
  transmute(DeployID = Deploy_ID,
            UTC = as.POSIXct(UTC, tz = 'UTC'),
            Latitude,
            Longitude,
            TripID = trip_no)

RFBOTracks <- read.csv('trackcode/gps/All_tracks/RFBO_1.5_trips_annotated.csv') %>%
  transmute(DeployID = Deploy_ID,
            UTC = as.POSIXct(UTC, tz = 'UTC'),
            Latitude,
            Longitude,
            TripID = trip_no)

Track <- rbind(BRBOTracks, RFBOTracks)

## Remove duplicate track points. Temporary workaround until trip breaker interpolation bug is fixed.
DropTrackPoints <- Track %>% 
  group_by(DeployID, UTC) %>% 
  summarize(N = n()) %>% 
  ungroup %>% 
  filter(N == 2) %>% 
  merge(Track, by = c('DeployID', 'UTC')) %>% 
  arrange(DeployID, UTC, TripID) %>% 
  mutate(Drop = row_number() %% 2 == 0) %>% 
  filter(Drop) %>% 
  dplyr::select(-Drop)
Track <- anti_join(Track,
                   DropTrackPoints)
Trip <- read.csv('trackcode/gps/All_tracks/AllSpecies_tripInfo_QAQC.csv',
                 stringsAsFactors = FALSE) %>%
  select(DeployID = Deploy_ID,
         TripID = trip_no,
         Begin = tripSt,
         End = tripEnd,
         BeginComplete = tripStComp, 
         EndComplete = tripEndComp,
         Duration = duration.hrs,
         Flag:Notes) %>%
  mutate(Begin = as.POSIXct(Begin, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric,
         End = as.POSIXct(End, tz = 'UTC', format = '%m/%d/%Y %H:%M') %>% as.numeric) %>%
  as.data.frame

# Rediscretize track data
## We have to edit how as.ltraj calculates dx and dy because we're using lat/lon, not UTM.
my.as.ltraj <- function (xy, date = NULL, id, burst = id, typeII = TRUE, slsp = c("remove", "missing"), 
                         infolocs = data.frame(pkey = paste(id, date, sep = "."), row.names = row.names(xy))) 
{
  if (typeII) {
    if (!inherits(date, "POSIXct")) 
      stop("For objects of type II,\n date should be of class \"POSIXct\"")
  }
  else {
    date <- 1:nrow(xy)
  }
  if (length(date) != nrow(xy)) 
    stop("date should be of the same length as xy")
  slsp <- match.arg(slsp)
  if (!is.null(infolocs)) {
    if (nrow(infolocs) != nrow(xy)) 
      stop("infolocs should have the same number of rows as xy")
  }
  if (length(id) == 1) 
    id <- factor(rep(as.character(id), nrow(xy)))
  if (length(id) != nrow(xy)) 
    stop("id should be of the same length as xy, or of length 1")
  if (min(table(id)) == 0) 
    stop("some id's are not present in the data")
  if (length(burst) == 1) 
    burst <- factor(rep(as.character(burst), nrow(xy)))
  if (length(burst) != nrow(xy)) 
    stop("burst should be of the same length as xy, or of length 1")
  if (min(table(burst)) == 0) 
    stop("some bursts are not present in the data")
  id1 <- factor(id)
  burst1 <- factor(burst)
  if (!all(apply(table(id1, burst1) > 0, 2, sum) == 1)) 
    stop("one burst level should belong to only one id level")
  x <- xy[, 1]
  y <- xy[, 2]
  res <- split(data.frame(x = x, y = y, date = date, row.names = row.names(xy)), 
               burst)
  liid <- split(id, burst)
  if (!is.null(infolocs)) 
    linfol <- split(infolocs, burst)
  if (!is.null(infolocs)) 
    linfol <- lapply(1:length(linfol), function(j) linfol[[j]][order(res[[j]]$date), 
                                                               , drop = FALSE])
  res <- lapply(res, function(y) y[order(y$date), , drop = FALSE])
  rr <- any(unlist(lapply(res, function(x) (length(unique(x$date)) != 
                                              length(x$date)))))
  if (rr) 
    stop("non unique dates for a given burst")
  x <- xy[, 1]
  y <- xy[, 2]
  resbb <- split(data.frame(x = x, y = y, date = date, row.names = row.names(xy)), 
                 id1)
  rr <- any(unlist(lapply(resbb, function(x) (length(unique(x$date)) != 
                                                length(x$date)))))
  if (rr) 
    stop("non unique dates for a given id")
  foo <- function(x) {
    x1 <- x[-1, ]
    x2 <- x[-nrow(x), ]
    # Original distance calculations (assumes UTM)
    # dist <- c(sqrt((x1$x - x2$x)^2 + (x1$y - x2$y)^2), NA)
    # R2n <- (x$x - x$x[1])^2 + (x$y - x$y[1])^2
    # dx <- distHaversine() c(x1$x - x2$x, NA)
    # dy <- c(x1$y - x2$y, NA)
    # abs.angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx))
    # New distance calculation (uses lat/lon)
    dist <- c(distHaversine(x1[,c('x','y')], x2[,c('x','y')]), NA)
    R2n <- distHaversine(x[1,c('x','y')], x[,c('x','y')])
    dt <- c(unclass(x1$date) - unclass(x2$date), NA)
    abs.angle <- c(bearing(x1, x2), NA)
    dx <- dist * cos(bearing)
    dy <- dist * sin(bearing)
    so <- cbind.data.frame(dx = dx, dy = dy, dist = dist, 
                           dt = dt, R2n = R2n, abs.angle = abs.angle)
    return(so)
  }
  speed <- lapply(res, foo)
  res <- lapply(1:length(res), function(i) cbind(res[[i]], 
                                                 speed[[i]]))
  ang.rel <- function(df, slspi = slsp) {
    ang1 <- df$abs.angle[-nrow(df)]
    ang2 <- df$abs.angle[-1]
    if (slspi == "remove") {
      dist <- c(sqrt((df[-nrow(df), "x"] - df[-1, "x"])^2 + 
                       (df[-nrow(df), "y"] - df[-1, "y"])^2), NA)
      wh.na <- which(dist < 1e-07)
      if (length(wh.na) > 0) {
        no.na <- (1:length(ang1))[!(1:length(ang1)) %in% 
                                    wh.na]
        for (i in wh.na) {
          indx <- no.na[no.na < i]
          ang1[i] <- ifelse(length(indx) == 0, NA, ang1[max(indx)])
        }
      }
    }
    res <- ang2 - ang1
    res <- ifelse(res <= (-pi), 2 * pi + res, res)
    res <- ifelse(res > pi, res - 2 * pi, res)
    return(c(NA, res))
  }
  rel.angle <- lapply(res, ang.rel)
  res <- lapply(1:length(res), function(i) data.frame(res[[i]], 
                                                      rel.angle = rel.angle[[i]]))
  res <- lapply(1:length(res), function(i) {
    x <- res[[i]]
    attr(x, "id") <- as.character(liid[[i]][1])
    attr(x, "burst") <- levels(factor(burst))[i]
    return(x)
  })
  if (!is.null(infolocs)) {
    res <- lapply(1:length(res), function(i) {
      x <- res[[i]]
      y <- linfol[[i]]
      row.names(y) <- row.names(x)
      attr(x, "infolocs") <- y
      return(x)
    })
  }
  class(res) <- c("ltraj", "list")
  attr(res, "typeII") <- typeII
  attr(res, "regular") <- is.regular(res)
  return(res)
}
assignInNamespace('as.ltraj', my.as.ltraj, ns = 'adehabitatLT')
RediscretizedTrack <- as.ltraj(xy = Track[,c('Longitude', 'Latitude')],
         date = Track$UTC,
         id = Track$DeployID,
         burst = paste(Track$DeployID, Track$TripID, sep = '-')) %>%
  redisltraj(u = 180, 
             samplex0 = TRUE, 
             type = 'time')

Track <- mutate(Track, UTC = as.numeric(UTC))  

# Test primary keys
pkeys <- list(DeploymentMetadata = 'DeployID',
     BirdMetadata = 'DeployID',
     Dive = c('DeployID', 'DiveID'),
     Track = c('DeployID', 'UTC'),
     Trip = c('DeployID', 'TripID'))
valid.pkeys <- sapply(seq_along(pkeys), function(i) {
  data <- names(pkeys)[i] %>% get
  keys <- pkeys[[i]]
  values.per.key <- data %>%
    group_by_(.dots = keys) %>%
    summarize(N = n())
  all(values.per.key$N == 1)
})

# Create SQLite database
if(all(valid.pkeys)) {
  MHIdb <- dbConnect(SQLite(), 'Hawaii_data/MHI_GPS_TDR.sqlite')
  dbWriteTable(MHIdb, 'DeploymentMetadata', DeploymentMetadata, overwrite = TRUE)
  dbWriteTable(MHIdb, 'BirdMetadata', BirdMetadata, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Dive', Dive, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Track', Track, overwrite = TRUE)
  dbWriteTable(MHIdb, 'Trip', Trip, overwrite = TRUE)
  dbDisconnect(MHIdb)
} else {
  error("Error in primary keys. Check valid.pkeys")
}

