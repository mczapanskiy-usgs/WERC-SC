library('plyr')
library('dplyr')

read.CEFAS <- function(CEFAS.file, output.file, wet.dry) {
  # Open log file
  file.contents <- readLines(CEFAS.file)
  
  # NOTE ON EVENT IDS
  # For Fast Log points, event id corresponds to index of Fast Log i.e. 1, 2, ..., count(FastLogs)
  # Data Block 0 points have event id 0
  # Example Data Block 0 data point: 13/06/14 12:00:00,-0.82
  # Example Fast Log data point: 15/06/14 06:45:42.100,-1.16
  
  data.point.pattern <- '[[:digit:]/]{8} [[:digit:]:]{8}[[:digit:].]*,[-[:digit:].]+'
  data.points <- data.frame(Row = grep(data.point.pattern, file.contents))
  
  ### Data Block 0 + Fast Logs produce points with pressure readings (explicit points) ###
  # NOTE: POSIXct handles fractional seconds unintuitively. Adding .01 is a workaround to ensure the 
  # correct value.
  
  # Verify file has data
  if(nrow(data.points) == 0) return(NA)
  
  # Extract date/time and pressure
  datetimepressure <- ldply(strsplit(file.contents[data.points$Row], ','),
                            function(parts) {
                              data.frame(UTC = as.POSIXct(parts[1], 
                                                          tz = "US/Hawaii", 
                                                          format = "%d/%m/%y %H:%M:%OS") + .01,
                                         Pressure = as.numeric(parts[2]))
                            })
  data.points <- cbind(data.points, datetimepressure)
  
  # Event bounds and IDs
  event.bounds <- data.frame(LowerBound = grep("Data points available", file.contents))
  event.bounds$EventId <- 0:(nrow(event.bounds) - 1)
  
  # Join data points with bounds to get event ids
  data.points$EventId <- event.bounds$EventId[findInterval(data.points$Row, event.bounds$LowerBound)]
  
  if(wet.dry) {
    ### Wet/Dry table produces points without pressure readings (implicit points) ###
    
    # Pull out wet and dry events
    # NOTE: POSIXct handles fractional seconds unintuitively. Adding .01 is a workaround to ensure the 
    # correct value.
    # Example Wet Times event: 15/06/14 05:43:37.300       15/06/14 06:06:55.000
    # Implied wet/dry points get negative event ids by event i.e. -1, -2, ..., -1 * count(Wet/Dry Events)
    wetdry.event.pattern <- '[[:digit:]/]{8} [[:digit:]:]{8}\\.[[:digit:]]{3}       [[:digit:]/]{8} [[:digit:]:]{8}\\.[[:digit:]]{3}'
    wetdry.events <- data.frame(Row = grep(wetdry.event.pattern, file.contents))
    wetdry.events$Wet <- as.POSIXct(x = substr(file.contents[wetdry.events$Row], 1, 21), 
                                    tz = "US/Hawaii", 
                                    format = "%d/%m/%y %H:%M:%OS") + .01
    wetdry.events$Dry <- as.POSIXct(x = substr(file.contents[wetdry.events$Row], 29, 49), 
                                    tz = "US/Hawaii", 
                                    format = "%d/%m/%y %H:%M:%OS") + .01
    wetdry.events$EventId <- 1:nrow(wetdry.events) * -1
  
    # Expand a wet/dry event into points at 1s intervals with NA pressure
    expand.wetdry.event <- function(Wet, Dry, EventId, Row) {
      wet <- round(x = Wet, units = "secs") + .01
      dry <- round(x = Dry, units = "secs") + .01
      duration <- ceiling(as.numeric(difftime(dry, wet, units = 'secs')))
      implied.points <- data.frame(UTC = wet + 0:duration)
      implied.points$EventId <- EventId
      implied.points$Row <- Row
      implied.points$Pressure <- NA
      implied.points
    }
    
    # Apply the expansion function to each row of the wetdry.events data frame\
    wetdry.points <- mdply(wetdry.events, expand.wetdry.event) %>%
      select(Row, UTC, Pressure, EventId)
    
    # Concatenate explicit data points (Data Block 0, FastLogs) with implicit data points (wet/dry events)
    data.points <- rbind(data.points, wetdry.points)
  }
  
  # Sort chronologically
  data.points <- arrange(data.points, UTC)
  
  # Write results to file
  # Drop the extra .01s from dates
  data.points$UTC <- strftime(x = data.points$UTC, format = "%Y-%m-%d %H:%M:%OS1", tz = "UTC")
  write.csv(x = data.points, file = output.file, row.names = FALSE)
  
  output.file
}
