library(oce)

# Moon index (Mi) is a proxy for the amount of moonlight experienced overnight.
# It has units of degree*hours
# For a given date and location, it is calculated as the product of 
# disc illumination (%), diameter (degrees), and moon time (hours). 
# Where moon time is calculated as:
# min(moonset, sunrise) - max(moonrise, sunset) and rise/set is when the 
# angle of the sun/moon over the horizon is 0 degrees.
# Parameters:
# date = Date object
# lon = longitude in decimal degrees
# lat = latitude in decimal degrees
moonIndex <- function(date, lon, lat) {
  if(class(date) != 'Date') stop('date must be a Date object')
  
  d <- data.frame(t = ISOdate(year(date), month(date), day(date), tz = 'UTC') + seq(0, 24*3600, length = 24*3600),
                  moonalt = moonAngle(t, lon, lat)$altitude,
                  sunalt = sunAngle(t, lon, lat)$altitude) %>%
    mutate(absma = abs(moonalt), 
           abssa = abs(sunalt),
           moonslope = sign(lead(moonalt) - moonalt),
           sunslope = sign(lead(sunalt) - sunalt))
  
  moonrise <- d %>%
    filter(moonslope > 0) %>%
    arrange(absma) %>%
    slice(1) %>%
    select(t) %>%
    first
  
  moonset <- d %>%
    filter(moonslope < 0) %>%
    arrange(absma) %>%
    slice(1) %>%
    select(t) %>%
    first
  
  sunrise <- d %>%
    filter(sunslope > 0) %>%
    arrange(abssa) %>%
    slice(1) %>%
    select(t) %>%
    first
  
  sunset <- d %>%
    filter(sunslope < 0) %>%
    arrange(abssa) %>%
    slice(1) %>%
    select(t) %>%
    first
  
  browser()
  
  diameter <- moonAngle(d$t[1], lon, lat)$diameter
  illumination <- moonAngle(d$t[1], lon, lat)$illuminatedFraction
  moontime <- difftime(max(moonrise, sunset), min(moonset, sunrise), units = 'hours') %>% as.numeric

  diameter * illumination * moontime
}
