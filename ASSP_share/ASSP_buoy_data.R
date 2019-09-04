
# Download NDBC buoy data 

install.packages(c('ncdf4','httr','lubridate','gridGraphics','parsedata','mapdata','ggplot2','RColorBrewer',
                   'devtools','grid'))
require(ncdf4)
require(httr)
require(lubridate)
require(gridGraphics)
require(parsedata)
require(mapdata)
require(ggplot2)
require(RColorBrewer)
require(devtools) 
require(grid)
library(dplyr)


devtools::install_github("ropensci/rerddap")
devtools::install_github("ropensci/plotdap")
devtools::install_github("rmendels/rerddapXtracto")
library(rerddap)
library(plotdap)
library(rerddapXtracto)

# Set working directory with ASSP_mistnetting_dates.csv file. 

setwd("C:/Users/ameli/Dropbox/USGS/Projects/ASSP/Analysis")

# download a temporal and spatial subset of Buoy data using tabledap in the
# rerddap package.
# list of all erddap collection datasets: 
# https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000

#description of the dataset (cwwcNDBCMet) and variables: 
# https://coastwatch.pfeg.noaa.gov/erddap/tabledap/cwwcNDBCMet.html

# add new variables using code names from the website
# Using wd (Wind Direction, degrees_true), wspd (Wind Speed, m s-1), 
# gst (Wind Gust Speed, m s-1), atmp (Air Temperature, degree_C), 
# and dewp (Dewpoint Temperature, degree_C). 

buoys_all <- tabledap(
  'cwwcNDBCMet', 
  fields=c('station', 'latitude',  'longitude', 'time', 'wd', 'wspd', 'gst', 'atmp', 'dewp'), 
  'time>=1994-01-01',   'time<=2018-12-31', 
  'latitude>=30.86','latitude<=41.75', 'longitude>=-128','longitude<=-116'
)

# Read ASSP_mistnetting_dates.csv file with mistnetting dates from CPUE data. Filter out "ND" for 
# sessions with no date recorded.

dates <- read.csv("ASSP_mistnetting_dates.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  filter(date !="ND") %>%
  transform(date = as.Date(date, format = "%m/%d/%Y"))

dates <- dates$date

str(dates)

str(buoys_all)

# Filter out three buoys. 

buoy <- buoys_all %>% filter(station %in% c(46025, 46053, 46054))

#clean up output (numeric and date formats)

buoy.df <-data.frame(station = buoy$station,
                 longitude = as.numeric(buoy$longitude),
                 latitude = as.numeric(buoy$latitude),
                 time = strptime(buoy$time, "%Y-%m-%dT%H:%M:%S"),
                 wd = as.numeric(buoy$wd),
                 wspd = as.numeric(buoy$wspd),
                 gst = as.numeric(buoy$gst),
                 atmp = as.numeric(buoy$atmp),
                 dewp = as.numeric(buoy$dewp)) %>%
  transform(date = date(time)) %>%
  transform(year = year(date))%>%
  filter(date %in% dates)

# Check that all stations were retrieved
unique.sta <- unique(buoy$sta)
n.sta <- length(unique.sta)

# Check the number of rows (dates)
date_num <- buoy.df %>% distinct(date) %>% nrow()

# Check the date range
date_range <- buoy.df %>% 
  summarize(min_date = min(date, na.rm = T), max_date = max(date, na.rm = T))

# Buoy data is hourly, so use either subset for one hour or take average daily

# Taking data points at midnight. 
avg_hr <- subset(buoy.df,hour(time)==00)

# Taking 24 hour average.
avg_daily <- buoy.df %>%
  group_by(year, date, station) %>%
  summarize(longitude = mean(longitude), 
            latitude = mean(latitude), 
            wd = mean(wd),
            wspd = mean(wspd),
            gst = mean(gst),
            atmp = mean(atmp),
            dewp = mean(dewp))

# visual check. 
ggplot(avg_daily, aes(date, wd)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()

ggplot(avg_daily, aes(date, wspd)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()

ggplot(avg_daily, aes(date, gst)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()

ggplot(avg_daily, aes(date, atmp)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()

ggplot(avg_daily, aes(date, dewp)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()


# write data to a csv so it can be imported and used for analyses later
# will be sent to working directory set at beginning of script. Can set another wd. 

write.csv(avg_daily, 'ASSP_mistnetting_avg_daily_buoy.csv')

write.csv(avg_hr, 'ASSP_mistnetting_avg_hr_buoy.csv')
