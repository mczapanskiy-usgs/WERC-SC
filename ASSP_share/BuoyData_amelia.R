
# Download NDBC buoy data 

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

# devtools::install_github("ropensci/rerddap")
# devtools::install_github("ropensci/plotdap")
# devtools::install_github("rmendels/rerddapXtracto")
library(rerddap)
library(plotdap)
library(rerddapXtracto)

# download a temporal and spatial subset of Buoy data using tabledap in the
# rerddap package.
# list of all erddap collection datasets: 
# https://coastwatch.pfeg.noaa.gov/erddap/info/index.html?page=1&itemsPerPage=1000

#description of the dataset (cwwcNDBCMet) and variables: 
# https://coastwatch.pfeg.noaa.gov/erddap/tabledap/cwwcNDBCMet.html

# add new variables using code names from the website
# insert new variables after 'wtmp' in parentheses and increase date range
buoys_all <- tabledap(
  'cwwcNDBCMet', 
  fields=c('station', 'latitude',  'longitude', 'time', 'wtmp'), 
  'time>=2018-01-01',   'time<=2018-12-31', 
  'latitude>=30.86','latitude<=41.75', 'longitude>=-128','longitude<=-116',
  'wtmp>0'
)

buoy <- buoys_all %>% filter(station %in% c(46025, 46053, 46054))

#clean up output (numeric and date formats)
buoy.df <-data.frame(station = buoy$station,
                 longitude = as.numeric(buoy$longitude),
                 latitude = as.numeric(buoy$latitude),
                 time = strptime(buoy$time, "%Y-%m-%dT%H:%M:%S"),
                 sst = as.numeric(buoy$wtmp)) %>%
  transform(date = date(time)) %>%
  transform(year = year(date))

# Check that all stations were retrieved
unique.sta <- unique(buoy$sta)
n.sta <- length(unique.sta)

# Check the number of rows (dates)
date_num <- buoy.df %>% distinct(date) %>% nrow()

# Check the date range
date_range <- buoy.df %>% 
  summarize(min_date = min(date, na.rm = T), max_date = max(date, na.rm = T))

# Buoy data is hourly, so either subset for one hour or take average daily
avg_hr <- subset(buoy.df,hour(time)==22)

# length of data frame will be (theoretically) 3 stations * 365 days
avg_daily <- buoy.df %>%
  group_by(year, date, station) %>%
  summarize(longitude = mean(longitude), 
            latitude = mean(latitude), 
            sst = mean(sst))

#visual check
ggplot(avg_daily, aes(date, sst)) +
  geom_point() + geom_line() +
  facet_wrap(~station) +
  theme_classic()

#write data to a csv so it can be imported and used for analyses later
#first check/set your working directory so you know where to find the csv file
#setwd('filepath')
write.csv(avg_daily, 'avg_daily_buoy.csv')
