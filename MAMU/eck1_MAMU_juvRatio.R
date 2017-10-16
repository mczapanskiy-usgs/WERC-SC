### JUVENILE RATIO
# this script calculates the ratio of HY to AHY MAMU on surveys

## set wd
setwd("~/WERC-SC/MAMU")

## load libraries
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)

## read in file
read.csv('~/WERC-SC/MAMU/MAMU_Obs_2017_Distance.csv',
         stringsAsFactors = FALSE) -> obs 
  


obs_juv <- obs %>% 
  filter(SPP == "MAMU") %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"), 
         jdate = yday(date)) %>% # add column with Julian Date
  filter(jdate > 191, jdate < 235) %>% # select date between JD 192 - 234 (time frame when juveniles are detected)

         
juvRatio <- obs_juv %>% 
  mutate(HYreg = (-1.5433 + 0.0098 * jdate), 
         HYadj = (HY/HYreg), 
         AHYreg = 1 - (18.7145545 - 0.18445455 * jdate  +  0.00045455 * (jdate^2)),
         AHYadj = (AHY/AHYreg),
         juvRat = sapply()