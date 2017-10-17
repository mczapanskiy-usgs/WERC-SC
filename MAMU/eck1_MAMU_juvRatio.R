### JUVENILE RATIO
# this script calculates the ratio of HY to AHY MAMU on surveys

## SET WORKING DIRECTORY
setwd("~/WERC-SC/MAMU")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)
library(magrittr)

## READ IN OBSERVATIONS FILE
read.csv('~/WERC-SC/MAMU/MAMU_Obs_2017_Distance.csv',
         stringsAsFactors = FALSE) -> obs 

## SELECT ONLY MAMU DETECTIONS WITHIN JUVENILE TIMEFRAME (JULY 10 - AUGUST 23)
## AND CALCUALTE ADJUSTED HY AND AHY DETECTIONS 
obs_mamu <- obs %>% 
  select(-OBJECTID, -Field1) %>% 
  distinct() %>% 
  filter(SPP == "MAMU") %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"), 
         jdate = yday(date)) %>% # add column with Julian Date
  filter(jdate > 191, jdate < 235) %>% # select date between JD 192 - 234 (time frame when juveniles are detected)
  mutate(HYadj = HY/(-1.5433 + 0.0098 * jdate), # apply regression to HY birds to account for those that haven't fledged by survey date
         AHYadj = AHY/(1 - (18.7145545 - 0.18445455 * jdate  +  0.00045455 * (jdate^2)))) # apply regression for AHY birds that are still incubting by survey date

## CALUCLATE JUVENILE RATIO FROM ADJUSTED HY AND AHY DETECTION VALUES
juvRatio <- obs_mamu %>% 
  group_by(YEAR_) %>% 
  summarise(n = n_distinct(YEAR_),
            HYsum = sum(HY, na.rm = TRUE),                  # annual total number of HY
            HYvar = var(HY, na.rm = TRUE),                  # annual variance in HY
            HYave = mean(HY, na.rm = TRUE),                 # annual mean  number of HY
            AHYsum = sum(AHY, na.rm = TRUE),                # annual total number of AHY
            AHYvar = var(AHY, na.rm = TRUE),                # annual variance in AHY
            AHYave = mean(AHY, na.rm = TRUE),               # annual mean number of AHY
            cov = cov(HY, AHY),                             # annual covariance of adjusted 
            HYsumAdj = sum(HYadj, na.rm = TRUE),            # annual total number of HY per year, adjusted
            HYvarAdj = var(HYadj, na.rm = TRUE),            # annual variance in adjusted number of HY 
            HYaveAdj = mean(HYadj, na.rm = TRUE),           # annual mean adjusted number of HY 
            AHYsumAdj = sum(AHYadj, na.rm = TRUE),          # annual total number of AHY, adjusted
            AHYvarAdj = var(AHYadj, na.rm = TRUE),          # annual variance in adjusted number of AHY
            AHYaveAdj = mean(AHYadj, na.rm = TRUE),         # annual mean adjusted number of AHY
            covAdj = cov(HYadj, AHYadj)) %>%                # annual covariance of adjusted HY and AHY
  mutate(juvRat = HYsum/AHYsum, # juvenile ratio per survey, adjusted 
         juvRatAdj = HYsumAdj/AHYsumAdj, # juvenile ratio per survey
         juvRatVar = (1/n)*(((HYvar/AHYave^2)+(?^2*AHYvar)/AHYave^4)-(((2*HYave)*cov)/AHYave^3))
         )
  